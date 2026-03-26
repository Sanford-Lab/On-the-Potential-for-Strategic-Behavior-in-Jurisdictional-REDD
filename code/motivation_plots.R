library(tidyverse)
library(here)
library(rio)
library(scales)
library(fuzzyjoin)
library(stringi)
palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "dark_blue" = "#00008b",
                "green" = "#00ab5b",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520",
                "purple" = "#880ED4")

source(here::here("code", "functions.R"))

fig_dir <- here::here("figs")
fig_dir_pdf <- here::here("figs_pdf")
dir.create(fig_dir_pdf, showWarnings = FALSE)
results_dir <- here::here("results")

load("data/processed/forest_loss.Rdata")

intervention_names <- readRDS("data/processed/ARTintervention_names_new.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Hansen data - find ART eligible
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ART_eligible_ADM1 <- dat_long %>% filter(treecover_2000 >= 2.5e6*10000) %>% filter(level == "ADM1")

ART_eligible_ADM2_ID <- dat_long %>% filter(year == 15, level == "ADM2") %>%
    filter(ADM1_CODE %in% ART_eligible_ADM1$ADM1_CODE) %>% select(ID) %>% pull()

ART_eligible <- dat_long %>%
    filter(level == "ADM2",
           ID %in% ART_eligible_ADM2_ID
    )%>%
    rbind(dat_long %>% filter(level == "ADM0")) %>%
    rbind(ART_eligible_ADM1) %>%
    mutate(ART_eligible = 1) %>% 
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, ART_eligible)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        ART_eligible ) %>%
    mutate(ART_eligible = replace_na(ART_eligible, 0))

# Match interventions back to dat_long
no_intervention_dat <- dat_long %>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    filter(is.na(intervention_name))%>%
    # Convert units to hectares
    mutate(cum_loss = cum_loss/10000,
           loss = loss /10000,
           treecover_2000 = treecover_2000/10000,
           share_area = treecover_2000 / (area/10000)
    )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### MORAL HAZARD FIGURE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# limit to jurisdiction of interest
fake_juris_spike <- no_intervention_dat %>%
    filter(ADM0_NAME == "Portugal", level == "ADM0")%>%
    mutate(year = year + 2000,
           scenario = "anticipatory spike")
placebo_year = 2018
spike_baseline = fake_juris_spike %>% filter(year < placebo_year & year >= (placebo_year - 5)) %>% select(loss) %>% pull() %>% mean() 

# limit to jurisdiction of interest
fake_juris_nospike <- fake_juris_spike %>%
    mutate(loss = ifelse(year == placebo_year - 1, loss/3, loss),
           loss = ifelse(year > placebo_year, loss*1.2, loss),
           scenario = "no anticipation")
nospike_baseline <- fake_juris_nospike %>% filter(year < placebo_year & year >= (placebo_year - 5)) %>% select(loss) %>% pull() %>% mean() 

fake_juris <- rbind(fake_juris_spike, fake_juris_nospike)%>%
    filter(year >= 2008)%>%
    mutate(loss = ifelse(year == placebo_year - 2, loss/1.5, loss),
           loss = ifelse(year == placebo_year , loss/1.5, loss))

baseline_period_shade <- annotate("rect", 
                                  xmin = placebo_year - 5,
                                  xmax = placebo_year - 0.25, 
                                  ymin = -Inf, ymax = Inf,
                                  alpha = .5,fill = palette$light_grey)

moralhazard_plot <- ggplot(fake_juris, aes(x = year, y = loss, color = scenario, linetype = scenario))+
    baseline_period_shade +
    geom_vline(xintercept = placebo_year, linetype = "dashed")+
    geom_line(linewidth = .75)+
    geom_segment(aes(y = nospike_baseline, x = placebo_year, yend = nospike_baseline, xend = placebo_year + 4), color = palette$gold, linewidth = 1.2, linetype = "dashed") +
    geom_segment(aes(y = spike_baseline, x = placebo_year, yend = spike_baseline, xend = placebo_year + 4), color = palette$gold, linewidth = 1.2) +
    geom_segment(aes(x = placebo_year+3, y = (nospike_baseline + .05*nospike_baseline), xend = placebo_year+3, yend = (spike_baseline - .04*spike_baseline)),
                 arrow = arrow(length = unit(0.25, "cm")), color = "black")+
    annotate("text", x = placebo_year+3, y = (spike_baseline*1.25)
             , label = "Shift in\nbaseline",
             size = 3.5)+
    annotate("text", x = placebo_year-4, y = spike_baseline + 0.75*spike_baseline
             , label = "Anticipatory spike\nin deforestation",
             size = 3.5)+
    geom_segment(aes(x = placebo_year-4, y = (spike_baseline + .75*nospike_baseline), xend = placebo_year-2, yend = (spike_baseline + .5*nospike_baseline)),
                 arrow = arrow(length = unit(0.25, "cm")), color = "black")+
    theme_bw()+
    scale_y_continuous(labels = scientific)+
    scale_linetype_manual(values = c("solid", "dashed"))+
    scale_color_manual(values = c(palette$blue, palette$red))+
    scale_linewidth_manual(values = c(1, 0.5))+
    theme(
        legend.position = "none"
        #legend.title = element_blank()
        , plot.title = element_text(hjust = 0.5)
    )+ 
    ylab("Annual deforestation")+xlab("Year")
moralhazard_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### FUNCTION FOR SLOPE FIGS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_juris_plot <- function(juris_name, placebo_year, facet_var = "ADM1_NAME",
                            slope = TRUE, show_y = TRUE) {
    
    this_juris <- no_intervention_dat %>%
        filter(.data[[facet_var]] == juris_name, level %in% str_remove(facet_var, "_NAME")) %>%
        mutate(year = year + 2000)
    
    this_placebo_baseline <- this_juris %>%
        filter(year == placebo_year) %>%
        pull(baseline)
    
    this_juris <- this_juris %>%
        select(level, ID, year, ADM0_NAME, ADM1_NAME, ADM2_NAME, loss) %>%
        mutate(
            baseline = ifelse(
                year >= placebo_year & year <= placebo_year + 4,
                this_placebo_baseline / 10000,
                NA
            )
        ) %>%
        pivot_longer(c(loss, baseline),
                     names_to = "defor_group",
                     values_to = "defor")
    
    baseline_period_shade <- annotate(
        "rect",
        xmin = placebo_year - 5,
        xmax = placebo_year - 0.25,
        ymin = -Inf, ymax = Inf,
        alpha = .5, fill = palette$light_grey
    )
    
    if(slope == T) {
        p <- ggplot(this_juris,
                    aes(year, defor,
                        color = defor_group,
                        linewidth = defor_group)) +
            baseline_period_shade +
            geom_vline(xintercept = placebo_year, linetype = "dashed") +
            geom_smooth(
                data = subset(this_juris,
                              year >= placebo_year - 5 & year < placebo_year),
                method = "lm", se = FALSE,
                linewidth = 1,
                color = palette$green,
                linetype = "dashed"
            ) +
            geom_line() +
            geom_point(
                data = subset(
                    this_juris,
                    defor_group == "loss" &
                        year >= placebo_year - 5 &
                        year < placebo_year
                ),
                size = 2.1,
                stroke = 0,
                show.legend = FALSE
            ) +
            scale_color_manual(values = c(palette$gold, palette$blue)) +
            scale_linewidth_manual(values = c(1, 0.5)) +
            scale_y_continuous(labels = scales::comma)+
            theme_bw() +
            facet_grid(. ~ .data[[facet_var]]) +
            theme(
                legend.position = "none",
                strip.background = element_rect(fill = "grey85"),
                strip.text = element_text(size = 10)
            ) +
            xlab("Year")
    } else{
        ## values needed for arrow
        last_pre <- this_juris %>%
            filter(defor_group == "loss", year == placebo_year - 1)
        
        baseline_val <- this_placebo_baseline / 10000
        
        ## vertical arrow from baseline to last pre-treatment value (shortened)
        gap <- last_pre$defor - baseline_val
        offset <- 0.2 * abs(gap)   # padding at each end
        
        p <- ggplot(this_juris,
                    aes(year, defor,
                        color = defor_group,
                        linewidth = defor_group)) +
            baseline_period_shade +
            geom_vline(xintercept = placebo_year, linetype = "dashed") +
            ## extended horizontal baseline (pre + post)
            geom_segment(
                x = placebo_year - 5,
                xend = placebo_year + 4,
                y = baseline_val,
                yend = baseline_val,
                color = palette$gold,
                linewidth = 1.1
            ) +
            ## main lines
            geom_line() +
            ## points in the 5 pre-placebo years
            geom_point(
                data = subset(
                    this_juris,
                    defor_group == "loss" &
                        year >= placebo_year - 5 &
                        year < placebo_year
                ),
                size = 1.8,
                show.legend = FALSE
            ) +
            scale_color_manual(values = c(palette$gold, palette$blue)) +
            scale_linewidth_manual(values = c(1, 0.5)) +
            scale_y_continuous(labels = scales::comma)+
            theme_bw() +
            facet_grid(. ~ .data[[facet_var]]) +
            theme(
                legend.position = "none",
                strip.background = element_rect(fill = "grey85"),
                strip.text = element_text(size = 10)
            ) +
            xlab("Year")+
            ## vertical arrow from baseline to last pre-treatment value
            geom_segment(
                x = placebo_year - 1,
                xend = placebo_year - 1,
                y = baseline_val + sign(gap) * offset,
                yend = last_pre$defor - sign(gap) * offset,
                arrow = arrow(length = unit(0.25, "cm")),
                color = "black",
                linewidth = 0.5
            )+
            geom_point(
                data = last_pre,
                aes(x = year, y = defor),
                inherit.aes = FALSE,
                color = palette$red,
                size = 2.4
            )
    }
    
    if (show_y) {
        p <- p + ylab("Annual deforestation (ha)")
    } else {
        p <- p + theme(axis.title.y = element_blank())
    }
    
    p
}

p_boqueron <- make_juris_plot(
    juris_name = "Boqueron",
    placebo_year = 2015,
    facet_var = "ADM1_NAME",
    slope = TRUE,
    show_y = TRUE
)
p_boqueron

p_piaui <- make_juris_plot(
    juris_name = "Piaui",
    placebo_year = 2012,
    facet_var = "ADM1_NAME",
    slope = TRUE,
    show_y = FALSE
)
p_piaui

p_salta <- make_juris_plot(
    juris_name = "Salta",
    placebo_year = 2015,
    facet_var = "ADM1_NAME",
    slope = FALSE,
    show_y = TRUE
)
p_salta

p_belarus <- make_juris_plot(
    juris_name = "Belarus",
    placebo_year = 2017,
    facet_var = "ADM0_NAME",
    slope = FALSE,
    show_y = FALSE
)
p_belarus

library(patchwork)
design <- 
"1122
3344
#55#"
final_plot <-
    p_boqueron + p_piaui +
    p_salta + p_belarus +
    wrap_elements(full = moralhazard_plot) +
    plot_layout(
        design  = design,
        widths  = 1,
        heights = c(1, 1, 1.2)
    ) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))

final_plot

ggsave(
    filename = paste0(fig_dir, "/motivation_plots.png"),
    final_plot,
    width = 10,
    height = 8.5
)

ggsave(
    filename = paste0(fig_dir_pdf, "/motivation_plots.pdf"),
    final_plot,
    width = 10,
    height = 8.5
)

