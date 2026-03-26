library(tidyverse)
library(zoo)
library(tidyquant)
library(fixest)
library(here)
library(modelsummary)
library(kableExtra)
library(ggpubr)
library(grid)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "grey" = "grey50",
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
#load("data/processed/forest_loss.Rdata")

ARTintervention_names <- readRDS("data/processed/ARTintervention_names.rds")
Verraintervention_names <- readRDS("data/processed/Verraintervention_names.rds")

intervention_names <- rbind(ARTintervention_names, Verraintervention_names) %>%
    #filter(intervention_year <= 2023) %>%
    mutate(intervention = 1) %>%
    # Be more specific about which columns to keep distinct
    distinct(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level, .keep_all = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### Hansen data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For ART TREES eligible
dat_long_ART <- readRDS("data/processed/dat_long_ART.rds")
dat_long_ART_beta <- readRDS("data/processed/dat_long_ART_beta.rds")

dat_long_ART_full <- dat_long_ART %>% left_join(dat_long_ART_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
    mutate(
        loss = loss/10000,
        last_year_diff = last_year_diff * -1 / 10000,
        est_effect = est_effect / 10000,
        treecover_remaining = treecover_remaining / 10000,
        area = area / 10000,
        intercept = intercept / 10000,
        slope = slope / 10000,
        baseline = baseline / 10000,
        endline = endline / 10000
    )

dat_long_ART_full <- dat_long_ART_full %>% mutate(
    pct_effect = est_effect/treecover_remaining,
    pct_intercept = intercept/treecover_remaining,
    pct_slope = slope/treecover_remaining,
    pct_last_year_diff = last_year_diff/treecover_remaining,
    pct_change_effect = est_effect/baseline,
    pct_change_intercept = intercept/baseline,
    pct_change_slope = slope/baseline,
    pct_change_last_year_diff = last_year_diff/baseline
)%>%
    mutate_all(~ifelse(is.infinite(.), NA, .)) %>%
    filter(loss >1)%>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(intervention = replace_na(intervention, 0))


# dat_long_ART_full %>% filter(ADM0_NAME == "Guyana")

# plot_jurisdiction(codes = 37, year_implement = 2015, data = dat_long_ART_full)
# plot_jurisdiction(codes = 107, year_implement = 2015, data = dat_long_ART_full)

ART_eligible_full <- dat_long_ART_full %>% 
    filter(intervention == 0 | intervention_fund == "ART TREES")%>%
    filter(intervention == 0 | level == intervention_level 
    )%>%
    filter(intervention == 1 | level != "ADM2" 
    )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For Verra JNR eligible

dat_long_Verra <- readRDS("data/processed/dat_long_Verra.rds")
dat_long_Verra_beta <- readRDS("data/processed/dat_long_Verra_beta.rds")

dat_long_Verra_full <- dat_long_Verra %>% left_join(dat_long_Verra_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
    mutate(
        loss = loss/10000,
        last_year_diff = last_year_diff * -1 / 10000,
        est_effect = est_effect / 10000,
        treecover_remaining = treecover_remaining / 10000,
        area = area / 10000,
        intercept = intercept / 10000,
        slope = slope / 10000,
        baseline = baseline / 10000
    )

dat_long_Verra_full <- dat_long_Verra_full %>% mutate(
    pct_effect = est_effect/treecover_remaining,
    pct_intercept = intercept/treecover_remaining,
    pct_slope = slope/treecover_remaining,
    pct_last_year_diff = last_year_diff/treecover_remaining,
    pct_change_effect = est_effect/baseline,
    pct_change_intercept = intercept/baseline,
    pct_change_slope = slope/baseline,
    pct_change_last_year_diff = last_year_diff/baseline
)%>%
    mutate_all(~ifelse(is.infinite(.), NA, .)) %>%
    filter(loss >1
    )%>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(intervention = replace_na(intervention, 0))

Verra_eligible_full <- dat_long_Verra_full %>% 
    filter(intervention == 0 | intervention_fund == "Verra JNR")%>%
    filter(intervention == 0 | level == intervention_level
    )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### Get datasets without intervention affected jurisdictions
######### And make sure there are at least five years in crediting period and reference period

table(ART_eligible_full$year)

ART_eligible <- ART_eligible_full %>%
    filter(intervention == 0 
           , year <= max(year, na.rm=T) - 4,
           , level != "ADM2"
    )

table(ART_eligible$year)

(ART_eligible_count <- length(unique(ART_eligible$ID)))

Verra_eligible <- Verra_eligible_full %>%
    filter(intervention == 0
           , year <= max(year, na.rm=T) - 4
    )

(Verra_eligible_count <- length(unique(Verra_eligible$ID)))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Omniscient enroller questions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -   What if a jurisdiction signs up in the year it has the most negative/first year it has negative slope?

# -   What if a jurisdiction signs up in the year it has the most negative/first year it has negative last year difference?

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For ART eligible Js
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mean effect for whole dataset
effect_all <- mean(ART_eligible$est_effect,na.rm=T)

effect_all_total <- sum(ART_eligible$est_effect,na.rm=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Most negative slope/difference:

# most negative value or most positive last value difference
ART_eligible_neg_slope <- ART_eligible %>% group_by(ID) %>% slice_min(slope)%>% ungroup
ART_eligible_last_year_diff <- ART_eligible %>% group_by(ID) %>%slice_min(last_year_diff)%>% ungroup

# Mean effect for jurisdictions choosing the most negative slope year
neg_slope <- mean(ART_eligible_neg_slope$est_effect,na.rm=T)
# Mean effect for jurisdictions choosing the most negative last year
neg_diff <- mean(ART_eligible_last_year_diff$est_effect,na.rm=T)

# Total effect for jurisdictions choosing the most negative slope year
neg_slope_total <- sum(ART_eligible_neg_slope$est_effect,na.rm=T)
# Total effect for jurisdictions choosing the most negative last year
neg_diff_total <- sum(ART_eligible_last_year_diff$est_effect,na.rm=T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%
### First negative slope or positive last value

ART_eligible_first_neg_slope <- ART_eligible %>% 
    group_by(ID) %>% 
    arrange(year) %>%
    slice(which(slope<0)[1]) %>% 
    ungroup

ART_eligible_first_diff <- ART_eligible %>% 
    group_by(ID) %>% 
    arrange(year) %>%
    slice(which(last_year_diff<0)[1])%>% 
    ungroup

#Mean effect for jurisdictions choosing the first negative slope year
first_neg_slope = mean(ART_eligible_first_neg_slope$est_effect,na.rm=T)
# Mean effect for jurisdictions choosing the first negative last year
first_neg_diff = mean(ART_eligible_first_diff$est_effect,na.rm=T)

# Total effect for jurisdictions choosing the most negative slope year
first_neg_slope_total <- sum(ART_eligible_first_neg_slope$est_effect,na.rm=T)
# Total effect for jurisdictions choosing the most negative last year
first_neg_diff_total <- sum(ART_eligible_first_diff$est_effect,na.rm=T)


table_ART <- data.frame(
    "Metric" = c("Most negative slope", "Most negative last year dev.", "First negative slope", "First negative last year dev."),
    "Group" = "ART eligible",
    "overcredited_deforestation_mean" = c(neg_slope, neg_diff, first_neg_slope, first_neg_diff),
    "overcredited_deforestation_total" = c(neg_slope_total, neg_diff_total, first_neg_slope_total, first_neg_diff_total)
)

kbl(table_ART %>% select(-Group), format.args = list(big.mark = ","), digits = 2,
    format = "latex",
    row.names = F,
    col.names = c(" ", "Mean", "Total"),
    align = c("c", "c", "c"),
    caption = "Overcredited deforestation if non-JREDD, ART eligible jurisdictions had enrolled in given years. These estimates represent the deforestation jurisdictions could have been credited for under ART TREES based on placebo enrollment (i.e., without having taken any action).",
    label = "omniscient-enroller-ART",
    booktabs = T) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c("Jurisdictions' enrollment year" = 1, "Overcredited deforestation (ha)" = 2))%>%
    save_kable(paste0(results_dir, "/omniscient_enroller_ART_table.tex"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For Verra eligible Js
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mean effect for whole dataset
effect_all <- mean(Verra_eligible$est_effect,na.rm=T)

effect_all_total <- sum(Verra_eligible$est_effect,na.rm=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Most negative slope/difference:

# most negative value or most positive last value difference
Verra_eligible_neg_slope <- Verra_eligible %>% group_by(ID) %>% slice_min(slope)%>% ungroup
Verra_eligible_last_year_diff <- Verra_eligible %>% group_by(ID) %>% slice_min(last_year_diff)%>% ungroup

# Mean effect for jurisdictions choosing the most negative slope year
neg_slope <- mean(Verra_eligible_neg_slope$est_effect,na.rm=T)
# Mean effect for jurisdictions choosing the most negative last year
neg_diff <- mean(Verra_eligible_last_year_diff$est_effect,na.rm=T)

# Total effect for jurisdictions choosing the most negative slope year
neg_slope_total <- sum(Verra_eligible_neg_slope$est_effect,na.rm=T)
# Total effect for jurisdictions choosing the most negative last year
neg_diff_total <- sum(Verra_eligible_last_year_diff$est_effect,na.rm=T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%
### First negative slope or positive last value

Verra_eligible_first_neg_slope <- Verra_eligible %>% 
    group_by(ID) %>% 
    arrange(year) %>%
    slice(which(slope<0)[1]) %>% 
    ungroup

Verra_eligible_first_diff <- Verra_eligible %>% 
    group_by(ID) %>% 
    arrange(year) %>%
    slice(which(last_year_diff<0)[1])%>% 
    ungroup

#Mean effect for jurisdictions choosing the first negative slope year
first_neg_slope = mean(Verra_eligible_first_neg_slope$est_effect,na.rm=T)
# Mean effect for jurisdictions choosing the first negative last year
first_neg_diff = mean(Verra_eligible_first_diff$est_effect,na.rm=T)

# Total effect for jurisdictions choosing the most negative slope year
first_neg_slope_total <- sum(Verra_eligible_first_neg_slope$est_effect,na.rm=T)
# Total effect for jurisdictions choosing the most negative last year
first_neg_diff_total <- sum(Verra_eligible_first_diff$est_effect,na.rm=T)


table_Verra <- data.frame(
    "Metric" = c("Most negative slope", "Most negative last year dev.", "First negative slope", "First negative last year dev."),
    "Group" = "Verra eligible",
    "overcredited_deforestation_mean" = c(neg_slope, neg_diff, first_neg_slope, first_neg_diff),
    "overcredited_deforestation_total" = c(neg_slope_total, neg_diff_total, first_neg_slope_total, first_neg_diff_total)
)

kbl(table_Verra %>% select(-Group), format.args = list(big.mark = ","), digits = 2,
    format = "latex",
    row.names = F,
    col.names = c(" ", "Mean", "Total"),
    align = c("c", "c", "c"),
    caption = "Overcredited deforestation if non-JREDD, Verra eligible jurisdictions had enrolled in given years. These estimates represent the deforestation jurisdictions could have been credited for under ART TREES based on placebo enrollment (i.e., without having taken any action).",
    label = "omniscient-enroller-Verra",
    booktabs = T) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c("Jurisdictions' enrollment year" = 1, "Overcredited deforestation (ha)" = 2))%>%
    save_kable(paste0(results_dir, "/omniscient_enroller_Verra_table.tex"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Find 15 jurisdictions with greatest average slope
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_js = 15

ART_avgslope_id <- ART_eligible %>% 
    group_by(ID) %>% 
    filter(!(ADM1_NAME %in% c("Administrative unit not available"))
    )%>%
    filter(length(ID) >= 10)%>%
    summarise(avg_pct_change_slope = mean(pct_change_slope, na.rm = T),
              length = length(ID),
              max_year = max(year)) %>%
    slice_max(avg_pct_change_slope, n = n_js) %>% 
    ungroup
ART_avgslope <- ART_eligible %>% 
    filter(ID %in% ART_avgslope_id$ID)

ART_avgslope_mean <- mean(ART_avgslope$est_effect, na.rm = T)

table_ART15 <- ART_avgslope %>% group_by(ADM0_NAME, ADM1_NAME, ADM2_NAME, level) %>%
    summarise(est_effect = mean(est_effect, na.rm = T),
              area = mean(area, na.rm = T))%>%
    ungroup %>%
    mutate(j_name = 
               ifelse(level == "ADM0", ADM0_NAME,
                      ifelse(level == "ADM1", paste0(ADM0_NAME, ": ", ADM1_NAME), 
                             paste0(ADM0_NAME, ": ", ADM1_NAME, ", ", ADM2_NAME))
               )
           , est_effect_pct = est_effect/area *100   
    )

kbl(table_ART15 %>% select(j_name, est_effect, est_effect_pct), 
    format.args = list(big.mark = ","), digits = 4,
    format = "latex",
    row.names = F,
    col.names = c("Jurisdiction", "Mean placebo effect (Ha)", "As % of jurisdiction area"),
    align = c("c", "c", "c"),
    caption = "Overcredited deforestation if the 15 non-JREDD, ART eligible jurisdictions with the greatest average slopes had enrolled. These estimates represent the deforestation jurisdictions would have been credited for under ART TREES based on placebo enrollment (i.e., without having taken any action) averaged across all possible enrollment years.",
    label = "ART-15-disincentive",
    booktabs = T) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    save_kable(paste0(results_dir, "/disincentive_ART_table.tex"))



Verra_avgslope_id <- Verra_eligible %>% group_by(ID) %>% 
    filter(!(ADM2_NAME %in% c("Administrative unit not available")),
           !(ADM1_NAME %in% c("Administrative unit not available"))
    )%>%
    summarise(avg_slope = mean(slope, na.rm = T)) %>%
    slice_max(avg_slope, n = n_js) %>% 
    ungroup
Verra_avgslope <- Verra_eligible %>% 
    filter(ID %in% Verra_avgslope_id$ID)

Verra_avgslope_mean <- mean(Verra_avgslope$est_effect, na.rm = T)

table_Verra15 <- Verra_avgslope %>% group_by(ADM0_NAME, ADM1_NAME, ADM2_NAME, level) %>%
    summarise(est_effect = mean(est_effect, na.rm = T),
              area = mean(area, na.rm = T))%>%
    ungroup%>%
    mutate(j_name = 
               ifelse(level == "ADM0", ADM0_NAME,
                      ifelse(level == "ADM1", paste0(ADM0_NAME, ": ", ADM1_NAME), 
                             paste0(ADM0_NAME, ": ", ADM1_NAME, ", ", ADM2_NAME))
               )
           , est_effect_pct = est_effect/area *100   
    )%>%
    select(j_name, est_effect, est_effect_pct)


kbl(table_Verra15 %>% select(j_name, est_effect, est_effect_pct), 
    format.args = list(big.mark = ","), digits = 4,
    format = "latex",
    row.names = F,
    col.names = c("Jurisdiction", "Mean placebo effect (Ha)", "As % of jurisdiction area" ),
    align = c("c", "c", "c"),
    caption = "Overcredited deforestation if the 15 non-JREDD, Verra eligible jurisdictions with the greatest average slopes had enrolled. These estimates represent the deforestation jurisdictions would have been credited for under ART TREES based on placebo enrollment (i.e., without having taken any action) averaged across all possible enrollment years.",
    label = "Verra-15-disincentive",
    booktabs = T) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    save_kable(paste0(results_dir, "/disincentive_Verra_table.tex"))






disincentive_slope_plots <- vector('list', n_js)
for(i in 1:n_js){
    
    this_id = ART_avgslope_id[i,]$ID
    
    row = ART_eligible_full %>% filter(ID == this_id) %>%
        filter(year <= 19)%>%
        filter(slope == min(slope, na.rm = T))
    
    if(row$ADM0_NAME == "Democratic Republic of the Congo"){
        row$ADM0_NAME = "DRC"
    } 
    
    if(row$level == "ADM0"){
        name = row$ADM0_NAME
    } else {
        name = paste0(row$ADM0_NAME, ": ", row$ADM1_NAME)
    } 
    
    # limit to jurisdiction of interest
    this_juris <- ART_eligible_full %>%
        filter(ID == this_id)%>%
        mutate(year = year + 2000,
               j_name = name)
    placebo_year = row$year + 2000# + 1
    
    test_baseline = this_juris %>% 
        filter(year == placebo_year) %>% 
        select(baseline) %>% pull()
    
    this_placebo_baseline <- this_juris %>%
        filter(year < placebo_year & year >= placebo_year - 5) %>%
        summarise(loss = mean(loss)) %>% pull()
    # 
    # this_placebo_baseline
    # test_baseline
    
    
    this_juris <- this_juris %>%
        select(level, ID, year, j_name, loss)%>%
        mutate(baseline = ifelse(year >= placebo_year & year <= placebo_year +4,
                                 this_placebo_baseline,
                                 NA)
        )%>%
        # filter(year <= placebo_year + 5)%>%
        pivot_longer(cols = c("loss", "baseline"), names_to = "defor_group", values_to = "defor")
    
    baseline_period_shade <- annotate("rect", 
                                      xmin = placebo_year - 5,
                                      xmax = placebo_year - 0.25, 
                                      ymin = -Inf, ymax = Inf,
                                      alpha = .5,fill = palette$light_grey)
    
    this_plot <- ggplot(this_juris, aes(x = year, y = defor, color = defor_group, linewidth = defor_group))+
        baseline_period_shade +
        geom_vline(xintercept = placebo_year, linetype = "dashed")+
        geom_line()+
        geom_smooth(
            data=subset(this_juris, year < placebo_year & year >= placebo_year - 5)
            , method = "lm", se = FALSE, linewidth = 0.75, color = palette$green, linetype = "dashed")+
        theme_bw()+
        scale_color_manual(values = c(palette$gold, palette$blue))+
        scale_linewidth_manual(values = c(1, 0.5))+
        theme(
            legend.position = "none"
            #, axis.text.y = element_blank()
            , axis.title.y = element_blank()
            , axis.title.x = element_blank()
            , plot.title = element_text(hjust = 0.5)
        )+ 
        scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2023))+
        facet_grid(. ~ j_name) +
        theme(strip.background = element_rect(fill="grey85"),
              strip.text = element_text(size=10, color="black"))
    this_plot
    disincentive_slope_plots[[i]] <- this_plot
    
    
}


figure <- ggarrange(plotlist=disincentive_slope_plots, 
                    ncol = 3, nrow = 5,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))

ggsave(filename = paste0(fig_dir, "/disincentive_slope_baselines_ART.png")
       , width = 10, height = 8)
ggsave(filename = paste0(fig_dir_pdf, "/disincentive_slope_baselines_ART.pdf")
       , width = 10, height = 8)

