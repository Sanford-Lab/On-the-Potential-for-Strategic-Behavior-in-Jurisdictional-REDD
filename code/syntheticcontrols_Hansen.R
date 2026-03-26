library(tidyverse)
library(kableExtra)
library(fixest)
library(microsynth)
library(here)
library(rio)
library(ggpubr)
library(grid)

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

ARTintervention_names <- readRDS("data/processed/ARTintervention_names_new.rds")
Verraintervention_names <- readRDS("data/processed/Verraintervention_names.rds")

intervention_names <- ARTintervention_names %>%
    filter(intervention_year <= 2023)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Hansen data - find ART and Verra eligible
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ART eligible

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Verra eligible

Verra_national <- dat_long %>% filter(level == "ADM0")
Verra_sub1 <- dat_long %>% filter(ADM0_CODE %in% (Verra_national %>% filter(area >= 2.5e6*10000) %>% pull(ADM0_CODE))) %>%
    filter(level == "ADM1")
Verra_sub2 <- dat_long %>% filter(ADM1_CODE %in% (Verra_sub1 %>% filter(area >= 5e6*10000) %>% pull(ADM1_CODE))) %>%
    filter(level == "ADM2")
Verra_eligible <- bind_rows(Verra_national,Verra_sub1,Verra_sub2)%>%
    mutate(Verra_eligible = 1) %>% 
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, Verra_eligible)

rm(Verra_national,Verra_sub1,Verra_sub2)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        Verra_eligible ) %>%
    mutate(Verra_eligible = replace_na(Verra_eligible, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Match interventions back to dat_long
intervention_dat <- dat_long %>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    # Convert units to hectares
    mutate(cum_loss = cum_loss/10000,
           loss = loss /10000,
           treecover_2000 = treecover_2000/10000,
           share_area = treecover_2000 / (area/10000)
    )

intervention_trios <- intervention_names %>% select(intervention_name, intervention_fund, intervention_level, intervention_year) %>% unique()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Set up  individual SC
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set.seed(06511)

my_cores = 1#detectCores()-1 seems to be a bug when using multiple cores
n_perm = 500
anticipation_years = 5
backdate_years = 5
cov.var <- c("treecover_2000", "area")

validation_results <- data.frame()
for(outcome_var in c("cum_loss", "loss")){
    
    validation_plot_list <- vector('list', nrow(intervention_trios)+1)
    
    for(r in 1:nrow(intervention_trios)){
        
        i = intervention_trios[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        print(iname)
        
        treat_dat <- intervention_dat %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 2000,
                   post = (year >= treat_year - backdate_years - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }
        
        if (ifund == "Verra JNR"){
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                   , Verra_eligible == 1
                   , level %in% donor_levels
            )
                
            
        } else{
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                   , ART_eligible == 1
                   , level %in% donor_levels
            )
                
        }
        
        this_treat_year <- treat_dat %>% select(treat_year) %>% pull() %>% unique() 
        this_treecover_2000 <- treat_dat %>% select(treecover_2000) %>% pull() %>% sum() %>% unique()
        
        
        control_dat <- control_dat %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   post = 0,
                   treated = 0)
        
        
        
        
        synth_dat <- rbind(treat_dat, control_dat)%>%
            rename(sc_ID = ID)
        
        final_match_year = this_treat_year - backdate_years - anticipation_years - 1
        
        match.out = outcome_var
        
        
             
        
        synth_out <- microsynth(synth_dat,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = this_treat_year - anticipation_years - 1,
                                test="twosided",
                                perm=FALSE,
                                jack=TRUE, use.backup=TRUE,
                                n.cores = my_cores
        )
        
        control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
            rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "synthetic", cols = synthetic.1:synthetic.22) %>%
            separate(X, into = c(NA, "year"))
        
        results_short <- data.frame("treatment" = synth_out$Plot.Stats$Treatment) %>% rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "treatment", cols = -c("outcome")) %>%
            separate(X, into = c(NA, "year"))%>%
            inner_join(control, by = c("outcome", "year")) %>%
            mutate(gap = treatment - synthetic)
        
        results_long <- results_short %>%
            mutate(intervention_name = iname)%>%
            pivot_longer(cols = c("treatment", "synthetic", "gap"),
                         names_to = "results_group", values_to = "value")%>%
            drop_na(value)%>%
            mutate_at(vars(year, value), as.numeric)
        
        start_gap <- results_short %>%
            filter(as.numeric(year) <= final_match_year)%>%
            mutate(gap_start = mean(gap/synthetic)
            )  %>%
            slice_head() %>% select(gap_start) %>% pull()
        
        if(outcome_var == "cum_loss"){
            this_validation_results <- results_short %>%
                filter(year == this_treat_year - anticipation_years - 1)%>%
                select(outcome, treatment, synthetic, gap)%>%
                mutate(intervention_name = iname,
                       gap_treecover = abs(gap/this_treecover_2000),
                       gap_start = abs(start_gap),
                       gap_synth = abs(gap/treatment),
                       validated = ifelse(gap_start > 0.25 | gap_treecover > 0.005 | gap_synth > 0.2,
                                          FALSE, TRUE)
                )   
            
        } else{
            this_validation_results <- results_short %>%
                filter(as.numeric(year) >= this_treat_year - anticipation_years)%>%
                select(outcome, treatment, synthetic, gap)%>%
                mutate(intervention_name = iname,
                       gap_synth = NA,
                       gap_treecover = abs(sum(gap/this_treecover_2000)),
                       gap_start = abs(start_gap),
                       validated = ifelse(gap_start > 0.25 | gap_treecover > 0.005,
                                          FALSE, TRUE)
                )  %>%
                slice_head()
            
        }
        
        
        if (this_validation_results$validated == TRUE) {
            this_plot_title <- iname
        } else{
            this_plot_title <- paste0(iname, "*")
        }
        
        validation_shade <- annotate("rect", 
                                     xmin = final_match_year + 2000,
                                     xmax = Inf, 
                                     ymin = -Inf, ymax = Inf,
                                     alpha = .2,fill = palette$purple)
        
        sc_path_results <- results_long %>% filter(results_group %in% c("synthetic", "treatment"))%>%
            filter(year <= this_treat_year - anticipation_years)%>%
            mutate(plot_title = this_plot_title,
                   year = year + 2000)
        
        sc_plot <- ggplot(sc_path_results, aes(x = year, y = value, color = results_group))+
            validation_shade + 
            geom_line()+
            theme_bw()+
            scale_color_manual(values = c(palette$blue, palette$red))+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                               labels = c(2000, 2005, 2010, 2015, 2020)
                                   
                                   )+
            theme(
                legend.title = element_blank()
                , plot.title = element_text(hjust = 0.5)
                , axis.title.x = element_blank()
                , axis.title.y = element_blank()
            )
        sc_plot <- sc_plot + facet_grid(. ~ plot_title) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        validation_plot_list[[r]] <- sc_plot + theme(legend.position='none')
        
        legend <- get_legend(sc_plot)
        
        validation_results <- this_validation_results %>%
            bind_rows(validation_results)
        # From TAP WEST et al. : We considered the SC method validated for the sites in which the gaps between the project and SC deforestation at the end of the validation interval were lower than 0.5% of the project area
        
    }
    
    validation_plot_list[[nrow(intervention_trios)+1]] <- as_ggplot(legend)
    
    assign(paste0("sc_plot_list_", outcome_var), validation_plot_list)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PLOTTING VALIDATION TEST RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#index = c(6, 15, 18, 7, 13, 8, 3, 9, 2, 14, 1, 10, 4, 20, 11, 16, 12, 5, 19, 17)
index = c(10, 9, 8, 13, 18, 1, 3, 6, 11, 7, 14, 17, 4, 12, 2, 16, 20, 5, 19, 15)

#%%%%%%%%%%%%%%%%%
# CUMULATIVE LOSS PLOTS
figure <- ggarrange(plotlist=sc_plot_list_cum_loss[order(c(index, length(index)+1))], 
          ncol = 3, nrow = 7,
          labels = NULL
         # , common.legend = TRUE
         # , legend = "top"
         )
annotate_figure(figure, left = textGrob("Cumulative deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_validation.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_validation.pdf")
       , width = 8, height = 9)

#%%%%%%%%%%%%%%%%%
# ANNUAL LOSS PLOTS
figure <- ggarrange(plotlist=sc_plot_list_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_validation_annual.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_validation_annual.pdf")
       , width = 8, height = 9)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### MAIN RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ovr_results <- data.frame()
set.seed(09301993)

for(outcome_var in c("cum_loss", "loss")){
    sc_plot_list <- vector('list', nrow(intervention_trios)+1)
    baseline_plot_list <- vector('list', nrow(intervention_trios)+1)
    gap_plot_list <- vector('list', nrow(intervention_trios))
    baselinecombo_plot_list <- vector('list', nrow(intervention_trios)+1)
    
    for(r in 1:nrow(intervention_trios)){
        
        i = intervention_trios[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        validated = validation_results %>% 
            filter(intervention_name == iname,
                   outcome == outcome_var) %>%
            select(validated) %>% pull()
        
        print(iname)
        
        treat_dat <- intervention_dat %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 2000,
                   post = (year >= treat_year - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }
        
        this_treat_year <- treat_dat %>% select(treat_year) %>% pull() %>% unique() 
        
        
        if (ifund == "Verra JNR"){
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                       , Verra_eligible == 1
                       , level %in% donor_levels
                )
            
            
        } else{
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                       , ART_eligible == 1
                       , level %in% donor_levels
                )
            
        }
        
        
        control_dat <- control_dat %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   post = 0,
                   treated = 0)
        
        
        synth_dat <- rbind(treat_dat, control_dat)%>%
            rename(sc_ID = ID)
        
        final_match_year = this_treat_year - anticipation_years - 1
        match.out = outcome_var
        
        # Synthetic DID
        # setup = panel.matrices(panel = synth_dat %>%
        #                            mutate(treated = ifelse(intervention ==1 & year >= this_treat_year - anticipation_years, 1, 0))%>%
        #                            as.data.frame() %>%
        #                            select(sc_ID, year, outcome_var, treated)
        #                        )
        # 
        # tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
        
        # synth_did_gap <- ggplot_build(synthdid_plot(tau.hat, overlay=1))$data[[1]] %>%
        #     mutate(group = ifelse(group == 2, "treated", "control"),
        #            year = as.character(x),
        #            value = as.numeric(y))%>%
        #     select(year, value, group)%>%
        #     pivot_wider(names_from = group)%>%
        #     mutate(intervention_name = iname,
        #            outcome = outcome_var,
        #            sdid_gap = treated - control,
        #            results_group = "sdid gap")%>%
        #     rename(value = sdid_gap)%>%
        #     select(-c(treated, control))
            
        
        # synth_did_synthetic <- ggplot_build(synthdid_plot(tau.hat, overlay=1))$data[[1]] %>%
        #     mutate(results_group = ifelse(group == 2, "treated", "synthetic (sdid)"),
        #            year = as.character(x),
        #            value = as.numeric(y))%>%
        #     filter(results_group == "synthetic (sdid)")%>%
        #     select(year, value, results_group)%>%
        #     mutate(intervention_name = iname,
        #            outcome = outcome_var)%>%
        #     rbind(synth_did_gap)
        
        
        
        synth_out <- microsynth(synth_dat,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = seq(from = final_match_year, to = 23),
                                test="twosided",
                                perm=n_perm,
                                jack=TRUE, use.backup=TRUE,
                                n.cores = my_cores
        )
        
        control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
            rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "synthetic", cols = synthetic.1:synthetic.22) %>%
            separate(X, into = c(NA, "year"))
        
        
        results_short <- data.frame("treatment" = synth_out$Plot.Stats$Treatment) %>% rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "treatment", cols = -c("outcome")) %>%
            separate(X, into = c(NA, "year"))%>%
            inner_join(control, by = c("outcome", "year")) %>%
            mutate(gap = treatment - synthetic
            )
        
        baselines <- results_short %>%
            filter(year < this_treat_year & year >= this_treat_year - 5)%>%
            mutate(baseline = mean(treatment, na.rm = T),
                      synthetic_baseline = mean(synthetic, na.rm = T))
        
        baseline <- mean(baselines$treatment)
        
        baseline_synthetic <- mean(baselines$synthetic)
        
        
        
        if(outcome_var == "cum_loss"){
            cum_start <- results_short %>%
                filter(year == this_treat_year - 1)%>%
                select(treatment) %>% pull() %>% sum()
            
            cum_start_synthetic <- results_short %>%
                filter(year == this_treat_year - 1)%>%
                select(synthetic) %>% pull() %>% sum()
            
            baseline_res <- data.frame("year" = as.character(seq(from = this_treat_year - 1, to = this_treat_year + 4)),
                                       "baseline_loss" = baseline,
                                       "cum_start" = cum_start,
                                       "baseline_loss_synthetic" = baseline_synthetic,
                                       "cum_start_synthetic" = cum_start_synthetic)%>%
                mutate(baseline_loss = ifelse(year == this_treat_year - 1, 0, baseline_loss),
                       baseline_cum = cumsum(baseline_loss),
                       baseline = cum_start + baseline_cum,
                       baseline_loss_synthetic = ifelse(year == this_treat_year - 1, 0, baseline_loss_synthetic),
                       baseline_cum_synthetic = cumsum(baseline_loss_synthetic),
                       baseline_synthetic = cum_start_synthetic + baseline_cum_synthetic)%>%
                select(year, baseline, baseline_synthetic)
        } else{
            
            baseline_res <- data.frame("year" = as.character(seq(from = this_treat_year, to = this_treat_year + 4)),
                                       "baseline_loss" = baseline,
                                       "baseline_loss_synthetic" = baseline_synthetic)%>%
                mutate(baseline = baseline_loss,
                       baseline_synthetic = baseline_loss_synthetic)%>%
                select(year, baseline, baseline_synthetic)
            
        }
        
        
        results_long <- results_short %>%
            full_join(baseline_res)%>%
            mutate(intervention_name = ifelse(validated == T, iname, paste(iname, "*")))%>%
            pivot_longer(cols = c("treatment", "synthetic", "gap", "baseline", "baseline_synthetic"),
                         names_to = "results_group", values_to = "value")%>%
            drop_na(value)%>%
            mutate_at(vars(year, value), as.numeric)%>%
            mutate(year = year + 2000)
        
        anticipation_shade <- annotate("rect", 
                                       xmin = this_treat_year - anticipation_years - 1.25 + 2000,
                                       xmax = this_treat_year - 0.25 + 2000, 
                                       ymin = -Inf, ymax = Inf,
                                       alpha = .5,fill = "grey85")
        
        
        sc_baseline_results <- results_long %>% 
            filter(results_group %in% c("synthetic", "baseline")
            )
        
        sc_baseline_plot <- ggplot(sc_baseline_results, aes(x = year, y = value, color = results_group, linewidth = results_group))+
            geom_vline(xintercept = this_treat_year +2000 - 0.1, linetype = "dashed")+
            geom_line()+
            theme_bw()+
            scale_color_manual(values = c(palette$gold, palette$blue, palette$dark_blue))+
            scale_linewidth_manual(values = c(1, .6))+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            theme(#legend.position="none"
                   legend.title = element_blank()
                  , plot.title = element_text(hjust = 0.5)
                  , axis.title.x = element_blank()
                  , axis.title.y = element_blank()
            )
        
        sc_baseline_plot <- sc_baseline_plot + facet_grid(. ~ intervention_name) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        baseline_plot_list[[r]] <- sc_baseline_plot
        
        sc_path_results <- results_long %>% filter(results_group %in% c("synthetic", "treatment"))
        
        sc_plot <- ggplot(sc_path_results, aes(x = year, y = value, color = results_group))+
            anticipation_shade + 
            geom_vline(xintercept = this_treat_year +2000 - 0.5, linetype = "dashed") + 
            geom_line()+
            theme_bw()+
            scale_color_manual(values = c(palette$blue, palette$red, palette$dark_blue))+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            theme(
                legend.title = element_blank()
                , plot.title = element_text(hjust = 0.5)
                , axis.title.x = element_blank()
                , axis.title.y = element_blank()
            )
        sc_plot <- sc_plot + facet_grid(. ~ intervention_name) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        sc_plot_list[[r]] <- sc_plot + theme(legend.position='none')
        
        
        combo_results <- results_long %>% filter(results_group %in% c("baseline", "baseline_synthetic", "synthetic", "treatment"))
        
        combo_plot <- ggplot(
            combo_results %>% mutate(
                results_group = case_when(results_group == "treatment" ~ "Observed",
                                          results_group == "baseline" ~ "Baseline",
                                          results_group == "baseline_synthetic" ~ "Synthetic baseline",
                                          results_group == "synthetic" ~ "Synthetic control"
                                          )
            )
            , aes(x = year, y = value, color = results_group, linetype = results_group#, linewidth = results_group
                                                ))+
            anticipation_shade + 
            geom_vline(xintercept = this_treat_year + 2000 - 0.5, linetype = "dashed") + 
            geom_line()+
            theme_bw()+
            scale_color_manual(values = c(palette$gold, palette$blue, palette$gold, palette$red))+
            scale_linetype_manual(values = c("solid", "solid", "dashed", "solid"))+
            #scale_linewidth_manual(values = c(1.5, 1, 1, 1))+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            theme(
               legend.title = element_blank()
                , plot.title = element_text(hjust = 0.5)
                , axis.title.x = element_blank()
                , axis.title.y = element_blank()
            )
        combo_plot <- combo_plot + facet_grid(. ~ intervention_name) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        baselinecombo_plot_list[[r]] <- combo_plot + theme(legend.position='none')
        
        
        this_intervention_results <- data.frame()
        for(i in seq(final_match_year, 23)){
            
            these_results <- data.frame(
                synth_out$Results[paste0(i)]
            )%>%
                mutate(year = i + 2000,
                       Rel_year = i - this_treat_year,
                       validated = validated,
                       iname = iname,
                       ifund = ifund)
            
            for ( col in 1:ncol(these_results)){
                colnames(these_results)[col] <-  sub(paste0("X", i, "."), "", colnames(these_results)[col])
            }
            
            these_results <- these_results %>%
                rownames_to_column("Outcome")%>%
                filter(Outcome != "Omnibus")%>%
                mutate(Effect = Trt - Con)%>%
                select(Outcome, Effect, Trt, Con, Pct.Chng, Perm.pVal, Rel_year, year, iname, ifund, validated)
            
            this_intervention_results <- these_results %>%
                bind_rows(this_intervention_results)%>%
                distinct()
            
        }
        
        sc_gap_results <- results_long %>% 
            filter(results_group %in% c("gap")
                  # , year >= final_match_year - 10
            ) %>%
            left_join(this_intervention_results %>% 
                          select(Perm.pVal, year) 
                      )%>%
            mutate(Z = abs(qnorm(Perm.pVal/2)),
                   se = abs(value/Z),
                   ci.upper = value + 1.96*se,
                   ci.lower = value - 1.96*se
            )
        
        sc_gap_plot <- ggplot(sc_gap_results, aes(x = year, y = value))+
            geom_hline(yintercept = 0 , linetype = "dashed", linewidth = 0.2)+
            anticipation_shade + 
            geom_ribbon(data = sc_gap_results, aes(ymin = ci.lower, ymax = ci.upper), color = NA, fill = palette$blue, alpha = 0.35, linetype = "dashed")+
            geom_line(linewidth = 1)+
            theme_bw()+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            theme(
                plot.title = element_text(hjust = 0.5)
                , axis.title.x = element_blank()
                , axis.title.y = element_blank()
            )
        sc_gap_plot <- sc_gap_plot + facet_grid(. ~ intervention_name) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        gap_plot_list[[r]] <- sc_gap_plot + theme(legend.position='none')
        names(gap_plot_list)[[r]] <- iname
        
        
        
        ovr_results <- this_intervention_results %>%
            bind_rows(ovr_results)
        
    }
    
    sc_plot_list[[nrow(intervention_trios)+1]] <- as_ggplot(get_legend(sc_plot))
    baseline_plot_list[[nrow(intervention_trios)+1]] <- as_ggplot(get_legend(sc_baseline_plot))
    baselinecombo_plot_list[[nrow(intervention_trios)+1]] <- as_ggplot(get_legend(combo_plot))
    
    assign(paste0("sc_plot_list_", outcome_var), sc_plot_list)
    assign(paste0("baseline_plot_list_", outcome_var), baseline_plot_list)
    assign(paste0("gap_plot_list_", outcome_var), gap_plot_list)
    assign(paste0("baselinecombo_plot_list_", outcome_var), baselinecombo_plot_list)
}

export(ovr_results, paste0(results_dir, "/scHansen_ovr_results_new.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PLOTTING HANSEN RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggpubr)
library(grid)

#%%%%%%%%%%%%%%%%%
# CUMULATIVE LOSS PLOTS
figure <- ggarrange(plotlist=sc_plot_list_cum_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Cumulative deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_main.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_main.pdf")
       , width = 8, height = 9)


figure <- ggarrange(plotlist=gap_plot_list_cum_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL)
annotate_figure(figure, left = textGrob("Cumulative deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_gap.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_gap.pdf")
       , width = 8, height = 9)

figure <- ggarrange(plotlist=baselinecombo_plot_list_cum_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                   # , common.legend = TRUE,
                  #  legend = "top"
                    )
annotate_figure(figure, left = textGrob("Cumulative deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_baselinecombo.png")
       , width = 9, height = 10)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_baselinecombo.pdf")
       , width = 9, height = 10)

#%%%%%%%%%%%%%%%%%
# ANNUAL LOSS PLOTS

figure <- ggarrange(plotlist=sc_plot_list_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_main_annual.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_main_annual.pdf")
       , width = 8, height = 9)

figure <- ggarrange(plotlist=baseline_plot_list_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL)
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_baseline_annual.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_baseline_annual.pdf")
       , width = 8, height = 9)

figure <- ggarrange(plotlist=gap_plot_list_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL)
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_gap_annual.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_gap_annual.pdf")
       , width = 8, height = 9)

figure <- ggarrange(plotlist=baselinecombo_plot_list_loss[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                   # , common.legend = TRUE,
                   # legend = "top"
                    )
annotate_figure(figure, left = textGrob("Annual deforestation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scHansen_baselinecombo_annual.png")
       , width = 9, height = 10)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_baselinecombo_annual.pdf")
       , width = 9, height = 10)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### SUBSET TO ONLY DONOR JURISDICTIONS IN TMF DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TMF_IDs <- readRDS(paste0(here::here("data", "processed"), "/dat_long_TMF.rds")) %>% 
    select(ID) %>%
    unique() %>%
    pull()

intervention_dat_TMFdonors <- intervention_dat %>%
    filter(ID %in% TMF_IDs | !is.na(intervention_name))

ovr_results_TMFdonors <- data.frame()
set.seed(06511)
for(outcome_var in c("cum_loss", "loss")){
    
    for(r in 1:nrow(intervention_trios)){
        
        i = intervention_trios[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        print(iname)
        
        validated = validation_results %>% 
            filter(intervention_name == iname,
                   outcome == outcome_var) %>%
            select(validated) %>% pull()
        
        treat_dat <- intervention_dat %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 2000,
                   post = (year >= treat_year - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }
        
        this_treat_year <- treat_dat %>% select(treat_year) %>% pull() %>% unique() 
        
        
        # if (ifund == "Verra JNR"){
        #     
        #     control_dat <- intervention_dat_TMFdonors %>%
        #         filter(is.na(intervention_name)
        #                , Verra_eligible == 1
        #                , level %in% donor_levels
        #         )
        #     
        #     
        # } else{
            
            control_dat <- intervention_dat_TMFdonors %>%
                filter(is.na(intervention_name)
                       , ART_eligible == 1
                       , level %in% donor_levels
                )
            
        # }
            
        
        
        control_dat <- control_dat %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   post = 0,
                   treated = 0)
        
        synth_dat <- rbind(treat_dat, control_dat)%>%
            rename(sc_ID = ID)
        
        final_match_year = this_treat_year - anticipation_years - 1
        match.out = outcome_var
        
        
        
        
        synth_out <- microsynth(synth_dat,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = seq(from = final_match_year, to = 23),
                                test="twosided",
                                perm=n_perm,
                                jack=TRUE, use.backup=TRUE,
                                n.cores = my_cores
        )
        
        control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
            rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "synthetic", cols = synthetic.1:synthetic.22) %>%
            separate(X, into = c(NA, "year"))
        
        
        this_intervention_results <- data.frame()
        for(i in seq(final_match_year, 23)){
            
            these_results <- data.frame(
                synth_out$Results[paste0(i)]
            )%>%
                mutate(year = i,
                       Rel_year = i - this_treat_year,
                       iname = iname,
                       ifund = ifund,
                       validated = validated)
            
            for ( col in 1:ncol(these_results)){
                colnames(these_results)[col] <-  sub(paste0("X", i, "."), "", colnames(these_results)[col])
            }
            
            these_results <- these_results %>%
                rownames_to_column("Outcome")%>%
                filter(Outcome != "Omnibus")%>%
                mutate(Effect = Trt - Con)%>%
                select(Outcome, Effect, Trt, Con, Pct.Chng, Perm.pVal, Rel_year, year, iname, ifund, validated)
            
            this_intervention_results <- these_results %>%
                bind_rows(this_intervention_results)%>%
                distinct()
            
        }
        ovr_results_TMFdonors <- this_intervention_results %>%
            bind_rows(ovr_results_TMFdonors)
    }
    
}

export(ovr_results_TMFdonors, paste0(results_dir, "/scHansen_ovr_results_TMFdonors.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### ALTERNATE ANTICIPATION WINDOW
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
anticipation_years = 3

ovr_results_alt <- data.frame()
set.seed(06511)
for(outcome_var in c("cum_loss", "loss")){
    
    for(r in 1:nrow(intervention_trios)){
        
        i = intervention_trios[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        print(iname)
        
        validated = validation_results %>% 
            filter(intervention_name == iname,
                   outcome == outcome_var) %>%
            select(validated) %>% pull()
        
        treat_dat <- intervention_dat %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 2000,
                   post = (year >= treat_year - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }
        
        this_treat_year <- treat_dat %>% select(treat_year) %>% pull() %>% unique() 
        
        
        if (ifund == "Verra JNR"){
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                       , Verra_eligible == 1
                       , level %in% donor_levels
                )
            
            
        } else{
            
            control_dat <- intervention_dat %>%
                filter(is.na(intervention_name)
                       , ART_eligible == 1
                       , level %in% donor_levels
                )
            
        }
        
        
        control_dat <- control_dat %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   post = 0,
                   treated = 0)
        
        
        synth_dat <- rbind(treat_dat, control_dat)%>%
            rename(sc_ID = ID)
        
        final_match_year = this_treat_year - anticipation_years - 1
        match.out = outcome_var
        
        
        
        
        synth_out <- microsynth(synth_dat,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = seq(from = final_match_year, to = 23),
                                test="twosided",
                                perm=n_perm,
                                jack=TRUE, use.backup=TRUE,
                                n.cores = my_cores
        )
        
        control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
            rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "synthetic", cols = synthetic.1:synthetic.22) %>%
            separate(X, into = c(NA, "year"))
        
        
        this_intervention_results <- data.frame()
        for(i in seq(final_match_year, 23)){
            
            these_results <- data.frame(
                synth_out$Results[paste0(i)]
            )%>%
                mutate(year = i,
                       Rel_year = i - this_treat_year,
                       iname = iname,
                       ifund = ifund,
                       validated = validated)
            
            for ( col in 1:ncol(these_results)){
                colnames(these_results)[col] <-  sub(paste0("X", i, "."), "", colnames(these_results)[col])
            }
            
            these_results <- these_results %>%
                rownames_to_column("Outcome")%>%
                filter(Outcome != "Omnibus")%>%
                mutate(Effect = Trt - Con)%>%
                select(Outcome, Effect, Trt, Con, Pct.Chng, Perm.pVal, Rel_year, year, iname, ifund, validated)
            
            this_intervention_results <- these_results %>%
                bind_rows(this_intervention_results)%>%
                distinct()
            
        }
        ovr_results_alt <- this_intervention_results %>%
            bind_rows(ovr_results_alt)
    }
    
}

export(ovr_results_alt, paste0(results_dir, "/scHansen_ovr_results_", anticipation_years, "yrs.rds"))

