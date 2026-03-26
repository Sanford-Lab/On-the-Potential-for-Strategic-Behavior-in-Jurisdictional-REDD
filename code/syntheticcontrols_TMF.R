library(tidyverse)
#library(zoo)
#library(tidyquant)
#library(kableExtra)
#library(fixest)
library(microsynth)
library(here)
#library(gsynth)

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

index = c(10, 9, 8, 13, 18, 1, 3, 6, 11, 7, 14, 17, 4, 12, 2, 16, 20, 5, 19, 15)

ARTintervention_names <- readRDS("data/processed/ARTintervention_names_new.rds")
Verraintervention_names <- readRDS("data/processed/Verraintervention_names.rds")

intervention_names <- ARTintervention_names %>%
    filter(intervention_year <= 2023)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### TMF data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dat_long_TMF <- readRDS(paste0(here::here("data", "processed"), "/dat_long_TMF.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ART eligible

ART_eligible_ADM1 <- dat_long_TMF %>% filter(treecover_2000 >= 2.5e6*10000) %>% filter(level == "ADM1")

ART_eligible_ADM2_ID <- dat_long_TMF %>% filter(level == "ADM2") %>%
    filter(ADM1_CODE %in% ART_eligible_ADM1$ADM1_CODE) %>% select(ID) %>% unique() %>% pull()

ART_eligible_TMF <- dat_long_TMF %>%
    filter(level == "ADM2",
           ID %in% ART_eligible_ADM2_ID
    )%>%
    rbind(dat_long_TMF %>% filter(level == "ADM0")) %>%
    rbind(ART_eligible_ADM1) %>%
    mutate(ART_eligible = 1) %>% 
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, ART_eligible)

dat_long_TMF <- dat_long_TMF %>% ungroup() %>%
    left_join(
        ART_eligible_TMF ) %>%
    mutate(ART_eligible = replace_na(ART_eligible, 0))


# dat_long_TMF_ART <- readRDS(paste0(here::here("data", "processed"), "/dat_long_TMF_ART.rds"))%>% select(ID, ART_eligible) %>% distinct()
# 
# dat_long_TMF <- readRDS(paste0(here::here("data", "processed"), "/dat_long_TMF.rds"))%>%
#     left_join(dat_long_TMF_ART)%>%
#     mutate(ART_eligible = replace_na(ART_eligible, 0))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Verra eligible

# Verra_national <- dat_long_TMF %>% filter(level == "ADM0")
# Verra_sub1 <- dat_long_TMF %>% filter(ADM0_CODE %in% (Verra_national %>% filter(area >= 2.5e6*10000) %>% pull(ADM0_CODE))) %>%
#     filter(level == "ADM1")
# Verra_sub2 <- dat_long_TMF %>% filter(ADM1_CODE %in% (Verra_sub1 %>% filter(area >= 5e6*10000) %>% pull(ADM1_CODE))) %>%
#     filter(level == "ADM2")
# Verra_eligible_TMF <- bind_rows(Verra_national,Verra_sub1,Verra_sub2)%>%
#     mutate(Verra_eligible = 1) %>% 
#     ungroup %>%
#     select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, Verra_eligible)
# 
# rm(Verra_national,Verra_sub1,Verra_sub2)
# 
# dat_long_TMF <- dat_long_TMF %>% ungroup() %>%
#     left_join(
#         Verra_eligible_TMF ) %>%
#     mutate(Verra_eligible = replace_na(Verra_eligible, 0))

intervention_dat_TMF <- dat_long_TMF %>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(cum_deforested = cum_deforested/10000,
           deforested_diff = deforested_diff /10000,
           cum_degraded = cum_degraded/10000,
           degraded_diff = degraded_diff /10000,
           treecover_2000 = treecover_2000/10000,
           share_area = treecover_2000 / (area/10000),
           cum_defordeg = cum_deforested + cum_degraded,
           defordeg = degraded_diff + deforested_diff)

# ALL interventions are in TMF data:
intervention_trios_TMF <- intervention_dat_TMF %>% drop_na(intervention_name) %>%
    select(intervention_name, intervention_fund, intervention_level) %>% unique()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### USING TMF DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set.seed(06511)

my_cores = 1
anticipation_years = 5
backdate_years = 5
n_perm = 250

cov.var <- c("treecover_2000", "area")

validation_results <- data.frame()
for(outcome_var in c("cum_defordeg", "defordeg")){
    
    sc_plot_list <- vector('list', nrow(intervention_trios_TMF))
    
    for(r in 1:nrow(intervention_trios_TMF)){
        
        i = intervention_trios_TMF[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        print(iname)
        
        treat_dat_TMF <- intervention_dat_TMF %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 1990,
                   year = year - 1990,
                   post = (year >= treat_year - backdate_years - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }
        
        if (ifund == "Verra JNR"){
            
            control_dat_TMF <- intervention_dat_TMF %>%
                filter(is.na(intervention_name)
                       , Verra_eligible == 1
                       , level %in% donor_levels
                )
            
            
        } else{
            
            control_dat_TMF <- intervention_dat_TMF %>%
                filter(is.na(intervention_name)
                       , ART_eligible == 1
                       , level %in% donor_levels
                )
            
        }
        
        this_treat_year <- treat_dat_TMF %>% select(treat_year) %>% pull() %>% unique() 
        this_area <- treat_dat_TMF %>% select(area) %>% pull() %>% sum() %>% unique()
        this_treecover_2000 <- treat_dat_TMF %>% select(treecover_2000) %>% pull() %>% sum() %>% unique()
        
        control_dat_TMF <- control_dat_TMF %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   year = year - 1990,
                   post = 0,
                   treated = 0)
        
        
        synth_dat_TMF <- rbind(treat_dat_TMF, control_dat_TMF)%>%
            rename(sc_ID = ID)%>%
            as.data.frame()%>%
            select(sc_ID, year, treated, outcome_var, cov.var)
        
        final_match_year = this_treat_year - backdate_years - anticipation_years - 1
        match.out <- outcome_var
        
        synth_out <- microsynth(synth_dat_TMF,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = this_treat_year - anticipation_years - 1,
                                test="twosided",
                                perm=FALSE,
                                use.backup = T,
                                jack=TRUE,
                                n.cores = my_cores
        )
        
        
        control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
            rownames_to_column("outcome") %>%
            pivot_longer(names_to = "X", values_to = "synthetic", cols = -c(outcome)) %>%
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
                                     xmin = final_match_year,
                                     xmax = Inf, 
                                     ymin = -Inf, ymax = Inf,
                                     alpha = .25,fill = palette$purple)
        
        sc_path_results <- results_long %>% filter(results_group %in% c("synthetic", "treatment"))%>%
            filter(year <= this_treat_year - anticipation_years)%>%
            mutate(plot_title = this_plot_title)
        
        sc_plot <- ggplot(sc_path_results, aes(x = year, y = value, color = results_group))+
            validation_shade + 
            geom_line()+
            theme_bw()+
            scale_color_manual(values = c(palette$blue, palette$red))+
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            theme(
                legend.position = "none"
                #legend.title = element_blank()
                , plot.title = element_text(hjust = 0.5)
                , axis.title.x = element_blank()
                , axis.title.y = element_blank()
            )
        sc_plot <- sc_plot + facet_grid(. ~ plot_title) +
            theme(strip.background = element_rect(fill="grey85"),
                  strip.text = element_text(size=10, color="black"))
        sc_plot_list[[r]] <- sc_plot
        
        validation_results <- this_validation_results %>%
            bind_rows(validation_results)
        # From TAP WEST et al. : We considered the SC method validated for the sites in which the gaps between the project and SC deforestation at the end of the validation interval were lower than 0.5% of the project area
        
    }
    
    assign(paste0("sc_plot_list_", outcome_var), sc_plot_list)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PLOTTING VALIDATION TEST RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggpubr)
library(grid)

#%%%%%%%%%%%%%%%%%
# CUMULATIVE LOSS PLOTS
figure <- ggarrange(plotlist=sc_plot_list_cum_defordeg[order(c(index, length(index)+1))],  
                    ncol = 3, nrow = 7,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Cumulative deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scTMF_validation.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scTMF_validation.pdf")
       , width = 8, height = 9)

#%%%%%%%%%%%%%%%%%
# ANNUAL LOSS PLOTS
figure <- ggarrange(plotlist=sc_plot_list_defordeg[order(c(index, length(index)+1))], 
                    ncol = 3, nrow = 7,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("Annual deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Year", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/scTMF_validation_annual.png")
       , width = 8, height = 9)
ggsave(filename = paste0(fig_dir_pdf, "/scTMF_validation_annual.pdf")
       , width = 8, height = 9)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### MAIN RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intervention_trios_TMF <- rbind(intervention_trios_TMF[10,], 
                                intervention_trios_TMF[17,],
                                intervention_trios_TMF[1:9,], 
                                intervention_trios_TMF[11:16,],
                                intervention_trios_TMF[18:20,])

my_outcomes <- "cum_defordeg"
ovr_results_TMF <- data.frame()
set.seed(093093)
for(outcome_var in my_outcomes){
    
    # sc_plot_list <- vector('list', nrow(intervention_trios_TMF))
    # baseline_plot_list <- vector('list', nrow(intervention_trios_TMF))
    # gap_plot_list <- vector('list', nrow(intervention_trios_TMF))
    
    # if(anticipation_years != 0){
    #     baselinecombo_plot_list <- vector('list', nrow(intervention_trios_TMF))
    # }
    
    for(r in 1:nrow(intervention_trios_TMF)){
        
        i = intervention_trios_TMF[r,]
        ifund = i %>% select(intervention_fund) %>% pull()
        iname = i %>% select(intervention_name) %>% pull()
        ilevel = i %>% select(intervention_level) %>% pull()
        
        validated = validation_results %>% 
            filter(intervention_name == iname,
                   outcome == outcome_var) %>%
            select(validated) %>% pull()
        
        print(iname)
        
        treat_dat_TMF <- intervention_dat_TMF %>%
            filter(intervention_name == iname,
                   level == ilevel,
                   intervention_fund == ifund)%>%
            mutate(intervention = 1,
                   treat_year = intervention_year - 1990,
                   year = year - 1990,
                   post = (year >= treat_year - backdate_years - anticipation_years)*1,
                   treated = post)
        
        if (ilevel == "ADM0") {
            donor_levels = c("ADM0", "ADM1")
        } else {
            donor_levels = c("ADM0", "ADM1", "ADM2")
        }

            
            control_dat_TMF <- intervention_dat_TMF %>%
                filter(is.na(intervention_name)
                       , ART_eligible == 1
                       , level %in% donor_levels
                )
            
        
        this_treat_year <- treat_dat_TMF %>% select(treat_year) %>% pull() %>% unique() 
        this_area <- treat_dat_TMF %>% select(area) %>% pull() %>% sum() %>% unique()
        this_treecover_2000 <- treat_dat_TMF %>% select(treecover_2000) %>% pull() %>% sum() %>% unique()
        
        control_dat_TMF <- control_dat_TMF %>%
            mutate(intervention = 0,
                   treat_year = 0,
                   year = year - 1990,
                   post = 0,
                   treated = 0)
        
        
        synth_dat_TMF <- rbind(treat_dat_TMF, control_dat_TMF)%>%
            rename(sc_ID = ID)%>%
            select(sc_ID, year, treated, outcome_var, cov.var)%>%
            as.data.frame()
        
        final_match_year = this_treat_year - anticipation_years - 1
        match.out <- outcome_var
        
        synth_out <- microsynth(synth_dat_TMF,
                                idvar="sc_ID",
                                timevar="year",
                                intvar="treated",
                                match.out=match.out, match.covar=cov.var,
                                result.var=outcome_var,
                                end.pre = final_match_year,
                                end.post = c(this_treat_year - 1, this_treat_year),
                                test="twosided",
                                perm=n_perm,
                                jack=TRUE, use.backup=TRUE,
                                n.cores = my_cores
        )
        
        # control <- data.frame("synthetic" = synth_out$Plot.Stats$Control) %>%
        #     rownames_to_column("outcome") %>%
        #     pivot_longer(names_to = "X", values_to = "synthetic", cols = -c(outcome)) %>%
        #     separate(X, into = c(NA, "year"))
        # 
        # if(outcome_var %in% c("cum_deforested", "deforested_diff")){
        #     baseline <- treat_dat_TMF %>%
        #         filter(year < treat_year & year >= treat_year - 5)%>%
        #         group_by(ID)%>%
        #         summarise(baseline = mean(deforested_diff, na.rm = T))%>% 
        #         ungroup %>% select(baseline) %>% pull() %>% sum()
        # } else{
        #     baseline <- treat_dat_TMF %>%
        #         filter(year < treat_year & year >= treat_year - 5)%>%
        #         group_by(ID)%>%
        #         summarise(baseline = mean(defordeg, na.rm = T))%>% 
        #         ungroup %>% select(baseline) %>% pull() %>% sum()
        # }
        # 
        # results_short <- data.frame("treatment" = synth_out$Plot.Stats$Treatment) %>% rownames_to_column("outcome") %>%
        #     pivot_longer(names_to = "X", values_to = "treatment", cols = -c("outcome")) %>%
        #     separate(X, into = c(NA, "year"))%>%
        #     inner_join(control, by = c("outcome", "year")) %>%
        #     mutate(gap = treatment - synthetic
        #     )%>%
        #     filter(year > 10)
        # 
        # if(outcome_var %in% c("cum_deforested", "cum_defordeg")){
        #     cum_start <- results_short %>%
        #         filter(year == this_treat_year - 1)%>%
        #         select(treatment) %>% pull() %>% sum()
        #     
        #     baseline_res <- data.frame("year" = as.character(seq(from = this_treat_year - 1, to = this_treat_year + 4)),
        #                                "baseline_loss" = baseline,
        #                                "cum_start" = cum_start)%>%
        #         mutate(baseline_loss = ifelse(year == this_treat_year - 1, 0, baseline_loss),
        #                baseline_cum = cumsum(baseline_loss),
        #                baseline = cum_start + baseline_cum)%>%
        #         select(year, baseline)
        # } else{
        #     
        #     baseline_res <- data.frame("year" = as.character(seq(from = this_treat_year, to = this_treat_year + 4)),
        #                                "baseline_loss" = baseline)%>%
        #         mutate(baseline = baseline_loss)%>%
        #         select(year, baseline)
        #     
        # }
        
        
        # results_long <- results_short %>%
        #     full_join(baseline_res)%>%
        #     mutate(intervention_name = iname)%>%
        #     pivot_longer(cols = c("treatment", "synthetic", "gap", "baseline"),
        #                  names_to = "results_group", values_to = "value")%>%
        #     drop_na(value)%>%
        #     mutate_at(vars(year, value), as.numeric)
        # 
        # anticipation_shade <- annotate("rect", 
        #                                xmin = final_match_year,
        #                                xmax = this_treat_year - 0.25, 
        #                                ymin = -Inf, ymax = Inf,
        #                                alpha = .25,fill = palette$purple)
        # 
        # 
        # sc_baseline_results <- results_long %>% 
        #     filter(results_group %in% c("synthetic", "baseline")
        #     )
        # 
        # sc_baseline_plot <- ggplot(sc_baseline_results, aes(x = year, y = value, color = results_group, linewidth = results_group))+
        #     geom_vline(xintercept = this_treat_year - 0.1, linetype = "dashed")+
        #     geom_line()+
        #     theme_bw()+
        #     scale_color_manual(values = c(palette$gold, palette$blue, palette$dark_blue))+
        #     scale_linewidth_manual(values = c(1, .6))+
        #     scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        #     theme(#legend.position="none"
        #         legend.title = element_blank()
        #         , plot.title = element_text(hjust = 0.5)
        #         , axis.title.x = element_blank()
        #         , axis.title.y = element_blank()
        #     )
        # 
        # sc_baseline_plot <- sc_baseline_plot + facet_grid(. ~ intervention_name) +
        #     theme(strip.background = element_rect(fill="grey85"),
        #           strip.text = element_text(size=10, color="black"))
        # baseline_plot_list[[r]] <- sc_baseline_plot
        # 
        # sc_path_results <- results_long %>% filter(results_group %in% c("synthetic", "treatment"))
        # 
        # sc_plot <- ggplot(sc_path_results, aes(x = year, y = value, color = results_group))+
        #     geom_vline(xintercept = this_treat_year - 0.5, linetype = "dashed") + 
        #     geom_line()+
        #     theme_bw()+
        #     scale_color_manual(values = c(palette$blue, palette$red, palette$dark_blue))+
        #     scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        #     theme(
        #         legend.position = "none"
        #         #legend.title = element_blank()
        #         , plot.title = element_text(hjust = 0.5)
        #         , axis.title.x = element_blank()
        #         , axis.title.y = element_blank()
        #     )
        # sc_plot <- sc_plot + facet_grid(. ~ intervention_name) +
        #     theme(strip.background = element_rect(fill="grey85"),
        #           strip.text = element_text(size=10, color="black"))
        # sc_plot_list[[r]] <- sc_plot
        # 
        # 
        # combo_results <- results_long %>% filter(results_group %in% c("baseline", "baseline_synthetic", "synthetic", "treatment"))
        # 
        # combo_plot <- ggplot(combo_results, aes(x = year, y = value, color = results_group, linetype = results_group#, linewidth = results_group
        # ))+
        #     anticipation_shade + 
        #     geom_vline(xintercept = this_treat_year - 0.5, linetype = "dashed") + 
        #     geom_line()+
        #     theme_bw()+
        #     scale_color_manual(values = c(palette$gold, palette$gold, palette$blue, palette$red))+
        #     scale_linetype_manual(values = c("solid", "dashed", "solid", "solid"))+
        #     #scale_linewidth_manual(values = c(1.5, 1, 1, 1))+
        #     scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        #     theme(
        #         legend.position = "none"
        #         #legend.title = element_blank()
        #         , plot.title = element_text(hjust = 0.5)
        #         , axis.title.x = element_blank()
        #         , axis.title.y = element_blank()
        #     )
        # combo_plot <- combo_plot + facet_grid(. ~ intervention_name) +
        #     theme(strip.background = element_rect(fill="grey85"),
        #           strip.text = element_text(size=10, color="black"))
        # baselinecombo_plot_list[[r]] <- combo_plot
        
        
        this_intervention_results <- data.frame()
        for(i in c(this_treat_year, this_treat_year - 1
                   )){
            
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
        
        # sc_gap_results <- results_long %>% 
        #     filter(results_group %in% c("gap")
        #                   , year >= final_match_year - 10
        #     ) %>%
        #     left_join(this_intervention_results %>% select(Perm.pVal, year)
        #               , by = c("year"))%>%
        #     mutate(Z = abs(qnorm(Perm.pVal/2)),
        #            se = abs(value/Z),
        #            ci.upper = value + 1.96*se,
        #            ci.lower = value - 1.96*se
        #     )
        # 
        # sc_gap_plot <- ggplot(sc_gap_results, aes(x = year, y = value, color = results_group))+
        #     anticipation_shade + 
        #     #geom_hline(yintercept = 0 , linetype = "dashed")+
        #     geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), color = palette$dark, fill = NA, linetype = "dashed")+
        #     geom_line(linewidth = 1, color = palette$red)+
        #     theme_bw()+
        #     scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        #     theme(
        #         legend.position = "none"
        #         #legend.title = element_blank()
        #         , plot.title = element_text(hjust = 0.5)
        #         , axis.title.x = element_blank()
        #         , axis.title.y = element_blank()
        #     )
        # sc_gap_plot <- sc_gap_plot + facet_grid(. ~ intervention_name) +
        #     theme(strip.background = element_rect(fill="grey85"),
        #           strip.text = element_text(size=10, color="black"))
        # gap_plot_list[[r]] <- sc_gap_plot
        # 
        
        ovr_results_TMF <- this_intervention_results %>%
            bind_rows(ovr_results_TMF)
        
    }
    
    # assign(paste0("sc_plot_list_", outcome_var), sc_plot_list)
    # assign(paste0("baseline_plot_list_", outcome_var), baseline_plot_list)
    # assign(paste0("gap_plot_list_", outcome_var), gap_plot_list)
    # assign(paste0("baselinecombo_plot_list_", outcome_var), baselinecombo_plot_list)
    
}
library(rio)
export(ovr_results_TMF, paste0(results_dir, "/scTMF_ovr_results.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PLOTTING TMF RESULTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# library(ggpubr)
# library(grid)
# 
# #%%%%%%%%%%%%%%%%%
# # CUMULATIVE LOSS PLOTS
# figure <- ggarrange(plotlist=sc_plot_list_cum_defordeg, 
#                     ncol = 3, nrow = 9,
#                     labels = NULL
#                     # , common.legend = TRUE,
#                     #  , legend = "top"
# )
# annotate_figure(figure, left = textGrob("Cumulative deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
#                 bottom = textGrob("Year", gp = gpar(cex = 1.1)))
# ggsave(filename = paste0(fig_dir, "/scTMF_main.png")
#        , width = 9, height = 14)
# 
# figure <- ggarrange(plotlist=gap_plot_list_cum_defordeg, 
#                     ncol = 3, nrow = 9,
#                     labels = NULL)
# annotate_figure(figure, left = textGrob("Cumulative deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
#                 bottom = textGrob("Year", gp = gpar(cex = 1.1)))
# ggsave(filename = paste0(fig_dir, "/scTMF_gap.png")
#        , width = 9, height = 14)
# 
# #%%%%%%%%%%%%%%%%%
# # ANNUAL LOSS PLOTS
# 
# figure <- ggarrange(plotlist=sc_plot_list_defordeg, 
#                     ncol = 3, nrow = 9,
#                     labels = NULL)
# annotate_figure(figure, left = textGrob("Annual deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
#                 bottom = textGrob("Year", gp = gpar(cex = 1.1)))
# ggsave(filename = paste0(fig_dir, "/scTMF_main_annualdefordeg.png")
#        , width = 9, height = 14)
# 
# figure <- ggarrange(plotlist=gap_plot_list_defordeg, 
#                     ncol = 3, nrow = 9,
#                     labels = NULL)
# annotate_figure(figure, left = textGrob("Annual deforestation and degradation (ha)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
#                 bottom = textGrob("Year", gp = gpar(cex = 1.1)))
# ggsave(filename = paste0(fig_dir, "/scTMF_gap_annualdefordeg.png")
#        , width = 9, height = 14)
