library(tidyverse)
library(zoo)
library(tidyquant)
library(fixest)
library(here)
library(modelsummary)
library(kableExtra)
# Use kableExtra (not tinytable) so modelsummary returns a kable object for kable_styling/save_kable
options(modelsummary_factory_default = "kableExtra")
options(modelsummary_factory_latex = "kableExtra")
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

ARTintervention_names <- readRDS("data/processed/ARTintervention_names_new.rds")
Verraintervention_names <- readRDS("data/processed/Verraintervention_names.rds")

intervention_names <- rbind(ARTintervention_names, Verraintervention_names) %>%
    filter(intervention_year <= 2025) %>%
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

dat_long_ART_full <- dat_long_ART %>% right_join(dat_long_ART_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
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
    filter(baseline >1)%>%
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

ART_intervention <- ART_eligible_full %>%
    filter(intervention ==1
           )

ART_preintervention <- ART_intervention %>%
    filter(year <= intervention_year - 2000)%>%
    mutate(intervention_year_dummy = ifelse(year == intervention_year - 2000, 1, 0))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For Verra JNR eligible

dat_long_Verra <- readRDS("data/processed/dat_long_Verra.rds")
dat_long_Verra_beta <- readRDS("data/processed/dat_long_Verra_beta.rds")

dat_long_Verra_full <- dat_long_Verra %>% right_join(dat_long_Verra_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
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
    filter(baseline >1
    )%>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(intervention = replace_na(intervention, 0))

Verra_eligible_full <- dat_long_Verra_full %>% 
    filter(intervention == 0 | intervention_fund == "Verra JNR")%>%
    filter(intervention == 0 | level == intervention_level
    )

Verra_intervention <- Verra_eligible_full %>%
    filter(intervention == 1)

Verra_preintervention <- Verra_intervention %>%
    filter(year <= intervention_year - 2000)%>%
    mutate(intervention_year_dummy = ifelse(year == intervention_year - 2000, 1, 0))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### TMF data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For ART TREES eligible

dat_long_TMF_ART <- readRDS("data/processed/dat_long_TMF_ART.rds")
dat_long_TMF_ART_beta <- readRDS("data/processed/dat_long_TMF_ART_beta.rds")

dat_long_TMF_ART_full <- dat_long_TMF_ART %>% right_join(dat_long_TMF_ART_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
    mutate(
    last_year_diff = last_year_diff * -1 / 10000,
    est_effect_deforested = est_effect_deforested / 10000,
    treecover_remaining = treecover_remaining / 10000,
    area = area / 10000,
    intercept = intercept / 10000,
    slope = slope / 10000,
    baseline_deforested = baseline_deforested / 10000
)


dat_long_TMF_ART_full <- dat_long_TMF_ART_full %>% mutate(
    pct_effect = est_effect_deforested/treecover_remaining,
    pct_intercept = intercept/treecover_remaining,
    pct_slope = slope/treecover_remaining,
    pct_last_year_diff = last_year_diff/treecover_remaining,
    pct_change_effect = est_effect_deforested/baseline_deforested,
    pct_change_intercept = intercept/baseline_deforested,
    pct_change_slope = slope/baseline_deforested,
    pct_change_last_year_diff = last_year_diff/baseline_deforested
)%>%
    mutate_all(~ifelse(is.infinite(.), NA, .)) %>%
    filter(baseline_deforested >1
           )%>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(intervention = replace_na(intervention, 0))

ART_eligible_TMF_full <- dat_long_TMF_ART_full %>%
    filter(intervention == 0 | intervention_fund == "ART TREES")%>%
    filter(intervention == 0 | level == intervention_level |
               (intervention_level == "ADM2" & level == "ADM1")
    )

ART_TMF_intervention <- ART_eligible_TMF_full %>%
    filter(intervention == 1)

ART_TMF_preintervention <- ART_TMF_intervention %>%
    filter(year <= intervention_year)%>%
    mutate(intervention_year_dummy = ifelse(year == intervention_year, 1, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### For Verra JNR eligible

dat_long_TMF_Verra <- readRDS("data/processed/dat_long_TMF_Verra.rds")
dat_long_TMF_Verra_beta <- readRDS("data/processed/dat_long_TMF_Verra_beta.rds")

dat_long_TMF_Verra_full <- dat_long_TMF_Verra %>% right_join(dat_long_TMF_Verra_beta %>% ungroup() %>% select(ID, year, slope, intercept, last_year_diff)) %>% 
    mutate(
        last_year_diff = last_year_diff * -1 / 10000,
        est_effect_deforested = est_effect_deforested / 10000,
        treecover_remaining = treecover_remaining / 10000,
        area = area / 10000,
        intercept = intercept / 10000,
        slope = slope / 10000,
        baseline_deforested = baseline_deforested / 10000
    )


dat_long_TMF_Verra_full <- dat_long_TMF_Verra_full %>% mutate(
    pct_effect = est_effect_deforested/treecover_remaining,
    pct_intercept = intercept/treecover_remaining,
    pct_slope = slope/treecover_remaining,
    pct_last_year_diff = last_year_diff/treecover_remaining,
    pct_change_effect = est_effect_deforested/baseline_deforested,
    pct_change_intercept = intercept/baseline_deforested,
    pct_change_slope = slope/baseline_deforested,
    pct_change_last_year_diff = last_year_diff/baseline_deforested
)%>%
    mutate_all(~ifelse(is.infinite(.), NA, .)) %>%
    filter(baseline_deforested >1
    )%>%
    left_join(intervention_names, by = c("ADM2_CODE", "ADM1_CODE", "ADM0_CODE", "ID", "level"))%>%
    mutate(intervention = replace_na(intervention, 0))

Verra_eligible_TMF_full <- dat_long_TMF_Verra_full %>%
    filter(intervention == 0 | intervention_fund == "Verra JNR")%>%
    filter(intervention == 0 | level == intervention_level
    )

Verra_TMF_intervention <- Verra_eligible_TMF_full %>%
    filter(intervention == 1)

Verra_TMF_preintervention <- Verra_TMF_intervention %>%
    filter(year <= intervention_year)%>%
    mutate(intervention_year_dummy = ifelse(year == intervention_year, 1, 0))

# Save slope analysis datasets to RDS file
slope_analysis_data <- list(
    dat_long_TMF_Verra_full = dat_long_TMF_Verra_full,
    dat_long_TMF_ART_full = dat_long_TMF_ART_full,
    dat_long_Verra_full = dat_long_Verra_full,
    dat_long_ART_full = dat_long_ART_full,
    Verra_eligible_TMF_full = Verra_eligible_TMF_full,
    ART_eligible_TMF_full = ART_eligible_TMF_full,
    Verra_eligible_full = Verra_eligible_full,
    ART_eligible_full = ART_eligible_full
)

saveRDS(slope_analysis_data, "data/processed/slope_analysis_data.rds")

# Compare the distributions of slope in dat_long_Verra_full and dat_long_TMF_Verra_full

# Create density plots for slope in both datasets
ggplot() +
    geom_density(data = dat_long_ART_full, aes(x = intercept, fill = "ART"), alpha = 0.5) +
    geom_density(data = dat_long_TMF_ART_full, aes(x = intercept, fill = "TMF ART"), alpha = 0.5) +
    scale_fill_manual(values = c("ART" = palette$blue, "TMF ART" = palette$green)) +
    theme_bw() +
    labs(
        title = "Comparison of Slope Distributions",
        x = "Slope",
        y = "Density",
        fill = "Dataset"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "top"
    ) +
    scale_x_continuous(limits = c(-100, 100))

ggplot(dat_long_TMF_ART_full, aes(x = abs(slope), y = abs(est_effect_deforested))) +
    geom_point(alpha = 0.5, color = palette$blue) +
    geom_smooth(method = "lm", se = FALSE, color = palette$red) +
    scale_x_log10(limits = c(0.001, 1e7)) +
    scale_y_log10() +
    theme_bw() +
    labs(
        title = "Scatterplot of Slope and Estimated Effect",
        x = "Slope (log scale)",
        y = "Estimated Effect (log scale)"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5)
    )

# ggplot(dat_long_TMF_ART_full, aes(x = abs(last_year_diff), y = abs(est_effect_deforested))) +
#     geom_point(alpha = 0.5, color = palette$blue) +
#     geom_smooth(method = "lm", se = FALSE, color = palette$red) +
#     scale_x_log10(limits = c(0.001, 1e7)) +
#     scale_y_log10() +
#     theme_bw() +
#     labs(
#         title = "Scatterplot of Slope and Estimated Effect",
#         x = "Last year diff. (log scale)",
#         y = "Estimated Effect (log scale)"
#     ) +
#     theme(
#         plot.title = element_text(hjust = 0.5)
#     )

# ggplot(dat_long_ART_full, aes(x = abs(slope), y = abs(est_effect))) +
#     geom_point(alpha = 0.5, color = palette$blue) +
#     geom_smooth(method = "lm", se = FALSE, color = palette$red) +
#     scale_x_log10(limits = c(0.001, 1e7)) +
#     scale_y_log10() +
#     theme_bw() +
#     labs(
#         title = "Scatterplot of Slope and Estimated Effect",
#         x = "Slope (log scale)",
#         y = "Estimated Effect (log scale)"
#     ) +
#     theme(
#         plot.title = element_text(hjust = 0.5)
#     )

# ggplot(dat_long_ART_full, aes(x = abs(last_year_diff), y = abs(est_effect))) +
#     geom_point(alpha = 0.5, color = palette$blue) +
#     geom_smooth(method = "lm", se = FALSE, color = palette$red) +
#     scale_x_log10(limits = c(0.001, 1e7)) +
#     scale_y_log10() +
#     theme_bw() +
#     labs(
#         title = "Scatterplot of Slope and Estimated Effect",
#         x = "Last year diff. (log scale)",
#         y = "Estimated Effect (log scale)"
#     ) +
#     theme(
#         plot.title = element_text(hjust = 0.5)
#     )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### Get datasets without intervention affected jurisdictions

ART_eligible_TMF <- ART_eligible_TMF_full %>%
    filter(intervention == 0
            , year <= max(year, na.rm=T) - 4
           )%>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

(ART_eligible_TMF_count <- length(unique(ART_eligible_TMF$ID)))

ART_eligible <- ART_eligible_full %>%
    filter(intervention == 0
            , year <= max(year, na.rm=T) - 4
           , level != "ADM2"
           )%>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

(ART_eligible_count <- length(unique(ART_eligible$ID)))

Verra_eligible <- Verra_eligible_full %>%
    filter(intervention == 0
            , year <= max(year, na.rm=T) - 4
           )%>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

(Verra_eligible_count <- length(unique(Verra_eligible$ID)))

Verra_eligible_TMF <- Verra_eligible_TMF_full %>%
    filter(intervention == 0
            , year <= max(year, na.rm=T) - 4
    )%>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

(Verra_eligible_TMF_count <- length(unique(Verra_eligible_TMF$ID)))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### Slope and last year regression models 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ART_eligible <- ART_eligible %>%
#     filter(between(slope, quantile(slope, 0.05, na.rm = T), quantile(slope, 0.95, na.rm = T)),
#            between(est_effect, quantile(est_effect, 0.05, na.rm = T), quantile(est_effect, 0.95, na.rm = T)))

# ART_eligible_TMF <- ART_eligible_TMF %>%
#     filter(between(slope, quantile(slope, 0.05, na.rm = T), quantile(slope, 0.95, na.rm = T)),
#            between(est_effect_deforested, quantile(est_effect_deforested, 0.05, na.rm = T), quantile(est_effect_deforested, 0.95, na.rm = T)))

# estimated effect of slope and last year on effect across three groups (All, Verra, ART elig)
raw_models_main <- list(
    # Raw - slope - ART
    "(1)" = feols(est_effect ~ slope,data = ART_eligible),
    # Raw - slope - ART - TMF data
    "(2)" = feols(est_effect_deforested ~ slope,data = ART_eligible_TMF),
    # Raw - slope - Verra
    "(3)" = feols(est_effect ~ slope,data = Verra_eligible),
    # Raw - slope - Verra TMF
    "(4)" = feols(est_effect_deforested ~ slope,data = Verra_eligible_TMF),
    # Raw - last_year_diff - ART
    "(5)" = feols(est_effect ~ last_year_diff,data = ART_eligible),
    # Raw - last_year_diff - ART TMF data
    "(6)" = feols(est_effect_deforested ~ last_year_diff,data = ART_eligible_TMF),
    # Raw - last_year_diff - Verra
    "(7)" = feols(est_effect ~ last_year_diff,data = Verra_eligible),
    # Raw - last_year_diff - Verra TMF
    "(8)" = feols(est_effect_deforested ~ last_year_diff,data = Verra_eligible_TMF)
)
raw_models_main



#Slope models
rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Eligibility', 'ART TREES', 'ART TREES', 'Verra JNR', 'Verra JNR', 'ART TREES', 'ART TREES', 'Verra JNR', 'Verra JNR',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
                )%>%
    as.data.frame()

coef_names = c("slope" = "Slope",
               "last_year_diff" = "Last year diff.")

#%%%%%%% Uncomment to remove dependence on siunitx LaTex package
#options("modelsummary_format_numeric_latex" = "plain")

output <- modelsummary(raw_models_main,
            output = "latex",
       #     title = 'Placebo effect of JREDD based on baseline slope and last year deviations',
             fmt = 4,
            # vcov = ~cluster_variable,
             stars = c('*' = .1, '**' = .05, '***' = .01),
            coef_map = coef_names,
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
            # , notes = ""
            , title = " \\label{tab:slope-regs} Placebo effect of JREDD based on baseline slope and last year deviations."
            # , save = paste0(results_dir, "/main_slope_table.tex")
) 

output

output %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, "Placebo impact of JREDD" = 8))%>%
    save_kable(paste0(results_dir, "/main_slope_table.tex"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Separating out positive vs. negative

interaction_models <- list(
    # Raw - slope - ART
    "(1)" = feols(est_effect ~ slope*positive_slope,data = ART_eligible),
    # Raw - slope - ART - TMF data
    "(2)" = feols(est_effect_deforested ~ slope*positive_slope,data = ART_eligible_TMF),
    # Raw - slope - Verra
    "(3)" = feols(est_effect ~ slope*positive_slope,data = Verra_eligible),
    # Raw - slope - Verra TMF
    "(4)" = feols(est_effect_deforested ~ slope*positive_slope,data = Verra_eligible_TMF),
    # Raw - last_year_diff - ART
    "(5)" = feols(est_effect ~ last_year_diff*positive_year_diff,data = ART_eligible),
    # Raw - last_year_diff - ART TMF data
    "(6)" = feols(est_effect_deforested ~ last_year_diff*positive_year_diff,data = ART_eligible_TMF),
    # Raw - last_year_diff - Verra
    "(7)" = feols(est_effect ~ last_year_diff*positive_year_diff,data = Verra_eligible),
    # Raw - last_year_diff - Verra TMF
    "(8)" = feols(est_effect_deforested ~ last_year_diff*positive_year_diff,data = Verra_eligible_TMF)
)
interaction_models

#Slope models
rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Eligibility', 'ART TREES', 'ART TREES', 'Verra JNR', 'Verra JNR', 'ART TREES', 'ART TREES', 'Verra JNR', 'Verra JNR',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
)%>%
    as.data.frame()

coef_names = c("slope" = "Slope",
               "positive_slope" = "1{slope > 0}",
               "slope:positive_slope" = "Slope x 1{slope > 0}",
               "last_year_diff" = "Last year diff.",
               "positive_year_diff" = "1{last year diff. > 0}",
               "last_year_diff:positive_year_diff" = "Last year diff. x 1{last year diff. > 0}"
               )
modelsummary(interaction_models)
output <- modelsummary(interaction_models,
             output="latex",
             #title = 'Placebo effect of JREDD based on baseline slope and last year deviations',
             fmt = 3,
             # vcov = ~cluster_variable,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = coef_names,
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             # , notes = ""
             , title = " \\label{tab:interaction-regs} Placebo effect of JREDD based on baseline slope and last year deviations."
)

output

output %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, "Placebo impact of JREDD" = 8))%>%
    save_kable(paste0(results_dir, "/interaction_slope_table.tex"))
    # group_tt(j = list(
    #     "Outcome var." = 1,
    #     "Placebo impact of JREDD" = 2:8
    # ))%>%
    # save_tt(paste0(results_dir, "/interaction_slope_table.tex"), overwrite = T)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Do lower slopes/last year differences actually predict enrollment decision/timing
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual ART TREES projects any different than the typical eligible jurisdiction

ART_eligible_full <- ART_eligible_full %>%
    mutate(intervention = replace_na(intervention, 0),
    intervention_start = as.numeric(year == intervention_year-2000))

ART_eligible_TMF_full <- ART_eligible_TMF_full %>%
    mutate(intervention = replace_na(intervention, 0),
    intervention_start = as.numeric(year == intervention_year))

art_diff <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ slope,data = ART_eligible_full),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ slope,data = ART_eligible_TMF_full),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ pct_slope,data = ART_eligible_full),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ pct_slope,data = ART_eligible_TMF_full),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ last_year_diff ,data = ART_eligible_full),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ last_year_diff,data = ART_eligible_TMF_full),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_full),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_TMF_full)
)
art_diff

art_diff_pctile <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ percent_rank(slope), data = ART_eligible_full),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ percent_rank(slope), data = ART_eligible_TMF_full),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ percent_rank(pct_slope), data = ART_eligible_full),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ percent_rank(pct_slope), data = ART_eligible_TMF_full),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ percent_rank(last_year_diff), data = ART_eligible_full),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ percent_rank(last_year_diff), data = ART_eligible_TMF_full),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ percent_rank(pct_last_year_diff), data = ART_eligible_full),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ percent_rank(pct_last_year_diff), data = ART_eligible_TMF_full)
)
art_diff_pctile
ART_eligible_full<- ART_eligible_full %>%
mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

ART_eligible_TMF_full<- ART_eligible_TMF_full %>%
mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))


art_diff_posneg <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ slope*positive_slope,data = ART_eligible_full),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ slope*positive_slope,data = ART_eligible_TMF_full),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ pct_slope*positive_slope,data = ART_eligible_full),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ pct_slope*positive_slope,data = ART_eligible_TMF_full),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ last_year_diff*positive_year_diff ,data = ART_eligible_full),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ last_year_diff*positive_year_diff,data = ART_eligible_TMF_full),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ pct_last_year_diff*positive_year_diff, data = ART_eligible_full),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ pct_last_year_diff*positive_year_diff, data = ART_eligible_TMF_full)
)

art_diff_posneg

interaction_model_hansen <- feols(intervention_start ~ pct_slope*positive_slope,data = ART_eligible_full)
interaction_model_TMF <- feols(intervention_start ~ pct_slope*positive_slope,data = ART_eligible_TMF_full)
# Plot interactions using marginaleffects
library(marginaleffects)
library(ggplot2)

# Create plots for Hansen data interaction
hansen_plot <- plot_predictions(interaction_model_hansen, 
                               condition = c("pct_slope", "positive_slope"),
                               draw = FALSE) %>%
  ggplot(aes(x = pct_slope, y = estimate, color = factor(positive_slope))) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(positive_slope)), 
              alpha = 0.2) +
  labs(title = "Predicted Intervention Probability by Slope (Hansen Data)",
       x = "Slope Percentile",
       y = "Predicted Probability",
       color = "Slope",
       fill = "Slope") +
  scale_color_discrete(labels = c("Negative", "Positive")) +
  scale_fill_discrete(labels = c("Negative", "Positive")) +
  theme_minimal()

# Create plots for TMF data interaction
tmf_plot <- plot_predictions(interaction_model_TMF, 
                            condition = c("pct_slope", "positive_slope"),
                            draw = FALSE) %>%
  ggplot(aes(x = pct_slope, y = estimate, color = factor(positive_slope))) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(positive_slope)), 
              alpha = 0.2) +
  labs(title = "Predicted Intervention Probability by Slope (TMF Data)",
       x = "Slope Percentile",
       y = "Predicted Probability",
       color = "Slope",
       fill = "Slope") +
  scale_color_discrete(labels = c("Negative", "Positive")) +
  scale_fill_discrete(labels = c("Negative", "Positive")) +
  theme_minimal()

# Display plots
hansen_plot
tmf_plot

# Create scatter plot with lowess curve for Hansen data
hansen_scatter <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile, y = intervention)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(title = "Intervention Probability by Slope Percentile (Hansen Data)",
       x = "Slope Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of slope percentile for Hansen data
hansen_hist <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(x = "Slope Percentile",
       y = "Count") +
  theme_minimal()

# Create scatter plot with lowess curve for TMF data
tmf_scatter <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile, y = intervention)) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "lightcoral", alpha = 0.3) +
  labs(title = "Intervention Probability by Slope Percentile (TMF Data)",
       x = "Slope Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of slope percentile for TMF data
tmf_hist <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile)) +
  geom_histogram(bins = 30, fill = "lightcoral", color = "red", alpha = 0.7) +
  labs(x = "Slope Percentile",
       y = "Count") +
  theme_minimal()

# Combine plots using patchwork
library(patchwork)

# Create combined plots with histograms at the bottom
hansen_combined <- hansen_scatter / hansen_hist + plot_layout(heights = c(3, 1))
tmf_combined <- tmf_scatter / tmf_hist + plot_layout(heights = c(3, 1))

# Display combined plots
hansen_combined
tmf_combined


# Create scatter plot with lowess curve for Hansen data (last_year_diff)
hansen_scatter_diff <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile, y = intervention)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(title = "Intervention Probability by Last Year Diff Percentile (Hansen Data)",
       x = "Last Year Diff Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of last_year_diff percentile for Hansen data
hansen_hist_diff <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(x = "Last Year Diff Percentile",
       y = "Count") +
  theme_minimal()

# Create scatter plot with lowess curve for TMF data (last_year_diff)
tmf_scatter_diff <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile, y = intervention)) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "lightcoral", alpha = 0.3) +
  labs(title = "Intervention Probability by Last Year Diff Percentile (TMF Data)",
       x = "Last Year Diff Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of last_year_diff percentile for TMF data
tmf_hist_diff <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile)) +
  geom_histogram(bins = 30, fill = "lightcoral", color = "red", alpha = 0.7) +
  labs(x = "Last Year Diff Percentile",
       y = "Count") +
  theme_minimal()

# Create combined plots with histograms at the bottom for last_year_diff
hansen_combined_diff <- hansen_scatter_diff / hansen_hist_diff + plot_layout(heights = c(3, 1))
tmf_combined_diff <- tmf_scatter_diff / tmf_hist_diff + plot_layout(heights = c(3, 1))

# Display combined plots for last_year_diff
hansen_combined_diff
tmf_combined_diff


# Calculate mean and sd by percentile of slope for Hansen data
hansen_percentiles <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of slope for TMF data
tmf_percentiles <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_percentiles <- bind_rows(hansen_percentiles, tmf_percentiles)

# Create plot of intervention rates by slope percentile
percentile_plot <- combined_percentiles %>%
  ggplot(aes(x = slope_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2) +
  labs(title = "Intervention Rate by Slope Percentile",
       x = "Slope Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
percentile_plot

# Display results
hansen_percentiles
tmf_percentiles


# Calculate mean and sd by percentile of last_year_diff for Hansen data
hansen_last_year_diff_percentiles <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of last_year_diff for TMF data
tmf_last_year_diff_percentiles <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_last_year_diff_percentiles <- bind_rows(hansen_last_year_diff_percentiles, tmf_last_year_diff_percentiles)

# Create plot of intervention rates by last_year_diff percentile
last_year_diff_percentile_plot <- combined_last_year_diff_percentiles %>%
  ggplot(aes(x = last_year_diff_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2) +
  labs(title = "Intervention Rate by Last Year Diff Percentile",
       x = "Last Year Diff Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
last_year_diff_percentile_plot

# Display results
hansen_last_year_diff_percentiles
tmf_last_year_diff_percentiles


# Get the range of slopes and last_year_diff for intervention==1 observations
hansen_intervention_slope_range <- ART_eligible_full %>% 
  filter(intervention == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

tmf_intervention_slope_range <- ART_eligible_TMF_full %>% 
  filter(intervention == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

hansen_intervention_diff_range <- ART_eligible_full %>% 
  filter(intervention == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

tmf_intervention_diff_range <- ART_eligible_TMF_full %>% 
  filter(intervention == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

# Calculate mean and sd by log scale units for Hansen data (adjusting for negative numbers)
hansen_log_scale <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  filter(slope >= hansen_intervention_slope_range$slope_min & 
         slope <= hansen_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (adjusting for negative numbers)
tmf_log_scale <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  filter(slope >= tmf_intervention_slope_range$slope_min & 
         slope <= tmf_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting
combined_log_scale <- bind_rows(hansen_log_scale, tmf_log_scale)

# Create plot of intervention rates by log slope unit
log_scale_plot <- combined_log_scale %>%
  ggplot(aes(x = log_slope_unit, y = mean_intervention, color = data_source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2) +
  labs(title = "Intervention Rate by Log Slope Unit",
       x = "Log Slope Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display log scale plot
log_scale_plot

# Display log scale results
hansen_log_scale
tmf_log_scale

# Calculate mean and sd by log scale units for Hansen data (last_year_diff)
hansen_log_scale_diff <- ART_eligible_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= hansen_intervention_diff_range$diff_min & 
         last_year_diff <= hansen_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (last_year_diff)
tmf_log_scale_diff <- ART_eligible_TMF_full %>% 
  filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= tmf_intervention_diff_range$diff_min & 
         last_year_diff <= tmf_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention, na.rm = TRUE),
    sd_intervention = sd(intervention, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting (last_year_diff)
combined_log_scale_diff <- bind_rows(hansen_log_scale_diff, tmf_log_scale_diff)

# Create plot of intervention rates by log last year diff unit
log_scale_diff_plot <- combined_log_scale_diff %>%
  ggplot(aes(x = log_diff_unit, y = mean_intervention, color = data_source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2) +
  labs(title = "Intervention Rate by Log Last Year Diff Unit",
       x = "Log Last Year Diff Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display log scale diff plot
log_scale_diff_plot

# Display log scale diff results
hansen_log_scale_diff
tmf_log_scale_diff

# Create combined figure with log scale and percentile plots
# Arrange: log scale plots on top row, percentile plots on bottom row
# slope plots on left, last year diff plots on right

combined_figure <- (log_scale_plot + log_scale_diff_plot) / 
                   (percentile_plot + last_year_diff_percentile_plot)

# Display the combined figure
combined_figure

# Save the combined figure
ggsave("figs/enrollment_analyses_combined.png", combined_figure, 
       width = 12, height = 10, dpi = 300)
ggsave(paste0(fig_dir_pdf, "/enrollment_analyses_combined.pdf"), combined_figure, 
       width = 12, height = 10)







# art_diff <- list(
#     # Slope ART
#     '(1)' = feglm(intervention ~ slope, data = ART_eligible_full %>% filter(intervention == 0 | year <= intervention_year - 2000), family = binomial()),
#     # Slope ART TMF
#     '(2)' = feglm(intervention ~ slope, data = ART_eligible_TMF_full %>% filter(intervention == 0 | year <= intervention_year), family = binomial()),
#     # Pct. Slope ART
#     '(3)' = feglm(intervention ~ pct_slope, data = ART_eligible_full %>% filter(intervention == 0 | year <= intervention_year - 2000), family = binomial()),
#     # Pct. Slope ART TMF
#     '(4)' = feglm(intervention ~ pct_slope, data = ART_eligible_TMF_full %>% filter(intervention == 0 | year <= intervention_year), family = binomial()),
#     # Last year diff. ART
#     '(5)' = feglm(intervention ~ last_year_diff, data = ART_eligible_full %>% filter(intervention == 0 | year <= intervention_year - 2000), family = binomial()),
#     # Last year diff. ART TMF
#     '(6)' = feglm(intervention ~ last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 0 | year <= intervention_year), family = binomial()),
#     # Pct. Last year diff. ART
#     '(7)' = feglm(intervention ~ pct_last_year_diff, data = ART_eligible_full %>% filter(intervention == 0 | year <= intervention_year - 2000), family = binomial()),
#     # Pct. Last year diff. ART TMF
#     '(8)' = feglm(intervention ~ pct_last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 0 | year <= intervention_year), family = binomial())
# )

art_diff
art_diff_pctile

#Slope models
rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Eligibility', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
)%>%
    as.data.frame()

coef_names = c("slope" = "Slope",
               "last_year_diff" = "Last year diff.",
               "pct_slope" = "Slope (%)",
               "pct_last_year_diff" = "Last year diff. (%)")

output <- modelsummary(art_diff,
                       output="latex",
                 #      title = 'Does slope predict ART TREES participation?',
                       fmt = 7,
                       # vcov = ~cluster_variable,
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       coef_map = coef_names,
                       gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
                       , add_rows = rows
                       # , notes = ""
                       , title = " \\label{tab:art-diff} Does slope predict ART TREES participation?"
)

output

output %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, " " = 8))%>%
    save_kable(paste0(results_dir, "/art_diff_table.tex"))
    # group_tt(j = list(
    #     "Outcome var." = 1,
    #     " " = 2:8
    # ))%>%
    # save_tt(paste0(results_dir, "/art_diff_table.tex"), overwrite = T)

# Create similar table for art_diff_pctile
rows_pctile <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Eligibility', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
)%>%
    as.data.frame()

coef_names_pctile = c("percent_rank(slope)" = "Slope (percentile)",
                      "percent_rank(last_year_diff)" = "Last year diff. (percentile)",
                      "percent_rank(pct_slope)" = "Slope % (percentile)",
                      "percent_rank(pct_last_year_diff)" = "Last year diff. % (percentile)")

output_pctile <- modelsummary(art_diff_pctile,
                       output="latex",
                       fmt = 7,
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       coef_map = coef_names_pctile,
                       gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE',
                       add_rows = rows_pctile,
                       title = " \\label{tab:art-diff-pctile} Does slope percentile predict ART TREES participation?"
)

output_pctile

output_pctile %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, " " = 8))%>%
    save_kable(paste0(results_dir, "/art_diff_pctile_table.tex"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual ART TREES project enrollment years any different than the pre-treatment years

art_timing <- list(
    # Slope ART
    '(1)' = feols(intervention_year_dummy ~ slope | ID,data = ART_preintervention),
    # Slope ART TMF
    '(2)' = feols(intervention_year_dummy ~ slope  | ID,data = ART_TMF_preintervention),
    # Pct. Slope ART
    '(3)' = feols(intervention_year_dummy ~ pct_slope | ID,data = ART_preintervention),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_year_dummy ~ pct_slope  | ID,data = ART_TMF_preintervention),
    # Last year diff. ART
    '(5)' = feols(intervention_year_dummy ~ last_year_diff  | ID,data = ART_preintervention),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_year_dummy ~ last_year_diff  | ID,data = ART_TMF_preintervention),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_year_dummy ~ pct_last_year_diff  | ID,data = ART_preintervention),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_year_dummy ~ pct_last_year_diff | ID,data = ART_TMF_preintervention )
)

art_timing <- list(
    # Slope ART
    '(1)' = feglm(intervention_year_dummy ~ slope | ID, data = ART_preintervention, family = binomial(link = "logit")),
    # Slope ART TMF
    '(2)' = feglm(intervention_year_dummy ~ slope | ID, data = ART_TMF_preintervention, family = binomial(link = "logit")),
    # Pct. Slope ART
    '(3)' = feglm(intervention_year_dummy ~ pct_slope | ID, data = ART_preintervention, family = binomial(link = "logit")),
    # Pct. Slope ART TMF
    '(4)' = feglm(intervention_year_dummy ~ pct_slope | ID, data = ART_TMF_preintervention, family = binomial(link = "logit")),
    # Last year diff. ART
    '(5)' = feglm(intervention_year_dummy ~ last_year_diff | ID, data = ART_preintervention, family = binomial(link = "logit")),
    # Last year diff. ART TMF
    '(6)' = feglm(intervention_year_dummy ~ last_year_diff | ID, data = ART_TMF_preintervention, family = binomial(link = "logit")),
    # Pct. Last year diff. ART
    '(7)' = feglm(intervention_year_dummy ~ pct_last_year_diff | ID, data = ART_preintervention, family = binomial(link = "logit")),
    # Pct. Last year diff. ART TMF
    '(8)' = feglm(intervention_year_dummy ~ pct_last_year_diff | ID, data = ART_TMF_preintervention, family = binomial(link = "logit"))
)

art_timing

rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Jurusdiction FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                'Eligibility', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES', 'ART TREES', 'ART TREES', 'ART TREES','ART TREES',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
)%>%
    as.data.frame()

coef_names = c("slope" = "Slope",
               "last_year_diff" = "Last year diff.",
               "pct_slope" = "Slope (%)",
               "pct_last_year_diff" = "Last year diff. (%)")

output <- modelsummary(art_timing,
                       output="latex",
                    #   title = 'Does within jurisdiction slope predict ART TREES enrollment timing?',
                       fmt = 7,
                       # vcov = ~cluster_variable,
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       coef_map = coef_names,
                       gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
                       , add_rows = rows
                       # , notes = ""
                       , title = " \\label{tab:art-timing} Does within jurisdiction slope predict ART TREES enrollment timing?"
)

output

output %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, " " = 8))%>%
    save_kable(paste0(results_dir, "/art_timing_table.tex"))
    # group_tt(j = list(
    #     "Outcome var." = 1,
    #     " " = 2:8
    # ))%>%
    # save_tt(paste0(results_dir, "/art_timing_table.tex"), overwrite = T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual Verra projects any different than the typical

verra_diff <- list(
    # Slope ART
    '(1)' = feols(intervention ~ slope, data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000)),
    # Slope ART TMF
    '(2)' = feols(intervention ~ slope, data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year)),
    # Pct. Slope ART
    '(3)' = feols(intervention ~ pct_slope , data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000)),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention ~ pct_slope, data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year)),
    # Last year diff. ART
    '(5)' = feols(intervention ~ last_year_diff  , data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000)),
    # Last year diff. ART TMF
    '(6)' = feols(intervention ~ last_year_diff  , data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year)),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention ~ pct_last_year_diff  , data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000)),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention ~ pct_last_year_diff  , data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year))
)

verra_diff

rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`,
                'Eligibility', 'Verra JNR', 'Verra JNR', 'Verra JNR', 'Verra JNR', 'Verra JNR', 'Verra JNR', 'Verra JNR', 'Verra JNR',
                'Data source', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF', 'Hansen', 'TMF',
                'Reference period', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years', '5 years'
)%>%
    as.data.frame()

coef_names = c("slope" = "Slope",
               "last_year_diff" = "Last year diff.",
               "pct_slope" = "Slope (%)",
               "pct_last_year_diff" = "Last year diff. (%)")

output <- modelsummary(verra_diff,
                       output = "latex",
                       title = " \\label{tab:verra-diff} Does slope predict Verra JNR participation?",
                       fmt = 7,
                       # vcov = ~cluster_variable,
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       coef_map = coef_names,
                       gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
                       , add_rows = rows
                       # , notes = ""
)

output

output %>%
    kable_styling(latex_options = c("hold_position", "booktabs"))%>%
    add_header_above(c("Outcome var." = 1, " " = 8))%>%
    save_kable(paste0(results_dir, "/verra_diff_table.tex"))
    # group_tt(j = list(
    #     "Outcome var." = 1,
    #     " " = 2:8
    # ))%>%
    # save_tt(paste0(results_dir, "/verra_diff_table.tex"), overwrite = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Split by positive slope/last year diff
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ART_eligible_full <- ART_eligible_full %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

ART_eligible_TMF_full <- ART_eligible_TMF_full %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))


art_diff_split <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ slope,data = ART_eligible_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_slope),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ slope,data = ART_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_slope),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ pct_slope,data = ART_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_slope),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ pct_slope,data = ART_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_slope),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ last_year_diff ,data = ART_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_year_diff),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ last_year_diff,data = ART_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_year_diff)
)

art_diff_split

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual ART TREES project enrollment years any different than the pre-treatment years

ART_preintervention <- ART_preintervention %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

ART_TMF_preintervention <- ART_TMF_preintervention %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))
art_timing_split <- list(
    # Slope ART
    '(1)' = feols(intervention_year_dummy ~ slope | ID,data = ART_preintervention,  
    fsplit = ~ positive_year_diff),
    # Slope ART TMF
    '(2)' = feols(intervention_year_dummy ~ slope | ID,data = ART_TMF_preintervention,  
    fsplit = ~ positive_year_diff),
    # Pct. Slope ART
    '(3)' = feols(intervention_year_dummy ~ pct_slope  | ID,data = ART_preintervention,  
    fsplit = ~ positive_year_diff),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_year_dummy ~ pct_slope | ID,data = ART_TMF_preintervention,  
    fsplit = ~ positive_year_diff),
    # Last year diff. ART
    '(5)' = feols(intervention_year_dummy ~ last_year_diff  | ID,data = ART_preintervention,  
    fsplit = ~ positive_year_diff),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_year_dummy ~ last_year_diff  | ID,data = ART_TMF_preintervention,  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_year_dummy ~ pct_last_year_diff  | ID,data = ART_preintervention,  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_year_dummy ~ pct_last_year_diff | ID,data = ART_TMF_preintervention,  
    fsplit = ~ positive_year_diff)
)

art_timing_split


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual Verra projects any different than the typical

Verra_eligible_full <- Verra_eligible_full %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))

Verra_eligible_TMF_full <- Verra_eligible_TMF_full %>%
    mutate(positive_slope = ifelse(slope > 0, 1, 0),
           positive_year_diff = ifelse(last_year_diff > 0, 1, 0))


Verra_diff_split <- list(
    # Slope Verra
    '(1)' = feols(intervention ~ slope,data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_slope),
    # Slope Verra TMF
    '(2)' = feols(intervention ~ slope,data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_slope),
    # Pct. Slope Verra
    '(3)' = feols(intervention ~ pct_slope,data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_slope),
    # Pct. Slope Verra TMF
    '(4)' = feols(intervention ~ pct_slope,data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_slope),
     # Last year diff. Verra
    '(5)' = feols(intervention ~ last_year_diff ,data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_year_diff),
    # Last year diff. Verra TMF
    '(6)' = feols(intervention ~ last_year_diff,data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. Verra
    '(7)' = feols(intervention ~ pct_last_year_diff, data = Verra_eligible_full %>% filter(intervention ==0 | year <= intervention_year - 2000),  
    fsplit = ~ positive_year_diff),
    # Pct. Last year diff. Verra TMF
    '(8)' = feols(intervention ~ pct_last_year_diff, data = Verra_eligible_TMF_full %>% filter(intervention ==0 | year <= intervention_year),  
    fsplit = ~ positive_year_diff)
)

Verra_diff_split




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Inductive approach
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Inductive approach for ART eligible Js

ART_inductive <- ART_eligible_full %>% filter(intervention == 0 | (intervention == 1 & year + 2000 == intervention_year))%>%
    ungroup() %>%
   # filter(year > 0)%>%
    mutate(percentile_effect = cume_dist(est_effect),
           percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_change_effect = cume_dist(pct_change_effect),
           percentile_slope = cume_dist(slope),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_change_slope = cume_dist(pct_change_slope),
           percentile_last_year_diff = cume_dist(last_year_diff),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
           percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
           pct_effect_positive = ifelse(pct_effect >0, 1, 0))

ART_TMF_inductive <- ART_eligible_TMF_full %>% filter(intervention == 0 | (intervention == 1 & year == intervention_year))%>%
    ungroup() %>%
    filter(year > 2000)%>%
    mutate(percentile_effect = cume_dist(est_effect_deforested),
           percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_change_effect = cume_dist(pct_change_effect),
           percentile_slope = cume_dist(slope),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_change_slope = cume_dist(pct_change_slope),
           percentile_last_year_diff = cume_dist(last_year_diff),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
           percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
           pct_effect_positive = ifelse(pct_effect >0, 1, 0))


my_binwidth = .025 #0.05

binned_pct_slope <- ART_inductive %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF <- ART_TMF_inductive %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff <- ART_inductive %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF <- ART_TMF_inductive %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)


intervention_percentile <- ART_inductive %>%
    filter(intervention ==1)

intervention_percentile_TMF <- ART_TMF_inductive %>%
    filter(intervention ==1)

#### PERCENTILES

colors <- c("Hansen" = palette$red, "TMF" = palette$blue)

# last year diff pct
lastyear_bar_ART <- ggplot() + 
    geom_bar(data = binned_pct_last_year_diff, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
   # geom_rug(data = intervention_percentile, aes(percentile_pct_last_year_diff, color = "Hansen"))+
   # geom_rug(data = intervention_percentile_TMF, aes(percentile_pct_last_year_diff, color = "TMF"))+
    geom_vline(xintercept = (filter(ART_inductive, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()

lastyear_smooth_ART <- ggplot() + 
    geom_smooth(data = ART_inductive %>% filter(intervention == 0), aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)+
    geom_smooth(data = ART_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)+
    labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))

lastyear_hist_ART <- ggplot()+
    geom_histogram(data = intervention_percentile, aes(percentile_pct_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF, aes(percentile_pct_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = colors)+
    labs(x = "Last year diff. percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw()

lastyear_distributions_ART <- ggplot()+
    geom_density(data = ART_preintervention %>% filter(intervention_year_dummy == 1), aes(pct_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_density(data = ART_preintervention %>% filter(intervention_year_dummy == 0), aes(pct_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = colors)+
    #labs(x = "Last year diff.", y = "ART TREES participants")+
   # scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw()

# slope pct
slope_bar_ART <- ggplot() + 
    geom_bar(data = binned_pct_slope, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
   # geom_rug(data = intervention_percentile, aes(percentile_pct_slope, color = "Hansen"))+
   # geom_rug(data = intervention_percentile_TMF, aes(percentile_pct_slope, color = "TMF"))+
    geom_vline(xintercept = (filter(ART_inductive, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()

slope_smooth_ART <- ggplot() + 
    geom_smooth(data = ART_inductive %>% filter(intervention == 0), aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)+
    geom_smooth(data = ART_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)+
    labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))

slope_hist_ART <- ggplot()+
    geom_histogram(data = intervention_percentile, aes(percentile_pct_slope, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF, aes(percentile_pct_slope, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = colors)+
    labs(x = "Slope percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Inductive approach for Verra eligible Js

Verra_inductive <- Verra_eligible_full %>% filter(intervention == 0 | (intervention == 1 & year + 2000 == intervention_year))%>%
    ungroup() %>%
    #filter(year > 10)%>%
    mutate(percentile_effect = cume_dist(est_effect),
           percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_change_effect = cume_dist(pct_change_effect),
           percentile_slope = cume_dist(slope),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_change_slope = cume_dist(pct_change_slope),
           percentile_last_year_diff = cume_dist(last_year_diff),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
           percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
           pct_effect_positive = ifelse(pct_effect >0, 1, 0))

Verra_TMF_inductive <- Verra_eligible_TMF_full %>% filter(intervention == 0 | (intervention == 1 & year == intervention_year))%>%
    ungroup() %>%
    filter(year > 2000)%>%
    mutate(percentile_effect = cume_dist(est_effect_deforested),
           percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_change_effect = cume_dist(pct_change_effect),
           percentile_slope = cume_dist(slope),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_change_slope = cume_dist(pct_change_slope),
           percentile_last_year_diff = cume_dist(last_year_diff),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
           percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
           pct_effect_positive = ifelse(pct_effect >0, 1, 0))


binned_pct_slope <- Verra_inductive %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF <- Verra_TMF_inductive %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff <- Verra_inductive %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF <- Verra_TMF_inductive %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)


intervention_percentile_Verra <- Verra_inductive %>%
    filter(intervention ==1)

intervention_percentile_Verra_TMF <- Verra_TMF_inductive %>%
    filter(intervention ==1)

#### PERCENTILES

colors <- c("Hansen" = palette$red, "TMF" = palette$blue)

# last year diff pct
lastyear_bar_Verra <- ggplot() + 
    geom_bar(data = binned_pct_last_year_diff, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    # geom_rug(data = intervention_percentile, aes(percentile_pct_last_year_diff, color = "Hansen"))+
    # geom_rug(data = intervention_percentile_TMF, aes(percentile_pct_last_year_diff, color = "TMF"))+
    geom_vline(xintercept = (filter(Verra_inductive, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()

lastyear_smooth_Verra <- ggplot() + 
    geom_smooth(data = Verra_inductive %>% filter(intervention == 0), aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)+
    geom_smooth(data = Verra_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)+
    labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))

# slope pct
slope_bar_Verra <- ggplot() + 
    geom_bar(data = binned_pct_slope, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    # geom_rug(data = intervention_percentile, aes(percentile_pct_slope, color = "Hansen"))+
    # geom_rug(data = intervention_percentile_TMF, aes(percentile_pct_slope, color = "TMF"))+
    geom_vline(xintercept = (filter(Verra_inductive, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()

slope_smooth_Verra <- ggplot() + 
    geom_smooth(data = Verra_inductive %>% filter(intervention == 0), aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)+
    geom_smooth(data = Verra_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)+
    labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = colors)+
    theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Percentile intead of percentile_pct
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# binned_percentile_slope <- ART_inductive %>%
#     mutate(bin = cut_interval(percentile_slope, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_slope_TMF <- ART_TMF_inductive %>%
#     mutate(bin = cut_interval(percentile_slope, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_last_year_diff <- ART_inductive %>%
#     mutate(bin = cut_interval(percentile_last_year_diff, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_last_year_diff_TMF <- ART_TMF_inductive %>%
#     mutate(bin = cut_interval(percentile_last_year_diff, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# 
# intervention_percentile <- ART_inductive %>%
#     filter(intervention ==1)
# 
# intervention_percentile_TMF <- ART_TMF_inductive %>%
#     filter(intervention ==1)
# 
# #### PERCENTILES
# 
# colors <- c("Hansen" = palette$red, "TMF" = palette$blue)
# 
# # last year diff percentile_
# lastyear_bar_ART <- ggplot() +
#     geom_bar(data = binned_percentile_last_year_diff, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
#     geom_bar(data = binned_percentile_last_year_diff_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
#    # geom_rug(data = intervention_percentile, aes(percentile_last_year_diff, color = "Hansen"))+
#    # geom_rug(data = intervention_percentile_TMF, aes(percentile_last_year_diff, color = "TMF"))+
#     geom_vline(xintercept = (filter(ART_inductive, abs(percentile_last_year_diff) == min(abs(percentile_last_year_diff), na.rm = T)))$percentile_last_year_diff, color = palette$red)+
#     geom_vline(xintercept = (filter(ART_TMF_inductive, abs(percentile_last_year_diff) == min(abs(percentile_last_year_diff), na.rm = T)))$percentile_last_year_diff, color = palette$blue)+
#     labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()
# 
# lastyear_smooth_ART <- ggplot() +
#     geom_smooth(data = ART_inductive %>% filter(intervention == 0), aes(x=percentile_last_year_diff, y=percentile_effect, color = "Hansen"), linewidth = 1.5)+
#     geom_smooth(data = ART_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_last_year_diff, y=percentile_effect, color = "TMF"), linewidth = 1.5)+
#     labs(x = "Last year diff. percentile", y = "Effect percentile") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
# 
# lastyear_hist_ART <- ggplot()+
#     geom_histogram(data = intervention_percentile, aes(percentile_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
#     geom_histogram(data = intervention_percentile_TMF, aes(percentile_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
#     scale_fill_manual(name = "Dataset", values = colors)+
#     labs(x = "Last year diff. percentile", y = "ART TREES participants")+
#     scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
#     theme_bw()
# 
# lastyear_distributions_ART <- ggplot()+
#     geom_density(data = ART_preintervention %>% filter(intervention_year_dummy == 1), aes(percentile_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
#     geom_density(data = ART_preintervention %>% filter(intervention_year_dummy == 0), aes(percentile_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
#     scale_fill_manual(name = "Dataset", values = colors)+
#     #labs(x = "Last year diff.", y = "ART TREES participants")+
#    # scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
#     theme_bw()
# 
# # slope percentile_
# slope_bar_ART <- ggplot() +
#     geom_bar(data = binned_percentile_slope, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
#     geom_bar(data = binned_percentile_slope_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
#    # geom_rug(data = intervention_percentile, aes(percentile_slope, color = "Hansen"))+
#    # geom_rug(data = intervention_percentile_TMF, aes(percentile_slope, color = "TMF"))+
#     geom_vline(xintercept = (filter(ART_inductive, abs(percentile_slope) == min(abs(percentile_slope), na.rm = T)))$percentile_slope, color = palette$red)+
#     geom_vline(xintercept = (filter(ART_TMF_inductive, abs(percentile_slope) == min(abs(percentile_slope), na.rm = T)))$percentile_slope, color = palette$blue)+
#     labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()
# 
# slope_smooth_ART <- ggplot() +
#     geom_smooth(data = ART_inductive %>% filter(intervention == 0), aes(x=percentile_slope, y=percentile_effect, color = "Hansen"), linewidth = 1.5)+
#     geom_smooth(data = ART_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_slope, y=percentile_effect, color = "TMF"), linewidth = 1.5)+
#     labs(x = "Slope percentile", y = "Effect percentile") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
# 
# slope_hist_ART <- ggplot()+
#     geom_histogram(data = intervention_percentile, aes(percentile_slope, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
#     geom_histogram(data = intervention_percentile_TMF, aes(percentile_slope, fill = "TMF"), color = palette$grey, alpha = 0.5)+
#     scale_fill_manual(name = "Dataset", values = colors)+
#     labs(x = "Slope percentile", y = "ART TREES participants")+
#     scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
#     theme_bw()
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Inductive approach for ART eligible Js
# my_binwidth = .025
# Verra_inductive <- Verra_eligible_full %>% filter(intervention == 0 | (intervention == 1 & year + 2000 == intervention_year))%>%
#     ungroup() %>%
#     #filter(year > 10)%>%
#     mutate(percentile_effect = cume_dist(est_effect),
#            percentile_pct_effect = cume_dist(pct_effect),
#            percentile_pct_change_effect = cume_dist(pct_change_effect),
#            percentile_slope = cume_dist(slope),
#            percentile_pct_slope = cume_dist(pct_slope),
#            percentile_pct_change_slope = cume_dist(pct_change_slope),
#            percentile_last_year_diff = cume_dist(last_year_diff),
#            percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
#            percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
#            pct_effect_positive = ifelse(pct_effect >0, 1, 0)) %>%
#            filter(baseline > 100)
# 
# Verra_TMF_inductive <- Verra_eligible_TMF_full %>% filter(intervention == 0 | (intervention == 1 & year == intervention_year))%>%
#     ungroup() %>%
#     filter(year > 2000)%>%
#     mutate(percentile_effect = cume_dist(est_effect_deforested),
#            percentile_pct_effect = cume_dist(pct_effect),
#            percentile_pct_change_effect = cume_dist(pct_change_effect),
#            percentile_slope = cume_dist(slope),
#            percentile_pct_slope = cume_dist(pct_slope),
#            percentile_pct_change_slope = cume_dist(pct_change_slope),
#            percentile_last_year_diff = cume_dist(last_year_diff),
#            percentile_pct_last_year_diff = cume_dist(pct_last_year_diff),
#            percentile_pct_change_last_year_diff = cume_dist(pct_change_last_year_diff),
#            pct_effect_positive = ifelse(pct_effect >0, 1, 0)) %>%
#            filter(baseline_deforested > 100)
# 
# 
# binned_percentile_slope <- Verra_inductive %>%
#     mutate(bin = cut_interval(percentile_slope, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_slope_TMF <- Verra_TMF_inductive %>%
#     mutate(bin = cut_interval(percentile_slope, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_last_year_diff <- Verra_inductive %>%
#     mutate(bin = cut_interval(percentile_last_year_diff, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# binned_percentile_last_year_diff_TMF <- Verra_TMF_inductive %>%
#     mutate(bin = cut_interval(percentile_last_year_diff, length=my_binwidth))%>%
#     group_by(bin)%>%
#     summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
#     drop_na(bin)%>%
#     separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
#     mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
#            bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
#            bin_mean = (bin_end + bin_start) / 2)
# 
# 
# intervention_percentile_Verra <- Verra_inductive %>%
#     filter(intervention ==1)
# 
# intervention_percentile_Verra_TMF <- Verra_TMF_inductive %>%
#     filter(intervention ==1)
# 
# #### PERCENTILES
# 
# colors <- c("Hansen" = palette$red, "TMF" = palette$blue)
# 
# # last year diff pct
# lastyear_bar_Verra <- ggplot() +
#     geom_bar(data = binned_percentile_last_year_diff, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
#     geom_bar(data = binned_percentile_last_year_diff_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
#     # geom_rug(data = intervention_percentile, aes(percentile_last_year_diff, color = "Hansen"))+
#     # geom_rug(data = intervention_percentile_TMF, aes(percentile_last_year_diff, color = "TMF"))+
#     geom_vline(xintercept = (filter(Verra_inductive, abs(percentile_last_year_diff) == min(abs(percentile_last_year_diff), na.rm = T)))$percentile_last_year_diff, color = palette$red)+
#     geom_vline(xintercept = (filter(Verra_TMF_inductive, abs(percentile_last_year_diff) == min(abs(percentile_last_year_diff), na.rm = T)))$percentile_last_year_diff, color = palette$blue)+
#     labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()
# 
# lastyear_smooth_Verra <- ggplot() +
#     geom_smooth(data = Verra_inductive %>% filter(intervention == 0), aes(x=percentile_last_year_diff, y=percentile_effect, color = "Hansen"), linewidth = 1.5)+
#     geom_smooth(data = Verra_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_last_year_diff, y=percentile_effect, color = "TMF"), linewidth = 1.5)+
#     labs(x = "Last year diff. percentile", y = "Effect percentile") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
# 
# lastyear_hist_Verra <- ggplot()+
#     geom_histogram(data = intervention_percentile_Verra, aes(percentile_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
#     geom_histogram(data = intervention_percentile_Verra_TMF, aes(percentile_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
#     scale_fill_manual(name = "Dataset", values = colors)+
#     labs(x = "Last year diff. percentile", y = "Verra JNR participants")+
#     scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
#     theme_bw()
# # slope pct
# slope_bar_Verra <- ggplot() +
#     geom_bar(data = binned_percentile_slope, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
#     geom_bar(data = binned_percentile_slope_TMF, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
#     # geom_rug(data = intervention_percentile, aes(percentile_slope, color = "Hansen"))+
#     # geom_rug(data = intervention_percentile_TMF, aes(percentile_slope, color = "TMF"))+
#     geom_vline(xintercept = (filter(Verra_inductive, abs(percentile_slope) == min(abs(percentile_slope), na.rm = T)))$percentile_slope, color = palette$red)+
#     geom_vline(xintercept = (filter(Verra_TMF_inductive, abs(percentile_slope) == min(abs(percentile_slope), na.rm = T)))$percentile_slope, color = palette$blue)+
#     labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()
# 
# slope_smooth_Verra <- ggplot() +
#     geom_smooth(data = Verra_inductive %>% filter(intervention == 0), aes(x=percentile_slope, y=percentile_effect, color = "Hansen"), linewidth = 1.5)+
#     geom_smooth(data = Verra_TMF_inductive %>% filter(intervention == 0), aes(x=percentile_slope, y=percentile_effect, color = "TMF"), linewidth = 1.5)+
#     labs(x = "Slope percentile", y = "Effect percentile") +
#     scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
#     scale_color_manual(name = "Dataset", values = colors)+
#     theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
# 
# slope_hist_Verra <- ggplot()+
#     geom_histogram(data = intervention_percentile_Verra, aes(percentile_slope, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
#     geom_histogram(data = intervention_percentile_Verra_TMF, aes(percentile_slope, fill = "TMF"), color = palette$grey, alpha = 0.5)+
#     scale_fill_manual(name = "Dataset", values = colors)+
#     labs(x = "Slope percentile", y = "Verra JNR participants")+
#     scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
#     theme_bw()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Multi panel plots for inductive approach

# ART eligible for both slope and last year diff.
figure <- ggarrange(slope_bar_ART, lastyear_bar_ART + theme(axis.title.y = element_blank()), 
                    slope_smooth_ART, lastyear_smooth_ART + theme(axis.title.y = element_blank()), 
                    slope_hist_ART, lastyear_hist_ART + theme(axis.title.y = element_blank()),
                    ncol = 2, nrow = 3,
                    heights = c(1,1,.8),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure

ggsave(filename = paste0(fig_dir, "/inductive_plots_ART.png")
       , width = 9, height = 11)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_ART.pdf")
       , width = 9, height = 11)

# Verra eligible for both slope and last year diff.
figure <- ggarrange(slope_bar_Verra, lastyear_bar_Verra + theme(axis.title.y = element_blank()), 
                    slope_smooth_Verra, lastyear_smooth_Verra + theme(axis.title.y = element_blank()), 
                  #  slope_hist_Verra, lastyear_hist_Verra + theme(axis.title.y = element_blank()),
                    ncol = 2, nrow = 2,
                    heights = c(1,1,.8),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure

ggsave(filename = paste0(fig_dir, "/inductive_plots_Verra.png")
       , width = 8, height = 8)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_Verra.pdf")
       , width = 8, height = 8)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Inductive plots split by forest cover (above/below median)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Calculate forest cover percentage and median for ART
ART_inductive <- ART_inductive %>%
    mutate(pct_forest_cover = treecover_remaining / area)

ART_TMF_inductive <- ART_TMF_inductive %>%
    mutate(pct_forest_cover = treecover_remaining / area)

# Calculate median forest cover for ART (using unique IDs to avoid double counting)
ART_median_forest_cover <- ART_inductive %>%
    filter(intervention == 0) %>%
    select(ID, pct_forest_cover) %>%
    distinct() %>%
    pull(pct_forest_cover) %>%
    median(na.rm = TRUE)

# Split ART datasets by forest cover
ART_inductive_high_forest <- ART_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= ART_median_forest_cover) %>%
    ungroup() %>%
    filter(has_high_forest)

ART_inductive_low_forest <- ART_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= ART_median_forest_cover) %>%
    ungroup() %>%
    filter(!has_high_forest)

ART_TMF_inductive_high_forest <- ART_TMF_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= ART_median_forest_cover) %>%
    ungroup() %>%
    filter(has_high_forest)

ART_TMF_inductive_low_forest <- ART_TMF_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= ART_median_forest_cover) %>%
    ungroup() %>%
    filter(!has_high_forest)

# Recalculate percentiles for split datasets
ART_inductive_high_forest <- ART_inductive_high_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

ART_inductive_low_forest <- ART_inductive_low_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

ART_TMF_inductive_high_forest <- ART_TMF_inductive_high_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

ART_TMF_inductive_low_forest <- ART_TMF_inductive_low_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

my_binwidth = .025 #0.05
# Create binned data for high forest ART
binned_pct_slope_ART_high <- ART_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF_ART_high <- ART_TMF_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_ART_high <- ART_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF_ART_high <- ART_TMF_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

# Create binned data for low forest ART
binned_pct_slope_ART_low <- ART_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF_ART_low <- ART_TMF_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_ART_low <- ART_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF_ART_low <- ART_TMF_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

# Intervention percentiles for ART
intervention_percentile_ART_high <- ART_inductive_high_forest %>%
    filter(intervention ==1)

intervention_percentile_TMF_ART_high <- ART_TMF_inductive_high_forest %>%
    filter(intervention ==1)

intervention_percentile_ART_low <- ART_inductive_low_forest %>%
    filter(intervention ==1)

intervention_percentile_TMF_ART_low <- ART_TMF_inductive_low_forest %>%
    filter(intervention ==1)

# Create plots for ART high forest cover
(slope_bar_ART_high <- ggplot() + 
    geom_bar(data = binned_pct_slope_ART_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF_ART_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(ART_inductive_high_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive_high_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(slope_smooth_ART_high <- {
    p <- ggplot()
    has_hansen <- nrow(ART_inductive_high_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(ART_TMF_inductive_high_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = ART_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = ART_TMF_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
    p + labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(slope_hist_ART_high <- ggplot()+
    geom_histogram(data = intervention_percentile_ART_high, aes(percentile_pct_slope, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF_ART_high, aes(percentile_pct_slope, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    labs(x = "Slope percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw())

(lastyear_bar_ART_high <- ggplot() + 
    geom_bar(data = binned_pct_last_year_diff_ART_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF_ART_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(ART_inductive_high_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive_high_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(lastyear_smooth_ART_high <- {
    p <- ggplot()
    has_hansen <- nrow(ART_inductive_high_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(ART_TMF_inductive_high_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = ART_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = ART_TMF_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
        p + labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(lastyear_hist_ART_high <- ggplot()+
    geom_histogram(data = intervention_percentile_ART_high, aes(percentile_pct_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF_ART_high, aes(percentile_pct_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    labs(x = "Last year diff. percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw())

# Create plots for ART low forest cover
(slope_bar_ART_low <- ggplot() + 
    geom_bar(data = binned_pct_slope_ART_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF_ART_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(ART_inductive_low_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive_low_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(slope_smooth_ART_low <- {
    p <- ggplot()
    has_hansen <- nrow(ART_inductive_low_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(ART_TMF_inductive_low_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = ART_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = ART_TMF_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
        p + labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(slope_hist_ART_low <- ggplot()+
    geom_histogram(data = intervention_percentile_ART_low, aes(percentile_pct_slope, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF_ART_low, aes(percentile_pct_slope, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    labs(x = "Slope percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw())

(lastyear_bar_ART_low <- ggplot() + 
    geom_bar(data = binned_pct_last_year_diff_ART_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF_ART_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(ART_inductive_low_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(ART_TMF_inductive_low_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(lastyear_smooth_ART_low <- {
    p <- ggplot()
    has_hansen <- nrow(ART_inductive_low_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(ART_TMF_inductive_low_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = ART_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = ART_TMF_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
    p + labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(lastyear_hist_ART_low <- ggplot()+
    geom_histogram(data = intervention_percentile_ART_low, aes(percentile_pct_last_year_diff, fill = "Hansen"), color = palette$grey, alpha = 0.5)+
    geom_histogram(data = intervention_percentile_TMF_ART_low, aes(percentile_pct_last_year_diff, fill = "TMF"), color = palette$grey, alpha = 0.5)+
    scale_fill_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    labs(x = "Last year diff. percentile", y = "ART TREES participants")+
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0,5))+
    theme_bw())

# Multi panel plots for ART split by forest cover
figure_ART_high <- ggarrange(slope_bar_ART_high, lastyear_bar_ART_high + theme(axis.title.y = element_blank()), 
                    slope_smooth_ART_high, lastyear_smooth_ART_high + theme(axis.title.y = element_blank()), 
                    slope_hist_ART_high, lastyear_hist_ART_high + theme(axis.title.y = element_blank()),
                    ncol = 2, nrow = 3,
                    heights = c(1,1,.8),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure_ART_high <- annotate_figure(figure_ART_high, 
                                    top = text_grob("Above median forest cover for ART eligible jurisdictions", 
                                                    size = 14, face = "bold"))
figure_ART_high

ggsave(filename = paste0(fig_dir, "/inductive_plots_ART_high_forest.png")
       , width = 9, height = 11)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_ART_high_forest.pdf")
       , width = 9, height = 11)

figure_ART_low <- ggarrange(slope_bar_ART_low, lastyear_bar_ART_low + theme(axis.title.y = element_blank()), 
                    slope_smooth_ART_low, lastyear_smooth_ART_low + theme(axis.title.y = element_blank()), 
                    slope_hist_ART_low, lastyear_hist_ART_low + theme(axis.title.y = element_blank()),
                    ncol = 2, nrow = 3,
                    heights = c(1,1,.8),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure_ART_low <- annotate_figure(figure_ART_low, 
                                  top = text_grob("Below median forest cover for ART eligible jurisdictions", 
                                                  size = 14, face = "bold"))
figure_ART_low

ggsave(filename = paste0(fig_dir, "/inductive_plots_ART_low_forest.png")
       , width = 9, height = 11)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_ART_low_forest.pdf")
       , width = 9, height = 11)

# Now do the same for Verra
# Calculate forest cover percentage and median for Verra
Verra_inductive <- Verra_inductive %>%
    mutate(pct_forest_cover = treecover_remaining / area)

Verra_TMF_inductive <- Verra_TMF_inductive %>%
    mutate(pct_forest_cover = treecover_remaining / area)

# Calculate median forest cover for Verra
Verra_median_forest_cover <- Verra_inductive %>%
    filter(intervention == 0) %>%
    select(ID, pct_forest_cover) %>%
    distinct() %>%
    pull(pct_forest_cover) %>%
    median(na.rm = TRUE)

# Split Verra datasets by forest cover
Verra_inductive_high_forest <- Verra_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= Verra_median_forest_cover) %>%
    ungroup() %>%
    filter(has_high_forest)

Verra_inductive_low_forest <- Verra_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= Verra_median_forest_cover) %>%
    ungroup() %>%
    filter(!has_high_forest)

Verra_TMF_inductive_high_forest <- Verra_TMF_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= Verra_median_forest_cover) %>%
    ungroup() %>%
    filter(has_high_forest)

Verra_TMF_inductive_low_forest <- Verra_TMF_inductive %>%
    group_by(ID) %>%
    mutate(has_high_forest = first(pct_forest_cover) >= Verra_median_forest_cover) %>%
    ungroup() %>%
    filter(!has_high_forest)

# Recalculate percentiles for split datasets
Verra_inductive_high_forest <- Verra_inductive_high_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

Verra_inductive_low_forest <- Verra_inductive_low_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

Verra_TMF_inductive_high_forest <- Verra_TMF_inductive_high_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

Verra_TMF_inductive_low_forest <- Verra_TMF_inductive_low_forest %>%
    mutate(percentile_pct_effect = cume_dist(pct_effect),
           percentile_pct_slope = cume_dist(pct_slope),
           percentile_pct_last_year_diff = cume_dist(pct_last_year_diff))

# Create binned data for high forest Verra
my_binwidth = .025 #0.05
binned_pct_slope_Verra_high <- Verra_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF_Verra_high <- Verra_TMF_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_Verra_high <- Verra_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF_Verra_high <- Verra_TMF_inductive_high_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

# Create binned data for low forest Verra
binned_pct_slope_Verra_low <- Verra_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_slope_TMF_Verra_low <- Verra_TMF_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_slope, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_Verra_low <- Verra_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

binned_pct_last_year_diff_TMF_Verra_low <- Verra_TMF_inductive_low_forest %>%
    mutate(bin = cut_interval(percentile_pct_last_year_diff, length=my_binwidth))%>%
    group_by(bin)%>%
    summarise(proportion_positive_effect = mean(pct_effect_positive, na.rm = T))%>%
    drop_na(bin)%>%
    separate(bin, into = c("bin_start", "bin_end"), sep = ",") %>%
    mutate(bin_start = as.numeric(gsub("[()]|[][]", "", bin_start)),
           bin_end = as.numeric(gsub("[()]|[][]", "", bin_end)),
           bin_mean = (bin_end + bin_start) / 2)

# Create plots for Verra high forest cover
(slope_bar_Verra_high <- ggplot() +
    geom_bar(data = binned_pct_slope_Verra_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF_Verra_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(Verra_inductive_high_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive_high_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(slope_smooth_Verra_high <- {
    p <- ggplot()
    has_hansen <- nrow(Verra_inductive_high_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(Verra_TMF_inductive_high_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = Verra_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = Verra_TMF_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
        p + labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(lastyear_bar_Verra_high <- ggplot() +
    geom_bar(data = binned_pct_last_year_diff_Verra_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF_Verra_high, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(Verra_inductive_high_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive_high_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(lastyear_smooth_Verra_high <- {
    p <- ggplot()
    has_hansen <- nrow(Verra_inductive_high_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(Verra_TMF_inductive_high_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = Verra_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = Verra_TMF_inductive_high_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
        p + labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

# Create plots for Verra low forest cover
(slope_bar_Verra_low <- ggplot() +
    geom_bar(data = binned_pct_slope_Verra_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_slope_TMF_Verra_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(Verra_inductive_low_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive_low_forest, abs(pct_slope) == min(abs(pct_slope), na.rm = T)))$percentile_pct_slope, color = palette$blue)+
    labs(x = "Slope percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(slope_smooth_Verra_low <- {
    p <- ggplot()
    has_hansen <- nrow(Verra_inductive_low_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(Verra_TMF_inductive_low_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = Verra_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = Verra_TMF_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_slope, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
        p + labs(x = "Slope percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

(lastyear_bar_Verra_low <- ggplot() +
    geom_bar(data = binned_pct_last_year_diff_Verra_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$red, alpha = 0.5)+
    geom_bar(data = binned_pct_last_year_diff_TMF_Verra_low, aes(x = bin_mean, y = proportion_positive_effect), stat='identity', fill = palette$blue, alpha = 0.5)+
    geom_vline(xintercept = (filter(Verra_inductive_low_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$red)+
    geom_vline(xintercept = (filter(Verra_TMF_inductive_low_forest, abs(pct_last_year_diff) == min(abs(pct_last_year_diff), na.rm = T)))$percentile_pct_last_year_diff, color = palette$blue)+
    labs(x = "Last year diff. percentile", y = "Proportion with placebo credits > 0") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
    scale_color_manual(name = "Dataset", values = c("Hansen" = palette$red, "TMF" = palette$blue))+
    theme_bw())

(lastyear_smooth_Verra_low <- {
    p <- ggplot()
    has_hansen <- nrow(Verra_inductive_low_forest %>% filter(intervention == 0)) > 0
    has_tmf <- nrow(Verra_TMF_inductive_low_forest %>% filter(intervention == 0)) > 0
    
    datasets_present <- c()
    if(has_hansen) {
        p <- p + geom_smooth(data = Verra_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "Hansen"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "Hansen")
    }
    if(has_tmf) {
        p <- p + geom_smooth(data = Verra_TMF_inductive_low_forest %>% filter(intervention == 0), 
                            aes(x=percentile_pct_last_year_diff, y=percentile_pct_effect, color = "TMF"), linewidth = 1.5)
        datasets_present <- c(datasets_present, "TMF")
    }
    
    color_map <- c("Hansen" = palette$red, "TMF" = palette$blue)
    active_colors <- color_map[names(color_map) %in% datasets_present]
    
    p + labs(x = "Last year diff. percentile", y = "Placebo credits generated percentile") +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0,1))+
        scale_color_manual(name = "Dataset", values = active_colors)+
        theme_bw()+ guides(color=guide_legend(override.aes=list(fill=NA)))
})

# Multi panel plots for Verra split by forest cover
figure_Verra_high <- ggarrange(slope_bar_Verra_high, lastyear_bar_Verra_high + theme(axis.title.y = element_blank()), 
                    slope_smooth_Verra_high, lastyear_smooth_Verra_high + theme(axis.title.y = element_blank()), 
                    ncol = 2, nrow = 2,
                    heights = c(1,1),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure_Verra_high <- annotate_figure(figure_Verra_high, 
                                     top = text_grob("Above median forest cover for Verra eligible jurisdictions", 
                                                     size = 14, face = "bold"))
figure_Verra_high

ggsave(filename = paste0(fig_dir, "/inductive_plots_Verra_high_forest.png")
       , width = 8, height = 8)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_Verra_high_forest.pdf")
       , width = 8, height = 8)

figure_Verra_low <- ggarrange(slope_bar_Verra_low, lastyear_bar_Verra_low + theme(axis.title.y = element_blank()), 
                    slope_smooth_Verra_low, lastyear_smooth_Verra_low + theme(axis.title.y = element_blank()), 
                    ncol = 2, nrow = 2,
                    heights = c(1,1),
                    common.legend = TRUE,
                    legend = "bottom",
                    labels="AUTO"
)
figure_Verra_low <- annotate_figure(figure_Verra_low, 
                                    top = text_grob("Below median forest cover for Verra eligible jurisdictions", 
                                                    size = 14, face = "bold"))
figure_Verra_low

ggsave(filename = paste0(fig_dir, "/inductive_plots_Verra_low_forest.png")
       , width = 8, height = 8)
ggsave(filename = paste0(fig_dir_pdf, "/inductive_plots_Verra_low_forest.pdf")
       , width = 8, height = 8)


