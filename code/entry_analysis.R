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

# Load slope analysis datasets
slope_analysis_data <- readRDS("data/processed/slope_analysis_data.rds")

# Extract individual datasets
dat_long_TMF_Verra_full <- slope_analysis_data$dat_long_TMF_Verra_full
dat_long_TMF_ART_full <- slope_analysis_data$dat_long_TMF_ART_full
dat_long_Verra_full <- slope_analysis_data$dat_long_Verra_full
dat_long_ART_full <- slope_analysis_data$dat_long_ART_full
Verra_eligible_TMF_full <- slope_analysis_data$Verra_eligible_TMF_full
ART_eligible_TMF_full <- slope_analysis_data$ART_eligible_TMF_full
Verra_eligible_full <- slope_analysis_data$Verra_eligible_full
ART_eligible_full <- slope_analysis_data$ART_eligible_full

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Do lower slopes/last year differences actually predict enrollment decision/timing
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Are actual ART TREES projects any different than the typical eligible jurisdiction

ART_eligible_full <- ART_eligible_full %>%
    mutate(intervention = replace_na(intervention, 0),
           intervention_start = case_when(
               intervention == 0 ~ 0,
               year < intervention_year - 2000 ~ 0,
               year == intervention_year - 2000 ~ 1,
               TRUE ~ NA_real_
           ))

ART_eligible_TMF_full <- ART_eligible_TMF_full %>%
    mutate(intervention = replace_na(intervention, 0),
           intervention_start = case_when(
               intervention == 0 ~ 0,
               year < intervention_year ~ 0,
               year == intervention_year ~ 1,
               TRUE ~ NA_real_
           ))

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

art_diff_timing <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ slope, data = ART_eligible_full %>% filter(intervention == 1)),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ slope, data = ART_eligible_TMF_full %>% filter(intervention == 1)),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ pct_slope, data = ART_eligible_full %>% filter(intervention == 1)),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ pct_slope, data = ART_eligible_TMF_full %>% filter(intervention == 1)),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ last_year_diff, data = ART_eligible_full %>% filter(intervention == 1)),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 1)),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_full %>% filter(intervention == 1)),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ pct_last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 1))
)
art_diff_timing

# Normalized versions of all regressions
art_diff_norm <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ scale(slope), data = ART_eligible_full),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ scale(slope), data = ART_eligible_TMF_full),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ scale(pct_slope), data = ART_eligible_full),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ scale(pct_slope), data = ART_eligible_TMF_full),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ scale(last_year_diff), data = ART_eligible_full),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ scale(last_year_diff), data = ART_eligible_TMF_full),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ scale(pct_last_year_diff), data = ART_eligible_full),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ scale(pct_last_year_diff), data = ART_eligible_TMF_full)
)
art_diff_norm

art_diff_timing_norm <- list(
    # Slope ART
    '(1)' = feols(intervention_start ~ scale(slope), data = ART_eligible_full %>% filter(intervention == 1)),
    # Slope ART TMF
    '(2)' = feols(intervention_start ~ scale(slope), data = ART_eligible_TMF_full %>% filter(intervention == 1)),
    # Pct. Slope ART
    '(3)' = feols(intervention_start ~ scale(pct_slope), data = ART_eligible_full %>% filter(intervention == 1)),
    # Pct. Slope ART TMF
    '(4)' = feols(intervention_start ~ scale(pct_slope), data = ART_eligible_TMF_full %>% filter(intervention == 1)),
     # Last year diff. ART
    '(5)' = feols(intervention_start ~ scale(last_year_diff), data = ART_eligible_full %>% filter(intervention == 1)),
    # Last year diff. ART TMF
    '(6)' = feols(intervention_start ~ scale(last_year_diff), data = ART_eligible_TMF_full %>% filter(intervention == 1)),
    # Pct. Last year diff. ART
    '(7)' = feols(intervention_start ~ scale(pct_last_year_diff), data = ART_eligible_full %>% filter(intervention == 1)),
    # Pct. Last year diff. ART TMF
    '(8)' = feols(intervention_start ~ scale(pct_last_year_diff), data = ART_eligible_TMF_full %>% filter(intervention == 1))
)
art_diff_timing_norm

# Create coefficient plot data
coef_data <- tibble(
    # Selection models
    model = c("Selection", "Selection", "Selection", "Selection", 
              "Selection", "Selection", "Selection", "Selection",
              # Timing models  
              "Timing", "Timing", "Timing", "Timing",
              "Timing", "Timing", "Timing", "Timing"),
    tmf = c("Hansen", "TMF", "Hansen", "TMF", 
            "Hansen", "TMF", "Hansen", "TMF",
            "Hansen", "TMF", "Hansen", "TMF", 
            "Hansen", "TMF", "Hansen", "TMF"),
    variable = c("Slope", "Slope", "Pct Slope", "Pct Slope",
                 "Last Year Diff", "Last Year Diff", "Pct Last Year Diff", "Pct Last Year Diff",
                 "Slope", "Slope", "Pct Slope", "Pct Slope",
                 "Last Year Diff", "Last Year Diff", "Pct Last Year Diff", "Pct Last Year Diff"),
    percent_forest = c("No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes",
                       "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"),
    coefficient = c(
        # Selection coefficients
        art_diff_norm[[1]]$coefficients[2], art_diff_norm[[2]]$coefficients[2],
        art_diff_norm[[3]]$coefficients[2], art_diff_norm[[4]]$coefficients[2],
        art_diff_norm[[5]]$coefficients[2], art_diff_norm[[6]]$coefficients[2],
        art_diff_norm[[7]]$coefficients[2], art_diff_norm[[8]]$coefficients[2],
        # Timing coefficients
        art_diff_timing_norm[[1]]$coefficients[2], art_diff_timing_norm[[2]]$coefficients[2],
        art_diff_timing_norm[[3]]$coefficients[2], art_diff_timing_norm[[4]]$coefficients[2],
        art_diff_timing_norm[[5]]$coefficients[2], art_diff_timing_norm[[6]]$coefficients[2],
        art_diff_timing_norm[[7]]$coefficients[2], art_diff_timing_norm[[8]]$coefficients[2]
    ),
    se = c(
        # Selection standard errors
        sqrt(diag(vcov(art_diff_norm[[1]])))[2], sqrt(diag(vcov(art_diff_norm[[2]])))[2],
        sqrt(diag(vcov(art_diff_norm[[3]])))[2], sqrt(diag(vcov(art_diff_norm[[4]])))[2],
        sqrt(diag(vcov(art_diff_norm[[5]])))[2], sqrt(diag(vcov(art_diff_norm[[6]])))[2],
        sqrt(diag(vcov(art_diff_norm[[7]])))[2], sqrt(diag(vcov(art_diff_norm[[8]])))[2],
        # Timing standard errors
        sqrt(diag(vcov(art_diff_timing_norm[[1]])))[2], sqrt(diag(vcov(art_diff_timing_norm[[2]])))[2],
        sqrt(diag(vcov(art_diff_timing_norm[[3]])))[2], sqrt(diag(vcov(art_diff_timing_norm[[4]])))[2],
        sqrt(diag(vcov(art_diff_timing_norm[[5]])))[2], sqrt(diag(vcov(art_diff_timing_norm[[6]])))[2],
        sqrt(diag(vcov(art_diff_timing_norm[[7]])))[2], sqrt(diag(vcov(art_diff_timing_norm[[8]])))[2]
    )
) %>%
    mutate(
        ci_lower = coefficient - 1.96 * se,
        ci_upper = coefficient + 1.96 * se,
        slope_type = case_when(
            str_detect(variable, "Slope") ~ "Slope",
            str_detect(variable, "Diff") ~ "Last Year Diff"
        ),
        # Create simplified labels
        y_label = paste(tmf, model, sep = ".")
    )

# Create coefficient plot
coef_plot <- coef_data %>%
    ggplot(aes(x = coefficient, y = slope_type, 
            color = tmf)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    facet_grid(model ~ percent_forest, 
               labeller = labeller(
                   percent_forest = c("No" = "Total Forest", 
                                      "Yes" = "Percent Forest Area")
               )) +
    labs(
        x = "Standardized Coefficient",
        y = "Independent Variable",
        color = "Data Source"
    ) +
    scale_color_manual(values = c("Hansen" = "#2E86AB", "TMF" = "#A23B72")) +
    theme_minimal() +
    theme(
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
    )

print(coef_plot)

# Save the coefficient plot
ggsave(here::here("figs", "entry_regressions_coefs.png"), 
       plot = coef_plot, 
       width = 10, 
       height = 4, 
       dpi = 500)
ggsave(here::here("figs_pdf", "entry_regressions_coefs.pdf"), 
       plot = coef_plot, 
       width = 10, 
       height = 4)

# Run regressions with interaction between slope/last_year_diff and pct_slope/pct_last_year_diff for both ART and ART TMF
art_diff <- list(
    # Slope x Last year diff. - ART
    '(1)' = feols(intervention_start ~ slope * last_year_diff, data = ART_eligible_full),
    # Slope x Last year diff. - ART TMF
    '(2)' = feols(intervention_start ~ slope * last_year_diff, data = ART_eligible_TMF_full),
    # Pct. Slope x Pct. Last year diff. - ART
    '(3)' = feols(intervention_start ~ pct_slope * pct_last_year_diff, data = ART_eligible_full),
    # Pct. Slope x Pct. Last year diff. - ART TMF
    '(4)' = feols(intervention_start ~ pct_slope * pct_last_year_diff, data = ART_eligible_TMF_full)
)
art_diff

art_diff_timing <- list(
    # Slope x Last year diff. - ART (intervention == 1)
    '(1)' = feols(intervention_start ~ slope * last_year_diff, data = ART_eligible_full %>% filter(intervention == 1)),
    # Slope x Last year diff. - ART TMF (intervention == 1)
    '(2)' = feols(intervention_start ~ slope * last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 1)),
    # Pct. Slope x Pct. Last year diff. - ART (intervention == 1)
    '(3)' = feols(intervention_start ~ pct_slope * pct_last_year_diff, data = ART_eligible_full %>% filter(intervention == 1)),
    # Pct. Slope x Pct. Last year diff. - ART TMF (intervention == 1)
    '(4)' = feols(intervention_start ~ pct_slope * pct_last_year_diff, data = ART_eligible_TMF_full %>% filter(intervention == 1))
)
art_diff_timing


# Coefficient plots for ART and ART TMF interaction models

# Prepare tidy summaries for plotting
library(broom)
library(tibble)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)

# Helper function to clean up term labels for plot
clean_term <- function(term) {
  term <- gsub("slope", "Slope", term)
  term <- gsub("last_year_diff", "Last year diff.", term)
  term <- gsub("pct_slope", "Pct. Slope", term)
  term <- gsub("pct_last_year_diff", "Pct. Last year diff.", term)
  term <- gsub(":", " × ", term)
  term
}

# Combine summary for art_diff
art_diff_models <- art_diff
art_diff_tidy <- map2_df(
  art_diff_models, names(art_diff_models),
  ~broom::tidy(.x, conf.int = TRUE) %>%
    mutate(model = .y,
           data = ifelse(.y %in% c("(1)", "(3)"), "Hansen", "TMF"),
           timing = "All")
)

art_diff_timing_models <- art_diff_timing
art_diff_timing_tidy <- map2_df(
  art_diff_timing_models, names(art_diff_timing_models),
  ~broom::tidy(.x, conf.int = TRUE) %>%
    mutate(model = .y,
           data = ifelse(.y %in% c("(1)", "(3)"), "Hansen", "TMF"),
           timing = "Intervention Only")
)

art_diff_alltidy <- bind_rows(art_diff_tidy, art_diff_timing_tidy) %>%
  mutate(term = clean_term(term)) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = case_when(
    model %in% c("(1)", "(2)") ~ "Slope × Last year diff.",
    model %in% c("(3)", "(4)") ~ "Pct. Slope × Pct. Last year diff.",
    TRUE ~ model
  )) %>%
  mutate(model = fct_inorder(model),
         data = ifelse(data == "Hansen", "Hansen", "TMF"),
         timing = fct_rev(as.factor(timing)))

# Plot
coef_artdiff_plot <- ggplot(art_diff_alltidy, aes(x=estimate, y=term, color=data, shape=timing)) +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0.2, position=position_dodge(width=0.5)) +
  geom_vline(xintercept=0, linetype="dashed", alpha=0.6) +
  facet_wrap(~model, ncol=1, scales="free_y") +
  labs(
    x = "Coefficient estimate",
    y = "",
    color = "Data source",
    shape = "Sample",
    title = "Coefficient plots: Interaction Models for ART/ART TMF"
  ) +
  scale_color_manual(values = c("Hansen" = "#2E86AB", "TMF" = "#A23B72")) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(coef_artdiff_plot)









       



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
                               condition = list("pct_slope" , "positive_slope"),
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
                            condition = list("pct_slope", "positive_slope"),
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
#   filter(intervention == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile, y = intervention_start)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(title = "Intervention Probability by Slope Percentile (Hansen Data)",
       x = "Slope Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of slope percentile for Hansen data
hansen_hist <- ART_eligible_full %>% 
#   filter(intervention == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(x = "Slope Percentile",
       y = "Count") +
  theme_minimal()

# Create scatter plot with lowess curve for TMF data
tmf_scatter <- ART_eligible_TMF_full %>% 
#   filter(intervention == 1) %>%
  mutate(slope_percentile = percent_rank(slope)) %>%
  ggplot(aes(x = slope_percentile, y = intervention_start)) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "lightcoral", alpha = 0.3) +
#   geom_point(size = 3, color = "red") +
  labs(title = "Intervention Probability by Slope Percentile (TMF Data)",
       x = "Slope Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of slope percentile for TMF data
tmf_hist <- ART_eligible_TMF_full %>% 
#   filter(intervention == 1) %>%
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
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile, y = intervention_start)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(title = "Intervention Probability by Last Year Diff Percentile (Hansen Data)",
       x = "Last Year Diff Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of last_year_diff percentile for Hansen data
hansen_hist_diff <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(x = "Last Year Diff Percentile",
       y = "Count") +
  theme_minimal()

# Create scatter plot with lowess curve for TMF data (last_year_diff)
tmf_scatter_diff <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = percent_rank(last_year_diff)) %>%
  ggplot(aes(x = last_year_diff_percentile, y = intervention_start)) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "lightcoral", alpha = 0.3) +
  labs(title = "Intervention Probability by Last Year Diff Percentile (TMF Data)",
       x = "Last Year Diff Percentile",
       y = "Intervention Probability") +
  theme_minimal()

# Create histogram of last_year_diff percentile for TMF data
tmf_hist_diff <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
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

########################################################
# Percentile plots
########################################################

# Calculate mean and sd by percentile of slope for Hansen data
hansen_percentiles <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of slope for TMF data
tmf_percentiles <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_percentiles <- bind_rows(hansen_percentiles, tmf_percentiles)

# Create plot of intervention rates by slope percentile
percentile_plot <- combined_percentiles %>%
  ggplot(aes(x = slope_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Slope Percentile",
       x = "Slope Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
percentile_plot

# Display results
hansen_percentiles
tmf_percentiles


# Calculate mean and sd by percentile of last_year_diff for Hansen data
hansen_last_year_diff_percentiles <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of last_year_diff for TMF data
tmf_last_year_diff_percentiles <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_last_year_diff_percentiles <- bind_rows(hansen_last_year_diff_percentiles, tmf_last_year_diff_percentiles)

# Create plot of intervention rates by last_year_diff percentile
last_year_diff_percentile_plot <- combined_last_year_diff_percentiles %>%
  ggplot(aes(x = last_year_diff_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Last Year Diff Percentile",
       x = "Last Year Diff Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme(legend.position = "bottom")

# Display plot
last_year_diff_percentile_plot

# Display results
hansen_last_year_diff_percentiles
tmf_last_year_diff_percentiles


# Get the range of slopes and last_year_diff for intervention==1 observations
hansen_intervention_slope_range <- ART_eligible_full %>% 
  filter(intervention_start == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

tmf_intervention_slope_range <- ART_eligible_TMF_full %>% 
  filter(intervention_start == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

hansen_intervention_diff_range <- ART_eligible_full %>% 
  filter(intervention_start == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

tmf_intervention_diff_range <- ART_eligible_TMF_full %>% 
  filter(intervention_start == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

# Calculate mean and sd by log scale units for Hansen data (adjusting for negative numbers)
hansen_log_scale <- ART_eligible_full %>% 
#   filter(intervention ==
  filter(slope >= hansen_intervention_slope_range$slope_min & 
         slope <= hansen_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (adjusting for negative numbers)
tmf_log_scale <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(slope >= tmf_intervention_slope_range$slope_min & 
         slope <= tmf_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  filter(!is.na(log_slope_unit) & !is.infinite(log_slope_unit)) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting
combined_log_scale <- bind_rows(hansen_log_scale, tmf_log_scale) %>%
  filter(!is.na(log_slope_unit) & !is.infinite(log_slope_unit) & 
         !is.na(mean_intervention) & !is.infinite(mean_intervention))

# Create plot of intervention rates by log slope unit
log_scale_plot <- combined_log_scale %>%
  ggplot(aes(x = log_slope_unit, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Log Slope Unit",
       x = "Log Slope Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme_minimal() +
  theme(legend.position = "bottom")

# Display log scale plot
log_scale_plot

# Display log scale results
hansen_log_scale
tmf_log_scale

# Calculate mean and sd by log scale units for Hansen data (last_year_diff)
hansen_log_scale_diff <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= hansen_intervention_diff_range$diff_min & 
         last_year_diff <= hansen_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (last_year_diff)
tmf_log_scale_diff <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= tmf_intervention_diff_range$diff_min & 
         last_year_diff <= tmf_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start, na.rm = TRUE),
    sd_intervention = sd(intervention_start, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting (last_year_diff)
combined_log_scale_diff <- bind_rows(hansen_log_scale_diff, tmf_log_scale_diff)

# Create plot of intervention rates by log last year diff unit
log_scale_diff_plot <- combined_log_scale_diff %>%
  ggplot(aes(x = log_diff_unit, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Log Last Year Diff Unit",
       x = "Log Last Year Diff Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
    scale_color_manual(values = c(palette$red, palette$blue))+
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

combined_figure <- ggarrange(
    log_scale_plot, log_scale_diff_plot, percentile_plot, last_year_diff_percentile_plot,
    ncol = 2, nrow = 2,
    common.legend = TRUE,
    legend = "bottom",
    labels=NULL#"AUTO"
)

# Display the combined figure
combined_figure

# Save the combined figure
ggsave("figs/enrollment_analyses_combined.png", combined_figure, 
       width = 12, height = 8, dpi = 300)
ggsave("figs_pdf/enrollment_analyses_combined.pdf", combined_figure, 
       width = 12, height = 8)


ART_eligible_full <- ART_eligible_full %>%
    mutate(intervention = replace_na(intervention, 0),
           intervention_start_5 = case_when(
               intervention == 0 ~ 0,
               year < intervention_year - 2000 - 5 ~ 0,
               year == intervention_year - 2000 - 5 ~ 1,
               TRUE ~ NA_real_
           ))

ART_eligible_TMF_full <- ART_eligible_TMF_full %>%
    mutate(intervention = replace_na(intervention, 0),
           intervention_start_5 = case_when(
               intervention == 0 ~ 0,
               year < intervention_year - 5 ~ 0,
               year == intervention_year - 5 ~ 1,
               TRUE ~ NA_real_
           ))

# Calculate mean and sd by percentile of slope for Hansen data
hansen_percentiles <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of slope for TMF data
tmf_percentiles <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(slope_percentile = ntile(slope, 10)) %>%
  group_by(slope_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_percentiles <- bind_rows(hansen_percentiles, tmf_percentiles)

# Create plot of intervention rates by slope percentile
percentile_plot <- combined_percentiles %>%
  ggplot(aes(x = slope_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Slope Percentile",
       x = "Slope Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
percentile_plot

# Display results
hansen_percentiles
tmf_percentiles


# Calculate mean and sd by percentile of last_year_diff for Hansen data
hansen_last_year_diff_percentiles <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by percentile of last_year_diff for TMF data
tmf_last_year_diff_percentiles <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  mutate(last_year_diff_percentile = ntile(last_year_diff, 10)) %>%
  group_by(last_year_diff_percentile) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine data for plotting
combined_last_year_diff_percentiles <- bind_rows(hansen_last_year_diff_percentiles, tmf_last_year_diff_percentiles)

# Create plot of intervention rates by last_year_diff percentile
last_year_diff_percentile_plot <- combined_last_year_diff_percentiles %>%
  ggplot(aes(x = last_year_diff_percentile, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Last Year Diff Percentile",
       x = "Last Year Diff Percentile",
       y = "Mean Intervention Rate",
       color = "Data Source") +
  scale_x_continuous(breaks = 1:10) +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
last_year_diff_percentile_plot

# Display results
hansen_last_year_diff_percentiles
tmf_last_year_diff_percentiles


# Get the range of slopes and last_year_diff for intervention==1 observations
hansen_intervention_slope_range <- ART_eligible_full %>% 
  filter(intervention_start_5 == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

tmf_intervention_slope_range <- ART_eligible_TMF_full %>% 
  filter(intervention_start_5 == 1) %>% 
  summarise(
    slope_min = min(slope, na.rm = TRUE),
    slope_max = max(slope, na.rm = TRUE)
  )

hansen_intervention_diff_range <- ART_eligible_full %>% 
  filter(intervention_start_5 == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

tmf_intervention_diff_range <- ART_eligible_TMF_full %>% 
  filter(intervention_start_5 == 1) %>% 
  summarise(
    diff_min = min(last_year_diff, na.rm = TRUE),
    diff_max = max(last_year_diff, na.rm = TRUE)
  )

# Calculate mean and sd by log scale units for Hansen data (adjusting for negative numbers)
hansen_log_scale <- ART_eligible_full %>% 
#   filter(intervention ==
  filter(slope >= hansen_intervention_slope_range$slope_min & 
         slope <= hansen_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (adjusting for negative numbers)
tmf_log_scale <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(slope >= tmf_intervention_slope_range$slope_min & 
         slope <= tmf_intervention_slope_range$slope_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_slope = ifelse(slope < 0, -log(abs(slope)), log(slope)),
    log_slope_unit = floor(log_slope / 2) * 2
  ) %>%
  filter(!is.na(log_slope) & !is.infinite(log_slope)) %>%
  group_by(log_slope_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting
combined_log_scale <- bind_rows(hansen_log_scale, tmf_log_scale) %>%
  filter(!is.na(log_slope_unit) & !is.infinite(log_slope_unit))

# Create plot of intervention rates by log slope unit
log_scale_plot <- combined_log_scale %>%
  ggplot(aes(x = log_slope_unit, y = mean_intervention, color = data_source)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Log Slope Unit",
       x = "Log Slope Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
    scale_color_manual(values = c(palette$red, palette$blue))+
  theme_minimal() +
  theme(legend.position = "bottom")

# Display log scale plot
log_scale_plot

# Display log scale results
hansen_log_scale
tmf_log_scale

# Calculate mean and sd by log scale units for Hansen data (last_year_diff)
hansen_log_scale_diff <- ART_eligible_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= hansen_intervention_diff_range$diff_min & 
         last_year_diff <= hansen_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "Hansen")

# Calculate mean and sd by log scale units for TMF data (last_year_diff)
tmf_log_scale_diff <- ART_eligible_TMF_full %>% 
#   filter(intervention == 0 | intervention_start == 1) %>%
  filter(last_year_diff >= tmf_intervention_diff_range$diff_min & 
         last_year_diff <= tmf_intervention_diff_range$diff_max) %>%
  mutate(
    # Handle negative numbers: log(-x) = -log(abs(x))
    log_last_year_diff = ifelse(last_year_diff < 0, -log(abs(last_year_diff)), log(last_year_diff)),
    log_diff_unit = floor(log_last_year_diff / 2) * 2
  ) %>%
  group_by(log_diff_unit) %>%
  summarise(
    mean_intervention = mean(intervention_start_5, na.rm = TRUE),
    sd_intervention = sd(intervention_start_5, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(data_source = "TMF")

# Combine log scale data for plotting (last_year_diff)
combined_log_scale_diff <- bind_rows(hansen_log_scale_diff, tmf_log_scale_diff)

# Create plot of intervention rates by log last year diff unit
log_scale_diff_plot <- combined_log_scale_diff %>%
  ggplot(aes(x = log_diff_unit, y = mean_intervention, color = data_source)) +
  geom_line(size = 1, position = position_dodge(width = 0.1)) +
  geom_point(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = mean_intervention - sd_intervention/sqrt(n), 
                    ymax = mean_intervention + sd_intervention/sqrt(n)), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Intervention Rate by Log Last Year Diff Unit",
       x = "Log Last Year Diff Unit",
       y = "Mean Intervention Rate",
       color = "Data Source") +
    scale_color_manual(values = c(palette$red, palette$blue))+
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

combined_figure <- ggarrange(
    log_scale_plot, log_scale_diff_plot, percentile_plot, last_year_diff_percentile_plot,
    ncol = 2, nrow = 2,
    common.legend = TRUE,
    legend = "bottom",
    labels=NULL#"AUTO"
)

# Display the combined figure
combined_figure
ggsave("figs/enrollment_analyses_combined_tminus5.png", combined_figure, 
       width = 12, height = 8, dpi = 300)
ggsave("figs_pdf/enrollment_analyses_combined_tminus5.pdf", combined_figure, 
       width = 12, height = 8)





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
                       , caption = " \\label{tab:art-diff} Does slope predict ART TREES participation?"
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
                       caption = " \\label{tab:art-diff-pctile} Does slope percentile predict ART TREES participation?"
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
                       , caption = " \\label{tab:art-timing} Does within jurisdiction slope predict ART TREES enrollment timing?"
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
                       output="latex",
                       title = 'Does slope predict Verra JNR participation?',
                       fmt = 7,
                       # vcov = ~cluster_variable,
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       coef_map = coef_names,
                       gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
                       , add_rows = rows
                       # , notes = ""
                       , caption = " \\label{tab:verra-diff}"
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

