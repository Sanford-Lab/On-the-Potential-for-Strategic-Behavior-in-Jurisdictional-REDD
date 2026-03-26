library(tidyverse)
library(ggplot2)



palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "grey" = "grey60",
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

scHansen_ovr_results <- readRDS(paste0(results_dir, "/scHansen_ovr_results_new.rds"))%>%
    mutate(data = "Hansen")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CUMULATIVE EFFECTS

# TOTAL EFFECT
ovr_results_cumulative <- scHansen_ovr_results %>%
    filter(year == 2023
           , Outcome == "cum_loss"
           , ifund == "ART TREES"
    )%>%
    mutate(coef = Pct.Chng,
           Z = abs(qnorm(Perm.pVal/2)),
           se = abs(coef/Z),
           ci.upper = coef + 1.645*se,
           ci.lower = coef - 1.645*se
    )%>%
    mutate(sig = case_when(
        Perm.pVal <= .1 & Perm.pVal > .05 ~ "*", 
        Perm.pVal <= .05 & Perm.pVal > .01 ~ "**", 
        Perm.pVal <= .01 ~ "***", 
        TRUE ~ ""
    ))


# TOTAL EFFECT
ovr_results_antic <- scHansen_ovr_results %>%
    filter(Rel_year == -1
           , Outcome == "cum_loss"
           , ifund == "ART TREES"
    )%>%
    mutate(coef = Pct.Chng,
           Z = abs(qnorm(Perm.pVal/2)),
           se = abs(coef/Z),
           ci.upper = coef + 1.645*se,
           ci.lower = coef - 1.645*se
    )%>%
    mutate(sig = case_when(
        Perm.pVal <= .1 & Perm.pVal > .05 ~ "*", 
        Perm.pVal <= .05 & Perm.pVal > .01 ~ "**", 
        Perm.pVal <= .01 ~ "***", 
        TRUE ~ ""
    ))

ggplot(ovr_results_antic, aes(x=reorder(iname, coef), y=coef)) + 
    geom_bar(stat="identity", position="dodge", fill = palette$red) + 
    geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), position=position_dodge(width=.9), width=.25) + 
    geom_text(aes(y=min(ci.lower) - 0.05, label=sig), position=position_dodge(width=.9)) +
    labs(y="ATT estimate", x = "") +
    theme_classic()+
    coord_flip() +
    ggtitle("Anticipation effects of jurisdictional REDD+")
ggsave(filename = paste0(fig_dir, "/scHansen_coefficients_anticipation.png")
       , width = 5, height = 5)
ggsave(filename = paste0(fig_dir_pdf, "/scHansen_coefficients_anticipation.pdf")
       , width = 5, height = 5)

# Cumulative results

scTMF_ovr_results <- readRDS(paste0(results_dir, "/scTMF_ovr_results.rds"))%>%
    mutate(data = "TMF")

scHansen_ovr_results_TMFdonors <- readRDS(paste0(results_dir, "/scHansen_ovr_results_TMFdonors.rds"))%>%
    mutate(data = "Hansen, TMF donors")

scHansen_ovr_results_3yrs <- readRDS(paste0(results_dir, "/scHansen_ovr_results_3yrs.rds"))%>%
    mutate(data = "Hansen, 3 year antic.")

# Anticipation results

antic_results <- rbind(scTMF_ovr_results, scHansen_ovr_results_TMFdonors, scHansen_ovr_results_3yrs) %>%
    filter(Rel_year == -1, 
           Outcome %in% c("cum_loss", "cum_defordeg")
           )

antic_results_cumulative <- antic_results %>%
    mutate(coef = Pct.Chng,
           Z = abs(qnorm(Perm.pVal/2)),
           se = abs(coef/Z),
           ci.upper = coef + 1.645*se,
           ci.lower = coef - 1.645*se
    )%>%
    mutate(sig = case_when(
        Perm.pVal <= .1 & Perm.pVal > .05 ~ "*", 
        Perm.pVal <= .05 & Perm.pVal > .01 ~ "**", 
        Perm.pVal <= .01 ~ "***", 
        TRUE ~ ""
    ))%>%
    rbind(ovr_results_antic) %>%
    ungroup

antic_results_cumulative$iname<- factor(antic_results_cumulative$iname, levels=rev(sort(unique(antic_results_cumulative$iname))))

ggplot(antic_results_cumulative, aes(x=iname, y=coef, fill = data)) + 
    geom_bar(stat="identity", position="dodge") + 
    geom_vline(xintercept = seq(from = 0.5, to = length(antic_results_cumulative$iname), by = 1), color=palette$dark, linewidth=.25, alpha=.2, linetype = "dashed") +# set vertical lines between x groups
    geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), position=position_dodge(width=.9), linewidth = 0.25, width=.35, color = palette$grey) + 
    #geom_text(aes(y=min(ci.lower), label=sig), position=position_dodge(width=.9), color = palette$grey) +
    scale_fill_manual(values = c(palette$red, palette$dark_blue, palette$purple, palette$blue))+
    scale_color_manual(values = c("grey70", palette$green), guide = "none")+
    labs(y="ATT estimate (% change cumulative deforestation)", x = "") +
    theme_classic()+
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank()) + # remove grid lines
    coord_flip() +
    ggtitle("Anticipation effects of ART TREES")
ggsave(filename = paste0(fig_dir, "/sc_coefficients_anticipation.png")
       , width = 7, height = 10)
ggsave(filename = paste0(fig_dir_pdf, "/sc_coefficients_anticipation.pdf")
       , width = 7, height = 10)

