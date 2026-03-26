library(ggplot2); library(dplyr)

plot_jurisdiction <- function(data = dat_long, codes = NA, year_implement = 2010, slopes = TRUE, slope_from_regs = TRUE) {
    year_implement <- year_implement - 2000
    yeardat <- data %>% filter(ID %in% codes & year == year_implement)
    
    if(!'baseline' %in% names(data)) {
        stop("data does not have a column 'baseline'")
    }
    
    if(nrow(yeardat) == 0) {
        stop("there is no data for these jursidictions in this year. Please check the 'codes' and 'year_implement'")
    }
    
    yeardat_agg <- yeardat %>%
        summarise(
            est_effect = sum(est_effect, na.rm = TRUE),
            loss = sum(loss, na.rm = TRUE),
            baseline = sum(baseline, na.rm = TRUE),
            endline = sum(endline, na.rm = TRUE),
            level = unique(level),
            ADM0_NAME = unique(ADM0_NAME),
            ADM1_NAME = if_else(level != "ADM0", paste(unique(ADM1_NAME), collapse = ', '), NA_character_),
            ADM2_NAME = if_else(level == "ADM2", paste(unique(ADM2_NAME), collapse = ', '), NA_character_)
        )
    
    # If endline is NA, calculate the average loss for up to 4 years after year_implement
    
    effect_in_km2 <- round(yeardat_agg$est_effect/1e6,digits = 2)
    
    data_agg <- data %>% 
        filter(ID %in% codes) %>%
        group_by(year) %>%
        summarise(
            loss = sum(loss, na.rm = TRUE),
            est_effect = sum(est_effect, na.rm = TRUE)
        )
    
    if(is.na(yeardat_agg$endline)) {
        yeardat_agg$endline <- data_agg %>% 
            filter(year > year_implement, year <= year_implement + 4) %>%
            summarise(avg_loss = mean(loss, na.rm = TRUE)) %>%
            pull(avg_loss)
    }

    dat_window <- data_agg %>%
        mutate(period = 
                   case_when(`year` < (`year_implement`-5) | `year` >= (`year_implement` + 5) ~ "outside",
                             `year` < `year_implement` & year >= (`year_implement` - 5) ~ "baseline",
                             `year` >= `year_implement` & year < (`year_implement` + 5) ~ "performance",
                             TRUE ~ "post"))
    
    gg <- dat_window %>% ggplot(aes(x=year, y=loss, group=period, color = period)) + 
        geom_point() +
        geom_vline(aes(xintercept = year_implement - 0.5), linetype = "dashed", color = "grey")  # adding the vertical line
    
    
    if(slopes) {
        gg <- gg + stat_smooth(data = dat_window %>% filter(period != "outside"), method = "lm", se = FALSE, linetype = "dashed")
    }
    if(slope_from_regs) {gg <- gg + geom_abline(data = yeardat, 
                           aes(slope = slope, intercept = intercept), linetype = "dotted")
    }
    gg <- gg + 
        labs(title = paste0(
            "Country: ", yeardat_agg$ADM0_NAME,
            if_else(!is.na(yeardat_agg$ADM1_NAME), paste("\nAdm 1: ", yeardat_agg$ADM1_NAME), ""),
            if_else(!is.na(yeardat_agg$ADM2_NAME), paste("\nAdm 2: ", yeardat_agg$ADM2_NAME), ""),
            "\n", (year_implement + 2000), 
            "\n", paste0(effect_in_km2, "sq km per year decrease in deforestation"))) +
        theme_bw()
    
    if(!is.na(yeardat_agg$baseline)) {
        gg <- gg + 
            geom_segment(aes(x=year_implement-5,xend=year_implement-1,y=yeardat_agg$baseline, yend = yeardat_agg$baseline), color = "#F8766D")
    }
    
    if(!is.na(yeardat_agg$endline) && yeardat_agg$endline != 0) {
        gg <- gg +
            geom_segment(aes(x=year_implement,xend=year_implement+4,y=yeardat_agg$endline, yend = yeardat_agg$endline), color = "#00BCF4")
    }
    
    plot(gg)
}

