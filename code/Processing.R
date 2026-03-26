library(tidyverse)
library(zoo)
library(tidyquant)
library(beepr)

forest_2000 <- read_csv("data/raw/forest_2000_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>%
    rename(treecover_2000 = "sum")

loss_01 <- read_csv("data/raw/loss_2001_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_01 = "sum")

loss_02 <- read_csv("data/raw/loss_2002_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_02 = "sum")

loss_03 <- read_csv("data/raw/loss_2003_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_03 = "sum")

loss_04 <- read_csv("data/raw/loss_2004_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_04 = "sum")

loss_05 <- read_csv("data/raw/loss_2005_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_05 = "sum")

loss_06 <- read_csv("data/raw/loss_2006_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_06 = "sum")

loss_07 <- read_csv("data/raw/loss_2007_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_07 = "sum")

loss_08 <- read_csv("data/raw/loss_2008_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_08 = "sum")

loss_09 <- read_csv("data/raw/loss_2009_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_09 = "sum")

loss_10 <- read_csv("data/raw/loss_2010_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_10 = "sum")

loss_11 <- read_csv("data/raw/loss_2011_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_11 = "sum")

loss_12 <- read_csv("data/raw/loss_2012_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_12 = "sum")

loss_13 <- read_csv("data/raw/loss_2013_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_13 = "sum")

loss_14 <- read_csv("data/raw/loss_2014_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_14 = "sum")

loss_15 <- read_csv("data/raw/loss_2015_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_15 = "sum")

loss_16 <- read_csv("data/raw/loss_2016_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_16 = "sum")

loss_17 <- read_csv("data/raw/loss_2017_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_17 = "sum")

loss_18 <- read_csv("data/raw/loss_2018_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_18 = "sum")

loss_19 <- read_csv("data/raw/loss_2019_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_19 = "sum")

loss_20 <- read_csv("data/raw/loss_2020_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_20 = "sum")

loss_21 <- read_csv("data/raw/loss_2021_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_21 = "sum")

loss_22 <- read_csv("data/raw/loss_2022_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_22 = "sum")

loss_23 <- read_csv("data/raw/loss_2023_globe.csv") %>% 
    select(-c(".geo", "system:index")) %>% 
    rename(loss_23 = "sum")

dat <- forest_2000 %>% 
    left_join(loss_01) %>% mutate(pct_forest = treecover_2000/area,
                                                     pct_loss_01 = loss_01/treecover_2000)


dat %>% ggplot(aes(x=pct_forest)) + geom_histogram()
dat %>% ggplot(aes(x=pct_loss_01)) + geom_histogram()

dat <- forest_2000 %>% 
    left_join(loss_01) %>% 
    left_join(loss_02) %>% 
    left_join(loss_03) %>%     
    left_join(loss_04) %>% 
    left_join(loss_05) %>% 
    left_join(loss_06) %>% 
    left_join(loss_07) %>% 
    left_join(loss_08) %>% 
    left_join(loss_09) %>% 
    left_join(loss_10) %>% 
    left_join(loss_11) %>% 
    left_join(loss_12) %>% 
    left_join(loss_13) %>% 
    left_join(loss_14) %>% 
    left_join(loss_15) %>% 
    left_join(loss_16) %>% 
    left_join(loss_17) %>% 
    left_join(loss_18) %>% 
    left_join(loss_19) %>% 
    left_join(loss_20) %>% 
    left_join(loss_21) %>%
    left_join(loss_22) %>%
    left_join(loss_23)

dat_long <- dat %>% pivot_longer(cols = loss_01:loss_23,
                            names_to = "year",
                            names_prefix = "loss_",
                            values_to = "loss") %>%
    group_by(ADM0_NAME, ADM0_CODE,ADM1_NAME, ADM1_CODE,ADM2_NAME,ADM2_CODE) %>%
    filter(ADM2_CODE != 23036,
           ADM2_CODE != 15426,
           ADM2_CODE != 22602,
           ADM2_CODE != 22917,
           ADM2_CODE != 48472) %>%
    mutate(year = as.numeric(year),
           date = ymd(paste0(year, "/01/01")),
           ID = ADM2_CODE,
           level = "ADM2")  %>%
    ungroup()

dat_long <- dat_long %>% 
    group_by(ADM0_NAME, ADM0_CODE,ADM1_NAME, ADM1_CODE,year, date) %>% 
    summarise(area = sum(area),
              treecover_2000 = sum(treecover_2000),
              loss = sum(loss),
              ADM2_NAME = NA,
              ADM2_CODE = NA,
              ID = ADM1_CODE,
              level = "ADM1"
              ) %>% unique() %>%
    ungroup() %>%
    bind_rows(dat_long)

dat_long <- dat_long %>% 
    group_by(ADM0_NAME, ADM0_CODE,year, date) %>% 
    summarise(area = sum(area),
              treecover_2000 = sum(treecover_2000),
              loss = sum(loss),
              ADM2_NAME = NA,
              ADM2_CODE = NA,
              ADM1_NAME = NA,
              ADM1_CODE = NA,
              ID = ADM0_CODE,
              level = "ADM0") %>% unique() %>%
    ungroup()  %>%
    bind_rows(dat_long)

dat_long <- dat_long %>%
    mutate( baseline = lag(rollmean(loss, k=5, fill=NA, align='right'),1,default = NA),
            endline = lead(baseline, 6, default = NA),
            est_effect = baseline-endline)


dat_long <- dat_long %>%
    group_by(ID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cum_loss = cumsum(loss),
           treecover_remaining = treecover_2000 - cum_loss)


# take dat_long, group by ID, for each year calculate whether the loss is greater than the baseline and by 
# what percent. Then count the number of years that the loss exceeds the baseline by 0-15%, 15-35%, 35-55%,
# 55-75%, and 75-100%, applying deductions of 0%, 15%, 25%, 35%, and 100% respectively.
dat_long <- dat_long %>% 
    group_by(ID) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(pct_forest_cover = treecover_remaining/area,
           loss_rate = loss/treecover_remaining,
           score = (100*pct_forest_cover-50)/100 + 0.5-loss_rate*100,
           HFLD_baseline = baseline + treecover_remaining*0.0005*score,
           loss_exceeds_baseline_pct = pmax((loss/baseline)*100 - 100,0),
           deduction = case_when(loss_exceeds_baseline_pct >= 0 & loss_exceeds_baseline_pct <= 15 ~ 0,
                                 loss_exceeds_baseline_pct > 15 & loss_exceeds_baseline_pct <= 35 ~ .15,
                                 loss_exceeds_baseline_pct > 35 & loss_exceeds_baseline_pct <= 55 ~ .25,
                                 loss_exceeds_baseline_pct > 55 & loss_exceeds_baseline_pct <= 75 ~ .35,
                                 loss_exceeds_baseline_pct > 75 & loss_exceeds_baseline_pct <= 100 ~ 1,
                                 TRUE ~ 0),
            penalty = pmax((HFLD_baseline-loss)*deduction,0),
            HFLD_est_effect = HFLD_baseline-loss-penalty)


beep(2)

save(dat_long, file = "data/processed/forest_loss.Rdata")


### Rolling Regressions ----
## This section calculates the slope for each 5-year baseline time period to be used for cheating and to see how much better we'd do 
## if we use slope and mean
## Rolling regression code borrowed from https://cran.r-project.org/web/packages/tidyfit/vignettes/Rolling_Window_Time_Series_Regression.html 

library(tidyfit)
library(estimatr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### ART eligible rolling regressions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ARTintervention_names <- readRDS("data/processed/ARTintervention_names.rds")

ART_eligible_ADM1 <- dat_long %>% 
    filter(treecover_2000 >= 2.5e6*10000) %>% 
    filter(level == "ADM1")

ART_eligible_ADM2_ID <- dat_long %>% filter(level == "ADM2") %>%
    filter(ADM1_CODE %in% ART_eligible_ADM1$ADM1_CODE) %>% select(ID) %>% unique() %>% pull()

ART_intervention <- dat_long %>% filter(ID %in% 
                                            (ARTintervention_names %>% filter(level == intervention_level)
                                                 )$ID
                                        )


ART_eligible <- dat_long %>%
    filter(level == "ADM2",
           ID %in% ART_eligible_ADM2_ID
    )%>%
    rbind(dat_long %>% filter(level == "ADM0")) %>%
    rbind(ART_eligible_ADM1) %>%
    filter(!(ID %in% ART_intervention$ID))%>%
    rbind(ART_intervention)%>%
    mutate(ART_eligible = 1) %>% 
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, ART_eligible)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        ART_eligible ) %>%
    mutate(ART_eligible = replace_na(ART_eligible, 0))

dat_long_ART <- dat_long %>%
    filter(ART_eligible == 1)

saveRDS(dat_long_ART , here::here("data", "processed", "dat_long_ART.rds"))

system.time(out <- dat_long_ART %>%
                group_by(ID) %>%
                regress(loss ~ year, m("lm"),
                        m("lm"),
                        .cv = "sliding_index", .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
                        .force_cv = TRUE, .return_slices = TRUE))


### Unnest stuff
dat_long_ART_beta <- coef(out) %>% 
    unnest(model_info) %>% 
    unique() %>%
    # filter(term == "year") %>%
    select(-c("std.error","statistic","p.value")) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(
        slope = year,#lead(year, 1),
        intercept = `(Intercept)`,#lead(`(Intercept)`,1),
        year = year(slice_id)-2000) %>% 
    select(-`(Intercept)`) %>%
    filter(year >= 5) %>% 
    left_join(dat_long_ART) %>% 
    mutate(last_year_diff = baseline-loss)

saveRDS(dat_long_ART_beta , here::here("data", "processed", "dat_long_ART_beta.rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Rolling regressions for only Verra JNR eligible 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Verraintervention_names <- readRDS("data/processed/Verraintervention_names.rds")

### over 5 years

Verra_national <- dat_long %>% filter(level == "ADM0")
Verra_sub1 <- dat_long %>% filter(ADM0_CODE %in% (Verra_national %>% filter(area >= 2.5e6*10000) %>% pull(ADM0_CODE))) %>%
    filter(level == "ADM1")
Verra_sub2 <- dat_long %>% filter(ADM1_CODE %in% (Verra_sub1 %>% filter(area >= 5e6*10000) %>% pull(ADM1_CODE))) %>%
    filter(level == "ADM2")
Verra_intervention <- dat_long %>% filter(ID %in% 
                                              (Verraintervention_names %>% filter(level == intervention_level)
                                          )$ID)

Verra_eligible <- bind_rows(Verra_national,Verra_sub1,Verra_sub2)%>%
    filter(!(ID %in% Verra_intervention$ID))%>%
    bind_rows(Verra_intervention)%>%
    mutate(Verra_eligible = 1)%>%
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, Verra_eligible)

rm(Verra_national,Verra_sub1,Verra_sub2, Verra_intervention)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        Verra_eligible ) %>%
    mutate(Verra_eligible = replace_na(Verra_eligible, 0))

dat_long_Verra <- dat_long %>%
    filter(Verra_eligible == 1)

saveRDS(dat_long_Verra , here::here("data", "processed", "dat_long_Verra.rds"))

system.time(out <- dat_long_Verra %>%
                group_by(ID) %>%
                regress(loss ~ year, m("lm"),
                        m("lm"),
                        .cv = "sliding_index", .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
                        .force_cv = TRUE, .return_slices = TRUE))


### Unnest stuff
dat_long_Verra_beta <- coef(out) %>% 
    unnest(model_info) %>% 
    unique() %>%
    # filter(term == "year") %>%
    select(-c("std.error","statistic","p.value")) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(
        slope = year,#lead(year, 1),
        intercept = `(Intercept)`,#lead(`(Intercept)`,1),
        year = year(slice_id)-2000) %>% 
    select(-`(Intercept)`) %>%
    filter(year >= 5) %>% 
    left_join(dat_long_Verra) %>% 
    mutate(last_year_diff = baseline-loss)

saveRDS(dat_long_Verra_beta , here::here("data", "processed", "dat_long_Verra_beta.rds"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Rolling regressions for All Jurisdictions 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Non-parallelized version, works but is very slow for all Js
# system.time(out <- dat_long %>%
#     group_by(ID) %>%
#     regress(loss ~ year, m("lm"),
#             m("lm"),
#             .cv = "sliding_index", .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
#             .force_cv = TRUE, .return_slices = TRUE))

# library(furrr)
# 
# 
# # Set up a parallel backend
# plan(multisession, workers = availableCores()-1)
# 
# # Split the data into a list of data frames, one for each group
# data_list <- dat_long %>% split(.$ID)
# 
# # Use future_map_dfr to apply the function to each data frame in the list
# options(future.rng.onMisuse="ignore")
# system.time(out <- future_map_dfr(data_list, function(df) {
#     df %>%
#         regress(loss ~ year, m("lm"), 
#                 m("lm"),
#                 .cv = "sliding_index", 
#                 .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
#                 .force_cv = TRUE, .return_slices = TRUE)
# }, .progress = TRUE))

## Testing Speed of parallelization
# df <- dat_long %>% filter(ID <=2000)
# 
# 
# library(furrr)
# 
# 
# # Set up a parallel backend
# plan(multisession)
# 
# # Split the data into a list of data frames, one for each group
# data_list <- df %>% split(.$ID)
# 
# # Use future_map_dfr to apply the function to each data frame in the list
# options(future.rng.onMisuse="ignore")
# system.time(out <- future_map_dfr(data_list, function(df) {
#     df %>%
#         regress(loss ~ year, m("lm"), 
#                 m("lm"),
#                 .cv = "sliding_index", 
#                 .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
#                 .force_cv = TRUE, .return_slices = TRUE)
# }))



### Unnest stuff
# dat_long_beta <- coef(out) %>% 
#     unnest(model_info) %>% 
#     unique() %>%
#     # filter(term == "year") %>%
#     select(-c("std.error","statistic","p.value")) %>% 
#     pivot_wider(names_from = term, values_from = estimate) %>%
#     mutate(
#            slope = lead(year, 1),
#            intercept = lead(`(Intercept)`,1),
#            year = year(slice_id)-2000) %>% 
#     select(-`(Intercept)`) %>%
#     filter(year >= 5) %>% 
#     left_join(dat_long) %>% 
#     mutate(last_year_diff = baseline-loss)
# 
# 
# library(fixest)
# feols(est_effect ~ last_year_diff + slope + intercept,dat = dat_long_beta)
# 
# dat_long_beta %>% filter(ADM0_NAME=="Indonesia") %>% feols(est_effect ~ slope + intercept,dat = .)
# 
# dat_long_beta %>% filter(ADM0_NAME=="Brazil") %>% feols(est_effect ~ slope + intercept,dat = .)
# 
# dat_long_beta %>% filter(ADM0_NAME=="Democratic Republic of the Congo") %>% feols(est_effect ~ slope + intercept,dat = .)
# 
# dat_long_beta %>% filter(ADM0_NAME == "Indonesia") %>% arrange(slope) %>% print(n=50)
# 
# 
# save(dat, dat_long, dat_long_beta, file = "data/processed/forest_loss.Rdata")




## Below this is scratch ----
# load("data/processed/forest_loss.Rdata")

# dat_long %>% ggplot(aes(x=year, y=loss)) + geom_smooth()
# dat_long %>% ggplot(aes(x=year, y=est_effect)) + geom_smooth()

# dat_long %>% filter(ADM0_NAME == "Indonesia") %>% arrange(desc(est_effect))

# plot_jurisdiction <- function(code = NA, year_implement = 2010) {
#     yeardat <- dat_long %>% filter(ADM2_CODE==code & year == year_implement)
#     effect_in_km2 <- round(yeardat$est_effect/1e6,digits = 2)
#     gg <- dat_long %>% 
#         filter(ADM2_CODE==code) %>%
#         mutate(period = 
#                    case_when(`year` <= (`year_implement`-5) | `year` > (`year_implement` + 5) ~ "outside",
#                              `year` <= `year_implement` & year > (`year_implement` - 5) ~ "baseline",
#                              `year` > `year_implement` & year <= (`year_implement` + 5) ~ "performance",
#                              TRUE ~ "post")) %>%
#         ggplot(aes(x=year, y=loss, group=period, color = period)) + 
#         geom_point() + 
#         labs(title = paste0(
#             paste(yeardat$ADM2_NAME, 
#                   yeardat$ADM1_NAME, 
#                   yeardat$ADM0_NAME, 
#                   paste0("20", yeardat$year), 
#                   sep = ", "), 
#             "\n",
#             paste0(effect_in_km2, "sq km per year decrease in deforestation")))
            
    
#     gg <- gg + 
#         geom_segment(aes(x=year_implement-4,xend=year_implement,y=yeardat$baseline, yend = yeardat$baseline), color = "#F8766D") + 
#         geom_segment(aes(x=year_implement+1,xend=year_implement+5,y=yeardat$endline, yend = yeardat$endline), color = "#00BCF4")
        
    
#         plot(gg)
# }

# plot_jurisdiction(code = 18102, year_implement = 17)
# plot_jurisdiction(code = 73825, year_implement = 14)
# plot_jurisdiction(code = 73651, year_implement = 14)


# out <- dat_long %>% filter(year == 5 | year == 10) %>% 
#     pivot_wider(id_cols = ADM2_CODE:treecover_2000,names_from = year, names_prefix = "yr_", values_from = est_effect) %>%
#     mutate(change = yr_5-yr_10, 
#            pctchange = (yr_5 - yr_10)/yr_5)

# out %>% ggplot(aes(x=pctchange)) + geom_histogram() + xlim(c(-5,1))

# out %>% arrange(desc(change))

# plot_jurisdiction <- function(code = NA) {
#     dat_long %>% 
#         filter(ADM2_CODE==code) %>%
#         mutate(period = year<=5) %>%
#         ggplot(aes(x=year, y=loss, group=period, color = period)) + geom_point()
# }


# ### With SD ----
# # Custom function to return mean, sd, 95% conf interval
# custom_stat_fun_2 <- function(x, na.rm = TRUE) {
#     # x     = numeric vector
#     # na.rm = boolean, whether or not to remove NA's
    
#     m  <- mean(x, na.rm = na.rm)
#     s  <- sd(x, na.rm = na.rm)
#     hi <- m + 2*s
#     lo <- m - 2*s
    
#     ret <- c(mean = m, stdev = s, hi.95 = hi, lo.95 = lo) 
#     return(ret)
# }

# dat_long <- dat_long %>%
#     group_by(ADM2_CODE) %>%
#     tq_mutate(
#         select     = loss,
#         mutate_fun = rollapply, 
#         # rollapply args
#         width      = 5,
#         align      = "right",
#         by.column  = FALSE,
#         FUN        = custom_stat_fun_2,
#         # FUN args
#         na.rm      = TRUE
#     )

# out <- dat_long %>% filter(year == 5 | year == 10) %>% 
#     pivot_wider(id_cols = ADM2_CODE:treecover_2000,names_from = year, names_prefix = "yr_", values_from = c(mean,hi.95,lo.95)) %>%
#     mutate(change = mean_yr_5-mean_yr_10, 
#            pctchange = (mean_yr_5 - mean_yr_10)/mean_yr_5)

# out %>% ggplot(aes(x=pctchange)) + geom_histogram() + xlim(c(-5,1))

# out %>% arrange(desc(change))

# plot_jurisdiction_conf <- function(code = NA, conf = T) {
#     start_mean <- out %>% filter(ADM2_CODE == code) %>% pull(mean_yr_5)
#     start_max <- out %>% filter(ADM2_CODE == code) %>% pull(hi.95_yr_5)
#     start_min <- out %>% filter(ADM2_CODE == code) %>% pull(lo.95_yr_5)
#     end_mean <- out %>% filter(ADM2_CODE == code) %>% pull(mean_yr_10)
#     end_max <- out %>% filter(ADM2_CODE == code) %>% pull(hi.95_yr_10)
#     end_min <- out %>% filter(ADM2_CODE == code) %>% pull(lo.95_yr_10)
#     dat_long %>% 
#         filter(ADM2_CODE==code) %>%
#         mutate(period = year<=5) %>%
#         ggplot(aes(x=year, y=loss, group=period, color = period)) + geom_point() +
#         geom_segment(aes(x=0,xend=5,y=start_mean, yend = start_mean), color = "#00BCF4") +
#         geom_segment(aes(x=0,xend=5,y=start_max, yend = start_max), color = "#00BCF4", linetype="dotted") +
#         geom_segment(aes(x=0,xend=5,y=start_min, yend = start_min), color = "#00BCF4", linetype="dotted") +
#         geom_segment(aes(x=6,xend=10,y=end_mean, yend = end_mean), color = "#F8766D") +
#         geom_segment(aes(x=6,xend=10,y=end_max, yend = end_max), color = "#F8766D", linetype="dotted") +
#         geom_segment(aes(x=6,xend=10,y=end_min, yend = end_min), color = "#F8766D", linetype="dotted")
        
# }

# plot_jurisdiction <- function(code = NA, conf = T) {
#     start_mean <- out %>% filter(ADM2_CODE == code) %>% pull(mean_yr_5)
#     start_max <- out %>% filter(ADM2_CODE == code) %>% pull(hi.95_yr_5)
#     start_min <- out %>% filter(ADM2_CODE == code) %>% pull(lo.95_yr_5)
#     end_mean <- out %>% filter(ADM2_CODE == code) %>% pull(mean_yr_10)
#     end_max <- out %>% filter(ADM2_CODE == code) %>% pull(hi.95_yr_10)
#     end_min <- out %>% filter(ADM2_CODE == code) %>% pull(lo.95_yr_10)
#     dat_long %>% 
#         filter(ADM2_CODE==code) %>%
#         mutate(period = year<=5) %>%
#         ggplot(aes(x=year, y=loss, group=period, color = period)) + geom_point() +
#         geom_segment(aes(x=0,xend=5,y=start_mean, yend = start_mean), color = "#00BCF4") +
#         geom_segment(aes(x=6,xend=10,y=end_mean, yend = end_mean), color = "#F8766D")

    
# }

# plot_jurisdiction(code = 18102)
# plot_jurisdiction(7758)
# plot_jurisdiction_conf(7758)




# ## Can adapt this plotting function maybe? for the slope predicting credits part

# df_beta %>% 
#     mutate(slice_id = as.Date(slice_id)) %>% 
#     filter(term == "year") %>% 
#     ggplot(aes(slice_id)) +
#     geom_hline(yintercept = 0) +
#     facet_wrap("ADM2_CODE", scales = "free") +
#     # geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.25) +
#     geom_line(aes(y = estimate)) +
#     geom_point(data = dat_long %>% 
#                    filter(ADM1_NAME == "Nangroe Aceh Darussalam"), 
#                aes(x=date, y=loss/1e6)) +
#     theme_bw(8)

