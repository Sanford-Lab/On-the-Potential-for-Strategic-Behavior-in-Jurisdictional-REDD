library(tidyverse)
library(zoo)
library(tidyquant)
library(here)
library(stringr)

# Define the names you want to use
names <- c("undisturbed", "degraded", "deforested", "regrowth", "water", "other")

# Define the range of years
years <- 1990:2023

metadata <- read_csv("data/raw/tmf/area_value_1_1990_globe.csv") %>%
    select(-c("system:index",#"area",
              ".geo"))

# Initialize an empty list to store data frames for each year
year_dfs <- list()

# Initialize an empty vector to store missing files
missing_files <- c()

# Loop over the years
for (y in years) {
    # Initialize an empty list to store data frames for each land type
    land_type_dfs <- list()
    
    # Loop over the numbers 1 to 6
    for (x in 1:6) {
        # Construct the filename
        filename <- paste0("data/raw/tmf/area_value_", x, "_", y, "_globe.csv")
        
        # Test if file exists
        if (!file.exists(filename)) {
            missing_files <- c(missing_files, filename)
            next
        }
        
        # Read the CSV file
        df <- read_csv(filename) %>% 
            select(c("ADM2_CODE","sum"))
        
        # Rename the 'sum' variable
        df <- df %>%
            rename(!!names[x] := sum)
        
        # Add the data frame to the list
        land_type_dfs[[names[x]]] <- df
    }
    
    # Join all data frames together based on the 'ADM1_NAME', 'ADM1_CODE', 'ADM2_NAME', 'ADM2_CODE', and 'area' columns
    df_year <- Reduce(function(df1, df2) {
        full_join(df1, df2)
    }, land_type_dfs)
    
    # Add the year as a column
    df_year$year <- y
    
    # Add the data frame to the list
    year_dfs[[as.character(y)]] <- df_year
}

# Print missing files
if (length(missing_files) > 0) {
    print("The following files were missing:")
    print(missing_files)
} else {
    print("No files were missing.")
}

# Merge all data frames together
dat <- metadata %>% left_join(bind_rows(year_dfs)) %>% select(-sum) 

dat_long <- dat %>%
    group_by(ADM0_NAME, ADM0_CODE,ADM1_NAME, ADM1_CODE,ADM2_NAME,ADM2_CODE) %>%
    filter(ADM2_CODE != 23036,
           ADM2_CODE != 15426,
           ADM2_CODE != 22602,
           ADM2_CODE != 22917,
           ADM2_CODE != 48472) %>%
    mutate(year = as.numeric(year),
           date = ymd(paste0(year, "/01/01")),
           undisturbed_diff = undisturbed - lag(undisturbed, 1, default = NA),
           degraded_diff = degraded - lag(degraded, 1, default = NA),
           deforested_diff = deforested - lag(deforested, 1, default = NA),
           regrowth_diff = regrowth - lag(regrowth, 1, default = NA))%>%
    drop_na(deforested_diff)

dat_long %>% filter(year == 2000) %>% mutate(treecover_2000 = undisturbed) %>%
    select(ADM0_NAME, ADM0_CODE,ADM1_NAME, ADM1_CODE,ADM2_NAME,ADM2_CODE,treecover_2000) %>%
    right_join(dat_long) -> dat_long

dat_long <- dat_long %>%
    mutate(ID = ADM2_CODE,
           level = "ADM2")  %>%
    ungroup() %>%
    filter(treecover_2000 > 0)%>%
    drop_na(treecover_2000)

dat_long <- dat_long %>% 
    group_by(ADM0_NAME, ADM0_CODE,ADM1_NAME, ADM1_CODE,year, date) %>% 
    summarise(degraded_diff = sum(degraded_diff, na.rm = T),
              degraded = sum(degraded, na.rm = T),
              treecover_2000 = sum(treecover_2000, na.rm = T),
              undisturbed_diff = sum(undisturbed_diff, na.rm = T),
              undisturbed = sum(undisturbed, na.rm = T),
              deforested_diff = sum(deforested_diff, na.rm = T),
              deforested = sum(deforested, na.rm = T),
              regrowth_diff = sum(regrowth_diff, na.rm = T),
              regrowth = sum(regrowth, na.rm = T),
              area = sum(area, na.rm = T),
              ADM2_NAME = NA,
              ADM2_CODE = NA,
              ID = ADM1_CODE,
              level = "ADM1"
              ) %>% unique() %>%
    ungroup() %>%
    bind_rows(dat_long)

dat_long <- dat_long %>% 
    group_by(ADM0_NAME, ADM0_CODE,year, date) %>% 
    summarise(degraded_diff = sum(degraded_diff, na.rm = T),
              degraded = sum(degraded, na.rm = T),
              treecover_2000 = sum(treecover_2000, na.rm = T),
              undisturbed_diff = sum(undisturbed_diff, na.rm = T),
              undisturbed = sum(undisturbed, na.rm = T),
              deforested_diff = sum(deforested_diff, na.rm = T),
              deforested = sum(deforested, na.rm = T),
              regrowth_diff = sum(regrowth_diff, na.rm = T),
              regrowth = sum(regrowth, na.rm = T),
              area = sum(area, na.rm = T),
              ADM2_NAME = NA,
              ADM2_CODE = NA,
              ADM1_NAME = NA,
              ADM1_CODE = NA,
              ID = ADM0_CODE,
              level = "ADM0") %>% unique() %>%
    ungroup()  %>%
    bind_rows(dat_long)

dat_long <- dat_long %>%
    mutate( baseline_undisturbed = lag(rollmean(undisturbed_diff, k=5, fill=NA, align='right'),1,default = NA),
            endline_undisturbed = lead(baseline_undisturbed, 6, default = NA),
            est_effect_undisturbed = baseline_undisturbed-endline_undisturbed,
            baseline_regrowth = lag(rollmean(regrowth_diff, k=5, fill=NA, align='right'),1,default = NA),
            endline_regrowth = lead(baseline_regrowth, 6, default = NA),
            est_effect_regrowth = baseline_regrowth-endline_regrowth,
            baseline_deforested = lag(rollmean(deforested_diff, k=5, fill=NA, align='right'),1,default = NA),
            endline_deforested = lead(baseline_deforested, 6, default = NA),
            est_effect_deforested = baseline_deforested-endline_deforested,
            baseline_degraded = lag(rollmean(degraded_diff, k=5, fill=NA, align='right'),1,default = NA),
            endline_degraded = lead(baseline_degraded, 6, default = NA),
            est_effect_degraded = baseline_degraded-endline_degraded)

dat_long <- dat_long %>%
    # Group the data by ID
    group_by(ID) %>%
    # Arrange the data by year within each group
    arrange(year, .by_group = TRUE) %>%
    mutate(
        # Calculate the cumulative sum of deforested_diff within each group
        cum_deforested = cumsum(deforested_diff),
        # Calculate the cumulative sum of degraded_diff within each group
        cum_degraded = cumsum(degraded_diff),
        # Calculate the cumulative sum of regrowth_diff within each group
        cum_regrowth = cumsum(regrowth_diff),
        # Calculate the remaining tree cover by adding undisturbed and regrowth
        treecover_remaining = undisturbed + regrowth)%>%
    ungroup

library(rio)
out_dir <- here::here("data", "processed")
export(dat_long, paste0(out_dir, "/dat_long_TMF.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### ART eligible rolling regressions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ARTintervention_names <- readRDS("data/processed/ARTintervention_names.rds")

ART_eligible_ADM1 <- dat_long %>% 
    filter(treecover_2000 >= 2.5e6*10000) %>% 
    filter(level == "ADM1")

ART_eligible_ADM2_ID <- dat_long %>% filter(level == "ADM2") %>%
    filter(ADM1_CODE %in% ART_eligible_ADM1$ADM1_CODE) %>% select(ID) %>% unique() %>% pull()

ART_intervention <- dat_long %>% filter(ID %in% ARTintervention_names$ID)


ART_eligible <- dat_long %>%
    filter(level == "ADM2",
           ID %in% ART_eligible_ADM2_ID
    )%>%
    rbind(dat_long %>% filter(level == "ADM0")) %>%
    rbind(ART_eligible_ADM1) %>%
    filter(!(ID %in% ARTintervention_names$ID))%>%
    rbind(ART_intervention)%>%
    mutate(ART_eligible = 1) %>% 
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, ART_eligible)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        ART_eligible ) %>%
    mutate(ART_eligible = replace_na(ART_eligible, 0))

dat_long_TMF_ART <- dat_long %>%
    filter(ART_eligible == 1)

saveRDS(dat_long_TMF_ART , here::here("data", "processed", "dat_long_TMF_ART.rds"))


# Non-parallelized version, slow but totally fine for only ART eligible
library(tidyfit)
library(estimatr)

dat_long_TMF_ART_ADM12 <- dat_long_TMF_ART %>%
    filter(level != "ADM2")

system.time(out <- dat_long_TMF_ART_ADM12 %>%
                group_by(ID) %>%
                regress(deforested_diff ~ year, m("lm"),
                        m("lm"),
                        .cv = "sliding_index", .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
                        .force_cv = TRUE, .return_slices = TRUE))

### Unnest stuff
dat_long_TMF_ART_beta <- coef(out) %>% 
    unnest(model_info) %>% 
    unique() %>%
    # filter(term == "year") %>%
    select(-c("std.error","statistic","p.value")) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(
        slope = year,
        intercept = `(Intercept)`,
        year = year(slice_id)) %>% 
    select(-`(Intercept)`) %>%
    filter(year >= 5) %>% 
    left_join(dat_long_TMF_ART_ADM12, by = c("ID", "year")) %>% 
    mutate(last_year_diff = baseline_deforested - deforested_diff)


saveRDS(dat_long_TMF_ART_beta , here::here("data", "processed", "dat_long_TMF_ART_beta.rds"))


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
Verra_intervention <- dat_long %>% filter(ID %in% Verraintervention_names$ID)

Verra_eligible <- bind_rows(Verra_national,Verra_sub1,Verra_sub2)%>%
    filter(!(ID %in% Verraintervention_names$ID))%>%
    bind_rows(Verra_intervention)%>%
    mutate(Verra_eligible = 1)%>%
    ungroup %>%
    select(ADM1_CODE, ADM2_CODE, ADM0_CODE, ID, year, Verra_eligible)

rm(Verra_national,Verra_sub1,Verra_sub2, Verra_intervention)

dat_long <- dat_long %>% ungroup() %>%
    left_join(
        Verra_eligible ) %>%
    mutate(Verra_eligible = replace_na(Verra_eligible, 0))

dat_long_TMF_Verra <- dat_long %>%
    filter(Verra_eligible == 1)

saveRDS(dat_long_TMF_Verra , here::here("data", "processed", "dat_long_TMF_Verra.rds"))

system.time(out <- dat_long_TMF_Verra %>%
                group_by(ID) %>%
                regress(deforested_diff ~ year, m("lm"),
                        m("lm"),
                        .cv = "sliding_index", .cv_args = list(lookback = years(4), step = 1, index = "date", complete = FALSE),
                        .force_cv = TRUE, .return_slices = TRUE))



### Unnest stuff
dat_long_TMF_Verra_beta <- coef(out) %>% 
    unnest(model_info) %>% 
    unique() %>%
    # filter(term == "year") %>%
    select(-c("std.error","statistic","p.value")) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(
        slope = year,
        intercept = `(Intercept)`,
        year = year(slice_id)) %>% 
    select(-`(Intercept)`) %>%
    filter(year >= 5) %>% 
    left_join(dat_long_TMF_Verra, by = c("ID", "year")) %>% 
   # left_join(dat_long_TMF_ART_ADM12, by = c("ID", "year")) %>% 
    mutate(last_year_diff = baseline_deforested - deforested_diff)

saveRDS(dat_long_TMF_Verra_beta , here::here("data", "processed", "dat_long_TMF_Verra_beta.rds"))

