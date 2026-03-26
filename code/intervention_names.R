library(tidyverse)
library(zoo)
library(tidyquant)
library(here)

load("data/processed/forest_loss.Rdata")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Outlining JREDD projects and their start dates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####ART TREES Programs

# Brazil Amapa 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Amapa") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique()%>%
    mutate(intervention_year = 2016,
           intervention_name = "Brazil Amapa",
           intervention_level = "ADM1")

# Costa Rica start 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Costa Rica") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Costa Rica",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Brazil Maranhao 2016
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Maranhao") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Brazil Maranhao",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Guyana 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Guyana") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Guyana",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Brazil Tocantins 2016
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Tocantins") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Brazil Tocantins",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Ecuador 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ecuador") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Ecuador",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Ghana 2017, Ahafo, Ashanti, Bono, Bono East, Central, Eastern, Greater Accra, Volta, Western, Western North
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ghana", level == "ADM1",
                                         ADM1_NAME %in% c("Brong Ahafo", "Ashanti", "Central", "Eastern", "Greater Accra", "Volta", "Western")) %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Ghana",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Papua New Guinea 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Papua New Guinea") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Papua New Guinea",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

#Peru 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Peru") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Peru",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# DRC - Tshuapa Province 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Democratic Republic of the Congo",
                                           ADM2_NAME == "Tshuapa") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "DRC Tshuapa ART",
           intervention_level = "ADM2")%>%
    rbind(ART_interventions)

# Ethiopia 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ethiopia") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Ethiopia",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Gabon 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Gabon") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Gabon",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Uganda 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Uganda") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Uganda",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

## Vietnam 2021 (some provinces)
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Viet Nam", level == "ADM1",
                               ADM1_NAME %in% c("Lam Dong", "Dak Nong", "Kak Lak","Gia Lai","Kon Tum","Quang Ngai","Binh Dinh","Phu Yen","Khanh Hoa","Ninh Thuan","Binh Thuan")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Vietnam",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Colombia 2021
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Colombia", level == "ADM1",
                                         ADM1_NAME %in% c("Amazonas","Caqueta","Guainia","Guaviare","Putumayo","Vaupes")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Colombia",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Mexico Quintana Roo 2021
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Quintana Roo") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Mexico Quintana Roo",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Nepal Bagmati, Gandaki, Lumbini 2022 (too new)
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Nepal", level == "ADM1",
                               ADM1_NAME %in% c("Bagmati", "Gandaki", "Lumbini")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Nepal",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Mexico Jalisco 2022
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Jalisco") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Mexico Jalisco",
           intervention_level = "ADM1") %>%
    rbind(ART_interventions)

# Brazil Mato Grosso 2024
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Mato Grosso") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2024,
           intervention_name = "Brazil Mato Grosso ART",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Burkina Faso 2024
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Burkina Faso", level == "ADM0") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2024,
           intervention_name = "Burkina Faso",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

ART_interventions <- ART_interventions %>%
  ungroup() %>% mutate(intervention_fund = "ART TREES")

saveRDS(ART_interventions , here::here("data", "processed", "ARTintervention_names.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Programs as of 10/2025
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ART_interventions <- NULL
# Brazil Amapa 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Amapa") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique()%>%
    mutate(intervention_year = 2016,
           intervention_name = "Brazil Amapa",
           intervention_level = "ADM1")

# Costa Rica start 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Costa Rica") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Costa Rica",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Brazil Maranhao 2016
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Maranhao") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Brazil Maranhao",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Guyana 2016
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Guyana") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2016,
           intervention_name = "Guyana",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Ecuador 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ecuador") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Ecuador",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Ghana 2017, Ahafo, Ashanti, Bono, Bono East, Central, Eastern, Greater Accra, Volta, Western, Western North
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ghana", level == "ADM1",
                                         ADM1_NAME %in% c("Brong Ahafo", "Ashanti", "Central", "Eastern", "Greater Accra", "Volta", "Western")) %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Ghana",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Papua New Guinea 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Papua New Guinea") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Papua New Guinea",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

#Peru 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Peru") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "Peru",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# DRC - Tshuapa Province 2017
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Democratic Republic of the Congo",
                                           ADM2_NAME == "Tshuapa") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2017,
           intervention_name = "DRC Tshuapa ART",
           intervention_level = "ADM2")%>%
    rbind(ART_interventions)

# Ethiopia 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Ethiopia") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Ethiopia",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Gabon 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Gabon") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Gabon",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Uganda 2018
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Uganda") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2018,
           intervention_name = "Uganda",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

## Vietnam 2021 (some provinces)
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Viet Nam", level == "ADM1",
                               ADM1_NAME %in% c("Lam Dong", "Dak Nong", "Kak Lak","Gia Lai","Kon Tum","Quang Ngai","Binh Dinh","Phu Yen","Khanh Hoa","Ninh Thuan","Binh Thuan")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Vietnam",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Colombia start 2021
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Colombia", level == "ADM1",
                                         ADM1_NAME %in% c("Amazonas","Caqueta","Guainia","Guaviare","Putumayo","Vaupes")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Colombia",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)


## Nepal Bagmati, Gandaki, Lumbini 2022 (too new)
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Nepal", level == "ADM1",
                               ADM1_NAME %in% c("Bagmati", "Gandaki", "Lumbini")) %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Nepal",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Mexico Jalisco 2022
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Jalisco") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Mexico Jalisco",
           intervention_level = "ADM1") %>%
    rbind(ART_interventions)


# Bhutan 2020
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Bhutan") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2020,
           intervention_name = "Bhutan",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)

# Brazil Tocantins 2020
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Tocantins") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2020,
           intervention_name = "Brazil Tocantins",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Mexico Quintana Roo 2022
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Quintana Roo") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Mexico Quintana Roo",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Brazil Acre 2023
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Acre") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2023,
           intervention_name = "Brazil Acre",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Brazil Para 2023
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Para") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2023,
           intervention_name = "Brazil Para",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Brazil Mato Grosso 2024
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Mato Grosso") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2024,
           intervention_name = "Brazil Mato Grosso",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

# Mexico Yucatan 2024
ART_interventions <- dat_long %>% filter(ADM1_NAME == "Yucatan") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2024,
           intervention_name = "Mexico Yucatan",
           intervention_level = "ADM1")%>%
    rbind(ART_interventions)

## Burkina Faso 2025
ART_interventions <- dat_long %>% filter(ADM0_NAME == "Burkina Faso", level == "ADM0") %>%
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2025,
           intervention_name = "Burkina Faso",
           intervention_level = "ADM0")%>%
    rbind(ART_interventions)



ART_interventions <- ART_interventions %>%
    ungroup() %>% mutate(intervention_fund = "ART TREES")
saveRDS(ART_interventions , here::here("data", "processed", "ARTintervention_names_new.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### VERRA

# Brazil Acre 2021
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Acre") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique()%>%
    mutate(intervention_year = 2021,
           intervention_name = "Brazil Acre",
           intervention_level = "ADM1")

# Brazil Amapa 2021
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Amapa") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Brazil Amapa",
           intervention_level = "ADM1")%>%
    rbind(Verra_interventions)

# Brazil Amazonas 2023
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Amazonas") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2023,
           intervention_name = "Brazil Amazonas",
           intervention_level = "ADM1")%>%
    rbind(Verra_interventions)

# Brazil Mato Grosso 2019
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Mato Grosso") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2019,
           intervention_name = "Brazil Mato Grosso Verra",
           intervention_level = "ADM1")%>%
    rbind(Verra_interventions)

# Brazil Para 2019
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Para") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2019,
           intervention_name = "Brazil Para",
           intervention_level = "ADM1")%>%
    rbind(Verra_interventions)

# Brazil Rondonia 2022
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Brazil", ADM1_NAME == "Rondonia") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Brazil Rondonia",
           intervention_level = "ADM1")%>%
    rbind(Verra_interventions)


# Cambodia 2022
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Cambodia") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Cambodia",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)


# Colombia 2020
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Colombia") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2020,
           intervention_name = "Colombia",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)


# DRC - Mai Ndombe Province 2021
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Democratic Republic of the Congo",
                                           ADM2_NAME == "Maï-Ndombe") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "DRC Mai Ndombe",
           intervention_level = "ADM2")%>%
    rbind(Verra_interventions)

## Need to add DRC back after fixed borders

# Guatemala 2022
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Guatemala") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Guatemala",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)

# Kenya 2022
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Kenya") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Kenya",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)

# Tanzania 2021
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "United Republic of Tanzania") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2021,
           intervention_name = "Tanzania",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)

# Zambia 2022
Verra_interventions <- dat_long %>% filter(ADM0_NAME == "Zambia") %>% 
    select(ADM2_CODE, ADM1_CODE, ADM0_CODE, ID, level) %>% 
    unique() %>%
    mutate(intervention_year = 2022,
           intervention_name = "Zambia",
           intervention_level = "ADM0")%>%
    rbind(Verra_interventions)

Verra_interventions <- Verra_interventions %>%
    ungroup() %>% 
    mutate(intervention_fund = "Verra JNR",
           intervention_name = paste0(intervention_name, " (", intervention_fund, ")"))

saveRDS(Verra_interventions , here::here("data", "processed", "Verraintervention_names.rds"))
