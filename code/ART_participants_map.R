#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Set up script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(here)
library(rio)
library(scales)
library(fuzzyjoin)
library(stringi)
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

intervention_names <- readRDS("data/processed/ARTintervention_names_new.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Map of ART TREES jurisdictions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ART_map_jurisdictions <- intervention_names %>%
    filter(intervention_fund == "ART TREES")

library(rnaturalearth)

df <- ne_countries(returnclass = "sf")
#df$admin

ART_countries <- c("Brazil",
                   "Bhutan",
                   "Colombia",
                   "Mexico",
                   "Costa Rica",
                   "Democratic Republic of the Congo",
                   "Ghana",
                   "Ecuador",
                   "Ethiopia",
                   "Gabon",
                   "Guyana",
                   "Papua New Guinea",
                   "Peru",
                   "Uganda",
                   "Vietnam")

df <- ne_countries(returnclass = "sf")
subset(df, admin %in% ART_countries)$admin

ne_countries(scale = 10, returnclass = 'sf') %>%
    filter(name != "Antarctica")%>%
    mutate(ARTTREES = ifelse(name %in% ART_countries, "Countries with existing ART TREES JREDD+", "not ART") )%>%
    ggplot(aes(fill = ARTTREES)) +
    geom_sf() +
    scale_fill_manual(
        values = c(
            "Countries with existing ART TREES JREDD+" = palette$red,
            "not ART" = "#e1e9b0"
        ),
        breaks = c("Countries with existing ART TREES JREDD+")
    )+
  #  xlim(5,9) +
  #  ylim(53,56) +
    theme(panel.background = element_rect(fill = 
                                          #    "lightblue"
                                          "#CCE2FF"
                                          ),
          panel.grid = element_line(size = 0.05),
          legend.title = element_blank(),
          legend.position = "bottom")

ART_adm0 <- c("Bhutan",
              "Colombia",
              "Costa Rica",
              "Ghana",
              "Ecuador",
              "Ethiopia",
              "Gabon",
              "Guyana",
              "Papua New Guinea",
              "Peru",
              "Uganda",
              "Vietnam")
ART_adm1 <- c("Tocantins", "Amapá", "Maranhão", "Acre", "Para",
              "Quintana Roo", "Jalisco", "Tshuapa")

df <- ne_states(returnclass = "sf")
subset(df, name %in% ART_adm1)$name
library(sf)

tshuapa <- read_sf("data/gadm41_COD_shp/gadm41_COD_1.shp") %>% filter(NAME_1 == "Tshuapa") 

ART_0 <- ne_countries(scale = 10, returnclass = 'sf') %>%
    filter(name != "Antarctica")%>%
    mutate(ARTTREES = ifelse(name %in% ART_adm0, "Jurisdictions with existing ART TREES JREDD+", "not ART") )

ART_1 <- ne_states(returnclass = 'sf') %>%
    filter(name %in% ART_adm1)

ggplot() +
    geom_sf(data = ART_0, aes(fill = ARTTREES)) +
    scale_fill_manual(
        values = c(
            "Jurisdictions with existing ART TREES JREDD+" = palette$red,
            "not ART" = "#e1e9b0"#"#e0e8a0"
        ),
        breaks = c("Jurisdictions with existing ART TREES JREDD+")
    )+
    geom_sf(data = ART_1, fill = palette$red)+
    geom_sf(data = tshuapa, fill = palette$red)+
    #  xlim(5,9) +
    #  ylim(53,56) +
    theme(panel.background = element_rect(fill = 
                                              "#CCE2FF"# "lightblue"
                                          ),
          panel.grid = element_line(size = 0.05),
          legend.title = element_blank(),
          legend.position = "none")

ggsave(filename = paste0(fig_dir, "/ART_map.png"),
       width = 8)
ggsave(filename = paste0(fig_dir_pdf, "/ART_map.pdf"),
       width = 8, height = 6)

start_years <- intervention_names %>%
    select(intervention_year, intervention_name)%>%
    distinct()%>%
    filter(intervention_year < 2024)%>%
    mutate(
        name_words = str_split(intervention_name, pattern = " ")
    ) %>%
    unnest(cols = c(name_words))

accents <- function(x){
    x <- stri_trans_general(x, id = "Latin-ASCII")
}

ART_years <- rbind(
    ART_1 %>% select(name),
    tshuapa %>% rename(name = NAME_1) %>% select(name)
) %>%
    rbind(ART_0 %>% filter(ARTTREES != "not ART") %>% select(name) )%>%
    mutate(name = accents(name)) %>%
    mutate(
        name_words = str_split(name, pattern = " ")
    ) %>%
    unnest(cols = c(name_words))%>%
    left_join(start_years, by = "name_words")%>%
    select(name, intervention_year)%>%
    distinct

ggplot() +
    geom_sf(data = ART_0, fill = "#e1e9b0", alpha = 0.9)+
    geom_sf(data = ART_years, aes(fill = intervention_year)) +
    scale_fill_gradient(
        low = "thistle1",
        high = "red4",
        guide = "colorbar",
        limits = c(2016, 2023.2),
        name = "ART TREES\nstart year",
    )+
    theme(
        panel.background = element_rect(fill = "#CCE2FF"),# "lightblue"
        panel.grid = element_line(linewidth = 0.05),
        legend.position = c(0.127, 0.275),
        legend.background =element_rect(fill = "#CCE2FF"),
        legend.text = element_text(size=8),
        legend.title=element_text(size=10)
    )

ggsave(filename = paste0(fig_dir, "/ART_date_map.png"),
       width = 8, height = 3.5)
ggsave(filename = paste0(fig_dir_pdf, "/ART_date_map.pdf"),
       width = 8, height = 3.5)
