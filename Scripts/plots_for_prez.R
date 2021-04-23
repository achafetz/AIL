# PROJECT:  AIL
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  charts for Sheperd Center Talk
# LICENSE:  MIT
# DATE:     2021-04-23
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(readxl)
library(tidytext)
library(sf)
library(rnaturalearth)
library(gisr)
  

# GLOBAL VARIABLES --------------------------------------------------------
  


# CUMULATIVE COVID CASES --------------------------------------------------

  #source: https://ourworldindata.org/grapher/cumulative-covid-cases-region
  df_covid_cases <- read_csv("Data/cumulative-covid-cases-region.csv") %>% 
    clean_names()
  
  df_covid_cases_glob <- df_covid_cases %>% 
    filter(entity == "World")

  date <- max(df_covid_cases_glob$day)
  
  df_covid_cases_glob %>% 
    ggplot(aes(day, total_confirmed_cases_of_covid_19)) +
    geom_area(fill = "#1B68B3") +
    geom_text(data = df_covid_cases_glob %>% filter(day == max(day)),
               aes(label = number(total_confirmed_cases_of_covid_19,
                                  accuracy = .1,
                                  suffix = "m",
                                  scale = 1/1000000)),
              hjust = .6,
              family = "Source Sans Pro SemiBold", color = "white") +
    scale_y_continuous(label = number_format(scale = 1/1000000,
                                             suffix = "m"),
                       position = "right") +
    scale_x_date(date_breaks = "2 month",
                 date_labels = "%b %y") +
    labs(x = NULL, y = "cumulative COVID cases",
         caption = glue("as of {date}
                        Source: Our World in Data")) +
    si_style() +
    theme(panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_line(color = "white"),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.text.y = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))

  si_save("Images/covid_global.png",
          width = 8.6, height = 4.84)  
  

# COVID DEATHS ------------------------------------------------------------

  #source: https://ourworldindata.org/grapher/cumulative-covid-deaths-region
  df_covid_deaths <- read_csv("Data/cumulative-covid-deaths-region.csv") %>% 
    clean_names()
  
  df_covid_deaths_glob <- df_covid_deaths %>% 
    filter(entity == "World")
  
  date <- max(df_covid_deaths_glob$day)
  
  df_covid_deaths_glob %>% 
    ggplot(aes(day, total_confirmed_deaths_due_to_covid_19)) +
    geom_area(fill = "#1B68B3") +
    geom_text(data = df_covid_deaths_glob %>% filter(day == max(day)),
              aes(label = number(total_confirmed_deaths_due_to_covid_19,
                                 accuracy = .1,
                                 suffix = "m",
                                 scale = 1/1000000)),
              hjust = .6,
              family = "Source Sans Pro SemiBold", color = "white") +
    scale_y_continuous(label = number_format(scale = 1/1000000,
                                             suffix = "m"),
                       position = "right") +
    scale_x_date(date_breaks = "2 month",
                 date_labels = "%b %y") +
    labs(x = NULL, y = "cumulative COVID deaths",
         caption = glue("as of {date}
                        Source: Our World in Data")) +
    si_style() +
    theme(panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_line(color = "white"),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.text.y = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))
  
  si_save("Images/covid_deaths_global.png",
          width = 8.6, height = 4.84)  
  
# PLHIV -------------------------------------------------------------------

  #source: https://ourworldindata.org/hiv-aids#
  
  df_plhiv <- read_csv("Data/number-of-people-living-with-hiv.csv") %>% 
    clean_names()
  
  df_plhiv_glob <- df_plhiv %>% 
    filter(entity == "World")
  
  year <- max(df_plhiv_glob$year)
  
  df_plhiv_glob %>% 
    ggplot(aes(year, prevalence_hiv_aids_sex_both_age_all_ages_number)) +
    geom_area(fill = "#1B68B3") +
    geom_text(data = df_plhiv_glob %>% filter(year == max(year)),
              aes(label = number(prevalence_hiv_aids_sex_both_age_all_ages_number,
                                 accuracy = .1,
                                 suffix = "m",
                                 scale = 1/1000000)),
              hjust = .6,
              family = "Source Sans Pro SemiBold", color = "white") +
    scale_y_continuous(label = number_format(scale = 1/1000000,
                                             suffix = "m"),
                       position = "right") +
    scale_x_continuous(breaks = seq(1990, 2017, 5)) +
    labs(x = NULL, y = "People living with HIV",
         caption = glue("through {year}
                        Source: Our World in Data")) +
    si_style() +
    theme(panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_line(color = "white"),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.text.y = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))
  
  si_save("Images/plhiv_global.png",
          width = 8.6, height = 4.84)  
  
  

# HIV DEATHS --------------------------------------------------------------

  #source: https://ourworldindata.org/hiv-aids#
  
  df_hiv_deaths <- read_csv("Data/deaths-from-aids-ihme.csv") %>% 
    clean_names()
  
  df_hiv_deaths_glob <- df_hiv_deaths %>% 
    filter(entity == "World")
  
  year <- max(df_hiv_deaths_glob$year)
  
  df_hiv_deaths_glob %>% 
    ggplot(aes(year, deaths_hiv_aids_sex_both_age_all_ages_number)) +
    geom_area(fill = "#1B68B3") +
    geom_text(data = df_hiv_deaths_glob %>% filter(year == max(year)),
              aes(label = number(deaths_hiv_aids_sex_both_age_all_ages_number,
                                 accuracy = .1,
                                 suffix = "m",
                                 scale = 1/1000000)),
              hjust = .6,
              family = "Source Sans Pro SemiBold", color = "white") +
    scale_y_continuous(label = number_format(scale = 1/1000000,
                                             accuracy = .1,
                                             suffix = "m"),
                       position = "right") +
    scale_x_continuous(breaks = seq(1990, 2017, 5)) +
    labs(x = NULL, y = "AIDS Deaths",
         caption = glue("through {year}
                        Source: Our World in Data")) +
    si_style() +
    theme(panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_line(color = "white"),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.text.y = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))
  
  si_save("Images/aids_deaths.png",
          width = 8.6, height = 4.84)  
  
  
  df_hiv_deaths_glob %>%
    mutate(cumsum = cumsum(deaths_hiv_aids_sex_both_age_all_ages_number)) %>% 
    ggplot(aes(year, cumsum)) +
    geom_area(fill = "#1B68B3") +
    geom_text(data = df_hiv_deaths_glob %>% 
                mutate(cumsum = cumsum(deaths_hiv_aids_sex_both_age_all_ages_number)) %>% 
                filter(year == max(year)),
              aes(label = number(cumsum,
                                 accuracy = .1,
                                 suffix = "m",
                                 scale = 1/1000000)),
              hjust = .6,
              family = "Source Sans Pro SemiBold", color = "white") +
    scale_y_continuous(label = number_format(scale = 1/1000000,
                                             suffix = "m"),
                       position = "right") +
    scale_x_continuous(breaks = seq(1990, 2017, 5)) +
    labs(x = NULL, y = "AIDS Deaths",
         caption = glue("through {year}
                        Source: Our World in Data")) +
    si_style() +
    theme(panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_line(color = "white"),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.text.y = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))
  
  si_save("Images/aids_cum_deaths.png",
          width = 8.6, height = 4.84)  

# LEADING CAUSE OF DEATH --------------------------------------------------

  #source:https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-leading-causes-of-death
  path <- "Data/GHE2019_COD_WBIncome_2000_201933383745-a750-4d94-8491-fb209dcece6f.xlsx"
  year <- 2019
  
  read_who <- function(filepath, year, group = "LI"){
    
    df <- read_excel(filepath,
               sheet = glue("{year} {group}"),
               skip =12,
               col_names = FALSE)
    
    df <- df %>% 
      select(category = ...2,
             category_name = ...3,
             cod_maj = ...4,
             cod_maj_name = ...5,
             cod = ...6,
             deaths = ...7) %>% 
      mutate(year = {year},
             income_group = {group},
             cod = ifelse(is.na(cod), cod_maj_name, cod),
             cod = ifelse(cod_maj == "Neonatal conditions", cod_maj, cod),
             category = case_when(!is.na(category) ~ word(category_name) %>% str_remove(",")),
             cod_maj = case_when(!is.na(cod_maj) ~ cod_maj_name,
                                 cod == "Neonatal conditions" ~ cod),
             ) %>%
      select(-ends_with("name")) %>% 
      fill(category, cod_maj) %>% 
      filter(!is.na(cod),
             !cod %in% c("Preterm birth complications",
                          "Birth asphyxia and birth trauma",
                          "Neonatal sepsis and infections",
                          "Other neonatal conditions"))
    
    return(df)
  }
  
  
  df_cod <- map_dfr(c(2000, 2019),
                    ~read_who(path, .x))
  
  df_cod_rank <- df_cod %>% 
    group_by(year) %>% 
    mutate(rank = min_rank(desc(deaths))) %>% 
    ungroup() %>% 
    arrange(year, rank)

  df_cod_rank %>% 
    filter(rank <=10) %>% 
    mutate(color = ifelse(cod == "HIV/AIDS", "#1B68B3", "#ffffff"),
           cod_formatted = glue("<span style='color:{color}'>{rank}\\. {cod}</span>")) %>% 
    ggplot(aes(deaths, reorder_within(cod_formatted, deaths, year), color = color)) +
    geom_segment(aes(x = 0, xend = deaths, yend = reorder_within(cod_formatted, deaths, year))) +
    geom_point(size = 4) +
    facet_wrap(~year, scales = "free_y") +
    scale_y_reordered() +
    scale_color_identity() +
    scale_x_continuous(label = number_format(scale = 1/1000,
                                             accuracy = 1,
                                             suffix = "k"), expand = c(.05, .5)) +
    labs(x = NULL, y = NULL,
         title = "SIGNIFICANT DECLINE IN HIV/AIDS DEATHS SINCE 2000",
         subtitle = "leading cause of deaths in Low Income Countries",
         caption = glue("Source: WHO Estimated deaths by cause and region")) +
    si_style() +
    theme(strip.text = element_text(family = "Source Sans Pro SemiBold", color = "#1B68B3"),
          axis.text.y = element_markdown(),
          panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(color = "#1B68B3"),
          axis.title.y = element_text(color = "#1B68B3"),
          plot.title = element_text(color = "#1B68B3"),
          plot.subtitle = element_text(color = "#1B68B3"),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7"))
  
  
  si_save("Images/leading_cod.png",
          width = 8.6, height = 4.84)  

  

# USAID MAP ---------------------------------------------------------------

 
   spdf <- ne_countries(type = "sovereignty", 
                       scale = 110, 
                       returnclass = "sf") %>% 
    select(sovereignt, admin, name, adm0_a3) %>% 
    filter(admin != "Antarctica") %>% # Remove Antarctica
    clean_countries(colname = "admin")
  
  ## Raw data
  df_ou <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  # Join MSD to spdf
  spdf_ou <- spdf %>% 
    left_join(df_ou %>% 
                filter(fundingagency == "USAID",
                       fiscal_year == 2021,
                       !is.na(targets)) %>% 
                distinct(operatingunit, countrynamename), 
              by = c("admin" = "countrynamename")) %>% 
    filter(!is.na(operatingunit))
  
  
  ## VIZ ---------------------------------------------------------
  
  #source: https://github.com/USAID-OHA-SI/lastmile/blob/master/Scripts/99_FY20Q4_USAID_PEPFAR_Countries.R
  ## Global Maps
  ggplot() +
    geom_sf(data = spdf, fill = NA, color = "white", size = .4) +
    geom_sf(data = spdf_ou,
            fill = "#1B68B3",
            color = "white",
            size = .2) +
    labs(
      caption = "USAID - Office of HIV/AIDS - Programs Overview as of FY 2021"
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(color = "white"),
      plot.background = element_rect(fill = "#A8C5E7", color = "#A8C5E7")
    )
  
  si_save("Images/usaid_pepfar_map.png",
          scale = 1.2, dpi = 310, 
          width = 10, height = 7)   
  
  
  