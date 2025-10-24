rm(list=ls())

library(tidyverse)

temp1 <- read.csv(file = "./Data/databook Majors.csv")
temp2 <- read.csv(file = "./Data/Tableau NIU Fall Majors.csv")


Clean <- temp1 %>%
  select(DEPART.MENT) %>%
  distinct()

temp1 <- temp1 %>%
  mutate(Major = str_to_title(Major),
         DEGREE = case_when(DEGREE == "BS" ~ "Bachelor",
                            DEGREE == "BA" ~ "Bachelor",
                            DEGREE == "BA/BS" ~ "Bachelor",
                            TRUE ~ DEGREE))

working <- temp2 %>%
  filter(Acadept %in% Clean$DEPART.MENT) %>%
  rename("Count" = "Count.of.OSIR") %>%
  pivot_wider(id_cols = c(Acadept, Degname, Major), names_from = Acad.Yr, names_prefix = "F",
              values_from = "Count") %>%
  mutate(DEGREE = str_split_i(Major, "\\(", 2),
         DEGREE = gsub("\\)", "", DEGREE),
         DEGREE = case_when(DEGREE == "BS" ~ "Bachelor",
                            DEGREE == "BA" ~ "Bachelor",
                            TRUE ~ DEGREE),
         MAJOR = str_split_i(Major, " \\(", 1)) %>%
  select(-c("Degname", "Major")) %>%
  replace_na(list(F2025 = 0, F2024 = 0, F2023 = 0, F2022 = 0, F2021 = 0, F2020 = 0,
                  F2019 = 0, F2018 = 0, F2017 = 0, F2016 = 0)) %>%
  arrange(Acadept, MAJOR, DEGREE) %>%
  group_by(Acadept, MAJOR, DEGREE) %>%
  summarize(across(F2025:F2016, sum)) %>%
  ungroup() %>%
  full_join(., temp1, by = c("DEGREE", "MAJOR" = "Major")) %>%
  arrange(Acadept, MAJOR, DEGREE) 
