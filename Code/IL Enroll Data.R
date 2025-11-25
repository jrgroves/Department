#This script deals with data recieved from NIU including information on the MA and
#Ph.D. enrollments and degrees confirred from 2021 through 2024 for the IL
#universities.

#Author: Jeremy R. Groves
#Date: November 25, 2025

rm(list=ls())

library(tidyverse)

data <- read.csv(file = "./Data/IL Econ Programs.csv")
totals <- c("Year Total", "Public Uni. Total", "Independent NFP Inst. Total")

data2 <- data |>
  pivot_longer(cols = c("PhD_Degree", "PhD_Enroll", "MA_Degree", "MA_Enroll"), 
                      names_to = "Program", values_to = "Counts") |>
  filter(!(Institution %in% totals)) |>
  filter(!is.na(Counts))

phd <- data2 %>%
  filter(Program == "PhD_Enroll") %>%
  select(Institution) %>%
  distinct()

tot <- data |>
  filter(Institution %in% totals) |>
  pivot_longer(cols = c("PhD_Degree", "PhD_Enroll", "MA_Degree", "MA_Enroll"), 
               names_to = "Program", values_to = "Counts")|>
  filter(!is.na(Counts))

data2 <- data2 %>%
  mutate(class = case_when(Institution %in% phd$Institution ~ "P",
                           TRUE ~ "M"))
share <- data2 %>%
  left_join(. , tot, by = c("Year", "Inst_Type", "Program")) %>%
  mutate(share_type = Counts.x / Counts.y) %>%
  left_join(., filter(tot, Inst_Type==4), by = c("Year", "Program")) %>%
  mutate(share_tot = Counts.x / Counts) %>%
  select(Year, class, Institution.x, Inst_Type.x, Program, starts_with("share")) %>%
  rename("Institution" = "Institution.x",
         "Inst_Type" = "Inst_Type.x")



temp <- share %>%
  filter(class == "P") %>%
  filter(Program == "MA_Enroll") %>%
  filter(share_type < .5)

ggplot(aes(x = Year), data = temp) +
  geom_line(aes(y = share_type, group = Institution, color = Institution)) +
  geom_line(aes(y = share_type, group = Institution), color = "darkred", linewidth = 2,
            data = filter(temp, Institution == "Northern Illinois University")) +
  theme_bw()


