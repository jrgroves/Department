#This file compiles the data presented by the AEA in the annual Papers and Proceedings
#from the annual survey.

#By: Jeremy Groves
#Created: May 15, 2025

rm(list=ls())

library(readxl)
library(tidyverse)

AEA.degree.in <- read_xlsx(path = "./Data/AEA Degrees and Enroll.Xlsx", sheet = "AEA Degree Awarded")
AEA.enrol.in  <- read_xlsx(path = "./Data/AEA Degrees and Enroll.Xlsx", sheet = "AEA Apps and Enroll")


#Clean Data

AEA.degree <- AEA.degree.in %>%
  filter(!is.na(Count)) %>%
  mutate(Average = Count / N) %>%
  select(-c(N, Count))

AEA.enroll <- AEA.enrol.in %>%
  pivot_wider(id_cols = c(Academic_Year, Institute_Type), names_from = Data, values_from = Count) %>%
  mutate(across(Applications:Enrolled, ~ ./ count)) %>%
  select(-count)

save(AEA.degree, AEA.enroll, file = "./Build/Output/AEA_Data.RData")