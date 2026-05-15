# The purpose of this script is to use Tableau Fall and Spring census data to show the number of 
# students in the department as of the ten-day count for the semester and compare across departments
# and within the college. This script is also processing all of the Tableau data available.
#
# The data is pulled from the Tableau Semester Enrollment Trend and is pasted into the Excel file
# read below in the appropriate sheet.

#By: Jeremy R. Groves
#Created: April 14, 2026
#Updated: May 15, 2026   : Added the other Tableau data files into a single Excel worksheet and added all processing here.

rm(list=ls())

library(tidyverse)
library(readxl)


fall <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall Headcount")
spring <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Spring Headcount")
bridge <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "College Map")
deg.in <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Degrees Conferred", .name_repair = "universal")
old.deg <- read.csv(file = "./Data/databook Confered.csv")
maj.in <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall Majors", .name_repair = "universal")

#Compile and Clean#####3

#Headcount data####

core <- fall %>%
  bind_rows(., spring) %>%
  left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
  arrange(Term)

terms <- unique(as.character(core$Term))

core <- core %>%
  mutate(Term = factor(Term, levels = terms))

rm(terms, fall, spring)

#Degrees Conferred ######
#Degree Definitions
bach <- c("BA", "BFA", "BGS", "BM", "BS", "BSED", "MULTBA", "MULTBM", "MULTBS", "MULTSBA3", "BA/BS")
mast <- c("MA", "MAC", "MAS", "MAT", "MBA", "MFA", "MM", "MPA", "MPH", "MS", "MSED",
          "MST", "MSTCH", "MULTMM", "MULTMS", "PC")
doc <- c("AUD", "DNP", "DPT", "EDD", "EDS", "JD", "PHD")

cbus <- c("ACCY", "CBUS", "FINA", "MGMT", "MKTG", "OMIS")
cedu <- c("CAHE", "CEDU", "ETRA", "KNPE", "LEPF", "LTCY", "TLRN")
ceet <- c("CEET", "ELE", "ISYE", "MEE", "TECH")
chhs <- c("AHP", "CHHS", "FCNS", "HLTH", "IHP", "NURS")
clas <- c("ANTH", "BIOS", "CHEM", "CLAS", "COMS", "CSCI", "EAE", "ECON", "ENVS", "FL",
          "HIST", "MATH", "NNGO", "PHIL", "PHYS", "POLS", "PSPA", "PSYC", "SOCI", "SPGA",
          "WOMS")
cvpa <- c("ART", "CVPA", "MUSC", "THEA")
law  <- c("LAW")
#Degree Information from Old Data Books
temp1a <- old.deg %>%
  select(-c(X2015, Major)) %>%
  rename("Department" = "Dept") %>%
  mutate(Department = case_when(Department == "STAT" ~ "MATH",
                                Department == "GEOG" ~ "EAE",
                                Department == "GEOL" ~ "EAE",
                                Department == "FACS" ~ "FCNS",
                                Department == "NGOLD" ~ "NNGO",
                                Department == "FLWC" ~ "FL",
                                Department == "FNCS" ~ "FCNS",
                                is.na(Department) ~ "NIU",
                                TRUE ~ Department)) %>%
  pivot_longer(cols = starts_with("FY"), names_to = "Acadyr", values_to = "Count") %>%
  group_by(Department, Degree, Acadyr) %>%
  mutate(Count = sum(Count)) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(id_cols = c(Department, Degree), names_from = Acadyr, values_from = Count)

#Degree Information from Tableau
degrees <- deg.in %>%
  rename("Count" = "Count.of.Sheet1",
         "Acadyr" = "Fiscal.Year") %>%
  select(Department, Degree, Acadyr, Count) %>%
  mutate(Degree2 = case_when(Degree %in% bach ~ "Bachelor",
                             Degree %in% mast ~ "Masters",
                             Degree %in% doc ~ "Doctorate", 
                             TRUE ~ "Other"),
         Degree = Degree2,
         Department = case_when(Department == "STAT" ~ "MATH",
                                Department == "GEOG" ~ "EAE",
                                Department == "GEOL" ~ "EAE",
                                Department == "FACS" ~ "FCNS",
                                Department == "NGOLD" ~ "NNGO",
                                Department == "FLWC" ~ "FL",
                                Department == "FNCS" ~ "FCNS",
                                is.na(Department) ~ "NIU",
                                TRUE ~ Department)) %>%
  select(-Degree2) %>%
  filter(Degree != "Other") %>%
  summarize(Count = sum(Count), .by = c(Department, Degree, Acadyr)) %>%
  pivot_wider(id_cols = c(Degree, Department), names_from = Acadyr, names_prefix = "FY",
              values_from = "Count")%>%
  full_join(., temp1a, by = c("Degree", "Department")) %>%
  arrange(Department, Degree) %>%
  relocate(FY2010:FY2014, .after = Department) %>%
  mutate(across(starts_with("FY"), ~replace_na(.x, 0)))%>%
  mutate(Degree = factor(Degree, levels = c("Bachelor", "Masters","Doctorate")),
         College = case_when(Department %in% cbus ~ "CBUS",
                             Department %in% cedu ~ "CEDU",
                             Department %in% ceet ~ "CEET",
                             Department %in% chhs ~ "CHHS",
                             Department %in% clas ~ "CLAS",
                             Department %in% cvpa ~ "CVPA",
                             Department %in% law ~ "LAW",
                             TRUE ~ "NIUD"))%>%
  select(College, Department, Degree, everything())

#Fall Major Counts#######
majors <- maj.in %>%
  filter(!is.na(Acadept)) %>%
  rename("Department" = "Acadept",
         "Degree" = "Degname",
         "Count" = "Count.of.OSIR") %>%
  select(-Major) %>%
  left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
  mutate(Degree2 = case_when(Degree %in% bach ~ "Bachelor",
                             Degree %in% mast ~ "Masters",
                             Degree %in% doc ~ "Doctorate", 
                             TRUE ~ "Other"),
         Degree = Degree2,
         Department = case_when(Department == "STAT" ~ "MATH",
                                Department == "GEOG" ~ "EAE",
                                Department == "GEOL" ~ "EAE",
                                Department == "FACS" ~ "FCNS",
                                Department == "NGOLD" ~ "NNGO",
                                Department == "FLWC" ~ "FL",
                                Department == "FNCS" ~ "FCNS",
                                is.na(Department) ~ "NIU",
                                TRUE ~ Department)) %>%
  select(-Degree2) %>%
  filter(Degree != "Other")     %>%
  summarize(Count = sum(Count), .by = c(Department, Degree, Acad.Yr)) %>%
  pivot_wider(id_cols = c(Degree, Department), names_from = Acad.Yr, names_prefix = "FY",
              values_from = "Count") %>%
  mutate( College = case_when(Department %in% cbus ~ "CBUS",
                              Department %in% cedu ~ "CEDU",
                              Department %in% ceet ~ "CEET",
                              Department %in% chhs ~ "CHHS",
                              Department %in% clas ~ "CLAS",
                              Department %in% cvpa ~ "CVPA",
                              Department %in% law ~ "LAW",
                              TRUE ~ "NIUD"))%>%
  rev(.) %>%
  select("College", "Department", "Degree", everything())
# Save Final Dataframes

save(core, degrees, majors, file = "./Build/Output/Tableau.RData")