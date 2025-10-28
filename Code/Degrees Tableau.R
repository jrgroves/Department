rm(list=ls())

library(tidyverse)

temp1 <- read.csv(file = "./Data/databook Confered.csv")
temp2 <- read.csv(file = "./Data/Tableau NIU Degree Confired.csv")
load(file = "./Data/Annual Instructional.RData")


bach <- c("BA", "BFA", "BGS", "BM", "BS", "BSED", "MULTBA", "MULTBM", "MULTBS", "MULTSBA3")
mast <- c("MA", "MAC", "MAS", "MAT", "MBA", "MFA", "MM", "MPA", "MPH", "MS", "MSED",
          "MST", "MSTCH", "MULTMM", "MULTMS", "PC")
doc <- c("AUD", "DNP", "DPT", "EDD", "EDS", "JD", "PHD")

temp1 <- temp1 %>%
  select(-c(X2015, Major)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Acadyr", values_to = "Count") %>%
  group_by(Dept, Degree, Acadyr) %>%
  mutate(Count = sum(Count)) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(id_cols = c(Dept, Degree), names_from = Acadyr, values_from = Count)

working <- temp2 %>%
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
                                TRUE ~ Department))%>%
  select(-Degree2) %>%
  filter(Degree != "Other") %>%
  group_by(Department, Degree, Acadyr) %>%
    summarize(Count = sum(Count)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(Degree, Department), names_from = Acadyr, names_prefix = "X",
              values_from = "Count") %>%
  full_join(., temp1, by = c("Degree", "Department" = "Dept")) %>%
  arrange(Department, Degree) %>%
  mutate(across(X2025:X2014, ~replace_na(.x, 0))) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Acadyr", values_to = "Count") %>%
  mutate(year = as.numeric(substr(Acadyr, 2, 5)),
         Degree = factor(Degree, levels = c("Bachelor", "Masters","Doctorate"))) %>%
  arrange(year)

#Instructor Counts

  temp.ins <- core %>%
    select(-c(Catalog, enrollment, c.num, Semester)) %>%
    filter(position == "Instructor") %>%
    group_by(Term, Level) %>%
    mutate(level_sec = n(),
           level_instr = n_distinct(instr)) %>%
    ungroup() %>%
    group_by(Term) %>%
    mutate(term_sec = n(),
           term_instr = n_distinct(instr)) %>%
    ungroup()  %>%
    select(-c(Section, instr, position)) %>%
    distinct() %>%
    mutate(Term = as.character(Term),
           Acadyr = as.numeric(paste0("20", substr(Term,2,3))),
           Acadyr = case_when(substr(Term,4,4) == "8" ~ Acadyr,
                              substr(Term, 4,4) == "2" ~ Acadyr + .5,
                              TRUE ~ 0000)) %>%
    select(-c(Term)) %>%
    arrange(Acadyr, Level) %>%
    filter(Level != "Level_500",
           Level != "Level_700")
  
  temp.level <- core %>%
    select(-c(Catalog, enrollment, c.num, Semester, position, instr)) %>%
    group_by(Term, Level) %>%
    mutate(sections = n()) %>%
    ungroup() %>%
    mutate(Term = as.character(Term),
           Acadyr = as.numeric(paste0("20", substr(Term,2,3))),
           Acadyr = case_when(substr(Term,4,4) == "8" ~ Acadyr,
                              substr(Term, 4,4) == "2" ~ Acadyr + .5,
                              TRUE ~ 0000)) %>%
    select(-c(Term, Section)) %>%
    distinct() %>%
    arrange(Acadyr, Level) %>%
    filter(Level != "Level_500",
           Level != "Level_700")
  
working2d <- working %>%
  filter(Degree == "Bachelor",
         Department == "ECON" ) %>%
  rename("D_Count" = "Count") %>%
  left_join(., temp.level, by = c("year" = "Acadyr")) %>%
  left_join(., temp.ins, by = c("year" = "Acadyr",  "Level")) %>%
  arrange(Acadyr, Level) %>%
  replace_na(list(sections = 0, level_sec = 0, level_instr = 0, term_sec = 0, term_instr = 0)) 

workingd <- working %>%
  filter(Department == "ECON") %>%
  rename("D_Count" = "Count") %>%
  select(-Acadyr)

rm(temp1, core, temp.ins, temp.level, temp2, bach, doc, mast)
load(file = "./Data/Majors.RData")

working <- working %>%
  left_join(., workingd, by = c("Acadyr" = "year", "DEGREE" = "Degree"))

ggplot(workingd) +
  geom_line(aes(x = year, y = D_Count, group = Degree, color = Degree), linewidth = 1.5) +
  geom_vline(xintercept = c(2010, 2011, 2012, 2012.5, 2014, 2014.5, 2015, 2015.5, 2017, 2023, 2024)) +
  annotate(
    "label",
    x = c(2010, 2011, 2012, 2012.5, 2014, 2014.5, 2015, 2015.5, 2017, 2023, 2024),
    y = 40, # Set y to the maximum of your data's y-values
    label = c("12", "13", "14", "13", "12", "10", "10", "9", "10", "11", "9"),
    vjust = 1.2,   # Adjust vertical justification to place the label just above the top edge
    hjust = 0.5,   # Center horizontally on the line
    fill = "white", # Optional: adds a background color for the "textbox" effect
    color = "black"
  ) +
  labs(y = "Degrees Awarded",
       x = "Academic Year",
       color = "Degree",
       title = "Total Degree Awarded and Faculty Count") +
  scale_x_continuous(breaks = workingd$year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

temp <- workingd


ggplot(filter(working, DEGREE == "Bachelor")) +
  geom_line(aes(x = Acadyr, y = Count, group = DEGREE, color = "red"), linewidth = 1.5) +
  geom_line(aes(x = Acadyr, y = D_Count, group = DEGREE, color = "blue"), linewidth = 1.5) +
  labs(y = "Degrees Awarded",
       x = "Academic Year",
       color = "Degree",
       title = "Total Degree Awarded and Majors") +
  scale_x_continuous(breaks = working$Acadyr) +
  scale_color_manual(name = "", values = c("red" = "red", "blue" = "blue"), labels = c("Degrees", "Majors")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

