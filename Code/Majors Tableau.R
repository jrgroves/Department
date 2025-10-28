rm(list=ls())

library(tidyverse)

temp1 <- read.csv(file = "./Data/databook Majors.csv")
temp2 <- read.csv(file = "./Data/Tableau NIU Fall Majors.csv")
load(file = "./Data/Annual Instructional.RData")


temp1 <- temp1 %>%
  filter(DEPART.MENT == "ECON")

working <- temp2 %>%
  filter(Acadept == "ECON") %>%
  rename("Count" = "Count.of.OSIR") %>%
  pivot_wider(id_cols = c(Acadept, Degname, Major), names_from = Acad.Yr, names_prefix = "F",
              values_from = "Count") %>%
  mutate(DEGREE = case_when(Degname == "BS" ~ "Bachelor",
                            Degname == "BA" ~ "Bachelor",
                            Degname == "BA/BS" ~ "Bachelor",
                            TRUE ~ Degname),
         MAJOR = str_split_i(Major, " \\(", 1)) %>%
  select(-c("Degname", "Major")) %>%
  replace_na(list(F2025 = 0, F2024 = 0, F2023 = 0, F2022 = 0, F2021 = 0, F2020 = 0,
                  F2019 = 0, F2018 = 0, F2017 = 0, F2016 = 0)) %>%
  arrange(Acadept, MAJOR, DEGREE) %>%
  group_by(Acadept, DEGREE) %>%
  summarize(across(F2025:F2016, sum)) %>%
  ungroup() %>%
  full_join(., temp1, by = c("DEGREE", "Acadept" = "DEPART.MENT")) %>%
  arrange(Acadept, DEGREE) %>%
  select(-c(COLLEGE, Major)) %>%
  mutate(across(F2025:F2015, as.numeric))%>%
  pivot_longer(cols = starts_with("F"), names_to = "Acadyr", values_to = "Count") %>%
  mutate(Acadyr = as.numeric(str_replace(Acadyr, "F", ""))) %>%
  arrange(DEGREE, Acadyr)

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
  
working2 <- working %>%
  filter(DEGREE == "Bachelor") %>%
  left_join(., temp.level, by = c("Acadyr")) %>%
  left_join(., temp.ins, by = c("Acadyr",  "Level")) %>%
  arrange(Acadyr, Level) %>%
  replace_na(list(sections = 0, level_sec = 0, level_instr = 0, term_sec = 0, term_instr = 0)) 

ggplot(working) +
  geom_line(aes(x = Acadyr, y = Count, group = DEGREE, color = DEGREE), linewidth = 1.5) +
  geom_vline(xintercept = c(2010, 2011, 2012, 2012.5, 2014, 2014.5, 2015, 2015.5, 2017, 2023, 2024))+
  annotate(
    "label",
    x = c(2010, 2011, 2012, 2012.5, 2014, 2014.5, 2015, 2015.5, 2017, 2023, 2024),
    y = max(working$Count) + 12, # Set y to the maximum of your data's y-values
    label = c("12", "13", "14", "13", "12", "10", "10", "9", "10", "11", "9"),
    vjust = 1.2,   # Adjust vertical justification to place the label just above the top edge
    hjust = 0.5,   # Center horizontally on the line
    fill = "white", # Optional: adds a background color for the "textbox" effect
    color = "black"
  ) +
  labs(y = "Majors",
       x = "Academic Year (Fall)",
       color = "Degree",
       title = "Total Majors and Faculty Count") +
  scale_x_continuous(breaks = working$Acadyr) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(filter(working, DEGREE == "Bachelor")) +
  geom_line(aes(x = Acadyr, y = Count, group = DEGREE), linewidth = 1.5, color = "red") +
  labs(y = "Majors",
       x = "Academic Year (Fall)",
       color = "Degree",
       title = "Total Undergraduate Majos") +
  scale_x_continuous(breaks = working$Acadyr) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

temp <- working2 %>%
  filter(Level == "Level_200") %>%
  select(Acadyr, term_sec, term_instr, Count) %>%
  distinct()

ggplot(temp) + 
  geom_line(aes(x = Acadyr, y = Count), color = "red", linewidth = 1.5) +
  geom_vline(xintercept = temp$Acadyr) +
  annotate("label",
    x = temp$Acadyr,
    y = max(temp$Count) + 12, # Set y to the maximum of your data's y-values
    label = temp$term_instr,
    vjust = 1.2,   # Adjust vertical justification to place the label just above the top edge
    hjust = 0.5,   # Center horizontally on the line
    fill = "white", # Optional: adds a background color for the "textbox" effect
    color = "black"
  ) +
  labs(y = "Majors",
       x = "Academic Year (Fall)",
       color = "Degree",
       title = "Total Majors and (Fall) Instructors") +
  scale_x_continuous(breaks = temp$Acadyr) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

temp <- working2 %>%
  filter(Level == "Level_300") 

ggplot(temp) + 
  geom_line(aes(x = Acadyr, y = Count, group = DEGREE), color = "red", linewidth = 1.5) +
  geom_vline(xintercept = temp$Acadyr) +
  annotate("label",
           x = temp$Acadyr,
           y = max(temp$Count) + 12, # Set y to the maximum of your data's y-values
           label = temp$sections,
           vjust = 1.2,   # Adjust vertical justification to place the label just above the top edge
           hjust = 0.5,   # Center horizontally on the line
           fill = "white", # Optional: adds a background color for the "textbox" effect
           color = "black"
  ) +
  labs(y = "Majors",
       x = "Academic Year (Fall)",
       color = "Degree",
       title = "Total Majors and 300 Level Sections") +
  scale_x_continuous(breaks = temp$Acadyr) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

save(working, working2, file = "./Data/Majors.RData")