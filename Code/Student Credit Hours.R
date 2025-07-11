rm(list=ls())

library(tidyverse)
library(RColorBrewer)

sch <- read.csv(file="./Data/SCH_data.csv", header = TRUE, as.is = TRUE)

core <- sch %>%
  mutate(sem = str_to_sentence(substr(Semester, 1 ,nchar(Semester)-4)),
         year = as.numeric(substr(Semester, nchar(Semester)-3, nchar(Semester))),
         acyear = case_when(sem == "Spring" ~ (year - 1),
                            sem == "Fall" ~ (year),
                            sem == "Summer" ~ (year - 1),
                            TRUE ~ year),
         Level = Crsenum,
         hours = Cscredit) %>%
  filter(acyear > 2017) %>%
  select(Semester, sem, year, acyear, hours, Level)

core2 <- core %>%
  select(-Semester) %>%
  pivot_wider(id_cols = acyear, values_from = hours, names_from = Level, names_prefix = "ECON ",
              values_fn = sum) %>%
  pivot_longer(!acyear, names_to = "Level", values_to = "Hours")

core_ug1 <- core %>%
  filter(Level < 500)

core_ug1 <- core %>%
  filter(Level < 500) %>%
  select(-Semester) %>%
  pivot_wider(id_cols = acyear, values_from = hours, names_from = Level, names_prefix = "ECON ",
              values_fn = sum) %>%
  pivot_longer(!acyear, names_to = "Level", values_to = "Hours")

ggplot(core)+
  geom_bar(aes(fill = factor(Level), y = hours, x = sem), position = "stack",
           stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ acyear) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Academic Year") +
  ylab("Student Creedit Hours") +
  labs(caption = "Data from NIU Tableau")

ggplot(core_ug1)+
  geom_bar(aes(fill = factor(Level), y = hours, x = sem), position = "stack",
           stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ acyear) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Academic Year") +
  ylab("Student Creedit Hours") +
  labs(caption = "Data from NIU Tableau")

ggplot(core2) + 
  geom_line(aes(x = acyear, y = Hours, group = Level, color = Level), linewidth = 1.25) +
  scale_x_continuous(breaks=seq(2018, 2024 , 1))+
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Academic Year") +
  ylab("Student Creedit Hours") +
  labs(caption = "Data from NIU Tableau")

ggplot(core_ug2) + 
  geom_line(aes(x = acyear, y = Hours, group = Level, color = Level), linewidth = 1.25) +
  scale_x_continuous(breaks=seq(2018, 2024 , 1))+
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Academic Year") +
  ylab("Student Creedit Hours") +
  labs(caption = "Data from NIU Tableau")
  
  
