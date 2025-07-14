rm(list = ls())

library(tidyverse)

#Load Data
AEA.degree <- read.csv(file = "./Data/AEA Degree Awarded.csv", header = TRUE, as.is = TRUE)
NIU.degree <- read.csv(file = "./Data/Tableau NIU Degree.csv", header = TRUE, as.is = TRUE)


#Degrees Awarded

niu.degree <- NIU.degree %>%
  filter(Degree != "NIU") %>%
  mutate(Degree2 = case_when(substr(Degree, 1, 1) == "B" ~ "BA",
                             TRUE ~ Degree)) %>%
  group_by(Degree2, Academic_Year) %>%
  summarise(count = sum(Count)) %>%
  ungroup() %>%
  mutate(N = 1,
         Institute_Type = "NIU") %>%
  rename("Degree" = "Degree2",
         "Count" = "count")

degree <- AEA.degree %>%
  bind_rows(niu.degree) %>%
  mutate(avg_degree = Count / N)



#Visualizations

degree2 <- degree %>%
  filter(Degree != "CLAS",
         Degree != "NIUD")

ggplot(degree2) +
  geom_line(aes(x = Academic_Year, y = avg_degree, color = Degree, 
                linetype = factor(Institute_Type, levels = c("NIU", "PhD")), group = interaction(Institute_Type, Degree)), 
            linewidth = 1.25,
            data=filter(degree2, Institute_Type == "PhD" | Institute_Type == "NIU"))+ 
  labs(title = "Degrees Awarded by PhD Granting Institutions & NIU",
       y = "Average Degrees / NIU Total",
       x = "Academic Year",
       caption = "Data from AEA and NIU Tablaeu",
       linetype = "Institution Type") +
  theme_bw()

ggplot(degree2) +
  geom_line(aes(x = Academic_Year, y = avg_degree, color = Degree, 
                linetype = factor(Institute_Type, levels = c("NIU", "MA")), group = interaction(Institute_Type, Degree)), 
            linewidth = 1.25,
            data=filter(degree2, Institute_Type == "MA" | Institute_Type == "NIU"))+ 
  labs(title = "Degrees Awarded by MA Only Institutions & NIU",
       y = "Average Degrees / NIU Total",
       x = "Academic Year",
       caption = "Data from AEA and NIU Tablaeu",
       linetype = "Institution Type") +
  theme_bw()

ggplot(degree) +
  geom_line(aes(x = Academic_Year, y = avg_degree, color = Degree, 
                linetype = factor(Institute_Type, levels = c("NIU", "BA")), group = interaction(Institute_Type, Degree)), 
            linewidth = 1.25,
            data=filter(degree2, Institute_Type == "BA" | Institute_Type == "NIU"))+ 
  labs(title = "Degrees Awarded by Bacholor Only Institutions & NIU",
       y = "Average Degrees / NIU Total",
       x = "Academic Year",
       caption = "Data from AEA and NIU Tablaeu",
       linetype = "Institution Type") +
  theme_bw()





#In terms of percentage changes in degrees awarded at NIU

year <- NIU.degree %>%
  filter(Degree != "BS",
         Degree != "BSFE" ,
         Degree != "MA",
         Degree != "PhD") %>%
  
  select(Academic_Year)


niu2 <- NIU.degree %>%
  mutate(Class = case_when(substr(Degree, 1, 1) == "B" ~ "Dept",
                             Degree == "MA" ~ "Dept",
                             Degree == "PhD" ~ "Dept",
                             TRUE ~ Degree)) %>%
  group_by(Class, Academic_Year) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  reframe(delta.count = Count/Count[1], .by = Class) %>%
  bind_cols(year) %>%
  pivot_wider(id_cols = Academic_Year, names_from = Class, values_from = delta.count ) %>%
  rename("NIUE" = "NIU") %>%
  mutate(growth_ad = Dept/NIUE,
         growth_da = Dept/NIUD,
         growth_clas = Dept/CLAS) %>%
  filter(!is.na(Dept)) %>%
  pivot_longer(cols = -"Academic_Year", names_to = "Level", values_to = "Rate")



ggplot(filter(niu2, Level == "growth_ad")) +
  geom_hline(aes(yintercept = 1.0), linewidth = 2) +
  #geom_hline(aes(yintercept = 0), linewidth = 1, linetype = "dashed") +
  #geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -0.55, ymax = max(Rate)+.05), alpha = 0.025)+
  #geom_rect(aes(xmin = 6.5, xmax = 7.5, ymin = -0.55, ymax = max(Rate)+.05), fill = "blue", alpha = 0.025)+
  geom_line(aes(x = Academic_Year, y = Rate, group = 1), color = "red", linewidth = 1.25)+
  labs(title = "Ratio of Changes in NIU Admission and Department Degrees",
       y = "Ratio",
       x = "Academic Year",
       caption = "Data from NIU; shaded area denotes divergent growth") +
  theme_bw()

ggplot(filter(niu2, Level == "growth_da")) +
  geom_hline(aes(yintercept = 1.0), linewidth = 2) +
  #geom_hline(aes(yintercept = 0), linewidth = 1, linetype = "dashed") +
  #geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = -0.55, ymax = max(Rate)+.05), alpha = 0.025)+
  #geom_rect(aes(xmin = 6.5, xmax = 7.5, ymin = -0.55, ymax = max(Rate)+.05), fill = "blue", alpha = 0.025)+
  #geom_rect(aes(xmin = 8.5, xmax = 9.5, ymin = -0.55, ymax = max(Rate)+.05), fill = "blue", alpha = 0.025)+
  geom_line(aes(x = Academic_Year, y = Rate, group = 1), color = "red", linewidth = 1.25)+
  labs(title = "Ratio of Changes in NIU Degrees and Department Degrees",
       y = "Ratio",
       x = "Academic Year",
       caption = "Data from NIU; shaded area denotes divergent growth") +
  theme_bw()

ggplot(filter(niu2, Level == "growth_clas")) +
  geom_hline(aes(yintercept = 1.0), linewidth = 2) +
  #geom_hline(aes(yintercept = 0), linewidth = 1, linetype = "dashed") +
  #geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = -0.55, ymax = max(Rate)+.05), alpha = 0.025)+
  #geom_rect(aes(xmin = 6.5, xmax = 7.5, ymin = -0.55, ymax = max(Rate)+.05), fill = "blue", alpha = 0.025)+
  #geom_rect(aes(xmin = 8.5, xmax = 9.5, ymin = -0.55, ymax = max(Rate)+.05), fill = "blue", alpha = 0.025)+
  geom_line(aes(x = Academic_Year, y = Rate, group = 1), color = "red", linewidth = 1.25)+
  labs(title = "Ratio of Changes in CLAS Degrees and Department Degrees",
       y = "Ratio",
       x = "Academic Year",
       caption = "Data from NIU; shaded area denotes divergent growth") +
  theme_bw()

ggplot(filter(niu2, !grepl("growth|NIUE", Level))) +
  geom_hline(aes(yintercept = 1), linewidth = 1) +
  geom_line(aes(x = Academic_Year, y = Rate, group = Level, color = Level), linewidth = 1.1) +
  labs(title = "Number of Degrees Awarded Normalized to 2015",
       y = "Ratio",
       x = "Academic Year",
       caption = "Data from NIU") +
  theme_bw()



