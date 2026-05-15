#BA BS program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

cap <- read_xlsx("./Data/BSBA Assessment.xlsx", sheet = "Capstone_Eval")

temp1 <- cap %>% 
  group_by(Acad.Yr) %>%
  summarize(across(Research:Writing , mean)) %>%
  ungroup()

temp1a <- cap %>% 
  select(Acad.Yr, Semester) %>%
  left_join(., temp1, by = "Acad.Yr") %>%
  distinct() %>%
  pivot_longer(cols = -c(Acad.Yr, Semester), names_to = "Criteria", values_to = "AY_Average") %>%
  select(-Acad.Yr) %>%
  mutate(AY_Average = as.character(format(round(AY_Average, 2), nsmall = 2)))

temp2 <- cap %>% 
  group_by(Semester) %>%
  summarize(across(Research:Writing , mean)) %>%
  ungroup() %>%
  pivot_longer(cols = -Semester, names_to = "Criteria", values_to = "Average") %>%
  mutate(Average = as.character(format(round(Average, 2), nsmall = 2)))

core <- cap %>%
  mutate(
         Research = case_when(Research > 4 ~ 5,
                       Research > 3 & Research < 5 ~ 4,
                       Research > 2 & Research < 4 ~ 3,
                       Research > 1 & Research < 3 ~ 2,
                       Research < 1 ~ 1,
                       TRUE ~ Research),
         Analysis = case_when(Analysis > 4 ~ 5,
                       Analysis > 3 & Analysis < 5 ~ 4,
                       Analysis > 2 & Analysis < 4 ~ 3,
                       Analysis > 1 & Analysis < 3 ~ 2,
                       Analysis < 1 ~ 1,
                       TRUE ~ Analysis),
         Thesis = case_when(Thesis > 4 ~ 5,
                       Thesis > 3 & Thesis < 5 ~ 4,
                       Thesis > 2 & Thesis < 4 ~ 3,
                       Thesis > 1 & Thesis < 3 ~ 2,
                       Thesis < 1 ~ 1,
                       TRUE ~ Thesis),
         Writing = case_when(Writing > 4 ~ 5,
                       Writing > 3 & Writing < 5 ~ 4,
                       Writing > 2 & Writing < 4 ~ 3,
                       Writing > 1 & Writing < 3 ~ 2,
                       Writing < 1 ~ 1,
                       TRUE ~ Writing)) %>%
  pivot_longer(cols = c(Research, Analysis, Thesis, Organization, Writing),
               names_to = "Criteria", values_to = "Score") %>%
  mutate(Score = factor(as.character(Score), levels = c("5", "4", "3", "2", "1")),
         Criteria = factor(Criteria, levels = c("Research", "Analysis", "Thesis", "Organization", "Writing")),
         Acad.Yr = factor(Acad.Yr, levels = c("AY2024-2025", "AY2023-2024")),
         N = 1) %>%
  left_join(., temp2, by = c("Criteria", "Semester")) %>%
  left_join(., temp1a, by = c("Semester", "Criteria"))

#SLO 1: Thesis Category  

temp <- core %>%
  filter(Criteria == "Thesis") %>%
  group_by(Acad.Yr) %>%
  mutate(Count = (sum(N))*.10) %>%
  ungroup()

ggplot(temp,  aes(y = N, x = Criteria)) +
  geom_bar(aes(fill = Score, y = N, x = Criteria), position = "stack",  stat = "identity", show.legend = TRUE) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  geom_hline(aes(yintercept = Count), linewidth = 2) +
  geom_bar_text(aes(label = AY_Average), 
                color = "black", 
                vjust = -20, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(35, "pt"),
                outside = TRUE)  +
  facet_grid(~Acad.Yr)  +
  scale_fill_manual(values = c("1" = "#9C0135", "2" = "#C70132", "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "SLO 1: Capstone Evaluation",
       y = "Count",
       x = "Criteria: Topic and Thesis Development",
       caption = "Number is criteria average. Above line represents 90%") +
  theme_cowplot(12)
  
ggsave("./Graphics/SLO1_Capstone.png")  

temp <- core %>%
  filter(Criteria == "Research" | Criteria == "Analysis") %>%
  group_by(Acad.Yr) %>%
  mutate(Count = (sum(N)/2)*.10) %>%
  ungroup()

ggplot(temp,  aes(y = N, x = Criteria)) +
  geom_bar(aes(fill = Score, y = N, x = Criteria), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_hline(aes(yintercept = Count), linewidth = 2) +
  geom_bar_text(aes(label = AY_Average), 
                color = "black", 
                vjust = -20, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(35, "pt"),
                outside = TRUE)  +
  facet_grid(~Acad.Yr)  +
  scale_fill_manual(values = c("1" = "#9C0135", "2" = "#C70132", "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "SLO 2: Capstone Evaluation",
       y = "Count",
       x = "Criteria",
       caption = "Number is criteria average. Above line represents 90%") +
  theme_cowplot(12)

ggsave("./Graphics/SLO2_Capstone.png")  

temp <- core %>%
  filter(Criteria == "Organization" | Criteria == "Writing") %>%
  group_by(Acad.Yr) %>%
  mutate(Count = (sum(N)/2)*.10) %>%
  ungroup()

ggplot(temp,  aes(y = N, x = Criteria)) +
  geom_bar(aes(fill = Score, y = N, x = Criteria), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_hline(aes(yintercept = Count), linewidth = 2) +
  geom_bar_text(aes(label = AY_Average), 
                color = "black", 
                vjust = -20, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(35, "pt"),
                outside = TRUE)  +
  facet_grid(~Acad.Yr)  +
  scale_fill_manual(values = c("1" = "#9C0135", "2" = "#C70132", "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "SLO 3: Capstone Evaluation",
       y = "Count",
       x = "Criteria",
       caption = "Number is criteria average. Above line represents 90%") +
  theme_cowplot(12)

ggsave("./Graphics/SLO3_Capstone.png")  