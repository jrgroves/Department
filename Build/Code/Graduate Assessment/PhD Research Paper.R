#BA BS program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

cap <- read_xlsx("./Data/MAPhD Assessment.xlsx", sheet = "PhD_Research")

temp1 <- cap %>% 
  group_by(Acad.Yr) %>%
  summarize(across(Theory:Writing , mean)) %>%
  ungroup() %>%
  pivot_longer(cols = -c(Acad.Yr), names_to = "Criteria", values_to = "AY_Average")

temp <- cap %>% 
  select(-Student) %>%
  pivot_longer(cols = -c(Acad.Yr), names_to = "Criteria", values_to = "Score") %>%
  mutate(Score = case_when(Score > 4 ~ 5,
                           Score > 3 & Score < 5 ~ 4,
                           Score > 2 & Score < 4 ~ 3,
                           Score > 1 & Score < 3 ~ 2,
                           Score < 1 ~ 1,
                           TRUE ~ Score),
         N = 1,
         Score = factor(as.character(Score), levels = c("5", "4", "3", "2", "1")),
         Criteria = factor(Criteria, levels = c("Theory", "Empirical", "Thesis", "Organization", "Writing"))) %>%
  group_by(Acad.Yr) %>%
  mutate(Count = (sum(N)/5)*.10) %>%
  ungroup() %>%
 left_join(., temp1, by = c("Acad.Yr", "Criteria"))

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
  labs(title = "Ph.D. Research Paper Evaluation",
       y = "Count",
       x = "Criteria",
       caption = "Number is criteria average. Above line represents 90%") +
  theme_cowplot(12)

ggsave("./Graphics/PhD_SLO3_Research.png")  