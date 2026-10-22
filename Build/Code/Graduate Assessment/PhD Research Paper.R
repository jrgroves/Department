#BA BS program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

cap <- read_xlsx("./Data/MA PhD Assessment.xlsx", sheet = "Research") %>%
  filter(Term != 2202) %>%
  mutate(AcadYr = case_when(Term == 2208 | Term == 2212 ~ "AY 2020-2021",
                            Term == 2218 | Term == 2222 ~ "AY 2021-2022",
                            Term == 2228 | Term == 2232 ~ "AY 2022-2023",
                            Term == 2238 | Term == 2242 ~ "AY 2023-2024",
                            Term == 2248 | Term == 2252 ~ "AY 2024-2025",
                            Term == 2258 | Term == 2262 ~ "AY 2025-2026",
                            Term == 2268 | Term == 2272 ~ "AY 2026-2027"))

years <- c("AY 2024-2025", "AY 2025-2026")

temp1 <- cap %>% 
  filter(Degree == "P") %>%
  summarize(across(Theory:Writing , mean), .by = c(AcadYr)) %>%
  pivot_longer(cols = -c(AcadYr), names_to = "Criteria", values_to = "AY_Average") 

temp <- cap %>%
  filter(Degree == "P") %>%
  select(-c(Advisor, Date, Degree, Term)) %>%
  pivot_longer(cols = -c(AcadYr), names_to = "Criteria", values_to = "Score") %>%
  mutate(Score = case_when(Score > 4 ~ 5,
                           Score > 3 & Score < 5 ~ 4,
                           Score > 2 & Score < 4 ~ 3,
                           Score > 1 & Score < 3 ~ 2,
                           Score < 1 ~ 1,
                           TRUE ~ Score),
         N = 1,
         Score = factor(as.character(Score), levels = c("5", "4", "3", "2", "1")),
         Criteria = factor(Criteria, levels = c("Theory", "Empirical", "Thesis", "Organization", "Writing"))) %>%
  group_by(AcadYr) %>%
    mutate(Count = (sum(N)/5)*.10) %>%
  ungroup()  %>%
  left_join(., temp1, by = c("AcadYr", "Criteria")) %>%
  filter(AcadYr %in% years)

ggplot(temp,  aes(y = N, x = Criteria)) +
  geom_bar(aes(fill = Score, y = N, x = Criteria), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_hline(aes(yintercept = Count), linewidth = 2) +
  geom_bar_text(aes(label = sprintf("%.1f", AY_Average)), 
                color = "black", 
                vjust = -15, 
                size = 4 * ggplot2::.pt, 
                min.size = 4 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(25, "pt"),
                outside = TRUE)  +
  facet_grid(~AcadYr)  +
  scale_fill_manual(values = c("1" = "#9C0135", "2" = "#C70132", "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "Ph.D. Research Paper Evaluation",
       y = "Count",
       x = "Criteria",
       caption = "Number is criteria average. Above line represents 90%") +
  theme_cowplot(12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("./Graphics/PhD_SLO3_Research.png")  