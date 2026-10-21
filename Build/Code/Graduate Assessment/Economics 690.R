#PhD program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

comp <- read_xlsx("./Data/MAPhD Assessment.xlsx", sheet = "Class Score")

temp <- comp %>%
  filter(Acad.Yr == "2024-2025") %>%
  filter(Degree == "P") %>%
  mutate(N = 1,
         Score = factor(as.character(Score), level = c("5", "4", "3", "2", "1", "0"))) %>%
  group_by(Course) %>%
  mutate(Count = sum(N)*.10) %>%
  ungroup()

ggplot(temp, aes(x = Course, y = N)) +
  geom_bar(aes(fill = Score, y = N, x = as.character(Course)), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_hline(aes(yintercept = Count), linewidth = 2) +
  facet_grid(~Acad.Yr)  +
  scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132" , "2" = "#ff7b8c", 
                               "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#6EAEA1") ,   drop = FALSE)  +
  labs(title = "Evaluation Score: Economics 690",
       y = "Students",
       x = "Economics 690",
       caption = "") +
  
  theme_cowplot(12)
ggsave("./Graphics/PhD_SLO2_E690.png")  