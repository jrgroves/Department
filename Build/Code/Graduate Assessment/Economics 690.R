#PhD program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

comp <- read_xlsx("./Data/MA PhD Assessment.xlsx", sheet = "Class")

years <- c("AY 2024-2025", "AY 2025-2026")

temp <- comp %>%
  filter(AcadYr %in% years) %>%
  filter(Degree == "P") %>%
  mutate(N = 1,
         Score2 = case_when(Score == 0 ~ 0,
                            Score > 0 & Score <= 1 ~ 0,
                            Score >= 1 & Score < 2 ~ 1,
                            Score >= 2 & Score < 3 ~ 2,
                            Score >= 3 & Score < 4 ~ 3,
                            Score >= 4 & Score < 5 ~ 4,
                            Score == 5 ~ 5,
                            TRUE ~ NA),
         Score2 = factor(as.character(Score2), level = c("5", "4", "3", "2", "1", "0")))
 

ggplot(temp, aes(x = Course, y = N)) +
  geom_bar(aes(fill = Score2, y = N, x = as.character(Course)), position = "stack",  stat = "identity", show.legend = TRUE) +
  #geom_hline(aes(yintercept = Count), linewidth = 2) +
  facet_grid(~AcadYr)  +
  scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132" , "2" = "#ff7b8c", 
                               "3" = "#98FB98", "4" = "#A0D6B4", "5" = "#163F37") ,   drop = FALSE)  +
  labs(title = "Course Evaluation Score",
       y = "Students",
       x = "Course",
       caption = "",
       fill = "Score") +
  theme_cowplot(12) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
        plot.margin = margin(10, 10, 10, 10))


ggsave("./Graphics/PhD_SLO2_E690.png")  