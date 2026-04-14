#PhD program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

comp <- read_xlsx("./Data/MAPhD Assessment.xlsx", sheet = "Comps")

temp <- comp %>%
  filter(Acad.Yr == "2024-2025") %>%
  mutate(N = 1,
         Result = factor(Result, level = c("Fail", "Marginal Pass", "Pass", "High Pass")),
         Attempt = factor(as.character(Attempt), level = c("1", "2", "3"))) %>%
  group_by(Exam, Semester, Attempt) %>%
  mutate(Count = sum(N)) %>%
  ungroup()

ggplot(filter(temp, Exam == "Micro"),  aes(y = N, x = Attempt)) +
  geom_bar(aes(fill = Result, y = N, x = Attempt), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_bar_text(aes(label = Count), 
                color = "white", 
                vjust = -1, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(5, "pt"),
                outside = TRUE)  +
  facet_grid(~Semester)  +
  scale_fill_manual(values = c("Fail" = "#9C0135", "Marginal Pass" = "#98FB98", "Pass" = "#A0D6B4", "High Pass" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "Comp Exam Results: Microeconomics",
       y = "Count",
       x = "Microeconomic Theory Exam",
       caption = "Number is count of exams.") +

  theme_cowplot(12) +
  theme(legend.position = "bottom")

ggsave("./Graphics/PhD_SLO1b_Comp Exams.png")  

ggplot(filter(temp, Exam == "Macro"),  aes(y = N, x = Attempt)) +
  geom_bar(aes(fill = Result, y = N, x = Attempt), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_bar_text(aes(label = Count), 
                color = "white", 
                vjust = -1, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(5, "pt"),
                outside = TRUE)  +
  facet_grid(~Semester)  +
  scale_fill_manual(values = c("Fail" = "#9C0135", "Marginal Pass" = "#98FB98", "Pass" = "#A0D6B4", "High Pass" = "#6EAEA1"),
                    drop = FALSE) +
  labs(title = "Comp Exam Results: Macroeconomics",
       y = "Count",
       x = "Macroeconomic Theory Exam",
       caption = "Number is count of exams.") +
  
  theme_cowplot(12) +
  theme(legend.position = "bottom")

ggsave("./Graphics/PhD_SLO1c_Comp Exams.png")  