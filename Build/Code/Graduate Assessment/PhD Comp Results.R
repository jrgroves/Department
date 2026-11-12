#PhD program annual assessment visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

comp <- read_xlsx("./Data/MA PhD Assessment.xlsx", sheet = "Comps")

years <- c("AY 2024-2025", "AY 2025-2026")

temp <- comp %>%
  filter(AcadYr %in% years) %>%
  select(-c(Student, Test.ID, Date)) %>%
  mutate(N = 1,
         Result = factor(Result, level = c("Fail", "Fail (MA pass)", "Marginal Pass", "Pass", "High Pass")),
         Attempt = factor(as.character(Attempt), level = c("1", "2", "3"))) %>%
  arrange(Test, AcadYr, Attempt) %>%
  mutate(Result2 = case_when(Result == "Pass" | Result == "High Pass" | Result == "Marginal Pass" ~ 1,
                             TRUE ~ 0)) %>%
  group_by(Test, AcadYr, Attempt) %>%
   mutate(Count = n(),
          Pass = sum(Result2)) %>%
  ungroup() %>%
  mutate(Pass.Rate = Pass / Count)

ggplot(filter(temp, Test == "Microeconomic Theory (PhD)"),  aes(y = N, x = Attempt)) +
  geom_bar(aes(fill = fct_rev(Result), y = N, x = Attempt), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_bar_text(aes(label = sprintf("%.2f", Pass.Rate)), 
                color = "white", 
                vjust = -1, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(5, "pt"),
                outside = TRUE)  +
  facet_grid(~AcadYr)  +
  scale_fill_manual(values = c("Fail" = "#9C0135", "Fail (MA pass)" = "#CD809A", 
                               "Marginal Pass" = "#98FB98", "Pass" = "#A0D6B4",
                               "High Pass" = "#163F37"),
                    drop = FALSE) +
  labs(title = "Comp Exam Results: Microeconomics",
       y = "Count",
       x = "Microeconomic Theory Exam",
       caption = "Number is pass rate for that attempt.",
       fill = NULL) +
  theme_cowplot(12) +
  theme(legend.position = "bottom")

ggsave("./Graphics/PhD_SLO1b_Comp Exams.png")  

ggplot(filter(temp, Test == "Macroeconomic Theory (PhD)"),  aes(y = N, x = Attempt)) +
  geom_bar(aes(fill = fct_rev(Result), y = N, x = Attempt), position = "stack",  stat = "identity", show.legend = TRUE) +
  geom_bar_text(aes(label = sprintf("%.2f", Pass.Rate)), 
                color = "white", 
                vjust = -1, 
                size = 5 * ggplot2::.pt, 
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(0, "pt"),
                padding.y = grid::unit(5, "pt"),
                outside = TRUE)  +
  facet_grid(~AcadYr)  +
  scale_fill_manual(values = c("Fail" = "#9C0135", "Fail (MA pass)" = "#CD809A",
                               "Marginal Pass" = "#98FB98", "Pass" = "#A0D6B4",
                               "High Pass" = "#163F37"),
                    drop = FALSE) +
  labs(title = "Comp Exam Results: Macroeconomics",
       y = "Count",
       x = "Macroeconomic Theory Exam",
       caption = "Number is pass rate for that attempt.",
       fill = NULL) +
  theme_cowplot(12) +
  theme(legend.position = "bottom")

ggsave("./Graphics/PhD_SLO1c_Comp Exams.png")  