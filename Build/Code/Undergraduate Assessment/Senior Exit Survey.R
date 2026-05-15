##BA BS program annual assessment visualizations - Exit Survey

rm(list=ls())

library(tidyverse)
library(readxl)
library(cowplot)
library(ggfittext)

cap <- read_xlsx("./Data/BSBA Assessment.xlsx", sheet = "Senior_Exit")

temp1 <- cap %>% 
  group_by(Acad.Yr) %>%
  summarize(across(Q1:Q8 , \(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

temp1a <- cap %>% 
  select(Acad.Yr, Semester) %>%
  left_join(., temp1, by = "Acad.Yr") %>%
  distinct() %>%
  pivot_longer(cols = -c(Acad.Yr, Semester), names_to = "Question", values_to = "AY_Average") %>%
  select(-Acad.Yr) %>%
  mutate(AY_Average = as.character(format(round(AY_Average, 2), nsmall = 2)))

temp2 <- cap %>% 
  group_by(Semester) %>%
  summarize(across(Q1:Q8 , \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(cols = -Semester, names_to = "Question", values_to = "Average") %>%
  mutate(Average = as.character(format(round(Average, 2), nsmall = 2)))


core <- cap %>%
  pivot_longer(cols = -c(Acad.Yr, Semester, Degree),
               names_to = "Question", values_to = "Score") %>%
  mutate(Score = factor(as.character(Score), levels = c("3", "2", "1", "0")),
         Acad.Yr = factor(Acad.Yr, levels = c("AY2024-2025", "AY2023-2024")),
         N = 1) %>%
  left_join(., temp2, by = c("Question", "Semester")) %>%
  left_join(., temp1a, by = c("Semester", "Question")) 

rm(temp1, temp1a, temp2, cap)

#SLO 1 - Questions 2 and 3

  temp <- core %>%
    filter(Question == "Q2" | Question == "Q3") %>%
    group_by(Acad.Yr) %>%
      mutate(Count = (sum(N)/2)*.10) %>%
    ungroup()
  B<-unique(temp$Count)
  
  ggplot(temp,   aes(y = N, x = Question)) +
    geom_bar(aes(fill = Score, y = N, x = Question), position = "stack",  stat = "identity", show.legend = TRUE) +
    scale_x_discrete(labels = c("Question 2", "Question 3")) +
    geom_hline(aes(yintercept = Count), linewidth = 2) +
    geom_bar_text(aes(label = AY_Average), 
                  color = "black", 
                  vjust = -20, 
                  size = 5 * ggplot2::.pt, 
                  min.size = 5 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(15, "pt"),
                  outside = TRUE)  +
    facet_grid(~Acad.Yr)   +
    scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132","2" = "#A0D6B4", "3" = "#6EAEA1"), na.value = "white",
                      drop = FALSE) +
    labs(title = "SLO 1: Senior Exit Survey - All Degrees",
         y = "Count",
         x = "Questions",
         caption = "Number is question average. Above line represents 90%") +

    theme_cowplot(12)
  ggsave("./Graphics/SLO1_Exit_1a.png")  
   
  temp <- core %>%
    filter(Question == "Q2" | Question == "Q3") %>%
    group_by(Acad.Yr, Degree) %>%
      mutate(Count = (sum(N)/2)*.10) %>%
    ungroup()
  
  ggplot(temp,aes(y = N, x = Question)) +
    geom_bar(aes(fill = Score, y = N, x = Question), position = "stack",  stat = "identity", show.legend = TRUE) +
    scale_x_discrete(labels = c("Question 2", "Question 3")) +
    geom_hline(aes(yintercept = Count), linewidth = 2) +
    geom_bar_text(aes(label = AY_Average), 
                  color = "black", 
                  vjust = -20, 
                  size = 5 * ggplot2::.pt, 
                  min.size = 5 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(15, "pt"),
                  outside = TRUE)  +
    facet_grid(~Acad.Yr+Degree)   +
    scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132","2" = "#A0D6B4", "3" = "#6EAEA1"), na.value = "white",
                      drop = FALSE) +
    labs(title = "SLO 1: Senior Exit Survey - By Degree",
         y = "Count",
         x = "Questions",
         caption = "Number is question average. Above line represents 90%") +
    theme_cowplot(12)
  ggsave("./Graphics/SLO1_Exit_1b.png")  
 
#SLO 2
  
  
  temp <- core %>%
    filter(Question == "Q4") %>%
    group_by(Acad.Yr, Degree) %>%
    mutate(Count = (sum(N))*.10) %>%
    ungroup()
  
  
  ggplot(temp,   aes(y = N, x = Question)) +
    geom_bar(aes(fill = Score, y = N, x = Question), position = "stack",  stat = "identity", show.legend = TRUE) +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    geom_hline(aes(yintercept = Count), linewidth = 2) +
    geom_bar_text(aes(label = AY_Average), 
                  color = "black", 
                  vjust = -20, 
                  size = 5 * ggplot2::.pt, 
                  min.size = 5 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(10, "pt"),
                  outside = TRUE)  +
    facet_grid(~Acad.Yr + Degree)   +
    scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132","2" = "#A0D6B4", "3" = "#6EAEA1"), na.value = "white",
                      drop = FALSE) +
    labs(title = "SLO 2: Senior Exit Survey",
         y = "Count",
         x = "Question 4: Would you be able to apply the tools you acquired in your economic courses to explain economic events?",
         caption = "Number is question average. Above line represents 90%") +
    
    theme_cowplot(12)
  ggsave("./Graphics/SLO2_Exit.png")  
#SLO 3
  temp <- core %>%
    filter(Question == "Q7") %>%
    group_by(Acad.Yr, Degree) %>%
    mutate(Count = (sum(N))*.10) %>%
    ungroup()
  
  
  ggplot(temp,   aes(y = N, x = Question)) +
    geom_bar(aes(fill = Score, y = N, x = Question), position = "stack",  stat = "identity", show.legend = TRUE) +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    geom_hline(aes(yintercept = Count), linewidth = 2) +
    geom_bar_text(aes(label = AY_Average), 
                  color = "black", 
                  vjust = -20, 
                  size = 5 * ggplot2::.pt, 
                  min.size = 5 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(10, "pt"),
                  outside = TRUE)  +
    facet_grid(~Acad.Yr + Degree)   +
    scale_fill_manual(values = c("0" = "#9C0135", "1" = "#C70132","2" = "#A0D6B4", "3" = "#6EAEA1"), na.value = "white",
                      drop = FALSE) +
    labs(title = "SLO 3: Senior Exit Survey",
         y = "Count",
         x = "Question 7: To what degree has your ability to write a research paper addressing an economic issue improved as a
result of your work in economics?",
         caption = "Number is question average. Above line represents 90%") +
    
    theme_cowplot(12)
  ggsave("./Graphics/SLO3_Exit.png")  