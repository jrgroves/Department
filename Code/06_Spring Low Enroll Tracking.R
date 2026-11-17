
#Jeremy Groves
#November 12, 2026
#Updated November 18, 2026: Added today's date to track daily changes and added
#                           code to include daily change in figure.

rm(list = ls())

library(tidyverse)

#Options and References
  
  ignore <- c(664, 665, 799, 699, 698, 494, 497, 498, 798, 397)
  bad.section <- c("PZ01")


#Get Data that was downloaded from Tableau

  e.data <- read.csv(file = "./Data/Spring Tab Enrollment Daily.csv", , fileEncoding = "UTF-16LE",
                   row.names = NULL, sep = "\t")

  e.data.2 <- e.data %>%
    mutate(Course = as.numeric(str_split_i(Class.and.Section, " ", 2)),
           Section = str_split_i(Class.and.Section, " ", 3),
           Modality = str_split_i(Mode.of.Instruction, ":", 1),
           Temp = case_when(Course == 403 | Course == 503 ~ 803,
                            Course == 442 | Course == 542 ~ 842,
                            Course == 450 | Course == 550 ~ 850,
                            Course == 460 | Course == 540 ~ 860,
                            Course == 470 | Course == 570 ~ 870,
                            Course == 489 | Course == 589 ~ 889,
                            Course == 490 | Course == 590 ~ 890,
                            Course == 491 | Course == 591 ~ 891,
                            TRUE ~ Course))%>%
    group_by(Temp) %>%
      mutate(Temp2 = max(Enrl.Tot)) %>%
    ungroup() %>%
    mutate(Enrl.Min = case_when(Temp < 299 ~ 20,
                                Temp > 299 & Temp < 499 ~ 15,
                                Temp > 499 & Temp < 599 ~ 8,
                                Temp > 599 & Temp < 799 ~ 5,
                                Temp > 800 & Enrl.Tot == Temp2 & Course < 500 ~ 15,
                                Temp > 800 & Enrl.Tot == Temp2 & Course >= 500 ~ 8,
                                Temp > 800 & Enrl.Tot != Temp2 & Course < 500 ~ 8,
                                Temp > 800 & Enrl.Tot != Temp2 & Course >= 500 ~ 15,
                                TRUE ~ NA)) %>%
    filter(!Course %in% ignore) %>%
    filter(!Section %in% bad.section) %>%
    select(Course, Section, Enrl.Min, Enrl.Tot, Temp) %>%
    mutate(Course = case_when(Temp == 850 ~ "450/550",
                              TRUE ~ as.character(Course)),
           Course = paste0("ECON ", Course, ".", Section)) %>%
    select(-c(Section)) %>%
    mutate(pct = case_when(Enrl.Tot / Enrl.Min <= 1 ~ Enrl.Tot/Enrl.Min,
                           TRUE ~ 1),
           Temp = case_when(Temp > 800 ~ Temp - 400,
                            TRUE ~ Temp),
           Date = Sys.Date()) %>%
    distinct(Course, .keep_all = TRUE) %>%
    arrange(Temp) %>%
    filter(Enrl.Tot > 0)

  
  load(file = "./Build/Output/Spring26.RData")
  spr.2026 <- spr.2026 %>%
    bind_rows(., e.data.2)
  save(spr.2026, file = "./Build/Output/Spring26.Rdata")
  
  temp <- spr.2026 %>%
    arrange(Course, Date) %>%
    mutate(diff = Enrl.Tot - lag(Enrl.Tot), .by = c(Course)) %>%
    filter(!is.na(diff),
           Date == Sys.Date()) %>%
    select(Course, diff)
  
  e.data.3 <- e.data.2 %>%
    left_join(., temp, by = "Course")
  
#Visualize
  
  ggplot(e.data.3) +
    geom_col(aes(x = 1, y = Course), fill = "white", color = "black", linewidth = .1) +
    geom_col(aes(x = pct, y = Course), fill = "red") +
    geom_col(aes(x = pct, y = Course), fill = "green", data = filter(e.data.2, pct == 1)) +
    geom_text(aes(x = 1, y = Course, label = paste0(Enrl.Tot, "/", Enrl.Min)),
              hjust = 1.15, size = 3.5) +
    geom_text(aes(x = 0.1, y = Course, label = paste0("\u0394",diff)),
              hjust = 1.15, size = 3.5) +
    scale_y_discrete(limits = rev) +
    labs(x = "Percent to Minimum",
         title = "Reaching Enrollment Threashold - Spring 2026") +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          axis.ticks       = element_blank(),   # tick marks on both axes
          panel.border     = element_blank(),   # box around the plotting area
          axis.text.x      = element_blank()    # x-axis value labels
    )

   ggsave(file = "./Graphics/Threashold.png")
  