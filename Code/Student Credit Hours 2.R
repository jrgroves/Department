rm(list = ls())

library(tidyverse)

#Load Data
sch_u <- read.csv(file = "./Data/SCH_data.csv", header = TRUE, as.is=TRUE)
  names(sch_u) <- c("Semester", "Crsenum", "SCH_U")
sch_c <- read.csv(file = "./Data/SCH_data clas.csv", header = TRUE, as.is=TRUE)
  names(sch_c) <- c("Semester", "Crsenum", "SCH_C")
sch_d <- read.csv(file = "./Data/SCH_data ECON.csv", header = TRUE, as.is=TRUE)
  names(sch_d) <- c("Semester", "Crsenum", "SCH_D")
  
#Clean and Manipulate Data
  
  core <- sch_u %>%
    left_join(., sch_c, by = c("Semester", "Crsenum")) %>%
    left_join(., sch_d, by = c("Semester", "Crsenum")) %>%
    filter(!is.na(SCH_C),
           !grepl("SUMMER", Semester)) %>%
    mutate(Crsenum = factor(as.character(Crsenum), levels = c("100", "200", "300", "400", "500", "600", "700")),
           SCH_D = replace_na(SCH_D, 0)) %>%
    pivot_longer(cols = c("SCH_U", "SCH_C", "SCH_D"), names_to = "Level", values_to = "SCH") %>%
    mutate(Level = case_when(Level == "SCH_U" ~ "University",
                             Level == "SCH_C" ~ "CLAS",
                             Level == "SCH_D" ~ "Department",
                             TRUE ~ "Other"),
           AcdemYr = case_when(Semester == "FALL2018" | Semester == "SPRING2019" ~ "AC2018",
                               Semester == "FALL2019" | Semester == "SPRING2020" ~ "AC2019",
                               Semester == "FALL2020" | Semester == "SPRING2021" ~ "AC2020",
                               Semester == "FALL2021" | Semester == "SPRING2022" ~ "AC2021",
                               Semester == "FALL2022" | Semester == "SPRING2023" ~ "AC2022",
                               Semester == "FALL2023" | Semester == "SPRING2024" ~ "AC2023",
                               Semester == "FALL2024" | Semester == "SPRING2025" ~ "AC2024",
                               TRUE ~ "Else"),
           AcdemYr = factor(AcdemYr, levels = c("AC2018","AC2019","AC2020","AC2021","AC2022","AC2023","AC2024"))) %>%
    reframe(tSCH = sum(SCH), .by = c(Level, Crsenum, AcdemYr)) %>%
    group_by(Level, Crsenum) %>%
      mutate(temp = tSCH[which(AcdemYr == "AC2018")],
             nSCH = tSCH/temp) %>%
    ungroup() %>%
    select(-temp)
  
  core2 <- core %>%
    mutate(Career = case_when(Crsenum == "500" ~ "Graduate",
                     Crsenum == "600" ~ "Graduate",
                     Crsenum == "700" ~ "Graduate",
                     TRUE ~ "Undergraduate")) %>%
    reframe(tSCH = sum(tSCH), .by = c(Level, AcdemYr, Career)) %>%
    group_by(Level, Career) %>%
    mutate(temp = tSCH[which(AcdemYr == "AC2018")],
           nSCH = tSCH/temp) %>%
    ungroup() %>%
    select(-temp)
  
  
  
#Visualizations
  
  ggplot(filter(core, Crsenum == "200")) +
    geom_line(aes(x = AcdemYr, y = nSCH, group = Level, color = Level), linewidth = 1) +
    labs(title = "Normalized Student Credit Hours - 200 Level Courses",
         y = "Normalized SCH",
         x = "Academic Year",
         caption = "Data from NIU") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  ggplot(filter(core, Crsenum == "300")) +
    geom_line(aes(x = AcdemYr, y = nSCH, group = Level, color = Level), linewidth = 1) +
    labs(title = "Normalized Student Credit Hours - 300 Level Courses",
         y = "Normalized SCH",
         x = "Academic Year",
         caption = "Data from NIU") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  ggplot(filter(core, Crsenum == "400")) +
    geom_line(aes(x = AcdemYr, y = nSCH, group = Level, color = Level), linewidth = 1) +
    labs(title = "Normalized Student Credit Hours - 400 Level Courses",
         y = "Normalized SCH",
         x = "Academic Year",
         caption = "Data from NIU") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  ggplot(filter(core2, Career == "Graduate")) +
    geom_line(aes(x = AcdemYr, y = nSCH, group = Level, color = Level), linewidth = 1) +
    labs(title = "Normalized Student Credit Hours - Graduate Level Courses",
         y = "Normalized SCH",
         x = "Academic Year",
         caption = "Data from NIU") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  ggplot(filter(core2, Career == "Undergraduate")) +
    geom_line(aes(x = AcdemYr, y = nSCH, group = Level, color = Level), linewidth = 1) +
    labs(title = "Normalized Student Credit Hours - Undergraduate Level Courses",
         y = "Normalized SCH",
         x = "Academic Year",
         caption = "Data from NIU") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
           
    