# This file uses MyNIU data to show the majors and minors within the department only and
# breaks them down by class. The values should be downloaded once at the start of the Fall semester
# and again in April. My NIU is constantly updated and so values will different from tableau data 
# which is a static snapshot. The data starts in 2005; however, early values are suspect.
#
# The data is obtain from the MyNIU query NIU_CQ_MAJORS_CURRENT_BY_PLAN and should be saved in the 
# excel file MyNIU Majors and Minors on the respective worksheet.
#
# This file will output total majors and minors (according to MyNIU) and also the options taken by 
# minors and the college home of our minors.

# Jeremy R. Groves
# Created: August 8, 2025
# Updated: April 14, 2026 - ran the data for spring and also changed code to read sheets of excel 

rm(list=ls())

library(tidyverse)
library(readxl)

#Load Data
BS.data <- read_excel(path = "./Data/MyNIU Majors and MInors.xlsx",sheet = "BS Students",
                      .name_repair = "universal")
BA.data <- read_excel(path = "./Data/MyNIU Majors and MInors.xlsx",sheet = "BA Students",
                      .name_repair = "universal")
BSFE.data <- read_excel(path = "./Data/MyNIU Majors and MInors.xlsx",sheet = "BS FE Student",
                        .name_repair = "universal")
MINOR.data <- read_excel(path = "./Data/MyNIU Majors and MInors.xlsx",sheet = "Minors",
                         .name_repair = "universal")

terms <- unique(as.character(BS.data$Term))

#Clean and process data####

    BS.data2 <- BS.data %>%
      select(Term, Campus.ID, First.Name, Last.Name, Suffix, Enrolled, Acad.Prog, 
             Acad.Level, Acad.Plan, Term.Hours, Cumulative.GPA) %>%
      filter(str_detect(Enrolled, "Enrolled")) %>%
      distinct(Term, Campus.ID,.keep_all = TRUE)%>%
      mutate(Acad.Plan = "BS")

  BA.data2 <- BA.data %>%
    select(Term, Campus.ID, First.Name, Last.Name, Suffix, Enrolled, Acad.Prog, 
           Acad.Level, Acad.Plan, Term.Hours, Cumulative.GPA) %>%
    filter(str_detect(Enrolled, "Enrolled"))  %>%
    distinct(Term, Campus.ID,.keep_all = TRUE)%>%
    mutate(Acad.Plan = "BA")
  
  BSFE.data2 <- BSFE.data %>%
    select(Term, Campus.ID, First.Name, Last.Name, Suffix, Enrolled, Acad.Prog, 
           Acad.Level, Acad.Plan, Term.Hours, Cumulative.GPA) %>%
    filter(str_detect(Enrolled, "Enrolled")) %>%
    distinct(Term, Campus.ID,.keep_all = TRUE) %>%
    mutate(Acad.Plan = "BSFE")
  
  MINOR.data2 <- MINOR.data %>%
    select(Term, Campus.ID, First.Name, Last.Name, Suffix, Enrolled, Acad.Prog, 
           Acad.Level, Acad.Plan, Sub.Plan, Term.Hours, Cumulative.GPA) %>%
    filter(str_detect(Enrolled, "Enrolled")) %>%
    distinct(Term, Campus.ID,.keep_all = TRUE) %>%
    mutate(Acad.Plan = "Minor")
  
  
  core <- BS.data2 %>%
    bind_rows(BA.data2, BSFE.data2, MINOR.data2) %>%
    mutate(Acad.Level = factor(Acad.Level, levels = c("Freshman", "Sophomore", "Junior", "Senior", "PostBacc")),
           Term = factor(Term, levels = terms))%>%
    distinct(Term, Campus.ID,.keep_all = TRUE)

#Create Data for Tables and Charts 
  
  degrees <- c("BA" = "#FFC20A", "BS" = "#DC3220", "BSFE" = "#005AB5", "Minor" = "#40B0A6")
  
  #Total Students By Year
  
    ggplot(core) +
      geom_bar(aes(x = Term, fill = Acad.Plan)) +
      scale_fill_manual(values = degrees) +
      labs(title = "Total Students by Academic Term and Degree",
           y = "Students",
           fill = "Degree",
           caption = "Based on data from MyNIU") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom")
      
    cut <- as.vector(tail(levels(core$Term), 10))
    
    temp <- core %>%
      filter(Term %in% cut) %>%
      group_by(Term) %>%
        mutate(n = n(),
               spot = max(n) - 3) %>%
      ungroup()
    
    ggplot(temp) +
      geom_bar(aes(x = Term, fill = Acad.Plan)) +
      scale_fill_manual(values = degrees) +
      geom_text(aes(y = spot, x = Term, label = n), color = "white") + 
      labs(title = "Total Students by Academic Term and Degree: Last 5 Years",
           y = "Students",
           fill = "Degree",
           caption = "Based on data from MyNIU") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom")
    
    # What Minor Options are Working
      options <- c("Option 1: Open" = "#DC3220", "Option 3: Math" = "#005AB5", "Option 2: Stat" = "#40B0A6")
      
        minor <- MINOR.data2 %>%
          select(Term, Acad.Prog, Sub.Plan) %>%
          mutate(Sub.Plan = case_when(Sub.Plan == "MATHOPTION" ~ "Option 3: Math",
                                      Sub.Plan == "Option 3 - Mathematics Option" ~ "Option 3: Math",
                                      Sub.Plan == "OPENOPTION" ~ "Option 1: Open",
                                      Sub.Plan == "Option 1 - Open Option" ~ "Option 1: Open",
                                      Sub.Plan == "STATOPTION" ~ "Option 2: Stat",
                                      Sub.Plan == "Option 2 - Statistics Option" ~ "Option 2: Stat",
                                      TRUE ~ "Option 1: Open"),
                 Acad.Prog = gsub("2", "", Acad.Prog),
                 Term = factor(Term, levels = terms))
       
        ggplot(minor) +
          geom_bar(aes(x = Term, fill = Sub.Plan)) +
          scale_fill_manual(values = options) +
          labs(title = "Minors by Options",
               y = "Students",
               fill = "Option",
               caption = "Based on data from MyNIU") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90),
                legend.position = "bottom")
        
        temp <- minor %>%
          filter(Term %in% cut) %>%
          group_by(Term) %>%
          mutate(n = n(),
                 spot = max(n) - 2) %>%
          ungroup()
        
        ggplot(temp) +
          geom_bar(aes(x = Term, fill = Sub.Plan)) +
          scale_fill_manual(values = options) +
          geom_text(aes(y = spot, x = Term, label = n), color = "white") + 
          labs(title = "Minors by Options: Last 5 Years",
               y = "Students",
               fill = "Option",
               caption = "Based on data from MyNIU") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90),
                legend.position = "bottom")
    
    # Where are Minors Coming From
        college <- c("BUS" = "#D81B60", "EDU" = "#FFC107", "EET" = "#004D40", "LAS" = "#1E88E5")
        
        ggplot(minor) +
          geom_bar(aes(x = Term, fill = Acad.Prog)) +
          scale_fill_manual(values = college) +
          labs(title = "Home College of Minors",
               y = "Students",
               fill = "Home College",
               caption = "Based on data from MyNIU") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90),
                legend.position = "bottom")
        
        ggplot(temp) +
          geom_bar(aes(x = Term, fill = Acad.Prog)) +
          scale_fill_manual(values = college) +
          labs(title = "Home College of Minors: Last 5 Years",
               y = "Students",
               fill = "Home College",
               caption = "Based on data from MyNIU") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90),
                legend.position = "bottom")
        