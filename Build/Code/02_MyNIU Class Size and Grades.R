# This script creates a list of the courses taught, the enrollment for each class, and the percentage
# filled for each course. Additionally, with the second query, we can get information for each student
# in each course to determine where students are coming from and what majors are taking. 
#
# My NIU Query NIU_CEDU_ENROLL_DEPT_ONLY  Gives enrollment numbers for each class. This query is run
# and then the data is added to the MyNIU_Enrollment.csv file in the DATA sub-directory.
#
# My NIU NIU_CQ_CRSE_ENROLLMENT_TERM gives student info. Must search for each class and saved in 
# sub-directory myniu. If we run this AFTER the semester, we also get grades for the classes.

#Jeremy R. Groves
#September 3, 2025
#Updated April 14, 2026: Improved table code and updated with spring values. Also moved graphs and tables to Analysis Code

rm(list=ls())

library(tidyverse)
library(flextable)

# Read Data
  
  #Data from first query
    data <- read.csv(file="./Data/MyNIU_Enrollment.csv", header = TRUE) 
  
  #Data from second query (Creates Student Database for all undergraduate classes)
    source("./Build/Code/02a_MyNIU_Enroll_In.R")
    load("./Data/myniu/Grades.RData")
    
# Clean Data
  # Clean the Course Data 
    
    recet <- c("A101", "A102", "A103", "A104","B201", "B202", "B203", "B204",
               "A001", "A002", "A003", "A004", "A005", "A006", "A007", "A008")
    
    data <- data %>%
      filter(!is.na(Pct.Fill),
             Enrl.Tot != 0,
             !Sec %in% recet)
  
  #Clean student data
  
  student.core <- STD %>%
    filter(!Section %in% recet) %>%
    select(!c(Track.Option, MI)) %>%
    mutate(level  = case_when(Catalog < 200                 ~ "100 Level",
                              Catalog > 199 & Catalog < 300 ~ "200 Level",
                              Catalog > 299 & Catalog < 400 ~ "300 Level",
                              TRUE ~ "400 Level"),
           Major.t = trimws(str_split_i(Major, "-", 1)),
           Major2 = trimws(str_split_i(Major.t, ":", 1)),
           Major2 = case_when(Major2 == "Pre" ~ "Pre-computer Science",
                              Major2 == "Industrial and Systems Engineering" ~ "Industrial & Systems Engineering",
                              TRUE ~ Major2),
           Major2 = trimws(gsub("Emphasis", "", Major2)),
           Term2 = case_when(Term == "Fall 2025" ~ "2258",
                            Term == "Spring 2026" ~ "2262",
                            TRUE ~ NA),
           Grade = factor(Grade, levels = c("A", "A-", "B+", "B", "B-", "C+", "C", "D", "F"))) %>%
    rename("zID" = "Empl.ID")
  
save(data, student.core, file = "./Data/MyNIU_class_grades.RData")

# Evaluation of the Data
  # Table of Majors for Students
      all <- student.core %>%
        select(Major2, level, Term) %>%
        summarize(frequency = n(), .by = c(Major2, level, Term)) %>%
        mutate(level = paste(Term,level, sep = "_")) %>%
        select(-Term) %>%
        arrange(level) %>%
        pivot_wider(id_cols = c("Major2"), names_from = "level", values_from = "frequency",
                    values_fill = 0) %>%
        arrange(desc(`Spring 2026_200 Level`)) %>%
        rename("Major" = "Major2")
  
  flextable(all) %>%
    separate_header() %>%
    add_header_lines(values = "Majors of Enrolled Students by Level") %>%
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>%
    align(j = 1, align = "left", part = "body") %>%
    bold(i = 1, part = "header") %>%
    vline(j = 4, part = "body") %>%
    autofit()
  
  # Grade Distribution
    grade <- student.core %>%
      select(Term, level, Grade)    %>%
      filter(!is.na(Grade)) %>%
      summarize(frequency = n(), .by = c(Grade, level, Term)) %>%
      group_by(Term, level) %>%
        mutate(total = sum(frequency)) %>%
      ungroup() %>%
      mutate(share = frequency / total)
    
    
    ggplot(grade, aes(x = Grade, y = share, fill = Term)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_continuous(breaks = seq(0, .6, by = .1)) +
      facet_wrap(~level) +
      labs(title = "Grade Distribution by Course Level",
           y = "Share") +
      theme_minimal() 
    