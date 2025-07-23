#This script uses total enrollment data held by the department for all classes. The data is not divided by section.
#The data has also been adjusted to reflect new course numbers which have changed previously (except for 370 sections).
#The original data file is maintains on the O:/ Drive under Academic/Enrollment and traces back to spring 1998.

#Jeremy R. Groves
#Created: July 23, 2025

rm(list=ls())

library(tidyverse)

#Read Data
  enroll <- read.csv("./Data/Count 1998.csv", header = TRUE, as.is = TRUE)
  bridge <- read.csv("./Data/bridge.csv", header = TRUE, as.is = TRUE)
  
#Clean Data
  enroll.t <- enroll %>%
    select(-Average) %>%
    filter(Course != "",
           !grepl("NOTE", Course)) %>%
    pivot_longer(cols = -Course, values_to = "enrollment", names_to = "semester") %>%
    mutate(cn = as.numeric(substr(Course, 1, 3)),
           level  = case_when(cn < 200 ~ "100 Level",
                         cn > 199 & cn < 300 ~ "200 Level",
                         cn > 299 & cn < 400 ~ "300 Level",
                         TRUE ~ "400 Level"),
           temp = substr(semester,3,5),
           temp2 = case_when(substr(temp,3,3) == "8" ~ temp,
                             substr(temp,3,3) == "2" ~ str_pad(as.character(as.numeric(temp)-4), 3, "left", "0")),
           temp2 = case_when(temp2 == "0-2" ~ "002",
                             TRUE ~ temp2),
           acadyear = paste0("20",substr(temp2, 1, 2), "-20",str_pad(as.character(as.numeric(substr(temp2,1,2))+1),
                                                                     2, "left", "0")),
           acadyear = case_when(temp2 == "978" ~ "1997-1998",
                                temp2 == "988" ~ "1998-1999",
                                temp2 == "998" ~ "1999-2000",
                                TRUE ~ acadyear)) %>%
    select(-c(cn, temp, temp2)) %>%
    left_join(., bridge, by = c("Course" = "Number")) %>%
    filter(!is.na(Name))
              
#Process Data
  enroll.1 <- enroll.t %>%
    mutate(enrollment = replace_na(enrollment, 0)) %>%
    summarise(across(enrollment, sum), .by=c(acadyear, level)) %>%
    group_by(level) %>%
    mutate(yr5_avg = (lag(enrollment, n=2) + lag(enrollment, n=1) + enrollment +
                      lead(enrollment, n=2) + lead(enrollment, n=1))/5) %>%
    ungroup()
  
  #Determines the years since the last non-zero enrollment (offering) of the course
      enroll.2 <- enroll.t %>%
        mutate(enrollment = replace_na(enrollment, 0)) %>%
        summarise(across(enrollment, sum), .by=c(acadyear, Course, Name)) %>%
        group_by(Course) %>%
          arrange(acadyear) %>%
          mutate(sem = seq_len(n())) %>%
        ungroup()  %>%
        mutate(enrollment = na_if(enrollment, 0)) %>%
        filter(!is.na(enrollment)) %>%
        summarise(across(sem, max), .by = c(Course, Name)) %>%
          mutate(years = 28 - sem)  %>%
        filter(years != 0) %>%
        select(-sem) %>%
        arrange(desc(years)) %>%
        mutate(Career = case_when(as.numeric(substr(Course,1,1)) > 4 ~ "Graduate",
                                             TRUE ~ "Undergraduate"))

#Visualize Data
  # Total Course Enrollment by Course per Academic Year
      ggplot(enroll.1) +
        geom_line(aes(x = acadyear, y = enrollment, group=level, color = level), linewidth = 1) +
        geom_vline(xintercept = "2010-2011", color = "blue", linewidth = .8) +
        geom_vline(xintercept = "2022-2023", color = "cyan", linewidth = .8) +   
        labs(title = "Undergraduate Sections per Academic Year",
             y = "Enrollment",
             x = "Academic Year",
             caption = "Blue line indicates large section start; cyan line indicates recitations; data in house.",
             color = "Course Level")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  # Total Course Enrollment by Course per Academic Year  - 5 Year Moving Average 
      ggplot(enroll.1) +
        geom_line(aes(x = acadyear, y = yr5_avg, group=level, color = level), linewidth = 1) +
        geom_vline(xintercept = "2010-2011", color = "blue", linewidth = .8) +
        geom_vline(xintercept = "2022-2023", color = "cyan", linewidth = .8) +   
        labs(title = "Undergraduate Sections per Academic Year - 5 Year Moving Average",
             y = "Enrollment",
             x = "Academic Year",
             caption = "Blue line indicates large section start; cyan line indicates recitations; data in house.",
             color = "Course Level")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      
  
 