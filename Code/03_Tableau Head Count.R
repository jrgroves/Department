# The purpose of this script is to use Tableau Fall and Spring census data to show the number of 
# students in the department as of the ten-day count for the semester and compare across departments
# and within the college.
#
# The data is pulled from the Tableau Semester Enrollment Trend and is pasted into the Excel file
# read below in the appropriate sheet.

#By: Jeremy R. Groves
#Created: April 14, 2026

rm(list=ls())

library(tidyverse)
library(readxl)


  fall <- read_excel(path = "./Data/Tableau NIU Department Head Count.xlsx", sheet = "Fall")
  spring <- read_excel(path = "./Data/Tableau NIU Department Head Count.xlsx", sheet = "Spring")
  bridge <- read_excel(path = "./Data/Tableau NIU Department Head Count.xlsx", sheet = "College Map")
  
#Compile and Clean

  core <- fall %>%
    bind_rows(., spring) %>%
    left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
    arrange(Term)
  
  terms <- unique(as.character(core$Term))
  
  core <- core %>%
    mutate(Term = factor(Term, levels = terms))
  
  rm(terms, fall, spring, bridge)
  
#Visualizations for ECON within CLAS
  
  temp <- core %>%
    group_by(Term, College) %>%
      mutate(Med = median(Count),
             Mean = mean(Count)) %>%
    ungroup()  %>%
    filter(Department == "ECON") 
  
  s.temp1 <- temp %>%
    select(Term, College, Med) %>%
    mutate(College = "LAS Median") %>%
    rename("Department" = "College",
           "Count" = "Med")
  s.temp2 <- temp %>%
    select(Term, College, Mean) %>%
    mutate(College = "LAS Mean") %>%
    rename("Department" = "College",
           "Count" = "Mean")
  
  temp <- temp %>%
    select(Term, Department, Count) %>%
    bind_rows(., s.temp1, s.temp2)
  rm(s.temp1, s.temp2)
  
  
  ggplot(temp) + 
    geom_line(aes(x = Term, y = Count, color = Department, group = Department), linewidth = .75) +
    labs(title = "10-Day Head Count by Term") +
    theme_bw() +
    theme(legend.position = "bottom")
  