# The purpose of this script is to use Tableau Fall and Spring census data to show the number of 
# students in the department as of the ten-day count for the semester and compare across departments
# and within the college. This file also tracks degrees conferred.
#
# The data is pulled from the Tableau 
# -  Degrees Data from Degrees Conferred (Fiscal Year) by Department and Degree Program. We change the semester
#    to Fall, Spring, and Summer and then merge them in the main excel file
# -  Fall Majors Data from the Enrollment Fall Semesters by Department and Degree Program.
# -  Fall Enrollment is from the Fall Enrollment Trend obtained by choosing the "crosstab" and then "Department"
# -  Spring Enrollment is from the Spring Enrollment Trend obtained by choosing the "crosstab" and then "Department"

# Data is then put in the master NIU Tableau Data.xlsx in the appropriate sheet and loaded below.

#By: Jeremy R. Groves
#Created: April 14, 2026
#Updated: August 28, 2026: Updated graduation data and explained sources better

rm(list=ls())

library(tidyverse)
library(readxl)


  headcount <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall Majors")
  degrees <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Degrees")
  bridge <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "College Map")
  fall <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall")
  spring <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Spring")
  
#Compile and Clean
  
  core <- fall %>%
    bind_rows(., spring) %>%
    left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
    arrange(Term)
  
  terms <- unique(as.character(core$Term))
  
  core <- core %>%
    mutate(Term = factor(Term, levels = terms))
  
  major <- headcount %>%
    filter(!is.na(Acadept)) %>%
    mutate(Degree = str_split_i(Major, "\\(", 2),
           Degree = str_replace_all(Degree, "\\)", "")) %>%
    rename("Department" = "Acadept",
           "Term" = `Acad Yr`,
           "Count" = `Count of OSIR`) %>%
    mutate(Term = paste0("2", Term - 2000, "8")) %>%
    select(Department, Term, Count, Degree) %>%
    arrange(Department, Degree, Term) %>%
    left_join(., bridge, by = "Department", relationship = "many-to-many")
  
  degree <- degrees %>%
    mutate(Term = paste0("2", `Fiscal Year`-2000, Term),
           Degree = str_split_i(`Major and Degree`, "\\(", 2),
           Degree = str_replace_all(Degree, "\\)", ""),
           Degree = str_replace_all(Degree, "MULT", "")) %>%
    group_by(Department, Degree, Term) %>%
      mutate(Count = sum(`Count of Sheet1`)) %>%
    ungroup() %>%
    select(Department, Degree, Term, Count) %>%
    distinct() %>%
    arrange(Department, Degree, Term) %>%
    left_join(., bridge, by = "Department", relationship = "many-to-many")
  
  rm(terms, fall, spring, bridge, headcount, degrees)
  
#Visualizations for ECON within CLAS
  
  #Fall Majors
  
  programs <- c("BS", "BA", "MA", "PHD")
  
  temp <- major %>%
    group_by(College, Term, Degree) %>%
      mutate(Coll.Count = sum(Count)) %>%
    ungroup() %>%
    mutate(share = Count / Coll.Count) %>%
    group_by(College, Term, Degree) %>%
      mutate(med.share = median(share),
             mean.share = mean(share)) %>%
    ungroup()
  
  temp1 <- temp %>%
    filter(College == "LAS") %>%
    distinct(Term, Degree, med.share, mean.share) 
  
  temp2 <- temp %>%
    filter(Department == "ECON") %>%
    select(Department, Term, Degree, share) %>%
   left_join(., temp1, by = c("Term", "Degree")) %>%
    distinct(Term, Degree, .keep_all = TRUE)
  
  
  ggplot(temp2) + 
    geom_line(aes(x = Term, y = share, color = Degree, group = Degree), linewidth = .75) +
    geom_line(aes(x = Term, y = mean.share, color = Degree, group = Degree), linetype = 2, linewidth = .75) +
    labs(title = "10-Day Majors for Fall by Program",
         caption = "Dashed lines are CLAS average shares by program") +
    xlab("Fall Term") +
    ylab("Share of CLAS Total") +
    theme_bw() +
    theme(legend.position = "bottom")
  