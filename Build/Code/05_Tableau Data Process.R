# The purpose of this script is to use Tableau Fall and Spring census data to show the number of 
# students in the department as of the ten-day count for the semester and compare across departments
# and within the college. This script is also processing all of the Tableau data available.
#
# The data is pulled from:
## Fall and Spring are from the Tableau Semester Enrollment Trend downloaded by Department into Excel
## Fall2 and Spring2 are from the same, but with the Career set to Undergraduate or Graduate
## Degrees from the Degrees Conferred (Fiscal Year) by Department and Degree Program for full FY
## Degrees2 is the same as above, but you have to change the Semester choice in the options to get by term
## Fall Majors is from the Enrollment Fall Semester by Department and Degree Program (fall 10 day count)


#By: Jeremy R. Groves
#Created: April 14, 2026
#Updated: May 15, 2026   : Added the other Tableau data files into a single Excel worksheet and added all processing here.
#         October 8, 2026: Cleaned up the processing and code and stated the source for data. Also updated fall 2026

rm(list=ls())

library(tidyverse)
library(readxl)
library(gt)


  fall <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall")
  spring <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Spring")
  fall.2 <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall2")
  spring.2 <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Spring2")
  bridge <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "College Map")
  deg.1 <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Degrees", .name_repair = "universal")
  deg.2 <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Degrees2", .name_repair = "universal")
  maj.in <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Fall Majors", .name_repair = "universal")
  seme <- read_excel(path = "./Data/NIU Tableau Data.xlsx", sheet = "Terms", .name_repair = "universal")

#Degree and College Definitions
  bach <- c("BA", "BFA", "BGS", "BM", "BS", "BSED", "MULTBA", "MULTBM", "MULTBS", "MULTSBA3", "BA/BS")
  mast <- c("MA", "MAC", "MAS", "MAT", "MBA", "MFA", "MM", "MPA", "MPH", "MS", "MSED",
            "MST", "MSTCH", "MULTMM", "MULTMS", "PC")
  doc <- c("AUD", "DNP", "DPT", "EDD", "EDS", "JD", "PHD")
  
  cbus <- c("ACCY", "CBUS", "FINA", "MGMT", "MKTG", "OMIS")
  cedu <- c("CAHE", "CEDU", "ETRA", "KNPE", "LEPF", "LTCY", "TLRN")
  ceet <- c("CEET", "ELE", "ISYE", "MEE", "TECH")
  chhs <- c("AHP", "CHHS", "FCNS", "HLTH", "IHP", "NURS")
  clas <- c("ANTH", "BIOS", "CHEM", "CLAS", "COMS", "CSCI", "EAE", "ECON", "ENVS", "FL",
            "HIST", "MATH", "NNGO", "PHIL", "PHYS", "POLS", "PSPA", "PSYC", "SOCI", "SPGA",
            "WOMS")
  cvpa <- c("ART", "CVPA", "MUSC", "THEA")
  law  <- c("LAW")
  
  seme <- seme %>%
    mutate(Term = as.character(Term))

#Compile and Clean#####3
    
    #Headcount Data for Whole Department####
    
    core <- fall %>%
      bind_rows(., spring) %>%
      left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
      arrange(Term)
    
     terms <- unique(core$Term) %>%
       sort() %>%
       as.character()
    
    count <- core %>%
      mutate(Term = factor(Term, levels = terms)) %>%
      mutate(Department = case_when(Department == "STAT" ~ "MATH",
                                    Department == "GEOG" ~ "EAE",
                                    Department == "GEOL" ~ "EAE",
                                    Department == "FACS" ~ "FCNS",
                                    Department == "NGOLD" ~ "NNGO",
                                    Department == "FLWC" ~ "FL",
                                    Department == "FNCS" ~ "FCNS",
                                    is.na(Department) ~ "NIU",
                                    TRUE ~ Department)) %>%
      rename("Majors" = "Count") %>%
      arrange(Term, College, Department)
    
    #Headcount data by Career####
    
    core.2 <- fall.2 %>%
      bind_rows(., spring.2) %>%
      left_join(., bridge, by = "Department", relationship = "many-to-many") %>%
      arrange(Term)
    
    terms <- unique(core$Term) %>%
      sort() %>%
      as.character()
    
    count.2 <- core.2 %>%
      mutate(Term = factor(Term, levels = terms)) %>%
      mutate(Department = case_when(Department == "STAT" ~ "MATH",
                                    Department == "GEOG" ~ "EAE",
                                    Department == "GEOL" ~ "EAE",
                                    Department == "FACS" ~ "FCNS",
                                    Department == "NGOLD" ~ "NNGO",
                                    Department == "FLWC" ~ "FL",
                                    Department == "FNCS" ~ "FCNS",
                                    is.na(Department) ~ "NIU",
                                    TRUE ~ Department)) %>%
      rename("Majors" = "Count") %>%
      arrange(Term, College, Department)
    
    rm(fall, spring, fall.2, spring.2)

#Degrees Conferred ######

  #Degree Information by Fiscal Year
    temp1a <- deg.2 %>%
      select(-Major.and.Degree) %>%
      mutate(Department = case_when(Department == "STAT" ~ "MATH",
                                    Department == "GEOG" ~ "EAE",
                                    Department == "GEOL" ~ "EAE",
                                    Department == "FACS" ~ "FCNS",
                                    Department == "NGOLD" ~ "NNGO",
                                    Department == "FLWC" ~ "FL",
                                    Department == "FNCS" ~ "FCNS",
                                    is.na(Department) ~ "NIU",
                                    TRUE ~ Department)) %>%
      group_by(Department, Degree, Fiscal.Year) %>%
      mutate(Count = sum(Count.of.Sheet1)) %>%
      ungroup() %>%
      select(-Count.of.Sheet1) %>%
      distinct() %>%
      pivot_wider(id_cols = c(Department, Degree), names_from = Fiscal.Year, names_prefix = "FY", values_from = Count)
    
    #Degree Information from Tableau by Terms
        degrees <- deg.1 %>%
          mutate(Term = case_when(Term == 8 ~ paste("2", Fiscal.Year - 2001, Term, sep = ""),
                                  Term == 6 ~ paste("2", Fiscal.Year - 2000, Term, sep = ""),
                                  Term == 2 ~ paste("2", Fiscal.Year - 2000, Term, sep = "")),
                 Department = case_when(Department == "STAT" ~ "MATH",
                                        Department == "GEOG" ~ "EAE",
                                        Department == "GEOL" ~ "EAE",
                                        Department == "FACS" ~ "FCNS",
                                        Department == "NGOLD" ~ "NNGO",
                                        Department == "FLWC" ~ "FL",
                                        Department == "FNCS" ~ "FCNS",
                                        is.na(Department) ~ "NIU",
                                        TRUE ~ Department)) %>%

          select(-Major.and.Degree) %>%
          summarise(Count = sum(Count.of.Sheet1), .by = c(Department, Degree, Term)) %>%
          distinct() %>%
          mutate(                 
            Degree2 = case_when(Degree %in% bach ~ "Bachelor",
                                                      Degree %in% mast ~ "Masters",
                                                      Degree %in% doc ~ "Doctorate", 
                                                      TRUE ~ "Other")) 
        
## Main Database
        
        dept <- unique(count$Department)
        
        core <- count %>%
          left_join(., degrees, by = c("Department", "Term"),
                    relationship = "many-to-many") %>%
          filter(Term != "2162",
                 Term != "2168") %>%
          mutate(Department = factor(Department, levels = dept))
    
        temp1 <- core %>%
          select(Department, Majors, Term, College) %>%
          distinct() %>%
          filter(College == "LAS",
                 Department == "ECON")

##Tables
        
        temp <- core %>%
          select(Name, Term, Majors, College.Name) %>%
          left_join(., seme, by = "Term") %>%
          filter(!is.na(Name)) %>%
          distinct(Name, College.Name, Term, Majors, .keep_all = TRUE)%>%
          pivot_wider(id_cols = c(College.Name, Name),
                      values_from = Majors,
                      names_from = Semester)
        
        table.1 <- temp %>%
          group_by(College.Name) %>%
          gt() %>%
          tab_header(
            title = "Semester Department Headcounts",
            subtitle = "Spring 2017 - Fall 2026"
          ) %>%
          sub_missing(
            columns = everything(), 
            missing_text = ""
          ) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_row_groups()
          ) %>%
          tab_style(
            style = cell_borders(
              sides = "right",          # Put the line on the right side of cells
              color = "gray80",         # Light gray color for a clean look
              weight = px(1),           # Line thickness in pixels
              style = "solid"
            ),
            locations = cells_body()    # Apply to all cells in the table body
          ) %>%  
          tab_style(
            style = cell_text(color = "red", weight = "bold"),
            locations = cells_body(
              rows = Name == "Economics"
            )
          )
          gtsave(table.1,
                 filename = "./Graphics/TermMajors.html")
        
        
          temp <- count.2 %>%
            select(Name, Term, Majors, College.Name, Career) %>%
            left_join(., seme, by = "Term") %>%
            filter(Career == "Undergraduate") %>%
            filter(!is.na(Name)) %>%
            distinct(Name, College.Name, Term, Majors, .keep_all = TRUE)%>%
            pivot_wider(id_cols = c(College.Name, Name),
                        values_from = Majors,
                        names_from = Semester)
          
          table.2 <- temp %>%
            group_by(College.Name) %>%
            gt() %>%
            tab_header(
              title = "Semester Department Headcounts: Undergraduates",
              subtitle = "Spring 2016 - Fall 2026"
            ) %>%
            sub_missing(
              columns = everything(), 
              missing_text = ""
            ) %>%
            tab_style(
              style = cell_text(weight = "bold"),
              locations = cells_row_groups()
            ) %>%
            tab_style(
              style = cell_borders(
                sides = "right",          # Put the line on the right side of cells
                color = "gray80",         # Light gray color for a clean look
                weight = px(1),           # Line thickness in pixels
                style = "solid"
              ),
              locations = cells_body()    # Apply to all cells in the table body
            ) %>%  
            tab_style(
              style = cell_text(color = "red", weight = "bold"),
              locations = cells_body(
                rows = Name == "Economics"
              )
            )
          gtsave(table.2,
                 filename = "./Graphics/TermMajors_Under.html")
        
          temp <- count.2 %>%
            select(Name, Term, Majors, College.Name, Career) %>%
            left_join(., seme, by = "Term") %>%
            filter(Career == "Graduate") %>%
            filter(!is.na(Name)) %>%
            distinct(Name, College.Name, Term, Majors, .keep_all = TRUE)%>%
            pivot_wider(id_cols = c(College.Name, Name),
                        values_from = Majors,
                        names_from = Semester)
          
          table.2 <- temp %>%
            group_by(College.Name) %>%
            gt() %>%
            tab_header(
              title = "Semester Department Headcounts: Graduates",
              subtitle = "Spring 2016 - Fall 2026"
            ) %>%
            sub_missing(
              columns = everything(), 
              missing_text = ""
            ) %>%
            tab_style(
              style = cell_text(weight = "bold"),
              locations = cells_row_groups()
            ) %>%
            tab_style(
              style = cell_borders(
                sides = "right",          # Put the line on the right side of cells
                color = "gray80",         # Light gray color for a clean look
                weight = px(1),           # Line thickness in pixels
                style = "solid"
              ),
              locations = cells_body()    # Apply to all cells in the table body
            ) %>%  
            tab_style(
              style = cell_text(color = "red", weight = "bold"),
              locations = cells_body(
                rows = Name == "Economics"
              )
            )
          gtsave(table.2,
                 filename = "./Graphics/TermMajors_Grad.html")
    