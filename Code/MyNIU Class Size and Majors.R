#My NIU Query NIU_CEDU_ENROLL_DEPT_ONLY  Gives enrollment numbers for each class
#My NIU NIU_CQ_CRSE_ENROLLMENT_TERM gives student info. Must search for each class and saved in subfolder myniu

#Jeremy R. Groves
#September 3, 2025

rm(list=ls())

library(tidyverse)
library(gt)
library(gtsummary)

# Read Data
  
  #Data from first query
    data <- read.csv(file="./Data/NIU_CEDU_ENROLL_DEPT_ONLY.csv", header = TRUE) 
  
  #Data from second query
    file_list <- list.files(path = "./Data/myniu", 
                            pattern = "\\.csv$", 
                            full.names = TRUE)
    STD <- file_list %>%
      map_dfr(read_csv)
    
    temp <- names(STD) %>%
      str_replace(., " ", ".") %>%
      str_replace(., "/", ".")
    
    names(STD) <- temp
    rm(temp, file_list)

# Clean Data
  
  recet <- c("A101", "A102", "A103", "A104","B201", "B202", "B203", "B204",
             "A001", "A002", "A003", "A004", "A005", "A006", "A007", "A008")
  
  data <- data %>%
    filter(!is.na(Pct.Fill),
           Enrl.Tot != 0,
           !Sec %in% recet)
  
  
  STD <- STD %>%
    filter(!Section %in% recet) %>%
    select(!c(Track.Option, MI)) %>%
    mutate(level  = case_when(Catalog < 200                 ~ "100 Level",
                              Catalog > 199 & Catalog < 300 ~ "200 Level",
                              Catalog > 299 & Catalog < 400 ~ "300 Level",
                              TRUE ~ "400 Level"),
           Major2 = trimws(str_split_i(Major, "-", 1)),
           Major2 = case_when(Major2 == "Pre" ~ "Pre-computer Science",
                              TRUE ~ Major2))
  
# Evaluation of the Data
  # Table of Majors for Students
  
  all <- STD %>%
    select(Major2, level) %>%
    tbl_hierarchical_count(Major2,
                           by = level) %>%
    sort_hierarchical(., sort = "descending") %>%
    filter_hierarchical(n > 2) %>%
    tbl_split_by_rows(row_numbers = c(8, 16, 24))
  
# Response of Emails
  
  
           