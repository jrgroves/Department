# My NIU NIU_CQ_CRSE_ENROLLMENT_TERM gives student info. Must search for each class and save in 
# sub-directory myniu. Ideally run after the end of the semester because grades are also included.
# This script will open the data, process all the files for the academic year and then it will combine it with previous years and save
# This way if run accidentally, the repeated data will be simply deleted. 

# Created By: Jeremy Groves
# Created On: May 15, 2026

library(tidyverse)

#Data from second query (Creates Student Database for all undergraduate classes)
file_list <- list.files(path = "./Data/myniu", 
                        pattern = "\\.csv$", 
                        full.names = TRUE)
STD.in <- file_list %>%
  map_df(~ .x %>%
           read.csv(.) %>%
           mutate_if(is.numeric, as.character))

load("./Data/myniu/Grades.RData")

STD <- STD %>%
  bind_rows(., STD.in %>% mutate(across(everything(), ~as.character(.x)))) %>%
  filter(Unit.Taken!=0) %>%
  distinct(Empl.ID, First.Name, Last.Name, Term, Catalog, Section, Class.Title, Grade, .keep_all = TRUE)

rm(STD.in)

save(STD, file = "./Data/myniu/Grades.RData")
