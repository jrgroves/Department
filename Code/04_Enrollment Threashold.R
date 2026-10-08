#This script uses data from MyNIU to measure the enrollment for courses taught by the department including information
#on meeting the threasholds, fill rates, cancellations, instructor of record, and mode of course.

#Utilize the NIU_CEDU_ENROLL_DEPT_ONLY query on MyNIU for enrollments.

#Created by: Jeremy Groves
#Created on: October 5, 2026

rm(list=ls())

library(tidyverse)
library(readxl)
library(gt)
library(openxlsx)
library(gtsummary)

#Define References
  valid_inst = c("Ai-Ru Cheng", "Alexander Garivaltis", "Anna Klis", "Bobby Barnes","Brian Richard",
                 "Carl Campbell", "Carlos Pulido Hernandez", "Eliakim Katz", "Evan Anderson",
                 "George Slotsve", "Ileana Brooks", "Harlan Holt", "Jeffrey Reynolds",
                 "Jeremy Groves", "Khan Mohabbat", "Laurel Adams", "Masayuki Hirukawa",
                 "Marlynne Ingram", "Maria Ponomareva", "Manjuri Talukdar", "Mohammad Mirhosseini",
                 "Neelam Jain", "Paul Stroik", "Pavlo Buryi", "Sowjanya Dharmasankar", "Stephen Karlson",
                 "Steven Nord", "Susan Porter-Hudak", "Tammy Batson", "Virginia Wilcox", "Wei Zhang")
  
  invalid_sec <- c("PZ00","PZ01","PZ02","PZ03","PZ04","PZ05","P1","P001","YE1","00Y1")
  rec.section <- c("A101", "A102","A103","A104","A105","A106","A107","A108","A109","A110","A111",
                   "A001","A002","A003","A004","A005","A006","A007","A008","A009","A010",
                   "B201","B202","B203","B204")
  
  noninst_course <- c("397","494","495","496X","497","498","695","698","699","699A","699B","798","799","795")
  
  Term.f <- c("2102","2108","2112","2118","2122","2128","2132","2138","2142","2148","2152","2158","2162","2168","2172","2178",
            "2182","2188","2192","2198","2202","2208","2212","2218","2222","2228","2232","2238","2242","2248","2252","2258",
            "2262","2268")

  Term.s <- c("2158","2162","2168","2172","2178","2188","2192","2198","2202","2208","2212","2218","2222","2228","2232","2238",
              "2242","2248","2252","2258","2262","2268")
  
  
#Read Enrollment Data and combine into one data frame
  file_paths <- list.files(path = "./Data/enroll", pattern = "\\.xlsx", full.names = TRUE)
    names(file_paths) <- basename(file_paths)
  
  combined_db <- map(
    file_paths,
    ~ read_xlsx(.x, sheet = 1, col_names = TRUE, skip = 1),
    .id = "file_name"
  )
  
  combined_db <- list_rbind(combined_db, names_to = "file_name") %>%
    filter(file_name != "NIU_CQ_INSTRUCTORS_TERM_DEPT.xlsx")
    names(combined_db) <- gsub("\\s+", "", names(combined_db))

#Read and clean instructor data
  inst_data <- read_xlsx("./Data/enroll/NIU_CQ_INSTRUCTORS_TERM_DEPT.xlsx", col_names = TRUE, skip = 1)
  names(inst_data) <- gsub("\\s+", "", names(inst_data))
  
  inst_data <- inst_data %>%
    filter(Role == "Primary Instructor") %>%
    filter(substr(Term, 4, 4) != "6") %>%
    filter(Access == "Post") %>%
    distinct(.keep_all = TRUE) %>%
    select(Subject, Catalog, Section, Term, Instr_FirstName, Instr_LastName, Role) %>%
    filter(!Catalog %in% noninst_course) %>%
    group_by(Catalog, Section, Term) %>%
      mutate(Inst.Count = n()) %>%
    ungroup() 

#Merge and Process data
  core <- combined_db %>%
    filter(!Sec %in% invalid_sec) %>%
    filter(!CatNbr %in% noninst_course) %>%
    select(Term, Dept, CatNbr, Sec, ClassNbr, Title, EnrlCap, EnrlTot,CancelDate) %>%
    rename(Subject = Dept,
           Section = Sec,
           Catalog = CatNbr) %>%
    full_join(., inst_data, by=c("Subject", "Catalog", "Section", "Term")) %>%
    filter(!Section %in% invalid_sec) %>%
    filter(!Section %in% rec.section) %>%
    #Corrections and adjustments for number changes
    mutate(Section = case_when(Section == "A0H1" ~ "00H1",
                               Section == "A1H1" ~ "00H1",
                               Section == "A100" ~ "0001",
                               Section == "B200" ~ "0002",
                               Section == "B0H1" ~ "00H1",
                               Section == "B1H1" ~ "00H1",
                               Section == "1" ~ "L001",
                               Catalog == "393A" ~ "L001",
                               Catalog == "390A" ~ "L001",
                               Catalog == "692"  ~ "L001",
                               TRUE ~ Section),
           Catalog = case_when(Catalog == "600" ~ "700",
                               Catalog == "601" ~ "701",
                               Catalog == "650" ~ "750",
                               Catalog == "651" ~ "751",
                               Catalog == "743" ~ "741",
                               Catalog == "460X" ~ "460",
                               Catalog == "484X" ~ "460",
                               Catalog == "584X" ~ "584",
                               TRUE ~ Catalog),
           Title = case_when(Title == "Econimics of The Public Sector" ~ "Public Sector Economics I",
                             Title == "Financing Government Activity" ~ "Public Sector Economics I",
                             Title == "Hist of Econ Thought" ~ "History of Economic Thought",
                             Title == "Intro Math Meth Econ" ~ "Intro to Math Method in Econ",
                             Title == "Math Meth For Econ" ~ "Math Methods for Economics",
                             Title == "Sem in Quantitative Economics" ~ "Fina & Time-Series Econometric",
                             Title == "Economic Data Analysis" ~ "Economic Data Analysis Excel",
                             Title == "Rsch Meth in Econ" ~ "Research Methods in Economics",
                             TRUE ~ Title),
           Course = paste(paste(Subject, Catalog, sep = " "), substr(Section,3,4), sep = "."),
           Instructor = paste(Instr_FirstName, Instr_LastName, sep = " "))   %>%
    filter(Catalog != "661A",
           Catalog != "661B",
           Catalog != "592",
           Title != "Sem Applied Public Economics",
           Title != "Sem Appl Urban&Region Econ",
           Title != "Sem Appl Labor Econ & Relation",
           Title != "Sem in Financial Economics") %>%
    filter(Subject == "ECON") %>%
    filter(EnrlCap > 0) %>%
    filter(Section != "L001") %>%
    arrange(Subject, Catalog, Section, Term) %>%
    select(Course, Title, Catalog, Section, Term, EnrlTot, EnrlCap, Instructor, Inst.Count, CancelDate) %>%
    #Calculation of Statistics
    mutate(number = as.numeric(Catalog),
           Cancelled = case_when(is.na(CancelDate) ~ 0,
                                 TRUE ~ 1),
           Threashold = case_when(number < 300 ~ 20,
                                  number > 299 & number < 500 ~ 15,
                                  number > 499 & number < 600 ~ 8,
                                  number > 599 ~ 5),
           Fill = EnrlTot/EnrlCap,
           Met = case_when(EnrlTot > Threashold ~ "Met",
                           EnrlTot == 0 & Cancelled == 1 ~ "Canceled",
                           TRUE ~ "Low_Enrolled"),
           Term = factor(Term, levels = Term.f)) %>%
    select(-CancelDate) %>%
    filter(Term %in% Term.s)
  

#Create Fill/Threshold Sheet
  
  #Table 1: Course Aggregates
      data.1 <- core %>%
        filter(Section != "00H1") %>%    #Remove Honors Sections since they always are under cap
        filter(EnrlTot > 0 | Cancelled != 0 ) %>%
        distinct(Course, Term, .keep_all = TRUE)  %>% #Remove dual teacher classes
        add_count(Catalog, Met, name = "Count") %>%
        add_count(Catalog, Catalog, name = "Total") %>%
        mutate(Course = paste("ECON",Catalog,sep = " ")) %>%
        select(Course, Title, Met, Count) %>%
        distinct()%>%
        pivot_wider(id_cols = c(Course,Title),
                    names_from = Met,
                    values_from = Count)  %>%
        mutate(Total = coalesce(Met,0) + coalesce(Low_Enrolled, 0),
               Share = (coalesce(Low_Enrolled, 0) / Total),
               across(everything(), ~ replace_na(., 0))) 
        
      
      table.1 <- data.1 %>%
        gt() %>%
        tab_header(title = "Economics Course Count Threashold",
                   subtitle = "Fall 2015 - Current") %>%
        cols_label(Low_Enrolled = "Low Enrolled") %>%
        fmt_percent(
          columns = Share,
          decimals = 2
        ) %>%
        cols_align(
          align = "center",
          columns = c(Canceled, Met, Low_Enrolled, Total, Share)
        ) %>%
        sub_missing(
          columns = everything(), 
          missing_text = ""
        )
      
      gtsave(
        data = table.1, 
        file = "./Table1.html"
      ) 
  
  #Table 2: Course Enrollment by Term
      data.2 <- core %>%
        filter(Section != "00H1") %>%    #Remove Honors Sections since they always are under cap
        filter(EnrlTot > 0 | Cancelled != 0 ) %>%
        distinct(number, Section, Term, .keep_all = TRUE) %>% #Remove dual teacher classes
        arrange(Term, number, Section) %>%
        select(Course, Title, Term, EnrlTot, Threashold)  %>%
        pivot_wider(id_cols = c(Course, Title, Threashold),
                    names_from = Term,
                    names_prefix = "T",
                    values_from = EnrlTot)  %>%
        filter(if_any(starts_with("T2"), ~!is.na(.x))) %>%
        arrange(Course)
    
      target <- names(data.2)[3:25]
      
      red_locs <- map(target, function(col_name){
        cells_body(
          columns = all_of(col_name),
          rows = .data[[col_name]] < Threashold
        )
      })
      
      blue_locs <- map(target, function(col_name){
        cells_body(
          columns = all_of(col_name),
          rows = .data[[col_name]] == 0
        )
      })
    
      
      table.2 <- data.2 %>%
        gt() %>%
        tab_header(title = "Economics Course Enrollment",
                   subtitle = "Courses By Term Fall 2018 - Current") %>%
        tab_style(
          style = cell_text(color = "red", weight = "bold"),
          locations = red_locs) %>%
        tab_style(
          style = cell_text(color = "blue", weight = "bold"),
          locations = blue_locs) %>%
        cols_hide(columns = Threashold) %>%
        sub_missing(
          columns = everything(), 
          missing_text = ""
        )
      
      gtsave(
        data = table.2, 
        file = "./Table2.html"
      )
  
  #Table 3: Instructor Aggregates
      data.1 <- core %>%
        filter(Section != "00H1") %>%    #Remove Honors Sections since they always are under cap
        filter(EnrlTot > 0 | Cancelled != 0 ) %>%
        distinct(Course, Term, .keep_all = TRUE)  %>% #Remove dual teacher classes
        filter(Catalog != 492,
               Catalog != 691) %>%
        add_count(Catalog, Met, name = "Count") %>%
        add_count(Catalog, Catalog, name = "Total") %>%
        mutate(Course = paste("ECON",Catalog,sep = " ")) %>%
        select(Course, Title, Met, Count) %>%
        distinct()%>%
        pivot_wider(id_cols = c(Course,Title),
                    names_from = Met,
                    values_from = Count)  %>%
        mutate(Total = coalesce(Met,0) + coalesce(Low_Enrolled, 0),
               Share = (coalesce(Low_Enrolled, 0) / Total),
               across(everything(), ~ replace_na(., 0))) 
      
      
      table.1 <- data.1 %>%
        gt() %>%
        tab_header(title = "Economics Course Count Threashold",
                   subtitle = "Fall 2015 - Current") %>%
        cols_label(Low_Enrolled = "Low Enrolled") %>%
        fmt_percent(
          columns = Share,
          decimals = 2
        ) %>%
        cols_align(
          align = "center",
          columns = c(Canceled, Met, Low_Enrolled, Total, Share)
        ) %>%
        sub_missing(
          columns = everything(), 
          missing_text = ""
        )
      
      gtsave(
        data = table.1, 
        file = "./Table1.html"
      ) 