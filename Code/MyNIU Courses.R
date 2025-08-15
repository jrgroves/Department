#Jeremy R. Groves
#Created: August 8, 2025

rm(list=ls())

library(tidyverse)

#Load Data

  s <- paste0("2", str_pad(seq(9, 25, 1), width = 2, side = "left", pad = "0"), "2")
  f <- paste0("2", str_pad(seq(8, 25, 1), width = 2, side = "left", pad = "0"), "8")

  for(i in s){
    temp <- read.csv(file = paste0("./Data/courses/",i,".csv"), header = TRUE)
    temp <- mutate(temp, Term2 = i)
    ifelse(i == "2092", data.s<-temp, data.s<-bind_rows(data.s, temp))
  }  
  
  for(i in f){
    temp <- read.csv(file = paste0("./Data/courses/",i,".csv"), header = TRUE)
    temp <- mutate(temp, Term2 = i)
    ifelse(i == "2088", data.f<-temp, data.f<-bind_rows(data.f, temp))
  } 

  recit <- paste0("A",str_pad(seq(1,10,1), width = 3, side = "left", pad = "0"))
  recit2 <- paste0("B",str_pad(seq(1,10,1), width = 3, side = "left", pad = "0"))
  recit3 <- paste0("A1",str_pad(seq(1,9,1), width = 2, side = "left", pad = "0"))
  recit4 <- c("A1H1", "B1H1", "A111", "A110", "PYE1", "YE1", "L001", "L002")
  
  core <- bind_rows(data.s, data.f) %>%
    filter(Subject == "ECON") %>%
    select(-c(Role, Access, Email, Type)) %>%
    mutate(t2 = as.numeric(Term2)) %>%
    filter(! Section %in% recit,
           ! Section %in% recit2,
           ! Section %in% recit3,
           ! Section %in% recit4) %>%
    mutate(Catalog = trimws(Catalog),
           Catalog = case_when(Catalog == "460X" ~ "484",
                               Catalog == "484X" ~ "484",
                               TRUE ~ trimws(Catalog))) %>%
    filter(Catalog != "390A",,
           Catalog != "397H",
           Catalog != "494",
           Catalog !="393A",
           Catalog !="496X") %>%
    mutate(Catalog = as.numeric(Catalog),
           Level = case_when(Catalog < 200 ~ "100 Level",
                             Catalog < 300 & Catalog > 199 ~ "200 Level",
                             Catalog < 400 & Catalog > 299 ~ "300 Level",
                             TRUE ~ "400 Level")) %>%
    group_by(Level, Term2) %>%
      mutate(sections = n()) %>%
    ungroup()


#Visuzliations

  ggplot(filter(core, ! Catalog %in% c("498", "497", "492", "490", "491")), aes(Term2))  + 
    geom_bar(aes(fill = Level), position = "stack") +
    labs(x = "Term",
         y = "Share",
         title = "Sections Taught") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

  ggplot(filter(core, ! Catalog %in% c("498", "497", "492", "490", "491")), aes(Term2))  + 
    geom_bar(aes(fill = Level, weight = sections), position = "fill") +
    labs(x = "Term",
         y = "Share",
         title = "Course Distribution by Level") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  
  
  ggplot(filter(core, ! Catalog %in% c("498", "497", "492", "490", "491")), aes(x = Term2))  + 
    geom_line(aes(y=sections, group = Level, color = Level)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  
  core2 <- core %>%
    group_by(Term, Catalog) %>%
    mutate(students = sum(Tot.Enrl)) %>%
    ungroup() %>%
    distinct(Catalog, Term, Term2, Level, students) %>%
    arrange(Term2, Level) %>%
    group_by(Term, Level) %>%
      mutate(enroll = sum(students)) %>%
    ungroup()
  
  ggplot(filter(core2, ! Catalog %in% c("498", "497", "492", "490", "491")), aes(x = Term2))  + 
    geom_line(aes(y = enroll, group = Level, color = Level)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  
  
  
  temp <- core2 %>%
    filter(! Catalog %in% c("498", "497", "492", "490", "491")) %>%
    select(-students) %>%
    distinct(Term2, Level, enroll)
  
  ggplot(temp, aes(Term2))  + 
    geom_bar(aes(fill = Level, weight = enroll), position = "fill") +
    labs(x = "Term",
         y = "Share",
         title = "Enrollment Share by Level") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  