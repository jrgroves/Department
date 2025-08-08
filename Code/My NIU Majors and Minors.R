#Jeremy R. Groves
#Created: August 8, 2025

rm(list=ls())

library(tidyverse)

#Load Data
BS.data <- read.csv(file = "./Data/MyNIU BS Students.csv", header = TRUE, as.is = TRUE)
BA.data <- read.csv(file = "./Data/MyNIU BA Students.csv", header = TRUE, as.is = TRUE)
BSFE.data <- read.csv(file = "./Data/MyNIU BS FE Students.csv", header = TRUE, as.is = TRUE)
MINOR.data <- read.csv(file = "./Data/MyNIU Minor Students.csv", header = TRUE, as.is = TRUE)


#Clean and process data####

BS.data <- BS.data %>%
  select(First.Name, MI, Last.Name, Suffix, Enrolled, Acad.Prog, Department, Plan.Type, 
         Academic.Plan.Major, Acad.Level, Term.Hours, Cumulative.GPA) %>%
  filter(str_detect(Enrolled, "Enrolled")) %>%
  mutate(Enrolled = gsub("Enrolled ", "", Enrolled),
         Enrolled2 = Enrolled) %>%
  separate(Enrolled, c("Semester", "Year"), sep = ' ') %>%
  mutate(Semester = case_when(Semester == "Fall" ~ "8",
                              Semester == "Spring" ~ "2",
                              TRUE ~ NA),
         Term = paste0("2",substr(Year, 3,4),Semester),
         Acad.Prog = gsub("2", "", Acad.Prog),
         Degree = "BS") %>%
  select(-c(Semester, Year))

BA.data <- BA.data %>%
  select(First.Name, MI, Last.Name, Suffix, Enrolled, Acad.Prog, Department, Plan.Type, 
         Academic.Plan.Major, Acad.Level, Term.Hours, Cumulative.GPA) %>%
  filter(str_detect(Enrolled, "Enrolled")) %>%
  mutate(Enrolled = gsub("Enrolled ", "", Enrolled),
         Enrolled2 = Enrolled) %>%
  separate(Enrolled, c("Semester", "Year"), sep = ' ') %>%
  mutate(Semester = case_when(Semester == "Fall" ~ "8",
                              Semester == "Spring" ~ "2",
                              TRUE ~ NA),
         Term = paste0("2",substr(Year, 3,4),Semester),
         Acad.Prog = gsub("2", "", Acad.Prog),
         Degree = "BA") %>%
  select(-c(Semester, Year))

BSFE.data <- BSFE.data %>%
  select(First.Name, MI, Last.Name, Suffix, Enrolled, Acad.Prog, Department, Plan.Type, 
         Academic.Plan.Major, Acad.Level, Term.Hours, Cumulative.GPA) %>%
  filter(str_detect(Enrolled, "Enrolled")) %>%
  mutate(Enrolled = gsub("Enrolled ", "", Enrolled),
         Enrolled2 = Enrolled) %>%
  separate(Enrolled, c("Semester", "Year"), sep = ' ') %>%
  mutate(Semester = case_when(Semester == "Fall" ~ "8",
                              Semester == "Spring" ~ "2",
                              TRUE ~ NA),
         Term = paste0("2",substr(Year, 3,4),Semester),
         Acad.Prog = gsub("2", "", Acad.Prog),
         Degree = "BSFE") %>%
  select(-c(Semester, Year))

MINOR.data <- MINOR.data %>%
  select(First.Name, MI, Last.Name, Suffix, Enrolled, Acad.Prog, Department, Plan.Type, 
         Academic.Plan.Major, Acad.Level, Term.Hours, Cumulative.GPA) %>%
  filter(str_detect(Enrolled, "Enrolled")) %>%
  mutate(Enrolled = gsub("Enrolled ", "", Enrolled),
         Enrolled2 = Enrolled) %>%
  separate(Enrolled, c("Semester", "Year"), sep = ' ') %>%
  mutate(Semester = case_when(Semester == "Fall" ~ "8",
                              Semester == "Spring" ~ "2",
                              TRUE ~ NA),
         Term = paste0("2",substr(Year, 3,4),Semester),
         Acad.Prog = gsub("2", "", Acad.Prog),
         Degree = "MINOR",
         Cumulative.GPA = as.numeric(Cumulative.GPA)) %>%
  select(-c(Semester, Year))

core <- BS.data %>%
  bind_rows(BA.data, BSFE.data, MINOR.data) %>%
  mutate(Acad.Level = factor(Acad.Level, levels = c("Freshman", "Sophomore", "Junior", "Senior", "PostBacc")),
         T2 = as.numeric(Term)) %>%
  filter(T2 > 2052) %>%
  select(-T2)




core.agg <- core %>%
  group_by(Term, Degree, Acad.Level) %>%
  mutate(n_level = n()) %>%
  ungroup() %>%
  group_by(Term, Degree) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  select(Enrolled2, Term, Degree, Acad.Level, count, n_level) %>%
  distinct()
 

core.minor <- core %>%
  filter(Degree == "MINOR") %>%
  group_by(Term, Acad.Level, Acad.Prog) %>%
    mutate(n_prog = n()) %>%
  ungroup() %>%
  group_by(Term, Acad.Level) %>%
    mutate(n_level = n()) %>%
  ungroup() %>%
  group_by(Term) %>%
    mutate(n_minor = n()) %>%
  ungroup() %>%
  select(Term, Acad.Level, Acad.Prog, n_minor, n_prog, n_level) %>%
  distinct()

#Visualization####

#All Degrees
ggplot(core, aes(x = Term)) +
  geom_bar(aes(fill = Acad.Level))+
  labs(y= "Count",
       fill = "Class",
       title = "Total Enrollment (BS/BA/BSFE/Minor)") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(core, aes(x = Term)) +
  geom_bar(aes(fill = Degree), position = "fill")+
  labs(y= "Count",
       fill = "Degree",
       title = "Total Enrollment (BS/BA/BSFE/Minor)") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  scale_fill_discrete(labels = c("BA" = "Bachelor of Arts", "BS" = "Bachelor of Science",
                                 "BSFE" = "BS w/ Financial Economics", "MINOR" = "Minor"))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(filter(core, Degree != "MINOR"), aes(x = Term)) +
  geom_bar(aes(fill = Acad.Level))+
  labs(y= "Count",
       fill = "Class",
       title = "Total Enrollment by Majors (BS/BA/BSFE)") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(filter(core, Degree != "MINOR"), aes(x = Term)) +
  geom_bar(aes(fill = Degree))+
  labs(y= "Count",
       fill = "Degree",
       title = "Total Enrollment by Majors (BS/BA/BSFE)") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  scale_fill_discrete(labels = c("BA" = "Bachelor of Arts", "BS" = "Bachelor of Science",
                                 "BSFE" = "BS w/ Financial Economics"))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 




ggplot(filter(core, Degree == "BS"), aes(x = Term)) +
  geom_bar(aes(fill = Acad.Level))+
  labs(y= "Count",
       fill = "Class",
       title = "Total B.S. Degree Enrollment") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(filter(core, Degree == "BA"), aes(x = Term)) +
  geom_bar(aes(fill = Acad.Level))+
  labs(y= "Count",
       fill = "Class",
       title = "Total B.A. Degree Enrollment") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(filter(core, Degree == "MINOR"), aes(x = Term)) +
  geom_bar(aes(fill = Acad.Level))+
  labs(y= "Count",
       fill = "Class",
       title = "Total Minors Enrollment") +
  scale_x_discrete("Term", labels = unique(core$Enrolled2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 


