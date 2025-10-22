rm(list=ls())

library(tidyverse)

temp <- read_csv("Data/NIU_CQ_INSTRUCTORS_TERM_DEPT.csv")


factulty <- c("Ai-Ru Cheng", "Alexander Garivaltis", "Anna Klis", "Carl Campbell",
              "Eliakim Katz", "Evan Anderson", "George Slotsve", "Jeremy Groves",
              "Khan Mohabbat", "Maria Ponomareva", "Marlynne Ingram", "Neelam Jain",
              "Stephen Karlson", "Steven Nord", "Susan Porter-Hudak", "Virginia Wilcox",
              "Wei Zhang", "Masayuki Hirukawa")

instrct <- c("Bobby Barnes", "Brian Richard", "Carlos Pulido Hernandez", "Harlan Holt",
           "Ileana Brooks", "Jeffrey Reynolds", "Laurel Adams", "Manjuri Talukdar",
           "Michael Youngblood", "Paul Stroik", "Pavlo Buryi", "Rick Pretzsch",
           "Tammy Batson", "Sowjanya Dharmasankar", "Mohammad Mirhosseini",
           "Yunran Wei")

remove <- c("Beatrix Hoffman","Anne Hanley", "Andrea Smalley")

section.out <- c("0Y01", "00Y1", "H0P1", "A1H1", "B1H1", "B0H1", "P001",
                 "PYE1", "PZ00", "YE1","A001", "A002", "A003", "A004",
                 "A005", "A006", "A007", "A008", "A010", "A101", "A102",
                 "A103", "A104", "A105", "A106", "A107", "A108", "A109",
                 "B201", "B202", "B203", "B204")


no_count <- c("494", "497", "498", "495", "699", "799", "698", "795", "796",
              "496X", "390A", "393A", "690A", "692", "661A", "699A", "397H",
              "699B", "592", "798", "664", "665", "397")

core <- temp %>%
  mutate(instr = paste(`Instr_First Name`, `Instr_Last Name`, sep = " "),
         enrollment = `Tot Enrl`,
         position = case_when(instr %in% factulty ~ "Faculty",
                              instr %in% instrct ~ "Instructor",
                              instr %in% remove ~ "Remove",
                              TRUE ~ "GA Instruct")) %>%
  select(Catalog, Section, instr, Term, enrollment, position) %>%
  arrange(Term, Catalog, Section) %>%
  filter(!Catalog %in% no_count,
         !Section %in% section.out) %>%
  mutate(Catalog = case_when(Catalog == "460X" ~ "460",
                             Catalog == "484X" ~ "460",
                             Catalog == "584X" ~ "560",
                             Catalog == "650" ~ "750",
                             Catalog == "651" ~ "751",
                             Catalog == "600" ~ "700", 
                             Catalog == "601" ~ "701",
                             TRUE ~ Catalog),
                             c.num = as.numeric(Catalog),
         Level = case_when(substr(Catalog, 1, 1) == "1" ~ "100 Level",
                           substr(Catalog, 1, 1) == "2" ~ "200 Level",
                           substr(Catalog, 1, 1) == "3" ~ "300 Level",
                           substr(Catalog, 1, 1) == "4" ~ "400 Level",
                           substr(Catalog, 1, 1) == "5" ~ "500 Level",
                           substr(Catalog, 1, 1) == "6" ~ "600 Level",
                           substr(Catalog, 1, 1) == "7" ~ "700 Level",
                           TRUE ~ "Other")) %>%
  filter(enrollment > 0)

u.core <- core %>%
  filter(c.num < 500) %>%
  mutate(Inst = case_when(position == "Instructor" ~ 1,
                          TRUE ~ 0),
         fact = case_when(position == "Faculty" ~ 1,
                          TRUE ~ 0),
         ga = 1 - Inst - fact) %>%
  group_by(Term, Catalog) %>%
      mutate(Course.n = n(),
             Course.Instrctor = sum(Inst),
             Course.Faculty = sum(fact),
             Course.GA = sum(ga),
             Course.Enroll = sum(enrollment)) %>%
  ungroup() %>%
  group_by(Term, Level) %>%
  mutate(Level.n = n(),
         Level.Instrctor = sum(Inst),
         Level.Faculty = sum(fact),
         Level.GA = sum(ga),
         Level.Enroll = sum(enrollment)) %>%
  ungroup() %>%
  select(-c(Inst, fact, ga)) %>%
  select(Term, Catalog, c.num, Level, position, starts_with("Level"), starts_with("Course")) %>%
  distinct() %>%
  mutate(y = substr(as.character(Term), 2, 3),
         s = substr(as.character(Term), 4, 4),
         yr = paste0("20", y),
         se = case_when(s == "8" ~ "Fall",
                        s == "2" ~ "Spring",
                        TRUE ~ "Summer"),
         Semester = paste(se, yr, sep = " ")) %>%
  filter(s != "6") %>%
  select(-c(y, s, yr, se))


temp <- u.core %>%
  select(Semester, Term, position, starts_with("Level")) %>%
  mutate(Semester = fct_reorder(Semester, Term)) %>%
  distinct()

ggplot(temp) +
  geom_bar(aes(Semester, fill = position), position = "fill") +
  theme(axis.text.x = element_text(size = 7, angle=90))


