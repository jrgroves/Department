rm(list=ls())

library(tidyverse)

temp <- read_csv("Data/NIU_CQ_INSTRUCTORS_TERM_DEPT.csv")
load(file = "./Data/majors_minors.RData")


factulty <- c("Ai-Ru Cheng", "Alexander Garivaltis", "Anna Klis", "Carl Campbell",
              "Eliakim Katz", "Evan Anderson", "George Slotsve", "Jeremy Groves",
              "Khan Mohabbat", "Maria Ponomareva", "Marlynne Ingram", "Neelam Jain",
              "Stephen Karlson", "Steven Nord", "Susan Porter-Hudak", "Virginia Wilcox",
              "Wei Zhang", "Masayuki Hirukawa", "Mohammad Mirhosseini")

instrct <- c("Bobby Barnes", "Brian Richard", "Carlos Pulido Hernandez", "Harlan Holt",
           "Ileana Brooks", "Jeffrey Reynolds", "Laurel Adams", "Manjuri Talukdar",
           "Michael Youngblood", "Paul Stroik", "Pavlo Buryi", "Rick Pretzsch",
           "Tammy Batson", "Sowjanya Dharmasankar",
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
         Level = case_when(substr(Catalog, 1, 1) == "1" ~ "Level_100",
                           substr(Catalog, 1, 1) == "2" ~ "Level_200",
                           substr(Catalog, 1, 1) == "3" ~ "Level_300",
                           substr(Catalog, 1, 1) == "4" ~ "Level_400",
                           substr(Catalog, 1, 1) == "5" ~ "Level_500",
                           substr(Catalog, 1, 1) == "6" ~ "Level_600",
                           substr(Catalog, 1, 1) == "7" ~ "Level_700",
                           TRUE ~ "Other")) %>%
  filter(enrollment > 0) %>%
  mutate(y = substr(as.character(Term), 2, 3),
         s = substr(as.character(Term), 4, 4),
         yr = paste0("20", y),
         se = case_when(s == "8" ~ "Fall",
                        s == "2" ~ "Spring",
                        TRUE ~ "Summer"),
         Semester = paste(se, yr, sep = " "),
         Semester = fct_reorder(Semester, Term)) %>%
  filter(s != "6") %>%
  select(-c(y, s, yr, se))

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
  select(Term, Semester, Catalog, c.num, Level, position, starts_with("Level"), starts_with("Course")) %>%
  distinct()

save(core, file = "./Data/Annual Instructional.RData")

temp <- u.core %>%
  select(Semester, Term, position, starts_with("Level")) %>%
  distinct()

ggplot(temp) +
  geom_bar(aes(Semester, fill = position), position = "fill") +
  theme(axis.text.x = element_text(size = 7, angle=90))


#Combining data from instructional staffing to student majors/minors####

temp1 <- core %>%
  group_by(Semester) %>%
  mutate(position = as.factor(position)) %>%
  count(position) %>%
  ungroup() %>%
  rename("Sections" = "n")

temp2 <- core %>%
  select(Semester, instr, position) %>%
  group_by(Semester) %>%
    distinct(instr, position) %>%
    count(position) %>%
  ungroup() %>%
  rename("Unique" = "n")

demo.core <- temp1 %>%
  right_join(., temp2, by = c("Semester", "position"))

temp2 <- core %>%
  select(Semester, position, Level) %>%
  group_by(Semester)  %>%
  count(position, Level) %>%
  ungroup()  %>%
  pivot_wider(id_cols = c(Semester, position), names_from = "Level", values_from = "n") %>%
  replace(is.na(.), 0)

demo.core <- demo.core %>%
  right_join(., temp2, by = c("Semester", "position")) %>%
  filter(position != "GA Instruct")

temp3 <- core.agg %>%
  select(Enrolled2, Degree, count) %>%
  distinct() %>%
  filter(Degree != "MINOR") %>%
  group_by(Enrolled2) %>%
  summarise(Majors = sum(count)) %>%
  ungroup()

temp2 <- core.agg %>%
  select(Enrolled2, Degree, count) %>%
  distinct() %>%
  group_by(Enrolled2, Degree) %>%
    summarise(students = sum(count)) %>%
  ungroup() %>%
  pivot_wider(id_cols = (Enrolled2), names_from = Degree, values_from = students) %>%
  replace_na(list(BSFE = 0)) %>%
  rename("Minors" = "MINOR") %>%
  left_join(., temp3, by = "Enrolled2")


  

demo.core <- demo.core %>%
  right_join(., temp2, by = c("Semester" = "Enrolled2")) %>%
  filter(!is.na(position)) %>%
  mutate(a = case_when(str_split_i(Semester, " ", 1) == "Fall" ~ "8",
                       TRUE ~ "2"),
         b = substr(str_split_i(Semester, " ", 2),3,4),
         Term = as.numeric(paste0("2",b,a)),
         Semester = fct_reorder(Semester, Term)) %>%
  select(-c(a, b))

core.agg2 <- demo.core %>%
  select(Semester, BA, BS, BSFE, Majors, Minors, Term) %>%
  distinct


ggplot(core.agg2) +
  geom_point(aes(x = Semester, y = Majors, group = Majors)) +
  geom_vline(xintercept = c(3, 5, 7, 9, 10, 11, 12, 14, 15, 27, 29))+
  annotate(
    "label",
    x = c(1, 3, 5, 7, 9, 10, 11, 12, 13, 14, 15, 27, 29),
    y = max(core.agg2$Majors) + 12, # Set y to the maximum of your data's y-values
    label = c("13", "14", "12", "13", "14", "13", "11", "10", "9", "10", "10", "9"),
    vjust = 1.2,   # Adjust vertical justification to place the label just above the top edge
    hjust = 0.5,   # Center horizontally on the line
    fill = "lightblue", # Optional: adds a background color for the "textbox" effect
    color = "black"
  ) 
  
  
  
  
  
  #geom_line(aes(x = Semester, y = Unique, group = position, color = position), linewidth = 2, data = demo.core) +
  theme(axis.text.x = element_text(size = 7, angle=90))

ggplot() +
  geom_bar(aes(x = Semester, y = (students/10)), stat = "identity", data = distinct(demo.core, Semester, .keep_all = TRUE))+
  geom_line(aes(x = Semester, y = Sections, group = position, color = position), linewidth = 2, data = demo.core) +
  theme(axis.text.x = element_text(size = 7, angle=90)) 

ggplot() +
  geom_bar(aes(x = Semester, y = (students/5)), stat = "identity", data = distinct(demo.core, Semester, .keep_all = TRUE))+
  geom_bar(aes(x = Semester, y = Level_300), stat = "identity", fill = "red", data = demo.core) +
  theme(axis.text.x = element_text(size = 7, angle=90)) 

ggplot() +
  geom_bar(aes(x = Semester, y = (students)), stat = "identity", data = distinct(demo.core, Semester, .keep_all = TRUE))+
  geom_bar(aes(x = Semester, y = Level_200), stat = "identity", fill = "red", data = demo.core) +
  theme(axis.text.x = element_text(size = 7, angle=90)) 
