#Enrollment

rm(list=ls())

library(tidyverse)

fall <- read.csv(file="./Data/Sheet 4.csv", header = TRUE, as.is = TRUE)
spring <- read.csv(file="./Data/Totals Table.csv", header = TRUE, as.is = TRUE)

sch <- read.csv(file="./Data/SCH.csv", header = TRUE, as.is = TRUE)

order.1 <- c("Fall.2022", "Spring.2023", "Fall.2023", "Spring.2024", "Fall.2024", "Spring.2025")
order.2 <- c("Freshman", "Sophomore", "Junior", "Senior", "Masters", "Doctoral", "Total")
order.3 <- c("FALL2018", "SPRING2019", "FALL2019", "SPRING2020", "FALL2020", "SPRING2021", 
             "FALL2021", "SPRING2022", "FALL2022", "SPRING2023", "FALL2023", "SPRING2024",
             "FALL2024", "SPRING2025")
order.4 <- c("100", "200", "300", "400", "500", "600", "700", "Total")

core <- fall %>%
  left_join(., spring, by=c("Class", "Cat")) %>%
  mutate(Class = case_when(str_split_i(Class, " ", 1) == "Graduate" ~ (str_split_i(Class, " ", 2)),
                           str_split_i(Class, " ", 1) == "Grand" ~ (str_split_i(Class, " ", 2)),
                           TRUE ~ str_split_i(Class, " ", 1)),
         Class = factor(Class, levels = order.2),
         Measure = case_when(Cat == "Number Enrolled" ~ "Enrolled",
                             Cat == "Enrolled Credit Hours" ~ "Hours",
                             TRUE ~ ""),
         Measure = as.factor(Measure))%>%
  select(-Cat) %>%
  pivot_longer(cols = -c(Class, Measure), names_to = "semester", values_to = "Count") %>%
  mutate(Count = as.numeric(gsub(",", "", Count)))%>%
  arrange(Measure, match(semester, order.1))

core.sch <- sch %>%
  pivot_longer(cols = -Semester, names_to = "Course.Level", values_to = "Hours")%>%
  mutate(Course.Level = gsub("Level.", "", Course.Level),
         Semester = factor(Semester, levels = order.3)) %>%
  arrange(match(Semester, order.3))

##Plots#####



plot.1 <- core %>%
  filter(Measure == "Enrolled") %>%
  mutate(Semester = factor(semester, levels = order.1)) %>%
  ggplot() + 
    geom_line(aes(x = Semester, y = Count, group = Class, colour = Class), linewidth = 1.25) +
    theme_bw()

plot.2 <- core %>%
  filter(Measure == "Enrolled") %>%
  filter(Class != "Total") %>%
  mutate(Semester = factor(semester, levels = order.1)) %>%
  ggplot() + 
  geom_line(aes(x = Semester, y = Count, group = Class, colour = Class), linewidth = 1.25) +
  theme_bw()
  
plot.3 <- core %>%
  filter(Measure == "Hours") %>%
  filter(Class == "Total") %>%
  mutate(Semester = factor(semester, levels = order.1)) %>%
  ggplot() + 
  geom_line(aes(x = Semester, y = Count, group = Class, colour = Class), linewidth = 1.25) +
  theme_bw()

plot.4 <- core %>%
  filter(Measure == "Hours") %>%
  filter(Class != "Total") %>%
  mutate(Semester = factor(semester, levels = order.1)) %>%
  ggplot() + 
  geom_line(aes(x = Semester, y = Count, group = Class, colour = Class), linewidth = 1.25) +
  theme_bw()

plot.5 <- core.sch %>%
  ggplot() +
  geom_rect(aes(xmin=4, xmax=7, ymin=0, ymax=Inf),fill = "gray92", alpha = 0.15) +
  geom_line(aes(x = Semester, y = Hours, group = Course.Level, colour = Course.Level), linewidth = 1.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plot.6 <- core.sch %>%
  filter(Course.Level != "Total",
         Course.Level != "200",
         Course.Level != "100") %>%
  ggplot() +
  geom_rect(aes(xmin=4, xmax=7, ymin=0, ymax=Inf),fill = "gray92", alpha = 0.15) +
  geom_line(aes(x = Semester, y = Hours, group = Course.Level, colour = Course.Level), linewidth = 1.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plot.7 <- core.sch %>%
  filter(Course.Level != "Total",
         #Course.Level != "200",
         Course.Level != "600",
         Course.Level != "700") %>%
  ggplot() +
  geom_rect(aes(xmin=4, xmax=7, ymin=0, ymax=Inf),fill = "gray92", alpha = 0.15) +
  geom_line(aes(x = Semester, y = Hours, group = Course.Level, colour = Course.Level), linewidth = 1.25) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

