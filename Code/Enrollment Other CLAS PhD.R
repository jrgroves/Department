#Jeremy R. Groves
#Created: July 23, 2025

rm(list=ls())

library(tidyverse)
library(gtsummary)

phd <- read.csv(file="./Data/PhD Enroll CLAS.csv", header = TRUE, as.is = TRUE)
ba <- read.csv(file="./Data/BA Enroll CLAS.csv", header = TRUE, as.is = TRUE)
all <- read.csv(file="./Data/CLAS majors.csv", header = TRUE, as.is = TRUE)


#Work data

core <- phd %>%
  mutate(Acadept = case_when(Acadept == "GEOG" ~ "EAE",
                             Acadept == "GEOL" ~ "EAE",
                             TRUE ~ Acadept)) %>%
  summarize(Enroll = sum(Count.of.OSIR), .by = c(Acadept, Acad.Yr)) %>%
  mutate(Degree = "PhD")

temp <- ba %>%
  mutate(Acadept = case_when(Acadept == "GEOG" ~ "EAE",
                             Acadept == "GEOL" ~ "EAE",
                             TRUE ~ Acadept)) %>%
  filter(!str_detect(Major, "Secondary Teaching ")) %>%
  summarize(Enroll = sum(Count.of.OSIR), .by = c(Acadept, Acad.Yr)) %>%
  mutate(Degree = "Bachelor") %>%
  group_by(Acadept) %>%
    mutate(mEnroll = max(Enroll)) %>%
  ungroup

temp1 <- temp %>%
  filter(Acad.Yr == "2015") %>%
  rename("bEnroll" = "Enroll") %>%
  select(Acadept, bEnroll) %>%
  right_join(., temp, by = "Acadept") %>%
  mutate(bEnroll = Enroll/bEnroll,
         mEnroll = Enroll/mEnroll)

core <- core %>%
  bind_rows(., temp1) %>%
  mutate(Acad.Yr = factor(as.character(Acad.Yr)))

temp <- core %>%
  filter(Acad.Yr=="2024",
         Degree == "Bachelor") %>%
  arrange(Enroll) %>%
  mutate(QR = factor(case_when(Enroll < 80 ~ "1",
                        Enroll >=80 & Enroll < 140 ~ "2",
                        Enroll >=140 & Enroll < 285.8 ~ "3",
                        Enroll > 285 ~ "4",
                        TRUE ~ "NOPE"))) %>%
  select(Acadept, QR) %>%
  mutate(QR2 = case_match(QR, "1" ~ "First", "2" ~ "Second", "3" ~ "Third", "4" ~ "Fourth"))


core <- core %>%
  left_join(., temp, by = "Acadept")

rm(temp, temp1)

#All CLAS majors



ggplot(data = filter(clas, quartile == "First" & Degname2 == "Bachelor"), 
       aes(x = Acad.Yr)) +
  geom_line(aes(y = Enroll, group=Acadept, color = Acadept))

ggplot(data = filter(clas, quartile == "Second" & Degname2 == "Bachelor"), 
       aes(x = Acad.Yr)) +
  geom_line(aes(y = Enroll, group=Acadept, color = Acadept))

ggplot(data = filter(clas, quartile == "Third" & Degname2 == "Bachelor"), 
       aes(x = Acad.Yr)) +
  geom_line(aes(y = Enroll, group=Acadept, color = Acadept))



#Plot Data and Visualizations

col <-c("ECON" = "black",
        "FWLC" = "cyan",
        "PHYS" = "lightblue",
        "CHEM" =  "darkblue",
        "EAE" = "green",
        "MATH" = "red",
        "ENGL" = "blue",
        "HIST" = "orange",
        "POLS" = "darkgray",
        "BIOS" = "darkred",
        "CSCI" = "purple",
        "PSYC" = "lightgray")

plot.e <-core %>%
  filter(Acadept == "ECON",
         Degree == "PhD")

for(i in seq(2,4,1)) {
  temp <- plot.e %>%
    mutate(QR = as.character(i))
  ifelse(i == 2,
         TEMP <-temp,
         TEMP <- bind_rows(TEMP, temp))
}
plot.e <- plot.e %>%
  bind_rows(TEMP) %>%
  mutate(QR2 = case_match(QR, "1" ~ "First", "2" ~ "Second", "3" ~ "Third", "4" ~ "Fourth"))

plot.d <- core %>%
  filter(Degree == "PhD")

ggplot(plot.d, aes(x = Acad.Yr, y = Enroll)) +
  geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
  geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = plot.e) +
  facet_wrap(~QR2, dir = "v") +
  scale_color_manual(values = col) + 
  #scale_y_continuous(breaks=seq(0,70,10)) +
  labs(x = "Academic Year",
       y = "Enrollment",
       color = "Department",
       title = "Ph.D. Enrollment Trend by Department Size (Undergrad) Quartiles") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 






plot.d <- core %>%
  filter(Degree == "Bachelor") %>%
  mutate(QR2 = case_match(QR, "1" ~ "First",
                             "2" ~ "Second", 
                             "3" ~ "Third", 
                             "4" ~ "Fourth")) %>%
  arrange(QR)

plot.e <- plot.d %>%
  filter(Acadept == "ECON")

    for(i in seq(2,4,1)) {
      temp <- plot.e %>%
        mutate(QR = as.character(i))
      ifelse(i == 2,
             TEMP <-temp,
             TEMP <- bind_rows(TEMP, temp))
    }

plot.e <- plot.e %>%
  bind_rows(TEMP) %>%
  mutate(QR2 = case_match(QR, "1" ~ "First", "2" ~ "Second", "3" ~ "Third", "4" ~ "Fourth"))

rm(temp, TEMP)

ggplot(filter(plot.d, QR!="4"), aes(Acad.Yr, Enroll)) +
  geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
  geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = filter(plot.e, QR!="4")) +
  facet_wrap(~QR2) +
  scale_color_manual(values = col) + 
  scale_y_continuous(breaks=seq(0,300, 50)) +
  labs(x = "Academic Year",
     y = "Enrollment",
     color = "Department",
     title = "Undergraduate Enrollment Trend by Department Size Quartiles") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") 

ggplot(plot.d, aes(Acad.Yr, bEnroll)) +
  geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
  geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = plot.e) +
  geom_hline(yintercept = 1.0, color = "darkgray", linewidth = 1.25, linetype = "dashed") +
  facet_wrap(~QR2, dir = "v") +
  scale_color_manual(values = col) + 
  scale_y_continuous(breaks=seq(0,300, 50)) +
  labs(x = "Academic Year",
       y = "Enrollment",
       color = "Department",
       title = "Undergraduate Enrollment Trend by Department Size Quartiles",
       caption = "Base Year = 2015") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom")

ggplot(plot.d, aes(Acad.Yr, mEnroll)) +
  geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
  geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = plot.e) +
  geom_hline(yintercept = 1.0, color = "darkgray", linewidth = 1.25, linetype = "dashed") +
  facet_wrap(~QR2, dir = "v") +
  scale_color_manual(values = col) + 
  scale_y_continuous(breaks=seq(0,300, 50)) +
  labs(x = "Academic Year",
       y = "Enrollment",
       color = "Department",
       title = "Undergraduate Enrollment Trend by Department Size Quartiles",
       caption = "Base Year = Max Enrollment") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom")

library(gt)



clas <- all %>%
  mutate(Acadept = case_when(Acadept == "GEOG" ~ "EAE",
                             Acadept == "GEOL" ~ "EAE",
                             TRUE ~ Acadept),
         Degname2 = case_when(substr(Degname, 1, 1) == "B" ~ "Bachelor",
                              substr(Degname, 1, 1) == "M" ~ "Master",
                              substr(Degname, 1, 1) =="P" ~ "Doctoral",
                              TRUE ~ "Other")) %>%
  filter(Acadept != "CLAS",
         Degname2 != "Other") %>%
  summarize(Enroll = sum(Count.of.OSIR), .by = c(Degname2, Acadept, Acad.Yr)) %>%
  group_by(Degname2, Acadept) %>%
  mutate(dep.avg = mean(Enroll)) %>%
  ungroup() %>%
  group_by(Degname2) %>%
  mutate(quartile = case_when(dep.avg <= quantile(dep.avg, probs=0.25) ~ "First",
                              dep.avg > quantile(dep.avg, probs=0.25) &
                                dep.avg <= quantile(dep.avg, probs=0.50) ~ "Second",
                              dep.avg > quantile(dep.avg, probs=0.50)  &
                                dep.avg <= quantile(dep.avg, probs=0.75) ~ "Third",
                              TRUE ~ "Fourth")) %>%
  ungroup() %>%
  mutate(quartile = case_when(Degname2 == "Master" ~ NA,
                              Degname2 == "Doctoral" ~ NA,
                              TRUE ~ quartile)) %>%
  group_by(Acadept)%>%
  fill(quartile, .direction = "downup") %>%
  ungroup()

temp <- clas %>%
  select(Acadept, Degname2) %>%
  filter(Degname2 == "Doctoral") %>%
  distinct()
temp2 <- clas %>%
  select(Acadept, Degname2) %>%
  filter(Degname2 == "Master") %>%
  filter(!Acadept %in% temp$Acadept) %>%
  distinct()

clas2 <- clas %>%
  filter(Degname2 == "Bachelor") %>%
  mutate(quartile = factor(quartile, levels = c("First", "Second", "Third", "Fourth"))) %>%
  group_by(Degname2, Acadept, Acad.Yr) %>%
  uncount(Enroll) %>%
  ungroup() %>%
  mutate(top.deg = case_when(Acadept %in% temp$Acadept ~ "Ph.D.",
                             Acadept %in% temp2$Acadept ~ "M.A./M.S.",
                             TRUE ~ "B.A./B.S."))

nd <- clas2 %>%
  tibble()%>%
  select(Acadept, quartile, dep.avg, top.deg) %>%
  distinct() %>%
  rename("label" = "Acadept")

tab <- clas2%>%
  filter(Degname2 == "Bachelor") %>%
  tbl_summary(.,
              include = c(Acadept),
              by = c(Acad.Yr)) %>%
  modify_table_body(~ .x %>%
                      left_join(nd, by = "label"))%>%
  modify_header(quartile ~ "Size Quartile",
                dep.avg ~ "Department Average") %>%
  modify_table_body(fun = ~ dplyr::arrange(.x, quartile, dep.avg)) %>%
  modify_table_styling(
    columns = everything(), # Or the column containing the values you want to bold
    rows = ifelse(top.deg == "Ph.D.", TRUE, FALSE), # Your condition
    text_format = "bold") %>%
  modify_header(label = "Department") %>%
  modify_caption(caption = "<div style='font-size: 28px;'> Majors/Minors by Department for CLAS </div>") %>%
  as_gt() %>%
  tab_style(
    style = list(cell_text(color = "red")),
    locations = cells_body(row = ifelse(label == "ECON",TRUE,FALSE))) 

gtsave(tab, filename = "./Tab.png")

