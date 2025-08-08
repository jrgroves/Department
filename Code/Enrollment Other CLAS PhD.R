#Jeremy R. Groves
#Created: July 23, 2025

rm(list=ls())

library(tidyverse)

phd <- read.csv(file="./Data/PhD Enroll CLAS.csv", header = TRUE, as.is = TRUE)
ba <- read.csv(file="./Data/BA Enroll CLAS.csv", header = TRUE, as.is = TRUE)


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
  scale_y_continuous(breaks=seq(0,70,10)) +
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
