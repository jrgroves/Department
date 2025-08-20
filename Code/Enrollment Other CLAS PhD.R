#This is a combination of several datasets from Tableau
#1) Fall Headcount Enrollment by Department and Degree Program
#2) Weekly Enrollment  for the Upcoming Fall By Department/Plan
#For the second data set which is used to update with the current semester data, must unselect minors.

#Jeremy R. Groves
#Created: July 23, 2025

rm(list=ls())

library(tidyverse)
library(ggtext)
library(gtsummary)
library(gt)

all <- read.csv(file="./Data/CLAS majors.csv", header = TRUE, as.is = TRUE)
all2 <- read.csv(file="./Data/CLAS_majors_current.csv", header = TRUE, as.is = TRUE)

#Work data

#This data includes all levels of study.
    clas <- all %>%
      mutate(Acadept = case_when(Acadept == "GEOG" ~ "EAE",
                                 Acadept == "GEOL" ~ "EAE",
                                 TRUE ~ Acadept),
             Degname2 = case_when(substr(Degname, 1, 1) == "B" ~ "Bachelor",
                                  substr(Degname, 1, 1) == "M" ~ "Master",
                                  substr(Degname, 1, 1) =="P" ~ "Doctoral",
                                  TRUE ~ "Other")) %>%
      bind_rows(., all2) %>%
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

#We are finding a list of names of department with a doctoral program and then just an masters program

    phd <- clas %>%
      select(Acadept, Degname2) %>%
      filter(Degname2 == "Doctoral") %>%
      distinct()
    mams <- clas %>%
      select(Acadept, Degname2) %>%
      filter(Degname2 == "Master") %>%
      filter(!Acadept %in% phd$Acadept) %>%
      distinct()

#This divides the data into quartiles based on undergraduate average enrollment and then adds a highest degree in the department
  clas2 <- clas %>%
    filter(Degname2 == "Bachelor") %>%
    mutate(quartile = factor(quartile, levels = c("First", "Second", "Third", "Fourth"))) %>%
    group_by(Degname2, Acadept, Acad.Yr) %>%
    uncount(Enroll) %>%
    ungroup() %>%
    mutate(top.deg = case_when(Acadept %in% phd$Acadept ~ "Ph.D.",
                               Acadept %in% mams$Acadept ~ "M.A./M.S.",
                               TRUE ~ "B.A./B.S."))

#This starts to create the table for the undergraduate enrollment listed in quartile order and highlighted if
  #highest degree is a PHD
  
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
    
    gtsave(tab, filename = "./Graphics/Tab.png")

    
#This starts to create graphical visualizations

  #Create a dataframe for plotting with the Academic year as a factor
    p.clas <- clas %>%
      mutate(Acad.Yr = as.factor(Acad.Yr))
  
    #Graph of Undergraduate Majors by Quartile for all programs in CLAS
      ggplot(data = filter(p.clas, quartile == "First" & Degname2 == "Bachelor"), 
             aes(x = Acad.Yr)) +
        geom_line(aes(y = Enroll, group=Acadept, color = Acadept), linewidth = 1.0) +
        geom_line(data = filter(p.clas, Acadept == "ECON" & Degname2 == "Bachelor"), aes(y = Enroll, group = Acadept), 
                  linewidth = 1.5, color = "darkred")+
        annotate("text", x=2.8, y=130, label= "Economics",
                 fontface = "bold", color = "darkred") + 
        labs(x = "Academic Year",
             y = "Enrollment",
             color = "Department",
             title = "Bachelor Students by Academic Year - First Quartile") +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      ggsave("./Graphics/bach_q1.png")
      
      
      ggplot(data = filter(p.clas, quartile == "Second" & Degname2 == "Bachelor"), 
             aes(x = Acad.Yr)) +
        geom_line(aes(y = Enroll, group=Acadept, color = Acadept), linewidth = 1.0) +
        #geom_line(data = filter(p.clas, Acadept == "ECON" & Degname2 == "Bachelor"), aes(y = Enroll, group = Acadept), 
        #          linewidth = 1.5, color = "darkred")+
        annotate("text", x=2.8, y=130, label= "Economics",
                 fontface = "bold", color = "darkred") + 
        labs(x = "Academic Year",
             y = "Enrollment",
             color = "Department",
             title = "Bachelor Students by Academic Year - Second Quartile") +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      ggsave("./Graphics/bach_q2.png")
      
      
      ggplot(data = filter(p.clas, quartile == "Third" & Degname2 == "Bachelor"), 
             aes(x = Acad.Yr)) +
        geom_line(aes(y = Enroll, group=Acadept, color = Acadept), linewidth = 1.0) +
        geom_line(data = filter(p.clas, Acadept == "ECON" & Degname2 == "Bachelor"), aes(y = Enroll, group = Acadept), 
                  linewidth = 1.5, color = "darkred")+
        annotate("text", x=2.8, y=130, label= "Economics",
                 fontface = "bold", color = "darkred") + 
        labs(x = "Academic Year",
             y = "Enrollment",
             color = "Department",
             title = "Bachelor Students by Academic Year - Third Quartile") +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      ggsave("./Graphics/bach_q3.png")
      
      
      ggplot(data = filter(p.clas, quartile == "Fourth" & Degname2 == "Bachelor"), 
             aes(x = Acad.Yr)) +
        geom_line(aes(y = Enroll, group=Acadept, color = Acadept), linewidth = 1.0) +
        geom_line(data = filter(p.clas, Acadept == "ECON" & Degname2 == "Bachelor"), aes(y = Enroll, group = Acadept), 
                  linewidth = 1.5, color = "darkred")+
        annotate("text", x=3.8, y=130, label= "Economics",
                 fontface = "bold", color = "darkred") + 
        labs(x = "Academic Year",
             y = "Enrollment",
             color = "Department",
             title = "Bachelor Students by Academic Year - Fourth Quartile") +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      ggsave("./Graphics/bach_q4.png")


#For only programs with Ph.D. programs

    col <-c("ECON" = "darkred",
            "PHYS" = "lightblue",
            "CHEM" =  "darkblue",
            "EAE" = "green",
            "MATH" = "red",
            "ENGL" = "blue",
            "HIST" = "orange",
            "POLS" = "darkgray",
            "BIOS" = "black",
            "CSCI" = "purple",
            "PSYC" = "lightgray")
  
    temp<-p.clas %>%
      filter(Acad.Yr == "2015") %>%
      select(Degname2, Acadept, Enroll) %>%
      rename("base" = "Enroll")  
    
  plot.e <- p.clas %>%
    filter(Acadept == "ECON",
           Degname2 == "Doctoral") %>%
    left_join(., temp, by=c("Degname2", "Acadept")) %>%
    mutate(nor_enroll = Enroll/base)
  
  plot.d <- p.clas %>%
    filter(Acadept %in% phd$Acadept) %>%
    mutate(quartile2 = case_when(quartile == "First" ~ "First/Second",
                                 quartile == "Second" ~ "First/Second",
                                 TRUE ~ quartile)) %>%
    left_join(., temp, by=c("Degname2", "Acadept")) %>%
    mutate(nor_enroll = Enroll/base)
  
  ggplot(filter(plot.d, Degname2 == "Doctoral"), aes(x = Acad.Yr, y = Enroll)) +
    geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
    geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = filter(plot.e, Degname2 == "Doctoral")) +
    labs(x = "Academic Year",
         y = "Enrollment",
         color = "Department",
         title = "Ph.D. Enrollment Trend by Department") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  ggsave("./Graphics/phd_enroll.png")
  
  ggplot(filter(plot.d, Degname2 == "Doctoral"), aes(x = Acad.Yr, y = nor_enroll)) +
    geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
    geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = filter(plot.e, Degname2 == "Doctoral")) +
    geom_hline(aes(yintercept = 1), linewidth = 1, linetype = 'dashed') +
    labs(x = "Academic Year",
         y = "Enrollment",
         color = "Department",
         title = "Ph.D. Enrollment Trend by Department - Base Year 2015") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  ggsave("./Graphics/phd_enroll_norm.png")
  
  plot.e <- p.clas %>%
    filter(Acadept == "ECON",
           Degname2 == "Bachelor")
  

  ggplot(filter(plot.d, Degname2 == "Bachelor"), aes(x = Acad.Yr, y = Enroll)) +
    geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
    geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = plot.e) +
    labs(x = "Academic Year",
         y = "Enrollment",
         color = "Department",
         title = "Bachelor Enrollment Trend by Department with Ph.D. Programs") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  ggsave("./Graphics/bach_phd.png")
  
  
  ggplot(filter(plot.d, Degname2 == "Bachelor"), aes(x = Acad.Yr, y = Enroll)) +
    geom_line(aes(group = Acadept, color = Acadept), linewidth = 1) +
    geom_line(aes(group = Acadept),linewidth = 1.5, color = "black", data = plot.e) +
    facet_grid(~factor(quartile2, levels=c("First/Second", "Third", "Fourth"))) +
    labs(x = "Academic Year",
         y = "Enrollment",
         color = "Department",
         title = "Bachelor Enrollment Trend by Department with Ph.D. Programs") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="bottom") 
  ggsave("./Graphics/bach_phd_q.png")
  
  
  
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





