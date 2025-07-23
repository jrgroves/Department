rm(list = ls())

library(tidyverse)

#Load Data

  data.book <- read.csv(file = "./Data/D3.csv", header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  sheet1 <- read.csv(file = "./Data/Sheet 1_data.csv", header = TRUE, as.is = TRUE)
  AEA.degree <- read.csv(file = "./Data/AEA Degree Awarded.csv", header = TRUE, as.is = TRUE)

#Clean and Organize Data
  new <- sheet1 %>%
    rename("Program" = "Major.and.Degree") %>%
    pivot_wider(id_cols = c(Department, Program, Degree, College), names_from = Year, 
                names_prefix = "AC", values_from = Count) %>%
    mutate(Degree2 = case_when(Degree == "BA" ~ "BA",
                               Degree == "BFA" ~ "BA",
                               Degree == "BGS" ~ "BA",
                               Degree == "BM" ~ "BA",
                               Degree == "BS" ~ "BA",
                               Degree == "BSED" ~ "BA",
                               Degree == "MULTBA" ~ "BA",
                               Degree == "MULTBM" ~ "BA",
                               Degree == "MULTBS" ~ "BA",
                               Degree == "MULTSBA3" ~ "BA",
                               Degree == "EDD" ~ "Doc",
                               Degree == "EDS" ~ "Doc",
                               Degree == "JD" ~ "Doc",
                               Degree == "PHD" ~ "Doc",
                               Degree == "DPT" ~ "Doc",
                               Degree == "DNP" ~ "Doc",
                               Degree == "MA" ~ "MA",
                               Degree == "MAC" ~ "MA",
                               Degree == "MAS" ~ "MA",
                               Degree == "MAT" ~ "MA",
                               Degree == "MBA" ~ "MA",
                               Degree == "MFA" ~ "MA",
                               Degree == "MPA" ~ "MA",
                               Degree == "MM" ~ "MA",
                               Degree == "MPH" ~ "MA",
                               Degree == "MS" ~ "MA",
                               Degree == "MSED" ~ "MA",
                               Degree == "MST" ~ "MA",
                               Degree == "MSTCH" ~ "MA",
                               Degree == "MULTMM" ~ "MA",
                               Degree == "MULTMS" ~ "MA",
                               Degree == "AUD" ~ "MA",
                               Degree == "MULTMM" ~ "MA",
                               Degree == "PC" ~ "MA",
                               Degree == "SSP" ~ "MA",
                               TRUE ~ "Other"),
           across(AC2024:AC2015, ~replace_na(.x, 0)))

  temp.econ <- new %>%
    filter(Department == "ECON") %>%
    group_by(Department, Degree2) %>%
      summarise(across(AC2024:AC2015, ~sum(.x))) %>%
    ungroup() %>%
    rename("Degree" = "Degree2")
  
  univ <- new %>%
    group_by(Degree2) %>%
    summarise(across(AC2024:AC2015, ~sum(.x))) %>%
    ungroup() %>%
    rename("Degree" = "Degree2") %>%
    filter(!is.na(Degree)) %>%
    mutate(College = "NIUD")
  
  colleges <- new %>%
    group_by(College, Degree2) %>%
     summarise(across(AC2024:AC2015, ~sum(.x))) %>%
    ungroup() %>%
    rename("Degree" = "Degree2") %>%
    filter(!is.na(Degree)) %>%
    bind_rows(univ)
  
  old <- data.book %>%
    mutate(across(AC2010:AC2014, ~suppressWarnings(as.numeric(.x))),
           across(AC2010:AC2014, ~replace_na(.x, 0)),
           College = case_when(College == "Business" ~ "CBUS",
                               College == "Education" ~ "CEDU",
                               College == "Engineering" ~ "CEET",
                               College == "HHS" ~ "CHHS",
                               College == "VPA" ~ "CVPA",
                               College == "MST" ~ "CEDU",
                               College == "Law" ~ "LAW",
                               TRUE ~ College),
           Degree = case_when(Degree == "PhD" ~ "Doc",
                              TRUE ~ Degree),
           Degree = case_when(College == "LAW" ~ "Doc",
                              TRUE ~ Degree))  %>%
    filter(Department != "TOTAL")
  
  ECON <- old %>%
    filter(Department == "Economics") %>%
    left_join(., temp.econ, by = c("Degree")) %>%
    select(-c(Department.x, Department.y)) %>%
    mutate(College = "ECON")
  
  core <- old %>%
    group_by(College, Degree) %>%
      summarise(across(AC2010:AC2014, ~sum(.x))) %>%
    ungroup() %>%
    left_join(., colleges, by = c("College", "Degree")) %>%
    bind_rows(ECON) %>%
    pivot_longer(cols = AC2010:AC2015, names_to = "Year", values_to = "Count") %>%
    mutate(Year = str_remove(Year, "AC")) %>%
    arrange(Year, College, Degree) %>%
    group_by(College, Degree) %>%
      mutate(nCount = Count/max(Count),
             nCount2 = Count/Count[1],
             College2 = paste0(College, "  (", max(Count), ")"),
             College3 = paste0(College, "  (", Count[1], ")"),
             color = recode(College,
                            CBUS = "#D98880", CEDU = "#D7BDE2", CEET = "#A9CCE3", CHHS = "#A3E4D7", 
                            CLAS = "#C8102E", CVPA = "#F5CBA7", ECON = "black")) %>%
    ungroup() %>%
    mutate(nCount = case_when(is.nan(nCount) ~ 1,
                               is.infinite(nCount) ~ Count,
                               TRUE ~ nCount),
           nCount2 = case_when(is.nan(nCount2) ~ 1,
                              is.infinite(nCount) ~ Count,
                              TRUE ~ nCount2)) %>%
    filter(College != "LAW")
  
  aea <- AEA.degree
  
  
  
  
#rm(old, new, temp.econ, univ, sheet1, data.book)  


#Visualizations
    plot.data <- filter(core, Degree == "BA" & College != "NIUD")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount, group = College2, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Bacholor's Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College2),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    plot.data <- filter(core, Degree == "MA" & College != "NIUD")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount, group = College2, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Master's Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College2),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    plot.data <- filter(core, Degree == "Doc" & College != "NIUD" & College != "CHHS")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount, group = College2, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Doctoral Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College2),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

    #-------    

    plot.data <- filter(core, Degree == "BA" & College != "NIUD")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount2, group = College3, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Bacholor's Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College3),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    plot.data <- filter(core, Degree == "MA" & College != "NIUD")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount2, group = College3, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Master's Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College3),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    plot.data <- filter(core, Degree == "Doc" & College != "NIUD" & College != "CHHS")
    ggplot(plot.data) +
      geom_line(aes(x = Year, y = nCount2, group = College3, color = color), linewidth = 1.25) +
      geom_hline(yintercept = 1.0, linewidth = 1.5, color = "gray") +
      labs(title = "Doctoral Degree Awarded by NIU College",
           y = "Normalized (max) Degrees",
           x = "Academic Year",
           caption = "Data from NIU")+
      scale_color_identity(name = "College", label = unique(plot.data$College3),
                           guide = guide_legend())+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    