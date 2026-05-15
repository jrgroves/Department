


#Visualizations for ECON within CLAS

temp <- core %>%
  group_by(Term, College) %>%
  mutate(Med = median(Count),
         Mean = mean(Count)) %>%
  ungroup()  %>%
  filter(Department == "ECON") 

s.temp1 <- temp %>%
  select(Term, College, Med) %>%
  mutate(College = "LAS Median") %>%
  rename("Department" = "College",
         "Count" = "Med")
s.temp2 <- temp %>%
  select(Term, College, Mean) %>%
  mutate(College = "LAS Mean") %>%
  rename("Department" = "College",
         "Count" = "Mean")

temp <- temp %>%
  select(Term, Department, Count) %>%
  bind_rows(., s.temp1, s.temp2)
rm(s.temp1, s.temp2)


ggplot(temp) + 
  geom_line(aes(x = Term, y = Count, color = Department, group = Department), linewidth = .75) +
  labs(title = "10-Day Head Count by Term") +
  theme_bw() +
  theme(legend.position = "bottom")
