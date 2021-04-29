library(tidyverse)
library(janitor)
library(ggplot2)
library(ggthemes)
library(dplyr)



# Import Data -------------------------------------------------------------


actual_vs_projected <- read_csv("~/Desktop/NBA DFS Project/Raw Data/NBA_DFS_Master.csv") %>% 
  clean_names()


# Tidy Data ---------------------------------------------------------------

a_v_p <- actual_vs_projected %>% 
  select(dkp, dk_sal, minutes) %>% 
  drop_na() %>% 
  mutate(salary = dk_sal/1000,
         actual_points = dkp,
         proj_points = salary * 5.7) %>% 
  mutate(salary = round(salary, digits = 1),
         minutes = round(minutes, digits = 0)) %>% 
  select(salary, minutes, actual_points, proj_points) %>%
  filter(minutes >= 16) %>% 
  group_by(salary) %>% 
  summarize(total_minutes = sum(minutes),
            total_points = sum(actual_points),
            total_proj = sum(proj_points),
            total_salary = sum(salary),
            proj_ppm = total_proj/total_minutes,
            act_ppm = total_points/total_minutes,
            act_pps = total_points/total_salary) %>% 
  mutate(act_ppm = round(act_ppm, digits = 2),
         act_pps = round(act_pps, digits = 2))

ggplot(data = a_v_p,
       mapping = aes(x = salary,
                     y = act_pps,
                     fill = act_pps)) +
  
  geom_col() +
  
  scale_y_continuous(limits = c(0, 7 ),
                     breaks = seq(from = 0, to = 7, by = 1))  +
  
  scale_x_continuous(limits = c(3, 11),
                     breaks = seq(from = 3, to = 11, by = .5)) +
  
  labs(title = "Points Per Salary",
       x = "Salary (000's)",
       y = "Point Per Salary",
       fill = "") +
  
  theme_gdocs() 

a_v_p %>% 
  count(act_pps >= 5.00) %>% 
  mutate(pct = n/sum(n))

write_rds(a_v_p,
          path = "data/a_v_p.rds")


  
  
  

    

  






