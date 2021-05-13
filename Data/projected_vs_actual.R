
# import libraries --------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(hrbrthemes)
library(ggtext)


# import data -------------------------------------------------------------

proj_vs_act <- read_csv("Raw Data/NBA_DFS_Master.csv") %>% 
  clean_names()

# proj_vs_act_1 -----------------------------------------------------------

proj_vs_act1 <- proj_vs_act %>% 
  select(first_last, dk_sal, minutes, dkp) %>% 
  drop_na() %>% 
  mutate(salary = dk_sal / 1000,
         projected_points = 6 * salary,
         actual_points = dkp,
         p_vs_a = actual_points - projected_points) %>% 
  select(-dk_sal, -dkp) %>% 
  group_by(salary) %>% 
  mutate(pct_pos = if_else(p_vs_a >= 0,
                           "fire",
                           "ice")) %>% 
  count(pct_pos) %>% 
  mutate(percent_fire = n / sum(n)) %>% 
  filter(pct_pos == "fire") %>% 
  mutate(percent_fire = round(percent_fire, digits = 2))

ggplot(data = proj_vs_act1, 
       mapping = aes(x = salary,
                     y = percent_fire,
                     fill = ifelse(percent_fire >= .2, "Outperform", "Underperform"))) +
  geom_col() +
  
  coord_flip() + 
  
  
  
  scale_y_continuous(limits = c(0, .5),
                     breaks = seq(from = 0, to = .5, by = .05))  +
  
  scale_x_continuous(limits = c(3, 11.5),
                     breaks = seq(from = 3, to = 11.5, by = .5)) +
  
  labs(title = "Percent Of Players That Outperform Their Salary (6x) Is Non-Linear",
       x = "Salary",
       y = "Percent of Salary Outperformance",
       fill = "") +
  
  theme_ipsum(axis_title_size = 0,
              grid = "XY") 















