library(tidyverse)
library(scales)
library(ggtext)
library(patchwork)
library(glue)


# data ----
qu_raw <- read_csv("mammograms_2023.csv") %>%
  slice(-c(1:2)) %>%
  mutate(Q1 = as.numeric(Q1),
         Q2 = as.numeric(Q2)/100) |> 
  filter(StartDate > as.Date("01-01-2023", format = "%m-%d-%Y"))

qu_raw_2021 <- qu_raw %>% mutate(
  year = as.numeric(substr(qu_raw$StartDate,1,4))) %>% 
  filter(year == 2023)

# graphs 2021 ----
gg_q1_2021 <- ggplot(qu_raw_2021, aes(Q1)) +
  geom_histogram(aes(y = stat(width*density)),
                 color = "white",
                 binwidth = 0.02) +
  theme_gray() +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0)) +
  scale_x_continuous(breaks = c(0.09, 0.25, 0.5, 0.75, 1),
                     limits = c(0,1),
                     labels = percent_format(accuracy = 1),
                     expand = c(0.04, 0)) +
  labs(x = "Guess for Pr(Cancer | +)",
       y = "Proportion of students",
       title = "Students given the<br>**Probability** Prompt",
       caption = glue("n = {sum(!is.na(qu_raw_2021$Q1))}")) +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      lineheight = 1.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank(),
        axis.text = element_text(color = "black"))


gg_q2_2021 <- ggplot(qu_raw_2021, aes(Q2)) +
  geom_histogram(aes(y = stat(width*density)),
                 color = "white",
                 binwidth = 0.02) +
  theme_gray() +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0)) +
  scale_x_continuous(limits =  c(0, 1),
                     labels = percent_format(accuracy = 1),
                     expand = c(0.04, 0)) +
  labs(x = "Guess for Pr(Cancer | +)",
       y = "Proportion of students",
       title = "Students given the<br>**Count** Prompt",
       caption = glue("n = {sum(!is.na(qu_raw_2021$Q2))}")) +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      lineheight = 1.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.x =  element_blank(),
        axis.text = element_text(color = "black"))


# save ----
gg_q1_2021 + gg_q2_2021
ggsave("mammogram_experiment_dist_2023.pdf",
       w = 6, h = 3.2)
ggsave("mammogram_experiment_dist_2023.png",
       dpi = 400,
       w = 6, h = 3.2)

