library(tidyverse)
library(scales)
library(ggtext)
library(patchwork)
library(glue)


# To get from Qualtrics, go to the "Data and Analysis" tab, click "Export and Import", "Export data," 
# and then click the download button, leave all settings the same (export labels, etc.)
# The file should save as a zip file, unzip it and rename the csv as mammograms.csv

# you will update the date in line 19 and 23, 75, and 77

# data ----
qu_raw <- read_csv("mammograms.csv") %>%
  slice(-c(1:2)) %>%
  mutate(Q1 = as.numeric(Q1),
         Q2 = as.numeric(Q2)/100) |> 
  filter(StartDate > as.Date("01-01-2025", format = "%m-%d-%Y"))

qu_raw_2021 <- qu_raw %>% mutate(
  year = as.numeric(substr(qu_raw$StartDate,1,4))) %>% 
  filter(year == 2025)

# Can you please let me know what percent of students in each group 
# said the cancer rate was 50% or more?



# graphs 2021 ----
gg_q1_2021 <- ggplot(qu_raw_2021, aes(Q1)) +
  geom_histogram(aes(y = stat(width*density)),
                 color = "white",
                 binwidth = 0.02) +
  theme_gray() +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0)) +
  scale_x_continuous(breaks = c(0,0.09, 0.25, 0.5, 0.75, 1),
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
  scale_x_continuous(breaks = c(0,0.09, 0.25, 0.5, 0.75, 1),
                     limits = c(0,1),
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
ggsave("mammogram_experiment_dist_2025.pdf",
       w = 6, h = 3.2)
ggsave("mammogram_experiment_dist_2025.png",
       dpi = 400,
       w = 6, h = 3.2)



# Can you please let me know what percent of students in each group 
# said the cancer rate was 50% or more?

q1 <- qu_raw_2021 |> filter(!is.na(Q1)) |> 
  select(StartDate, Q1) |>
  mutate(q1_50pct = (Q1 >= .5)) |>
  summarise(ct = sum(q1_50pct),
            q1_50pct = mean(q1_50pct))

q2 <- qu_raw_2021 |> filter(!is.na(Q2)) |> 
  select(StartDate, Q2) |>
  mutate(q2_50pct = (Q2 >= .5)) |>
  summarise(ct = sum(q2_50pct),
            q2_50pct = mean(q2_50pct))

