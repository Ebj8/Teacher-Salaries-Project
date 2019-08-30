library(tidyverse)

adjusted_teacher_salary <- read_csv("adjusted_teacher_salary.csv") %>%
  select(2, 9) %>%
  rename(avg_salary = `2016-2017`)

Climate_data <- read_csv("Climate_data.csv")

Climate_data_month <- read_csv("Climate_data_month.csv") %>%
  select(2:11)

common_core <- read_csv("Common-Core.csv")

cost_of_living <- read_csv("cost_of_living_by_state.csv")

crime <- read_csv("crime_per_100k.csv") %>%
select(2:17)

starting_salary <- read_csv("Starting-Salaries.csv") %>%
  select(2:3) %>%
  rename(starting_salary = `Avg. Starting Salary`)

regions <- read_csv("states_by_region.csv") %>%
  select(2, 5) %>%
  rename(Region = `Census Region`)

teacher_dataset <- left_join(adjusted_teacher_salary, Climate_data_month) %>%
  left_join(., common_core) %>%
  left_join(., cost_of_living) %>%
  left_join(., crime) %>%
  left_join(., starting_salary) %>%
  left_join(., regions)

write_csv(teacher_dataset, "teacher_dataset.csv")
