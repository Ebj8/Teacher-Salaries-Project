library(tidyverse)
library(readxl)
library(downloader)

# dedFile <- tempfile()
# download("https://nces.ed.gov/programs/digest/d17/tables/xls/tabn211.60.xls", dedFile, mode = "wb")
# teacher_salary <- read_excel(dedFile, skip = 1) %>%
#   na.omit() %>%
#   slice(3:53) %>%
#   rename(`1969-1970` = `Current dollars`, `1979-1980` = `...3`, `1989-1990` = `...4`, `1999-2000` = `...5`,
#          `2009-2010` = `...6`, `2015-2016` = `...7`, `2016-2017` = `...8`) %>%
#   select(1:8)
# 
# dedFile <- tempfile()
# download("https://nces.ed.gov/programs/digest/d17/tables/xls/tabn211.60.xls", dedFile, mode = "wb")
# adjusted_teacher_salary <- read_excel(dedFile, skip = 1) %>%
#   na.omit() %>%
#   slice(3:53) %>%
#   select(1, 9:15) 
# 
# write.csv(teacher_salary, "C:/Users/ebj800/Documents/Teacher Salaries Project/teacher_salary.csv")
# 
# write.csv(adjusted_teacher_salary, "C:/Users/ebj800/Documents/Teacher Salaries Project/adjusted_teacher_salary.csv")

starting_salary <- read_excel("C:/Users/ebj800/Documents/Teacher Salaries Project/Starting-Salaries.xlsx") %>%
  slice(1:9, 11:52)

write.csv(starting_salary, "C:/Users/ebj800/Documents/Teacher Salaries Project/Starting-Salaries.csv")

cost_of_living <- read_csv("C:/Users/ebj800/Documents/Teacher Salaries Project/cost_of_living_by_state.csv")

teacher_salary <- read_csv("C:/Users/ebj800/Documents/Teacher Salaries Project/teacher_salary.csv") %>%
  select(2:9)

adjusted_teacher_salary <- read_csv("C:/Users/ebj800/Documents/Teacher Salaries Project/adjusted_teacher_salary.csv") %>%
  select(2:9)

crime <- read_csv("http://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/estimated_crimes.csv") %>%
  filter(year == 2017) %>%
  slice(2:52) %>%
  select(1:15) %>%
  mutate(total = violent_crime + homicide + rape_revised + robbery + 
           aggravated_assault + property_crime + burglary + larceny +
           motor_vehicle_theft) %>%
  rename(State = state_name)

crime_per_100k <- crime %>%
  mutate(violent_crime = 100000 * violent_crime/population,
         homicide = 100000 * homicide/population,
         rape_revised = 100000 * rape_revised/population,
         robbery = 100000 * robbery/population,
         aggravated_assault = 100000 * aggravated_assault/population,
         property_crime = 100000 * property_crime/population,
         burglary = 100000 * burglary/population,
         larceny = 100000 * larceny/population,
         motor_vehicle_theft = 100000 * motor_vehicle_theft/population,
         total = 100000 * total/population)

write.csv(crime, "C:/Users/ebj800/Documents/Teacher Salaries Project/crime.csv")

write.csv(crime_per_100k, "C:/Users/ebj800/Documents/Teacher Salaries Project/crime_per_100k.csv")
