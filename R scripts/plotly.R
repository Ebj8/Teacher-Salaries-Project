library(tidyverse)
library(plotly)

salary <- read_csv("salary_by_city.csv") 

aggregate <- salary %>%
  group_by(State_Abbr) %>%
  summarise(avg_salary = mean(Salary),
            median = median(Salary)) %>%
  arrange(desc(avg_salary))

Salary <- left_join(salary, aggregate) %>%
  arrange(desc(median))

ggplot(aggregate) +
  geom_col(aes(reorder(State_Abbr, -avg_salary), avg_salary), fill = "skyblue")

plot_ly(aggregate, y = ~avg_salary, x = ~State_Abbr, type = "bar")%>%
  layout(title = "Average Salary by State",
         xaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~State_Abbr),
         yaxis = list(title = "Avg. Salary"))


ggplot(salary) +
  geom_boxplot(aes(reorder(State_Abbr, -Salary, FUN = median), Salary))

plot_ly(Salary, y = ~Salary, x = ~State_Abbr, type = "box")  %>%
  layout(title = "Salaries by State",
         xaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~State_Abbr))

