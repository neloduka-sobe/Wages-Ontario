library(tidyverse)
library(ggplot2)

wages = read.csv("wages.csv") %>% mutate_if(is.character, str_trim)

wages$Education.level =
  factor(wages$Education.level,
         levels = c("Above bachelor's degree",
                    "Bachelor's degree",
                    "University certificate below bachelors degree",
                    "University degree",
                    "Community college, CEGEP",
                    "Trade certificate or diploma",
                    "Post-secondary certificate or diploma",
                    "Some post-secondary",
                    "High school graduate",
                    "Some high school",
                    "PSE  (5,6,7,8,9))",
                    "No PSE  (0,1,2,3,4)",
                    "0 - 8  years",
                    "Total, all education levels"),
         ordered = TRUE)

wages$Age.group =
  factor(wages$Age.group,
         levels = c("25-64 years",
                    "25-54 years",
                    "25-34 years",
                    "20-34 years",
                    "15-24 years",
                    "55 years and over",
                    "25 years and over",
                    "15 years and over"),
         ordered = TRUE)

# Fuel
fuel = read.csv("fuel.csv") %>% mutate_if(is.character, str_trim)


fuel.year = fuel %>%
  mutate(Year = year(as.Date(Date))) %>%
  group_by(Year) %>%
  summarise(Avg.Price = mean(Ontario.Average.Moyenne.provinciale))

wages.year = wages %>%
  filter(Geography == 'Ontario',
         Wages == 'Average hourly wage rate',
         Type.of.work == 'Both full- and part-time',
         Education.level == 'Total, all education levels',
         Age.group == '15 years and over') %>%
  select(YEAR, Both.Sexes)


merged_data <- merge(wages.year, fuel.year, by.x = "YEAR", by.y = "Year")
# fuel vs wage
ggplot(merged_data, aes(x = Both.Sexes, y = Avg.Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Average wage", 
       y = "Average fuel price", 
       title = "Relationship between Wages and Fuel Price")

# fuel and wage vs year
ggplot(merged_data, aes(x = YEAR)) +
  geom_line(aes(y = Both.Sexes, color = "Both Sexes"), size = 1.5) +
  geom_line(aes(y = Avg.Price, color = "Avg. Price")) +
  # scale_y_continuous(sec.axis = sec_axis(~./0.1, name = "Avg. Price")) +
  labs(x = "Year", y = "Both Sexes", color = "Legend")

kable(avg_wage_by_education %>%
  mutate(pay.gap = male_avg - female_avg))





