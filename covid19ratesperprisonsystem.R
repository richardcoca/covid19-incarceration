
# Load necessary libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# Load the data
prison_data <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/prisons/systems.csv")

# Explore the data
head(prison_data)
summary(prison_data)

# Generate Table 1
table1 <- prison_data %>% 
  select(system, inmate_tests, total_inmate_cases, total_inmate_deaths, latest_inmate_population, max_inmate_population_2020, total_officer_cases, total_officer_deaths) %>% 
  na.omit() %>% 
  summarise(
    num_prisons = n(),
    num_inmate_tests = sum(inmate_tests),
    num_inmate_cases = sum(total_inmate_cases),
    num_inmate_deaths = sum(total_inmate_deaths),
    latest_inmate_population = sum(latest_inmate_population),
    max_inmate_population_2020 = sum(max_inmate_population_2020),
    num_officer_cases = sum(total_officer_cases),
    num_officer_deaths = sum(total_officer_deaths)
  ) 

# Print Table 1
table1

# Let's visualize this data
prison_data %>% 
  filter(latest_inmate_population > 0) %>% 
  mutate(inmate_case_rate = total_inmate_cases / inmate_tests * 100) %>% 
  ggplot(aes(x = inmate_case_rate, y = system)) +
  geom_col(fill = "blue") +
  labs(title = "COVID-19 Inmate Case Rate by System",
       x = "Inmate Case Rate (%)",
       y = "System") +
  theme_minimal()

# Save the figure
ggsave("inmate_case_rate_by_system.png", dpi = 300, width = 8, height = 6)
