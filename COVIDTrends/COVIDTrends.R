library(tidyverse)
library(ggplot2)

covid_df <- read_csv("COVIDTrends/covid19.csv")

dim(covid_df)

vector_cols <- colnames(covid_df)

vector_cols

head(covid_df)

glimpse(covid_df)

covid_df_all_states <- covid_df %>% 
                       filter(Province_State == "All States") %>% 
                       select(!Province_State)

covid_df_all_states_cumulative <- covid_df_all_states %>%
                                  select(Date,
                                         Continent_Name,
                                         Two_Letter_Country_Code,
                                         positive,
                                         hospitalized,
                                         recovered,
                                         death,
                                         total_tested)

covid_df_all_states_daily <- covid_df_all_states %>% 
                             select(Date,
                                    Country_Region,
                                    active,
                                    hospitalizedCurr,
                                    daily_tested,
                                    daily_positive)

covid_df_all_states_cumulative_max <- covid_df_all_states_cumulative %>% 
    group_by(Continent_Name, Two_Letter_Country_Code) %>% 
    summarize(max_deaths = max(death)) %>% 
    filter(max_deaths > 0)

covid_df_all_states_cumulative_max

ggplot(covid_df_all_states_cumulative_max,
       aes(x = Two_Letter_Country_Code,
           y = max_deaths,
           color = Continent_Name)) +
    geom_point()

death_top_3 <- c("US", "IT", "GB")

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
    group_by(Country_Region) %>% 
    summarize(tested = sum(daily_tested),
              positive = sum(daily_positive),
              active = sum(active),
              hospitalized = sum (hospitalizedCurr)) %>% 
    arrange(desc(tested))

covid_df_all_states_daily_sum

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <-  covid_top_10$active
hospitalized_cases <-  covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

positive_cases/tested_cases

positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)

united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)

colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

covid_mat

question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)

datasets <- list(
    original = covid_df,
    allstates = covid_df_all_states,
    daily = covid_df_all_states_daily,
    top_10 = covid_top_10
)

matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)

data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)

covid_analysis_list <- list(question, answer, data_structure_list)
