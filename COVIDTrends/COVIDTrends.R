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

covid_mat <- cbind(tested_cases, positive_cases, active_cases, hospitalized_cases)

population <- c(331002651, 145934462, 60461826, 1380004385, 84339067, 37742154, 67886011, 25499884, 32971854, 37846611)

covid_mat <- covid_mat * 100 / population

covid_mat

tested_cases_rank <- rank(covid_mat[,"tested_cases"])
positive_cases_rank <- rank(covid_mat[,"positive_cases"])
active_cases_rank <- rank(covid_mat[,"active_cases"])
hospitalized_cases_rank <- rank(covid_mat[,"hospitalized_cases"])

covid_mat_rank <- rbind(tested_cases_rank, 
                        positive_cases_rank, 
                        active_cases_rank, 
                        hospitalized_cases_rank)

print(covid_mat_rank)

covid_mat_rank_trimmed <- covid_mat_rank[-1, ]

aggregated_rankings <- colSums(covid_mat_rank_trimmed)

aggregated_rankings

best_effort_tested_cases_top_3 <- names(sort(covid_mat_rank["tested_cases_rank", ], decreasing = TRUE)[1:3])

most_affected_country <- names(which.max(aggregated_rankings))

least_affected_country <- names(which.min(aggregated_rankings))

question_list <- list(
    "Which countries have had the highest number of deaths due to COVID-19?",
    "Which countries have had the highest number of positive cases against the number of tests?",
    "Which countries have made the best effort in terms of the number of COVID-19 tests conducted related to their population?",
    "Which countries were ultimately the most and least affected related to their population?"
)

answer_list <- list(
    "Death" = death_top_3,
    "Positive tested cases" = positive_tested_top_3,
    "The best effort in test related to the population" = best_effort_tested_cases_top_3,
    "The most affected country related to its population" = most_affected_country,
    "The least affected country related to its population" = least_affected_country
)

dataframes_list <- list(covid_df, covid_df_all_states, covid_df_all_states_cumulative, covid_df_all_states_daily)
matrices_list <- list(covid_mat, covid_mat_rank)
vectors_list <- list(vector_cols, population, countries)

data_structure_list <- list(
    "Dataframes" = dataframes_list,
    "Matrices" = matrices_list,
    "Vectors" = vectors_list
)

covid_analysis_list <- list(
    "Questions" = question_list,
    "Answers" = answer_list,
    "Data Structures" = data_structure_list
)
