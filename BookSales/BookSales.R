library(tidyverse)

book_df <- read_csv("BookSales/book_reviews.csv")

glimpse(book_data)

check_unique_or_range <- function(column) {
    if (is.numeric(column)) {
        return(range(column, na.rm = TRUE))
    } else {
        return(unique(column))
    }
}

result <- lapply(book_df, check_unique_or_range)

print(result)

for (col_name in colnames(book_df)){
    missing_count <- sum(is.na(book_df[[col_name]]))
    if (missing_count > 0) {
        cat("Column", col_name, "has", missing_count, "missing values\n")
    }
}

book_df_clean <- book_df %>% filter(!is.na(review))

book_df_clean_2 <- book_df %>% filter(!if_any(everything(), is.na))
