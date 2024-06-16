Investigating COVID-19 Virus Trends
================
Christopher Letton
2024-06-15

## Introduction

This project evaluates the COVID-19 data from 20th January to the 1st
June 2020. My analysis will try to answer the question of which
countries reported the highest number of positive cases in relation to
the number of tests conducted.

# Understanding the Data

## Loading the dataset

``` r
library(readr)
covid_df <- read_csv("covid19.csv")
```

    ## Rows: 10903 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Continent_Name, Two_Letter_Country_Code, Country_Region, Province_...
    ## dbl  (9): positive, hospitalized, recovered, death, total_tested, active, ho...
    ## date (1): Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# dimension of dataframe
dim(covid_df)
```

    ## [1] 10903    14

``` r
# store column names in a variable
vector_cols <- colnames(covid_df)
vector_cols
```

    ##  [1] "Date"                    "Continent_Name"         
    ##  [3] "Two_Letter_Country_Code" "Country_Region"         
    ##  [5] "Province_State"          "positive"               
    ##  [7] "hospitalized"            "recovered"              
    ##  [9] "death"                   "total_tested"           
    ## [11] "active"                  "hospitalizedCurr"       
    ## [13] "daily_tested"            "daily_positive"

``` r
# display first few rows of dataframe
head(covid_df)
```

    ## # A tibble: 6 × 14
    ##   Date       Continent_Name Two_Letter_Country_C…¹ Country_Region Province_State
    ##   <date>     <chr>          <chr>                  <chr>          <chr>         
    ## 1 2020-01-20 Asia           KR                     South Korea    All States    
    ## 2 2020-01-22 North America  US                     United States  All States    
    ## 3 2020-01-22 North America  US                     United States  Washington    
    ## 4 2020-01-23 North America  US                     United States  All States    
    ## 5 2020-01-23 North America  US                     United States  Washington    
    ## 6 2020-01-24 Asia           KR                     South Korea    All States    
    ## # ℹ abbreviated name: ¹​Two_Letter_Country_Code
    ## # ℹ 9 more variables: positive <dbl>, hospitalized <dbl>, recovered <dbl>,
    ## #   death <dbl>, total_tested <dbl>, active <dbl>, hospitalizedCurr <dbl>,
    ## #   daily_tested <dbl>, daily_positive <dbl>

``` r
# display a global view of dataset - glimpse provides the combined output of dim(), colnames(), and head()
library(tibble)
glimpse(covid_df)
```

    ## Rows: 10,903
    ## Columns: 14
    ## $ Date                    <date> 2020-01-20, 2020-01-22, 2020-01-22, 2020-01-2…
    ## $ Continent_Name          <chr> "Asia", "North America", "North America", "Nor…
    ## $ Two_Letter_Country_Code <chr> "KR", "US", "US", "US", "US", "KR", "US", "US"…
    ## $ Country_Region          <chr> "South Korea", "United States", "United States…
    ## $ Province_State          <chr> "All States", "All States", "Washington", "All…
    ## $ positive                <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 4, 0, 3, 0, 0, 0, 0, 1…
    ## $ hospitalized            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ recovered               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ death                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ total_tested            <dbl> 4, 1, 1, 1, 1, 27, 1, 1, 0, 0, 0, 0, 0, 0, 0, …
    ## $ active                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ hospitalizedCurr        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ daily_tested            <dbl> 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ daily_positive          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

# Isolating the required rows

- Selecting only the rows related to “All States” and removing the
  “Province_State”.

``` r
library(dplyr)
# Filter "All states" and remove the "Province_state" column
covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State) # can remove column as only value is "All states"
```

# Isolating the required Columns

``` r
# selecting columns with cumulative numbers
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

head(covid_df_all_states_daily)
```

    ## # A tibble: 6 × 6
    ##   Date       Country_Region active hospitalizedCurr daily_tested daily_positive
    ##   <date>     <chr>           <dbl>            <dbl>        <dbl>          <dbl>
    ## 1 2020-01-20 South Korea         0                0            0              0
    ## 2 2020-01-22 United States       0                0            0              0
    ## 3 2020-01-23 United States       0                0            0              0
    ## 4 2020-01-24 South Korea         0                0            5              0
    ## 5 2020-01-24 United States       0                0            0              0
    ## 6 2020-01-25 Australia           0                0            0              0

# Extracting Top Ten countries in the number of tested cases

## Summarising data based on “Country_Region” column.

``` r
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active = sum(active),
            hospitalized = sum(hospitalizedCurr)) %>%
  arrange(-tested)

covid_df_all_states_daily_sum
```

    ## # A tibble: 108 × 5
    ##    Country_Region   tested positive  active hospitalized
    ##    <chr>             <dbl>    <dbl>   <dbl>        <dbl>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0
    ## # ℹ 98 more rows

## Take top 10

``` r
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)

covid_top_10
```

    ## # A tibble: 10 × 5
    ##    Country_Region   tested positive  active hospitalized
    ##    <chr>             <dbl>    <dbl>   <dbl>        <dbl>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0

# Identifying Highest Positive Against Tested Cases

## Getting vectors

``` r
countries <- covid_top_10 %>% pull(Country_Region)
tested_cases <- covid_top_10 %>% pull(tested)
positive_cases <- covid_top_10 %>% pull(positive)
active_cases <- covid_top_10 %>% pull(active)
hospitalized_cases <- covid_top_10 %>% pull(hospitalized)
```

## Naming vectors

``` r
names(positive_cases) <- countries
names(tested_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
```

## Identifying

``` r
positive_cases/tested_cases
```

    ##  United States         Russia          Italy          India         Turkey 
    ##    0.108618191    0.038546552    0.061523368    0.016507300    0.080711720 
    ##         Canada United Kingdom      Australia           Peru         Poland 
    ##    0.054915490    0.113260617    0.005746668    0.060910738    0.025840932

## Conclusion

``` r
# manually inspected to select top 3 
positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)
```

# Keep relevant infomration

``` r
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

# combine the vectors
covid_mat <- rbind(united_kingdom, united_states, turkey)

# name columns
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

covid_mat
```

    ##                Ratio   tested positive  active hospitalized
    ## united_kingdom  0.11  1473672   166909       0            0
    ## united_states   0.10 17282363  1877179       0            0
    ## turkey          0.08  2031192   163941 2980960            0

# Pulling everything together

``` r
question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)

datasets <- list(
  original = covid_df,
  allstates = covid_df_all_states,
  daily = covid_df_all_states_daily,
  top_10 = covid_top_10)

matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)

data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)

covid_analysis_list <- list(question, answer, data_structure_list)

covid_analysis_list[[2]]
```

    ## Positive tested cases.United Kingdom  Positive tested cases.United States 
    ##                                 0.11                                 0.10 
    ##         Positive tested cases.Turkey 
    ##                                 0.08

From the analysis, it is clear that United Kingdom, United States, and
Turkey have had the highest number of positive cases against the number
of test cases as the start of the pandemic.
