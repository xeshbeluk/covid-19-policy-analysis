
# Optimized COVID-19 Mortality Analysis Script

# Load packages
library(tidyverse)
library(data.table)
library(ggpubr)
library(astsa)

# Suppress scientific notation
options(scipen = 10)

# Load data
main_data <- read.csv('owid-covid-data.csv') %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'))

# Define country ISO codes and labels
countries <- c('RUS','USA','BRA','UKR','ITA','GBR','DEU','FRA','BGR',
               'SRB','ROU','PER','MEX','POL','ESP','PHL')

country_labels <- c('Russia','USA','Brazil','Ukraine','Italy','UK','Germany','France','Bulgaria',
                    'Serbia','Romania','Peru','Mexico','Poland','Spain','Philippines')

# Helper function to prepare country data
prepare_country_data <- function(iso) {
  df <- filter(main_data, iso_code == iso & date >= "2020-02-01" & date <= "2022-08-01")
  df$group <- case_when(
    df$date <= as.Date('2020-12-31') ~ '2020',
    df$date <= as.Date('2021-12-31') ~ '2021',
    TRUE ~ '2022'
  )
  return(df)
}

# Apply to all countries
data_list <- lapply(countries, prepare_country_data)
names(data_list) <- countries

# Function to filter outliers using Cook’s distance
remove_outliers <- function(df) {
  reg <- lm(new_deaths ~ new_cases, data = df)
  cooksd <- cooks.distance(reg)
  outliers <- as.numeric(names(cooksd)[(cooksd > 4 * mean(cooksd, na.rm = TRUE))])
  df_clean <- df[-outliers,]
  return(df_clean)
}

# Apply outlier filtering
clean_list <- lapply(data_list, remove_outliers)

# Function to run regression summary
regression_summary <- function(df) {
  summary(lm(new_deaths ~ new_cases, data = df))
}

# Summaries
reg_summaries <- lapply(data_list, regression_summary)

# Function for quadratic regression
quadratic_summary <- function(df) {
  df$new_cases2 <- df$new_cases^2
  summary(lm(new_deaths ~ new_cases + new_cases2, data = df))
}

# Filter only 2022 data
data_2022 <- lapply(data_list, function(df) filter(df, group == "2022"))
quad_summaries <- lapply(data_2022, quadratic_summary)

# Function for correlation
get_cor <- function(df) {
  cor(df$new_cases, df$new_deaths, use = "complete.obs")
}

correlations <- sapply(data_list, get_cor)
correlations_clean <- sapply(clean_list, get_cor)

# Function for scatter plot
make_scatter_plot <- function(df, title) {
  ggplot(df, aes(x = scale(new_cases), y = scale(new_deaths), color = factor(group))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1.2) +
    xlim(-0.5, 6) + ylim(-0.5, 3) +
    labs(x = 'Spreading', y = 'Mortality', title = title) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.line = element_line(color = 'black')
    )
}

# Generate plots
scatter_plots <- mapply(make_scatter_plot, data_list, country_labels, SIMPLIFY = FALSE)

# Arrange into grids
scatter_grid1 <- ggarrange(plotlist = scatter_plots[1:4], labels = countries[1:4], ncol = 2, nrow = 2)
scatter_grid2 <- ggarrange(plotlist = scatter_plots[5:8], labels = countries[5:8], ncol = 2, nrow = 2)
scatter_grid3 <- ggarrange(plotlist = scatter_plots[9:12], labels = countries[9:12], ncol = 2, nrow = 2)
scatter_grid4 <- ggarrange(plotlist = scatter_plots[13:16], labels = countries[13:16], ncol = 2, nrow = 2)

# Show one of the grids
print(scatter_grid1)

# Function for mortality vs. spreading over time
make_time_series_plot <- function(df, title) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = scale(new_cases)), color = "red") +
    geom_line(aes(y = scale(new_deaths)), color = "blue") +
    geom_point(aes(y = scale(excess_mortality)), color = "green", size = 2) +
    labs(x = 'Date', y = 'Mortality vs Spreading', title = title) +
    scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
    ylim(-2, 6.5) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      axis.line = element_line(color = 'black')
    )
}

# Generate time series plots (only for countries with excess_mortality)
time_series_plots <- mapply(function(df, name) {
  if ("excess_mortality" %in% names(df)) {
    make_time_series_plot(df, name)
  } else {
    NULL
  }
}, data_list, country_labels, SIMPLIFY = FALSE)

# Remove NULLs
time_series_plots <- time_series_plots[!sapply(time_series_plots, is.null)]

# Arrange time series
time_series_grid1 <- ggarrange(plotlist = time_series_plots[1:4], ncol = 2, nrow = 2)
print(time_series_grid1)
