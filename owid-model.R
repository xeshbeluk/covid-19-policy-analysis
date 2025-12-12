library(cmdstanr)
library(tidyverse)
library(boot)
library(dplyr)
library(ggplot2)
library(gridExtra)

# data_full <- read.csv("owid-covid-data.csv") # for reference


SIRfun2 <- function (IFR_plug, ind, countries, iter_warmup, iter_sampling) {
  
  data <- read_csv("owid-covid-data.csv") |>
    mutate(wk = week(ymd(date)),
           year = year(ymd(date))) |>
    group_by(year,wk,location) |>
    summarise(
      deaths = sum(new_deaths),
      cases = sum(new_cases)
    ) |>
    filter(location %in% c("France","Italy",
                           "Japan","Mexico","Peru",
                           "Romania","Serbia","Spain")) |>
    ungroup() |>
    rename(country = location) |>
    mutate(year_wk = paste0(year,"_",wk))
  
  data <- data[data$wk != 53,]
  
  
  deaths <- data |> select(-cases) |>
    pivot_wider(names_from = "country", values_from = "deaths") |>
    filter(year < 2023) |>
    select(-year,-wk,-year_wk) |>
    replace_na(list("France"=0,"Italy"=0,
                    "Japan"=0,"Mexico"=0,"Peru"=0,
                    "Romania"=0,"Serbia"=0,"Spain"=0))
  
  cases <- data |> select(-deaths) |>
    pivot_wider(names_from = "country", values_from = "cases") |>
    filter(year < 2023) |>
    replace_na(list("France"=0,"Italy"=0,
                    "Japan"=0,"Mexico"=0,"Peru"=0,
                    "Romania"=0,"Serbia"=0,"Spain"=0))
  
  rgammaAlt <- function (n, mean, cv = 1) {
    shape <- cv^-2
    scale <- mean/shape
    stats::rgamma(n = n, shape = shape, scale = scale)
  }
  
  # various distributions required for modelling
  mean1 <- 5.1/7; cv1 <- 0.86; # infection to onset
  mean2 <- 17.8/7; cv2 <- 0.45 # onset to death
  x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
  x2 <- rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
  f_cached <- ecdf(x1+x2) # empirical cumulative distribtion function
  
  N2 <- nrow(deaths) + 1
  
  predictor <- 1:N2 %/% 4 + 1
  X <- model.matrix(~as.factor(predictor))
  
  IFR <- IFR_plug
  convolution <- function(u) (IFR * f_cached(u))
  
  f <- rep(0,N2) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0))
  for(i in 2:N2) {
    f[i] = (convolution(i+.5) - convolution(i-.5)) 
  }
  
  # int <lower=1> M; // number of countries
  # int <lower=1> N0; // number of days for which to impute infections
  # int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  # int<lower=1> N2; // days of observed data + # of days to forecast
  # int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  # matrix[N2, M] f; // h * s
  # int EpidemicStart[M];
  # real pop[M];
  # real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
  
  p_data <- read_csv("COVID-19-up-to-date.csv") |>
    group_by(countriesAndTerritories) |>
    summarise(
      pop = first(popData2018)
    ) |>
    filter(countriesAndTerritories %in% c("France","Italy",
                                          "Japan","Mexico","Peru",
                                          "Romania","Serbia","Spain")) |>
    rename(country = countriesAndTerritories)
  
  p_data$country <- gsub("_", " ", p_data$country)
  
  nms <- colnames(deaths)
  pop <- p_data
  
  M <- 1
  stan_data <- list(
    M = 1,
    N0 = 1,
    P = ncol(X),
    X = X,
    N = rep(nrow(deaths),1),
    N2 = N2,
    deaths = matrix(rbind(as.matrix(deaths),matrix(-1,1,8))[,ind],nrow = N2, ncol = 1),
    f = matrix(rep(f,M),N2,M),
    EpidemicStart = rep(1,M),
    pop = pop$pop[ind],
    K_ncp = nrow(deaths) - 3,
    K_cp = 3,
    w = rep(0.2,nrow(deaths)),
    idx_cp = c(53,106,157),
    idx_ncp = setdiff(1:157, c(53,106,157)),
    SI = c(pgamma(1.5,3,0.5),pgamma(2:N2+0.5,3,0.5) - pgamma(2:N2-0.5,3,0.5))
  )
  stan_data$deaths
  stan_data$w[c(53,106,157)] <- 1
  
  mod <- cmdstan_model("base_general_speed2-one-country.stan")
  
  fit <- mod$sample(data = stan_data, parallel_chains = 4, chains = 4, iter_warmup = iter_warmup, iter_sampling = iter_sampling,
                    init = 0.5, max_treedepth = 12)
  
  draws <- fit$draws("prediction",format="draws_matrix")
  mu <- colMeans(draws)
  nms_mu <- names(mu)
  sel_vec <- grepl(",1]",nms_mu)
  
  upper <- apply(draws,2,quantile,0.95)
  lower <- apply(draws,2,quantile,0.05)
  
  start_week <- 20
  end_week <- 100
  differ <- end_week - start_week + 1
  
  mu_truncated <- mu[sel_vec][start_week:end_week]
  upper_truncated <- upper[sel_vec][start_week:end_week]
  lower_truncated <- lower[sel_vec][start_week:end_week]
  actual_data_truncated <- cases[[countries]][start_week:end_week]
  
  data <- data.frame(
    Weeks = 1:differ,
    Upper = upper_truncated,
    Mu = mu_truncated,
    Lower = lower_truncated,
    Actual = actual_data_truncated
  )
  data$Weeks <- data$Weeks + start_week - 1
  
  p <- ggplot(data, aes(x = Weeks)) +
    geom_line(aes(y = Upper), color = "blue", linetype = "dashed") +
    geom_line(aes(y = Mu), color = "red") +
    geom_line(aes(y = Lower), color = "blue", linetype = "dashed") +
    geom_point(aes(y = Actual)) +
    scale_x_continuous(breaks = seq(20, 20 + differ - 1, by = 20)) +
    labs(title = countries, x = "Weeks (20 to 100)", y = "Cases")
  
  # plot(1:differ,head(upper_truncated,-1),type="l",col="blue",lty=2, main = countries)
  # lines(1:differ,head(mu_truncated,-1),col="red",lty=1)
  # lines(1:differ,head(lower_truncated,-1),col="blue",lty=2)
  # points(actual_data_truncated)
  
  epsilon <- 1e-6  # constant to avoid division by 0
  
  
  
  mae <- mean(abs(mu_truncated - actual_data_truncated), na.rm = TRUE)
  rmse <- sqrt(mean((mu_truncated - actual_data_truncated)^2, na.rm = TRUE))
  mape <- mean(abs((mu_truncated - actual_data_truncated) / (actual_data_truncated + epsilon)), na.rm = TRUE) * 100
  coverage <- mean(actual_data_truncated >= lower_truncated & actual_data_truncated <= upper_truncated, na.rm = TRUE) * 100
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  cat("MAPE:", mape, "\n")
  cat("CI Coverage (%):", coverage, "\n")
  
  
  outcome <- c(mae, rmse, mape, coverage)
  return(list(outcome = outcome, plot = p))
  
}


IFR_data2 <- c(0.016, 0.018, 
               0.01, 0.025, 0.025, 
               0.02, 0.012, 0.012)
country_indecies2 <- c(1:8)
country_names2 <- c("France","Italy",
                   "Japan","Mexico","Peru",
                   "Romania","Serbia","Spain")




results2 <- as.data.frame(matrix(NA, nrow = 8, ncol = 5))
names(results2) <- c('Country', 'MAE', 'RMSE', 'MAPE', 'CI_coverage')
plots2 <- list()

for (i in 1:length(country_indecies2)) {
  result <- SIRfun2(0.02, country_indecies2[i], country_names2[i], 500, 500)
  outcome <- result$outcome
  plot <- result$plot
  results2$MAE[i] <- outcome[1] 
  results2$RMSE[i] <- outcome[2] 
  results2$MAPE[i] <- outcome[3] 
  results2$CI_coverage[i] <- outcome[4]
  results2$Country <- c("France","Italy",
                       "Japan","Mexico","Peru",
                       "Romania","Serbia","Spain")
  results2$IFR <- IFR_data2
  plots2[[country_names2[i]]] <- plot
}

plot3 <- grid.arrange(
  plots2[['France']], plots2[['Italy']], 
  plots2[['Japan']], plots2[['Mexico']], 
  ncol = 2
)

plot4 <- grid.arrange(
  plots2[['Peru']], plots2[['Romania']], 
  plots2[['Serbia']], plots2[['Spain']], 
  ncol = 2
)

# Save the combined plots as images
ggsave("combined_plot1.png", plot3, width = 14, height = 10)
ggsave("combined_plot2.png", plot4, width = 14, height = 10)

