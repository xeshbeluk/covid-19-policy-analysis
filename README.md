
# COVID-19 Mortality Simulation Using MCMC and SIR Modeling

This repository contains an analytical framework for simulating and analyzing COVID-19 case and mortality data using a combination of regression modeling, time series diagnostics, and Bayesian inference with a custom SIR (Susceptible-Infectious-Recovered) model implemented in Stan. The analysis spans multiple countries and includes a specific focus on excess mortality in Russia.

## Project Structure

```
.
├── acf.R                         # ACF/Time Series diagnostics for Russia
├── base_general_speed2-one-country.stan  # Stan model for MCMC sampling
├── main-data.R                   # Multinational COVID-19 regression modeling (2020–2022)
├── mortality.R                  # Excess mortality data preparation and Russian case alignment
├── owid-model2.R                # Stan-based SIR MCMC modeling across eight countries
├── owid-covid-data.csv          # Main OWID COVID dataset (assumed to be present)
├── russia_excess_deaths.csv     # Excess deaths time series for Russia
├── russia_excess_deaths_summary.csv # Monthly summary of excess mortality
```

## Key Components

### 1. `main-data.R`
- Loads and processes COVID-19 case and death data for 16 countries.
- Performs outlier detection using Cook’s distance.
- Fits linear and quadratic regressions on new cases vs new deaths.
- Generates comparative mortality vs. case spread plots (scaled) by year.
- Countries analyzed include: RUS, USA, BRA, UKR, ITA, GBR, GER, FRA, BGR, SRB, ROU, PER, MEX, POL, ESP, PHL.

### 2. `acf.R`
- Focused ACF and PACF analysis on Russian data to assess autocorrelation structure.
- Scales and visualizes smoothed trends in cases, deaths, and testing.

### 3. `mortality.R`
- Compares Russian new case trends against excess mortality.
- Aligns OWID daily data to monthly excess death reporting.
- Plots scaled case counts vs. scaled excess deaths.

### 4. `owid-model2.R`
- Core MCMC simulation using CmdStanR with a custom SIR model.
- Weekly aggregated case and death data are modeled for:
  - France, Italy, Japan, Mexico, Peru, Romania, Serbia, Spain
- Incorporates Infection Fatality Rate (IFR) parameters per country.
- Generates posterior predictive intervals with credible intervals.
- Evaluates predictive performance using MAE, RMSE, MAPE, and CI coverage.
- Saves output plots to `combined_plot1.png` and `combined_plot2.png`.

### 5. `base_general_speed2-one-country.stan`
- A Bayesian SIR model designed for a single country’s time series.
- Takes preprocessed deaths and infection-to-death distributions.
- Supports change-point and non-change-point parameter estimation.

## Dependencies

Install the following R packages to run the scripts:

```R
install.packages(c("dplyr", "ggplot2", "astsa", "data.table", "Synth", "tidyverse", "cmdstanr", "boot", "gridExtra", "ggpubr"))
cmdstanr::install_cmdstan()
```

## Running the Simulation

1. Ensure all data files (`owid-covid-data.csv`, `russia_excess_deaths.csv`, and `russia_excess_deaths_summary.csv`) are in the project directory.
2. Run `main-data.R` to generate initial visualizations and regression summaries.
3. Run `acf.R` for time series analysis on Russian data.
4. Run `mortality.R` to compare OWID cases with excess deaths.
5. Run `owid-model2.R` to fit the Bayesian model and evaluate predictive performance.

## Results

- Regression analyses across countries show nonlinear trends between new cases and deaths.
- Russian data shows temporal alignment between scaled cases and excess mortality.
- MCMC simulations accurately track observed COVID-19 mortality trends with strong coverage metrics for most countries.

## License

This project is open for academic use. Please cite appropriately if using the models or plots in publications.
