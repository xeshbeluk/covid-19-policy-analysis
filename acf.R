library(astsa)
main_data <- read.csv('owid-covid-data.csv')
russan_cases <- filter(main_data, iso_code == 'RUS')
attach(russan_cases)
russan_cases$date <- as.Date(russan_cases$date, format = '%Y-%m-%d')
russian_mortality <- read.csv('russia_excess_deaths.csv')



library(ggplot2)
russan_cases$sc_new_cases <- scale(russan_cases$new_cases)
russan_cases$sc_new_deaths <- scale(russan_cases$new_deaths)

ggplot(data = russan_cases, aes(x = date, y = sc_new_cases)) + geom_line(color = "red") + geom_line(y = russan_cases$sc_new_deaths, color = "blue") + labs(x = 'Date', y = 'COVID-related mortality vs new cases (daily, scaled)') + scale_x_date(date_breaks = '3 months', date_labels = '%Y-%m') + ylim(-1,7)

       
       
       
       
plot(new_tests_smoothed, type = 'l', col = 'red')

plot(new_cases_smoothed, type = 'l', col = 'blue')

plot(new_deaths_smoothed, type = 'l', col = 'black')

plot.ts(scale(new_cases))
lines(scale(new_deaths))
lines(scale(people_vaccinated))

plot.ts(diff(new_cases))
plot.ts(diff(new_deaths))
plot.ts(diff(new_tests))
plot.ts(diff(total_vaccinations))


acf2((diff(new_cases)), 900)
acf2((diff(new_deaths)), 800)
acf2((diff(new_tests)), 300)


acf2(new_cases, 30)
acf2(new_deaths, 30)

