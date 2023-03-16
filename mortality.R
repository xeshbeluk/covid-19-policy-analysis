data1 <- read.csv('russia_excess_deaths_summary.csv')
data2 <-read.csv('russia_excess_deaths.csv')
actual_data <- data2[-c(2:86),]
cases <- read.csv('owid-covid-data.csv')
russan_cases <- filter(cases, iso_code == 'RUS')
russan_cases$date <- as.Date(russan_cases$date, format = '%Y-%m-%d')


one_month <- subset(russan_cases, format(date,'%Y-%m') == '2022-05')
a <- sum(one_month$new_cases)
rus_cas2[nrow(rus_cas2)+1,] <- a


rus_cas2 <- data.frame(matrix(ncol = 1, nrow = 0))





rus_cas<- as.data.frame(russan_cases$new_cases)
rus_cas$date <- as.Date(russan_cases$date, format = '%Y-%m-%d')












rus_cas_scaled <- rus_cas
rus_cas_scaled <- subset(rus_cas, format(date, '%d') == '28')
rus_cas_scaled <- rus_cas_scaled[-c(29,30),]
colnames(rus_cas_scaled) <- c('new_cases', 'date')







colnames(actual_data) <- c('X','Jan2020', 'Feb2020', "Mar2020", 'Apr2020', 'May2020','Jun2020','Jul2020','Aug2020','Sep2020','Oct2020','Nov2020','Dec2020', 'Jan2021', 'Feb2021', "Mar2021", 'Apr2021', 'May2021','Jun2021','Jul2021','Aug2021','Sep2021','Oct2021','Nov2021','Dec2021', 'Jan2022', 'Feb2022', "Mar2022", 'Apr2022', 'May2022')
rownames(actual_data) <- 'Deaths'

v <- as.data.frame(t(actual_data))
v <- as.data.frame(v[-c(1),])
colnames(v) <- 'Deaths'


plot.ts(scale(v), ylab = 'Excessive mortality vs. New cases (monthly, scaled)', xlab = "Months", col = 'red', ylim = c(-2,4.5))
lines(scale(rus_cas2), col = 'blue')

plot.ts(v, ylab = 'Excessive mortality', xlab = "Months", col = 'red')
lines(rus_cas2, col = 'blue')


