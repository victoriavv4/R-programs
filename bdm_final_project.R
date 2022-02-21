# importing all necessary libraries 
library(dplyr)
install.packages('lubridate')
library(lubridate)
install.packages('forecast')
install.packages("tseries")
library(forecast)
library(tseries)
install.packages("Hmisc")
library("Hmisc")
library(ggpubr)
theme_set(theme_pubr())
# importing data 
cases = read.csv('C:/Users/victo/OneDrive/Desktop/Case_Accumulation_Canada.csv')
View(cases)
# summarizing the data
summary(cases)

# EDA 

# correlations 
# replacing null values with mean of column for correlation 
cases$TotalDeaths[is.na(cases$TotalDeaths)] <- mean(cases$TotalDeaths, na.rm = TRUE)
cases$TotalVaccinated[is.na(cases$TotalVaccinated)] <- mean(cases$TotalVaccinated, na.rm = TRUE)

cor(cases[, c(7, 17)])

plot(cases$TotalDeaths ~ cases$TotalVaccinated, pch = 19, col = "blue")

# renaming the date column
colnames(cases)
names(cases)[names(cases) == "ï..Date"] = "Date"
names(cases)[names(cases) == "FREQUENCY"] = "Freq"
colnames(cases)


# subseting the dataframe to include only columns beneficial for business question
cases_sub = cases[ -c(3:18)]
View(cases_sub)

# view column data class
class(cases_sub$Date)

# formatting dates to readable date format 
cases_sub$Date = as.POSIXct(cases_sub$Date, format = "%Y/%m/%d %H: %M: %S")
class(cases_sub$Date)

# grouping by year and month 

cases_sub$year_month = floor_date(cases_sub$Date, "month")
cases_agg = cases_sub %>%
  group_by(year_month) %>%
  dplyr::summarize(Freq = sum(Freq)) %>%
  as.data.frame()

# frequency chart for frequency of cases and date 
View(cases_agg)

ggplot(cases_agg, aes(x = year_month, y = Freq)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme_pubclean()

# change freq trend to time series 
cases_ts = ts(cases_agg$Freq, start = c(2020, 1), end = c(2021, 11), frequency = 12)
class(cases_ts)

# plot the cases time series 
plot(cases_ts)

# CONDUCTION STATIONARY TESTS TO PREPARE FOR ARIMA

# checking autocorrelation graph to visualize stationary 
acf(cases_ts)
# based on the graph output, the data is stationary. (The lines rarely cross the lue dotted lines)
# graph also shows low autocorrelation

# check partial autocorrelation
pacf(cases_ts)
# the graph output shows not issues sine the solid black lines
# do not cross the blue lines 

# augmented test
adf.test(cases_ts)
# test confirmed data is not stationary 

# need to convert data to stationary data using auto-ARIMA analysis
freq_model = auto.arima(cases_ts, ic="aic", trace = TRUE)
# Best model: ARIMA(3,0,1) (p, d, q)
# ARIMA(3,0,1)            with zero mean     : 561.3065

acf(ts(freq_model$residuals))
pacf(ts(freq_model$residuals))

myfreq_forecast = forecast(freq_model, level = c(95), h=2*12)
myfreq_forecast
plot(myfreq_forecast)

Box.test(myfreq_forecast$residuals, lag = 5, type = 'Ljung-Box')
# X-squared = 0.3256, df = 5, p-value = 0.9971
# p-value > 0.05 therefore no autocorrelation issues and model is running.