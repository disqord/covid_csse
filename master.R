


######=====================================================================###### 
###### Source functions

source("aggregation_functions.R")
source("plot_functions.R")

######=====================================================================###### 
###### FETCH and AGGREGATE fresh data

ts_global_confirmed.data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE)
per_country_ts.data <- aggregate_per_country_and_melt(ts_global_confirmed.data)

######=====================================================================###### 
###### PLOT


# Separate plots
country_confirmed_cases_cum("Sweden", per_country_ts.data)
country_confirmed_cases_daily("Sweden", per_country_ts.data)

# daily and cumulative combined
cum_daily_stack("Belgium")

# compare two time series plots (check x-axes are same!!)
time_series_stack(country_confirmed_cases_daily("Russia", per_country_ts.data),
									country_confirmed_cases_daily("Iran", per_country_ts.data))



