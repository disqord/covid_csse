


######=====================================================================###### 
###### Source functions

source("aggregation_functions.R")
source("plot_functions.R")
source("general_functions.R")
source("clustering_functions.R")

######=====================================================================###### 
###### FETCH and AGGREGATE fresh data

ts_global_confirmed.data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE)
per_country_ts.data <- aggregate_per_country_and_melt(ts_global_confirmed.data)

######=====================================================================###### 
###### PLOT

# Separate plots for a country: cumulative or daily
country_confirmed_cases_cum("Sweden", per_country_ts.data)
country_confirmed_cases_daily("Sweden", per_country_ts.data)

# daily and cumulative combined for a country
cum_daily_stack("Belgium")

# compare two time series plots (check x-axes are same!!)
time_series_stack(country_confirmed_cases_daily("Mexico", per_country_ts.data),
									country_confirmed_cases_daily("India", per_country_ts.data))

# daily case heatmap with country clustering
daily_case_pheatmap(per_country_ts.data)




