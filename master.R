


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
time_series_stack(country_confirmed_cases_daily("US", per_country_ts.data),
									country_confirmed_cases_daily("Romania", per_country_ts.data))

# same as above, for more than 2 plots. Feed plots in list.
time_series_stack_N(list(country_confirmed_cases_daily("Sweden", per_country_ts.data),
												 country_confirmed_cases_daily("India", per_country_ts.data),
												 country_confirmed_cases_daily("Belgium", per_country_ts.data)))

# daily case heatmap with country clustering
daily_case_pheatmap(per_country_ts.data)

# daily case county clustering fan diagram
daily_case_cluster_dendrogram(per_country_ts.data)

# Cumulative stacked bars for top 10 current cumulative case countries
topx_countries_cumulative_cases(per_country_ts.data, topx = 11) + theme_minimal()





