

library(reshape2)



#### Melt, fix date and aggregate confirmed cases per country


aggregate_per_country_and_melt <- function(full_set.data){
	
	### TEST
	#full_set.data <- ts_global_confirmed.data
	###
	
	### Get rid of lat long and province info
	rm.idx <- grep("lat|long|province", colnames(full_set.data), ignore.case = TRUE)
	full_set.filtered <- full_set.data[,-rm.idx]
	
	### Melt
	molten <- melt(full_set.filtered, id.vars = "Country.Region")
	
	### fix the date column
	molten$variable <- gsub("^X","",molten$variable)
	molten$variable <- gsub("\\.","-",molten$variable)
	molten$variable <- as.Date(molten$variable, format = "%m-%d-%y")
	
	### aggregate
	molten.agg <- aggregate(value~variable + Country.Region,data = molten, FUN = "sum")
	
	### Colnames
	colnames(molten.agg) <- c("Date","Country","Cases")
	
	cat("Data loaded for the following countries:\n")
	cat(unique(molten.agg$Country),sep = " - ")
	
	return(molten.agg)
	
}



