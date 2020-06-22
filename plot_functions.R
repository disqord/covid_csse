


library(ggplot2)
library(gtable)
library(grid)

##### Function to plot a country

country_confirmed_cases_cum <- function(country, per_country_ts.data){
	
	
	#### test
	# country <- "Belgium"
	# per_country_ts.data <- molten.agg
	
	#### Country extract
	countryname <- per_country_ts.data[,"Country"][grep(country,per_country_ts.data[,"Country"])][1]
	country_extract <- per_country_ts.data[grep(countryname,per_country_ts.data[,"Country"]),]
	
	varname <- colnames(country_extract)[3]
	colnames(country_extract)[3] <- "value"
	
	myplot <- ggplot(country_extract, aes(x=Date, y=value)) +
		geom_line(size=2) +
		scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
		theme_minimal() +
		ggtitle(countryname) +
		ylab(paste0("Cumulative ",tolower(varname))) +
		theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
	
	return(myplot)

}


country_confirmed_cases_daily <- function(country, per_country_ts.data, barfill = "#ff4d94", smoothcol = "#000099"){
	
	
	#### test
	# country <- "Belgium"
	# per_country_ts.data <- molten.agg
	
	#### Country extract
	countryname <- per_country_ts.data[,"Country"][grep(country,per_country_ts.data[,"Country"])][1]
	country_extract <- per_country_ts.data[grep(countryname,per_country_ts.data[,"Country"]),]
	
	#### cum -> daily: sort by date and use diff to revert cumsum
	country_extract <- country_extract[order(country_extract$Date),]
	country_extract[,3] <- c(country_extract[,3][1],diff(country_extract[,3]))
	
	varname <- colnames(country_extract)[3]
	colnames(country_extract)[3] <- "value"

	myplot <- ggplot(country_extract, aes(x=Date, y=value)) +
		#geom_line(size=0.5) +
		#geom_ribbon(ymin=0, aes(ymax = Cases), fill="#cc66ff") +
		geom_bar(stat = "identity", fill = barfill, col = barfill) +
		geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs") ,se=FALSE, col=smoothcol) +
		scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
		theme_minimal() +
		ggtitle(countryname) +
		ylab(paste0("Daily ",tolower(varname))) +
		theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
	
	return(myplot)
	
}



##### Plot two time series in grid

cum_daily_stack <- function(country){
	
	p1 <- country_confirmed_cases_cum(country, per_country_ts.data)
	p2 <- country_confirmed_cases_daily(country, per_country_ts.data)
	
	p1 <- p1 +
		theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
	p2 <- p2 + ggtitle("")
	
	g1 <- ggplotGrob(p1)
	g2 <- ggplotGrob(p2)
	g <- rbind(g1, g2, size = "first")
	g$widths <- unit.pmax(g1$widths, g2$widths)
	grid.newpage()
	grid.draw(g)
	
}


##### Plot two time series in grid

time_series_stack <- function(p1,p2){
	
	p1 <- p1 +
		theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
	
	# remove second plot title if same
	if(p1$labels$title == p2$labels$title){
		p2 <- p2 + ggtitle("")
	}
	
	g1 <- ggplotGrob(p1)
	g2 <- ggplotGrob(p2)
	g <- rbind(g1, g2, size = "first")
	g$widths <- unit.pmax(g1$widths, g2$widths)
	grid.newpage()
	grid.draw(g)
	
}

time_series_stack_N <- function(plot_list){
	
	#### TEST
	# plot_list <- list()
	# plot_list[[1]] <- country_confirmed_cases_cum("Sweden", per_country_ts.data)
	# plot_list[[2]] <- country_confirmed_cases_daily("Sweden", per_country_ts.data)
	# plot_list[[3]] <- country_confirmed_cases_daily("Belgium", per_country_ts.data)
	# 
	# p1 <- country_confirmed_cases_cum("Sweden", per_country_ts.data)
	# p2 <- country_confirmed_cases_cum("Belgium", per_country_ts.data)
	####
	

	
	groblist <- list()
	for (i in 1:length(plot_list)) {
		
		# if not last, remove date axis
		if(i != length(plot_list)){
			plot_list[[i]] <- plot_list[[i]] + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
		}
		
		groblist[[i]] <- ggplotGrob(plot_list[[i]])
	}
	
	# bind the first two grobs
	gee <- rbind(groblist[[1]], groblist[[2]], size = "first")
	# bind the rest
	if(length(groblist) > 2){
		for (i in 3:length(plot_list)) {
			gee <- rbind(gee, groblist[[i]], size = "first")
		}
	}
	
	# fix that width thingy
	# first two grobs
	gee$widths <- unit.pmax(groblist[[1]]$widths, groblist[[2]]$widths)
	# bind the rest
	if(length(groblist) > 2){
		for (i in 3:length(plot_list)) {
			gee$widths <- unit.pmax(gee$widths, groblist[[i]]$widths)
		}
	}
	
	grid.newpage()
	grid.draw(gee)
	
}

##### Plot the cumulative world champs time series

topx_countries_cumulative_cases <- function(per_country_ts.data, topx = 10){
	
	per_country_ts.wide <- dcast(data = per_country_ts.data, formula = Country~Date, value.var = "Cases")
	per_country_ts.wide <- data.frame(per_country_ts.wide, row.names = "Country", stringsAsFactors = FALSE)
	
	sorted_countries <- row.names(per_country_ts.wide)[order(per_country_ts.wide[,dim(per_country_ts.wide)[2]], decreasing = TRUE)]
	topx_countries <- sorted_countries[1:topx]
	
	## assign "other" to non topx countries
	topx.data <- per_country_ts.data
	topx.data$Country[!topx.data$Country %in% topx_countries] <- "Other"
	
	## Re-aggregate
	topx.agg <- aggregate(Cases~Date + Country, data = topx.data, FUN = "sum")
	
	## Sort the factors
	topx.agg$Country <- factor(topx.agg$Country, levels = c(topx_countries,"Other"))
	
	ggplot(topx.agg, aes(x=Date, y=Cases, fill=Country, col=Country)) +
		geom_bar(stat = "identity", position = "stack") +
		scale_fill_brewer(palette = "Set3") +
		scale_color_brewer(palette = "Set3")
	
}




