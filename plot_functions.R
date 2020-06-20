


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

	
	myplot <- ggplot(country_extract, aes(x=Date, y=Cases)) +
		geom_line(size=2) +
		scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
		theme_minimal() +
		ggtitle(countryname) +
		ylab("Cumulative cases") +
		theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
	
	return(myplot)

}


country_confirmed_cases_daily <- function(country, per_country_ts.data){
	
	
	#### test
	# country <- "Belgium"
	# per_country_ts.data <- molten.agg
	
	#### Country extract
	countryname <- per_country_ts.data[,"Country"][grep(country,per_country_ts.data[,"Country"])][1]
	country_extract <- per_country_ts.data[grep(countryname,per_country_ts.data[,"Country"]),]
	
	#### cum -> daily: sort by date and use diff to revert cumsum
	country_extract <- country_extract[order(country_extract$Date),]
	country_extract$Cases <- c(country_extract$Cases[1],diff(country_extract$Cases))

	
	myplot <- ggplot(country_extract, aes(x=Date, y=Cases)) +
		#geom_line(size=0.5) +
		#geom_ribbon(ymin=0, aes(ymax = Cases), fill="#cc66ff") +
		geom_bar(stat = "identity", fill = "#cc66ff", col = "#cc66ff") +
		scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
		theme_minimal() +
		ggtitle(countryname) +
		ylab("Daily cases") +
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







