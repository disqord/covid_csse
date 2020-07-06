




library(rworldmap)
#### Daily report



#### World heatmap



heatmap.filename <- paste0("world_heatmap_",Sys.Date(),".png")

png(filename = heatmap.filename, width = 1200, height = 2800, res = 100)
daily_case_pheatmap(per_country_ts.data, generate_new_legend = FALSE)
dev.off()



regDF <- data.frame(cbind(row.names(country_region_table), country_region_table$Region))

regMap <- joinCountryData2Map(regDF, joinCode = "NAME",
															nameJoinColumn = "X1")

## sort the col pal alphabetically
sorted_my_col <- my_colour[[1]][order(names(my_colour[[1]]))]

png(filename = "heatmap_legend.png", width = 900, height = 500, res = 100)

mapCountryData(regMap, nameColumnToPlot="X2", catMethod = "categorical",
							 missingCountryCol = gray(.8), colourPalette = sorted_my_col, addLegend = FALSE, mapTitle = "Legend",
							 borderCol = "black")

dev.off()



scaled_noncum.ts.wide <- scale(noncum.ts.wide)
pheatmap(t(scale(t(noncum.ts.wide))), 
				 scale = "row", 
				 cluster_cols = FALSE, 
				 color = c(rep(viridis(100)[1],200),viridis(100),rep(viridis(100)[100],100)), 
				 #color = viridis(100),
				 labels_col = date_labels,
				 annotation_row = country_region_table,
				 annotation_colors = my_colour,
				 legend = TRUE)



pheatmap(noncum.ts.wide, 
								 scale = "row", 
								 cluster_cols = FALSE, 
								 color = c(rep(viridis(100)[1],200),viridis(100),rep(viridis(100)[100],100)), 
								 #color = viridis(100),
								 labels_col = date_labels,
								 annotation_row = country_region_table,
								 annotation_colors = my_colour,
								 legend = TRUE)


##### Plot latest row from heatmap

### neg to zero
lowerbound <- 0
upperbound <- 5

scaled <- t(scale(t(noncum.ts.wide)))
recentcol <- scaled[,dim(scaled)[2]]

recentDF <- data.frame(cbind(names(recentcol), unname(recentcol)), stringsAsFactors = FALSE)
recentDF$X2 <- as.numeric(as.character(recentDF$X2))

recentDF$X2[recentDF$X2 < lowerbound] <- lowerbound
recentDF$X2[recentDF$X2 > upperbound] <- upperbound
recentMap <- joinCountryData2Map(recentDF, joinCode = "NAME",
															nameJoinColumn = "X1")


png(filename = paste0("scaled_score_worldmap_",Sys.Date(),".png"), width = 900, height = 500, res = 100)

mapCountryData(recentMap, nameColumnToPlot="X2", catMethod = c(0:(upperbound*10))/10,
							 missingCountryCol = gray(.8), colourPalette = viridis(upperbound*10), addLegend = TRUE, mapTitle = "Today's scores",
							 borderCol = "black")
dev.off()
