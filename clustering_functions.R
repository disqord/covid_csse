



#### 

library(reshape2)
library(pheatmap)
library(viridis)
library(igraph)
library(ape)
library(RColorBrewer)

#### Source this one again, as some are used
source("general_functions.R")
#country_region_table <- readRDS("country_region_table.rds")


#######   Country clustering

daily_case_pheatmap <- function(per_country_ts.data, generate_new_legend = FALSE){
	

	
	#### TEST
	#per_country_ts.data
	####
	country_region_table <- read.delim("country_region_table.txt", stringsAsFactors = FALSE)
	
	#### Make the aggregate non-cumulative
	noncum.ts <- uncum_molten(per_country_ts.data, subset_var = "Country", cumvar = "Cases")
	
	#### Cast the aggregate
	noncum.ts.wide <- dcast(data = noncum.ts, formula = Country~Date, value.var = "Cases")
	noncum.ts.wide <- data.frame(noncum.ts.wide, row.names = "Country", stringsAsFactors = FALSE)
	
	
	#### SET CORRECTION DAYS TO ZERO
	#noncum.ts.wide[noncum.ts.wide < 0] <- 0
	
	#### Make weekly date labels for heatmap axis
	date_labels <- gsub("\\.","-",gsub("^X","",colnames(noncum.ts.wide)))
	date_labels[!rep(c(TRUE,rep(FALSE,6)),ceiling(length(date_labels)/7))[1:length(date_labels)]] <- ""
	

	# annot_cols <- list()
	# annot_cols <- brewer.pal(n = 9, name = "Set3")
	# names(annot_cols) <- "Region"
	
	Var1        <- brewer.pal(n = 12, name = "Set3")
	names(Var1) <- unique(country_region_table$Region)
	anno_colors <- list()
	anno_colors[[1]] <- Var1
	names(anno_colors)[1] <- "Region"
	
	my_colour = list(Region = Var1)
	
	my_colour[[1]]["Western Europe"] <- "blue"
	my_colour[[1]]["Eastern Europe"] <- "#db03fc"
	my_colour[[1]]["Northern Europe"] <- "light blue"
	my_colour[[1]]["Southern Europe"] <- "purple"
	
	my_colour[[1]]["Middle East & North Africa"] <- "yellow"
	my_colour[[1]]["Sub-Saharan Africa"] <- "dark green"
	my_colour[[1]]["North America"] <- "pink"
	my_colour[[1]]["Latin America & Caribbean"] <- "orange"
	my_colour[[1]]["East Asia & Pacific"] <- "red"
	my_colour[[1]]["South Asia"] <- "brown"

	
	
	pheatmap(noncum.ts.wide, 
					 scale = "row", 
					 cluster_cols = FALSE, 
					 color = c(rep(viridis(100)[1],200),viridis(100),rep(viridis(100)[100],100)), 
					 #color = viridis(100),
					 labels_col = date_labels,
					 annotation_row = country_region_table,
					 annotation_colors = my_colour,
					 legend = FALSE)
	
	
	if(generate_new_legend == TRUE){
		
		
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
		
	}
	
}




daily_case_network_cluster <- function(per_country_ts.data, corr.cut = 0.40){
	
	#### Make the aggregate non-cumulative
	noncum.ts <- uncum_molten(per_country_ts.data, subset_var = "Country", cumvar = "Cases")
	
	#### Cast the aggregate
	noncum.ts.wide <- dcast(data = noncum.ts, formula = Country~Date, value.var = "Cases")
	noncum.ts.wide <- data.frame(noncum.ts.wide, row.names = "Country", stringsAsFactors = FALSE)
	
	####
	mat <- cor(t(noncum.ts.wide))
	
	mat[mat<corr.cut] <- 0
	
	network <- graph_from_adjacency_matrix( mat, weighted=T, mode="undirected", diag=F)
	
	# Basic chart
	plot(network)
	
}




daily_case_cluster_dendrogram <- function(per_country_ts.data){
	
	#### Make the aggregate non-cumulative
	noncum.ts <- uncum_molten(per_country_ts.data, subset_var = "Country", cumvar = "Cases")
	
	#### Cast the aggregate
	noncum.ts.wide <- dcast(data = noncum.ts, formula = Country~Date, value.var = "Cases")
	noncum.ts.wide <- data.frame(noncum.ts.wide, row.names = "Country", stringsAsFactors = FALSE)
	
	hc <- hclust(dist(t(scale(t(noncum.ts.wide)))))
	
	# plot(hc)
	
	# hcd <- as.dendrogram(hc)
	
	# plot(hcd,  xlab = "Height", horiz = TRUE)
	# 
	# plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
	
	plot(as.phylo(hc), type = "fan")
	
	# plot(as.phylo(hc), type = "unrooted", cex = 0.6,
	# 		 no.margin = TRUE)
	
	
	
}







