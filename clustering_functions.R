



#### 

library(reshape2)
library(pheatmap)
library(viridis)
library(igraph)
library(ape)

#### Source this one again, as some are used
source("general_functions.R")



#######   Country clustering

daily_case_pheatmap <- function(per_country_ts.data){
	

	
	#### TEST
	#per_country_ts.data
	####
	
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
	
	pheatmap(noncum.ts.wide, scale = "row", cluster_cols = FALSE, color = c(rep(viridis(100)[1],100),viridis(100)), labels_col = date_labels)
	
	
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



