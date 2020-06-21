




##### Make a molten cumulative aggregate non-cumulative



uncum_molten <- function(molten_cum, subset_var = "Country", cumvar = "Cases"){
	
	
	#### TEST
	# molten_cum <- per_country_ts.data
	
	
	### Checks
	if(!subset_var %in% colnames(molten_cum)){stop("Subset var not in molten cum colnames!\nOptions: ", paste(colnames(molten_cum), collapse = ", "))}
	if(!cumvar %in% colnames(molten_cum)){stop("Cumvar not in molten cum colnames!\nOptions: ", paste(colnames(molten_cum), collapse = ", "))}
	if(!"Date" %in% colnames(molten_cum)){stop("No date in molten cum colnames!\n")}
	
	
	molten_uncum <- NULL
	
	### For each "Country" or other subset col value: extract, uncum and join to result
	subsetvar.values <- unique(molten_cum[,subset_var])
	
	for (subsetvar.value in subsetvar.values) {
		
		molten_cum_subset <- molten_cum[molten_cum[,subset_var] == subsetvar.value,]
		
		molten_cum_subset <- molten_cum_subset[order(molten_cum_subset$Date),]
		molten_cum_subset[,cumvar] <- c(molten_cum_subset[,cumvar][1],diff(molten_cum_subset[,cumvar]))
		
		molten_uncum <- rbind(molten_uncum, molten_cum_subset)
		
	}
	
	return(molten_uncum)
	
}
