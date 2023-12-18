#' Function to create baseline prediction and compute excess 
#' 
#' @param time_case string indicating monthly or weekly input
#' @param T number of time periods
#' @param years a sequence of years to show
#' @param morData the long format data frame
#' @param sexCol column name for sex variable
#' @param ageCol column name for age variable
#' @param popCol column name for population variable
#' @param timeCol column name for within-year time variable
#' @param use.rate predict based on rate
#' 
#' @import plotly  
#' @import DT
#' @importFrom stats sd aggregate qnorm as.formula
#' @importFrom grDevices colorRampPalette 
#' @importFrom lubridate ym  
#' @importFrom lubridate parse_date_time  
#' @importFrom lubridate days  
#' @importFrom lubridate ceiling_date  
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import scales
#' @importFrom dplyr left_join
#' 
#' @return a list of calculated excess mortality
#' 
#' @export
#' 
#' @examples
#' data(SampleInput1)
#' SampleInput1$sex <- SampleInput1$age <- "All"
#' out <- base_model(time_case = "Monthly", T = 12, years = c(2015:2021), morData = SampleInput1, 
#' 			  sexCol = "sex", ageCol = "age", popCol = "population", timeCol = "month")
#' data(SampleInput2)
#' out <- base_model(time_case = "Monthly", T = 12, years = c(2015:2021), morData = SampleInput2, 
#' 			  sexCol = "sex", ageCol = "age", popCol = "population", timeCol = "month")
#' data(SampleInput3)
#' out <- base_model(time_case = "Weekly", T = 53, years = c(2015:2021), morData = SampleInput3, 
#' 			  sexCol = "sex", ageCol = "age", popCol = "population", timeCol = "week")

base_model <- function(time_case, T, years, morData, sexCol, ageCol, popCol, timeCol, use.rate = FALSE){
	year <- NULL
	years_obs <- sort(years[years < 2020])
	years_pand <- sort(years[years >= 2020])

	base_model_internal <- function(time_case, T, years, morData, grouping = NULL, use.rate){
		if(is.null(grouping)){
			f1 <- as.formula(paste0("deaths ~ year + ", timeCol))
			f2 <- as.formula(paste0(popCol, " ~ year + ", timeCol))
		}else{
			ff <- paste0(grouping, collapse = "+ ")
			f1 <- as.formula(paste0("deaths ~ year + ", timeCol, " + ",  ff))
			f2 <- as.formula(paste0(popCol, " ~ year + ", timeCol, " + ",  ff))
		}
		
		dd <- aggregate(f1, data = morData, FUN = sum) 
		if(use.rate){
			dd.pop <- aggregate(f2, data = morData, FUN = sum)
			dd <- dplyr::left_join(dd, dd.pop)
			dd$deaths <- dd$deaths / dd[, popCol]
		}
		if(is.null(grouping)){
			f3 <- as.formula(paste0("deaths ~ ", timeCol))
		}else{
			f3 <- as.formula(paste0("deaths ~ ", timeCol, " + ", ff))
		}
		dd.mean <- aggregate(f3, data = dd, FUN = mean)
		colnames(dd.mean)[colnames(dd.mean) == "deaths"] <- "mean"
		dd.sd <- aggregate(f3, data = dd, FUN = function(x){sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))})
		colnames(dd.sd)[colnames(dd.sd) == "deaths"] <- "se"
		out <- dplyr::left_join(dd.mean, dd.sd)
		out$lower <- out$mean + qnorm(0.025) * out$se
		out$upper <- out$mean + qnorm(1 - 0.025) * out$se
		return(out)
	}

	excess_internal <- function(time_case, T, years, pandData, baseEst, grouping = NULL, use.rate){
		if(is.null(grouping)){
			f1 <- as.formula(paste0("deaths ~ year + ", timeCol))
			f2 <- as.formula(paste0(popCol, " ~ year + ", timeCol))
		}else{
			ff <- paste0(grouping, collapse = "+ ")
			f1 <- as.formula(paste0("deaths ~ year + ", timeCol, " + ",  ff))
			f2 <- as.formula(paste0(popCol, " ~ year + ", timeCol, " + ",  ff))
		}
		
		dd <- aggregate(f1, data = pandData, FUN = sum) 
		if(use.rate){
			dd.pop <- aggregate(f2, data = pandData, FUN = sum)
			dd <- dplyr::left_join(dd, dd.pop)
			dd$deaths <- dd$deaths / dd[, popCol]
		}
		excess <- dplyr::left_join(dd, baseEst)
		excess$mean <- excess$deaths - excess$mean
		tmp <- excess$deaths - excess$upper
		excess$upper <- excess$deaths - excess$lower
		excess$lower <- tmp
		if(use.rate){
			excess$mean <- excess$mean * excess[, popCol]
			excess$lower <- excess$lower * excess[, popCol]
			excess$upper <- excess$upper * excess[, popCol]
		}
		colnames(excess)[colnames(excess) == "mean"] <- "excess"
		return(excess)
	}

	# Output: baseline, sd, CI by month/week
	base.all <- base_model_internal(time_case, T, years, subset(morData, year %in% years_obs), grouping = NULL, use.rate = use.rate)
	base.sex <- base_model_internal(time_case, T, years, subset(morData, year %in% years_obs), grouping = sexCol, use.rate = use.rate)
	base.age <- base_model_internal(time_case, T, years, subset(morData, year %in% years_obs), grouping = ageCol, use.rate = use.rate)
	base.sexage <- base_model_internal(time_case, T, years, subset(morData, year %in% years_obs), grouping = c(sexCol, ageCol), use.rate = use.rate)

	base.out <- NULL 
	base.out[["All"]][["All"]] <- base.all
	for(s in unique(morData[, sexCol])){
		base.out[[s]][["All"]] <- base.sex[base.sex[, sexCol] == s, ]
	}
	for(a in unique(morData[, ageCol])){
		base.out[["All"]][[a]] <- base.age[base.age[, ageCol] == a, ]
	}
	for(s in unique(morData[, sexCol])){
		for(a in unique(morData[, ageCol])){
			base.out[[s]][[a]] <- base.sexage[base.sexage[, sexCol] == s & base.sexage[, ageCol] == a, ]
		}
	}


	# Output: excess, sd, CI by month/week and year
	pand <- subset(morData, year %in% years_pand)
	excess.all <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.all, grouping = NULL, use.rate = use.rate)
	excess.sex <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sex, grouping = sexCol, use.rate = use.rate)
	excess.age <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.age, grouping = ageCol, use.rate = use.rate)
	excess.sexage <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sexage, grouping = c(sexCol, ageCol), use.rate = use.rate)

	excess.out <- NULL 
	excess.out[["All"]][["All"]] <- excess.all
	for(s in unique(morData[, sexCol])){
		excess.out[[s]][["All"]] <- excess.sex[excess.sex[, sexCol] == s, ]
	}
	for(a in unique(morData[, ageCol])){
		excess.out[["All"]][[a]] <- excess.age[excess.age[, ageCol] == a, ]
	}
	for(s in unique(morData[, sexCol])){
		for(a in unique(morData[, ageCol])){
			excess.out[[s]][[a]] <- excess.sexage[excess.sexage[, sexCol] == s & excess.sexage[, ageCol] == a, ]
		}
	}

	return(list(base = base.out, excess = excess.out))



}
