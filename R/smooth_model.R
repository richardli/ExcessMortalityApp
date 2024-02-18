#' Function to create baseline prediction and compute excess using time series model
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
#' @param knots_per_year number of AR1 knots per year.
#' @param return_model logical indicator to print the model summary
#' 
#' @import plotly  
#' @import DT
#' @importFrom shinyjs useShinyjs
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
#' library(INLA)
#' data(SampleInput1)
#' SampleInput1$sex <- SampleInput1$age <- "All" 
#' out <- smooth_model(time_case = "Monthly", T = 12, years = c(2015:2021), 
#' 			  morData = SampleInput1, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "month")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "month",
#' 				month_or_week = "Monthly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "month", 
#' 				month_or_week = "Monthly", plot_show = "Excess Death Counts")
#'  
#' data(SampleInput2)
#' out <- smooth_model(time_case = "Monthly", T = 12, years = c(2015:2021), 
#' 			  morData = SampleInput2, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "month")
#' mortality_plot(model = out, sex = "All", age = "65+", timeCol = "month", 
#' 				month_or_week = "Monthly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "Female", age = "65+", timeCol = "month", 
#' 				month_or_week = "Monthly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "Male", age = "65+",  timeCol = "month",
#' 				month_or_week = "Monthly", plot_show = "Excess Death Counts")
#' 
#' data(SampleInput3)
#' SampleInput3$sex <- SampleInput3$age <- "All"
#' out <- smooth_model(time_case = "Weekly", T = 52, years = c(2015:2021), 
#' 			  morData = SampleInput3, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "week")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "week",
#' 				month_or_week = "Weekly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "week",
#' 				month_or_week = "Weekly", plot_show = "Excess Death Counts")
#' 
#' data(SampleInput3)
#' out <- smooth_model(time_case = "Weekly", T = 53, years = c(2015:2021), 
#' 			  morData = SampleInput3, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "week", return_model=TRUE)
#' summary(out$model$all[[1]])

smooth_model <- function(time_case, T, years, morData, sexCol, ageCol, popCol, timeCol, use.rate = TRUE, knots_per_year = 4, return_model = FALSE){
	group <- inla <- NULL 
    if(!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
         stop("You need to install the packages 'INLA'. Please run in your R terminal:\n  install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)")
    }
    year <- NULL
	years_obs <- sort(years[years < 2020])
	years_pand <- sort(years[years >= 2020])

	smooth_model_internal <- function(time_case, T, years, morData, years_obs, grouping = NULL, use.rate){
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
			if(sum(is.na(morData[, popCol])) > 0){
				dd[, popCol] <- 1				
			}else{
				dd.pop <- aggregate(f2, data = morData, FUN = sum)
				dd <- dplyr::left_join(dd, dd.pop)	
			}
		}
		dd$yearID <- dd$year - min(dd$year) + 1
		if(time_case == "Monthly"){
			dd$periodID <- dd[, timeCol]
			n.seas <- 12
		}else{
			dd$periodID <- dd[, timeCol]
			n.seas <- 52
		}
		dd$timeID <- (dd$yearID - 1) * max(dd$periodID) + dd$periodID
		if(time_case == "Weekly") dd$periodID[dd$periodID == 53] <- 52
		dd$timeID <- match(dd$timeID, sort(unique(dd$timeID), decreasing = FALSE))
		hyperar1 = list(prec = list(prior = "pc.prec", param = c(1, 0.01)), 
                theta2 = list(prior = "pc.cor1", param = c(.7, .9)))
		hyperprec = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

		TT <- max(dd$timeID)
		if(time_case == "Monthly"){
			np <- 12 / knots_per_year
		}else{
			np <- 52 / knots_per_year
		}
		knots <- seq(1, TT, by = np)
		mesh1d <- inla.mesh.1d(loc=knots)
		mat <- inla.spde.make.A(mesh=mesh1d, loc=1:TT)
		m <- deaths ~ -1 + intercept  + f(period, model = "rw1", hyper = hyperprec, constr = TRUE) + 
					  f(wtimeID, model = "ar1", hyper = hyperar1)
		 

		# m <- deaths ~ f(periodID, model = "seasonal", season.length=n.seas, constr = TRUE) + 
		# 			  f(timeID, model = "ar1", hyper = hyperar1)
		 
		
		dd <- dd[order(dd$timeID), ]
		if(!is.null(grouping)){
			dd$group <- apply(dd[, grouping, drop = FALSE], 1, paste, collapse = ".")
		}else{
			dd$group <- 1
		}			 
		out <- NULL
		fitall <- NULL
		for(g in unique(dd$group)){
			ddo <- subset(dd, group == g)
			if(use.rate){
				E <- ddo[, popCol]
			}else{
				E <- NULL
			}
			ddo$deaths[!ddo$year %in% years_obs] <- NA

			# w1.t <- 2*pi/n.seas*ddo$timeID
			# w2.t <- 2*pi/n.seas*2*ddo$timeID
			stk <- inla.stack(data = list(deaths = ddo$deaths),  
							  A = list(1, mat), 
							  effect = list(list(intercept=1, 
							  					 time = ddo$timeID,
							  					 period = ddo$periodID), 
							  				list(wtimeID = 1:dim(mat)[2]))
							  )
			fit <- inla(m, data = inla.stack.data(stk),
						  family='poisson', E = E,
			              control.predictor=list(A = inla.stack.A(stk), compute=TRUE, link = 1))
			fitall[[g]] <- fit

			pred <- fit$summary.fitted.values[1:TT, c("0.5quant", "0.025quant", "0.975quant")]
			colnames(pred) <- c("mean", "lower", "upper")
			dd1 <- cbind(subset(dd, group == g), pred)
			if(!is.null(E)){
				dd1$mean <- dd1$mean * E
				dd1$lower <- dd1$lower * E
				dd1$upper <- dd1$upper * E
			}
			if(time_case == "Monthly"){
				out <- rbind(out, dd1[, c("timeID", "year", timeCol, grouping, "deaths", "mean", "lower", "upper")])
			}else{
				out <- rbind(out, dd1[, c("timeID", "year", timeCol, grouping, "deaths","mean", "lower", "upper")])
			}
		}
		if(return_model){
			return(list(out, fitall))
		}else{
			return(out)
		}
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
		# if(use.rate){
		# 	dd.pop <- aggregate(f2, data = pandData, FUN = sum)
		# 	dd <- dplyr::left_join(dd, dd.pop)
		# }
		dd$deaths <- NULL
		excess <- dplyr::left_join(dd, baseEst)
		excess$mean <- excess$deaths - excess$mean
		tmp <- excess$deaths - excess$upper
		excess$upper <- excess$deaths - excess$lower
		excess$lower <- tmp
		# if(use.rate){
		# 	excess$mean <- excess$mean * excess[, popCol]
		# 	excess$lower <- excess$lower * excess[, popCol]
		# 	excess$upper <- excess$upper * excess[, popCol]
		# }
		colnames(excess)[colnames(excess) == "mean"] <- "excess"
		return(excess)
	}

	# Output: baseline, sd, CI by month/week
	base.all <- smooth_model_internal(time_case, T, years, morData, years_obs, grouping = NULL, use.rate = use.rate)
	if(sum(morData[, sexCol] != "All") > 0){
		base.sex <- smooth_model_internal(time_case, T, years,  morData, years_obs, grouping = sexCol, use.rate = use.rate)
	}else{
		base.sex <- cbind("All", base.all)
		colnames(base.sex)[1] <- sexCol
	}
	if(sum(morData[, sexCol] != "All") > 0){
		base.age <- smooth_model_internal(time_case, T, years,  morData, years_obs, grouping = ageCol, use.rate = use.rate)
	}else{
		base.age <- cbind("All", base.all)
		colnames(base.age)[1] <- ageCol
	}
	if(sum(morData[, sexCol] != "All") > 0 || sum(morData[, ageCol] != "All") > 0){
		base.sexage <- smooth_model_internal(time_case, T, years,  morData, years_obs, grouping = c(sexCol, ageCol), use.rate = use.rate)
	}else{
		base.sexage <- cbind("All", "All", base.all)
		colnames(base.sexage)[1:2] <- c(ageCol, sexCol)
	}

	if(!return_model){
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
	}else{
		base.out <- NULL 
		base.out[["All"]][["All"]] <- base.all[[1]]
		for(s in unique(morData[, sexCol])){
			base.out[[s]][["All"]] <- base.sex[[1]][base.sex[[1]][, sexCol] == s, ]
		}
		for(a in unique(morData[, ageCol])){
			base.out[["All"]][[a]] <- base.age[[1]][base.age[[1]][, ageCol] == a, ]
		}
		for(s in unique(morData[, sexCol])){
			for(a in unique(morData[, ageCol])){
				base.out[[s]][[a]] <- base.sexage[[1]][base.sexage[[1]][, sexCol] == s & base.sexage[[1]][, ageCol] == a, ]
			}
		}
		model.out <- list(all = base.all[[2]], 
						  sex = base.sex[[2]], 
						  age = base.age[[2]], 
						  sexage = base.sexage[[2]])
	}
	


	# Output: excess, sd, CI by month/week and year
	pand <- subset(morData, year %in% years_pand)
	if(!return_model){
		excess.all <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.all, grouping = NULL, use.rate = use.rate)
		excess.sex <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sex, grouping = sexCol, use.rate = use.rate)
		excess.age <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.age, grouping = ageCol, use.rate = use.rate)
		excess.sexage <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sexage, grouping = c(sexCol, ageCol), use.rate = use.rate)
	}else{
		excess.all <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.all[[1]], grouping = NULL, use.rate = use.rate)
		excess.sex <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sex[[1]], grouping = sexCol, use.rate = use.rate)
		excess.age <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.age[[1]], grouping = ageCol, use.rate = use.rate)
		excess.sexage <- excess_internal(time_case, T, years, pandData = pand, baseEst = base.sexage[[1]], grouping = c(sexCol, ageCol), use.rate = use.rate)
	}

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

	if(!return_model){
		return(list(base = base.out, excess = excess.out))
	}else{
		return(list(base = base.out, excess = excess.out, model = model.out))
	}


}
