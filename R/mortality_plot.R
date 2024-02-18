#' Function to plot excess mortality 
#' 
#' @param model output from base_model
#' @param sex character variable for sex
#' @param age character variable for age
#' @param month_or_week character variable for monthly or weekly input
#' @param timeCol column specifying time variable
#' @param plot_show character variable for which plot to show
#' @return a ggplot object
#' @export
#' 
#' @examples
#' data(SampleInput1)
#' SampleInput1$sex <- SampleInput1$age <- "All"
#' out <- base_model(time_case = "Monthly", T = 12, 
#' 			  years = c(2015:2021), morData = SampleInput1, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "month")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "month",  
#' 				month_or_week = "Monthly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "All", age = "All", timeCol = "month", 
#' 				month_or_week = "Monthly", plot_show = "Excess Death Counts")
#' 
#' data(SampleInput3)
#' out <- base_model(time_case = "Weekly", T = 53, 
#' 			  years = c(2015:2021), morData = SampleInput3, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "week")
#' mortality_plot(model = out, sex = "Male", age = "All", timeCol = "week",
#' 			  month_or_week = "Weekly", plot_show = "Death Counts")
#' mortality_plot(model = out, sex = "Female", age = "All", timeCol = "week", 
#' 			  month_or_week = "Weekly", plot_show = "Excess Death Counts")
#' 

mortality_plot <- function(model, sex, age, month_or_week, plot_show, timeCol = "timeCol"){
	   lower <- upper <- deaths <- year <- excess <- time <- NULL 

	   toplot1 <- model$base[[sex]][[age]]
       toplot2 <- model$excess[[sex]][[age]]
       toplot2$year <- factor(toplot2$year)
       if(month_or_week == "Monthly"){
          toplot1$month <- toplot1[, timeCol]
          toplot2$month <- toplot2[, timeCol]
          timeLabel = "month"
       }else{
          toplot1$week <- toplot1[, timeCol]
          toplot2$week <- toplot2[, timeCol]
          timeLabel = "week"
       }


      
       # plotly ribborn requires no NA values
       toplot1 <- subset(toplot1, !is.na(lower))
       toplot2 <- subset(toplot2, !is.na(lower))
       
       if(plot_show == "Death Counts Overlay"){
         title <- paste0("Observed Deaths by ", ifelse(month_or_week == "Monthly", "Month", "Week"), 
                       " Compared to Historical Data")
         colors <- grDevices::colorRampPalette(c('#fd8d3c', '#b10026'))(length(unique(toplot2$year)))

    	   g <- ggplot() + 
              geom_ribbon(data = toplot1, aes(x = .data[[timeLabel]], ymin = lower, ymax = upper), 
                          fill = "#7fc97f", color = "#beaed4", alpha = 0.7) + 
              geom_line(data = toplot1, aes(x = .data[[timeLabel]], y = mean), color = "#4daf4a") + 
              geom_point(data = toplot2, aes(x = .data[[timeLabel]], y = deaths, color = year), alpha = 0.7) + 
              geom_line(data = toplot2, aes(x = .data[[timeLabel]], y = deaths, color = year)) + 
              ylab("Observed Deaths") + 
              scale_color_manual("Year", values = colors) +
              theme_bw()

           if(month_or_week == "Monthly"){
                g <- g + scale_x_continuous(breaks = 1:12, labels = month.name) 
           }else{
                g <- g + scale_x_continuous(breaks = scales::breaks_extended(12), expand = c(0.01, 0.01),) 
           }

       }

       if(plot_show == "Death Counts"){
        title <- paste0("Observed Deaths by ", ifelse(month_or_week == "Monthly", "Month", "Week"), 
                       " Compared to Expected Deaths")
         colors <- grDevices::colorRampPalette(c('#fd8d3c', '#b10026'))(length(unique(toplot2$year)))

            if(month_or_week == "Monthly"){
              toplot1$time <- paste(toplot1$year, toplot1$month, sep = "-")
              toplot1$time <- lubridate::ym(toplot1$time)
              toplot1$time <- lubridate::ceiling_date(toplot1$time, "month") - lubridate::days(1)
            }else{
              # remove any week 53 that cannot be calculated
              toplot1$time <- paste(toplot1$year, toplot1$week, sep = "-W")
              toplot1$time_order <- as.numeric(as.character(toplot1$year)) * 100 + as.numeric(toplot1$week)
              weeks_all <- unique(toplot1$time)
              weeks_sorted <- weeks_all[order(toplot1$time_order[match(weeks_all, toplot1$time)])]
              toplot1$time <- match(toplot1$time,  weeks_sorted)
            }
            g <- ggplot(toplot1) + 
                  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), 
                              fill = "#a6cee3", color = NA, alpha = 0.5) + 
                  geom_line(aes(x = time, y = mean, color = "Expected Deaths")) + 
                  geom_line(aes(x = time, y = deaths, color = 'Observed Deaths'), linewidth = 0.9) + 
                  ylab("Deaths") +  
                  theme_bw()+ 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
                  scale_color_manual(name = "",
                                     values = c("Expected Deaths" = "#1f78b4", 
                                                "Observed Deaths" = "#d95f02"))

            if(month_or_week == "Weekly"){
              breaks <- seq(1, length(weeks_sorted), by = round(length(weeks_sorted) / 20))
              values <- weeks_sorted[breaks]
              g <- g + scale_x_continuous(breaks = breaks, labels = values)
            }else{
              g <- g + scale_x_date(breaks = "3 month", expand = c(0.01, 0.01), date_labels = "%Y-%m")
            }

       }


       if(plot_show == "Excess Death Counts"){
            title <- paste0("Excess Deaths by ", ifelse(month_or_week == "Monthly", "Month", "Week"))
            if(month_or_week == "Monthly"){
              toplot2$time <- paste(toplot2$year, toplot2$month, sep = "-")
              toplot2$time <- lubridate::ym(toplot2$time)
              toplot2$time = lubridate::ceiling_date(toplot2$time, "month") - lubridate::days(1)
			}else{
			  # remove any week 53 that cannot be calculated
			  toplot2 <- subset(toplot2, !is.na(excess))	
              toplot2$time <- paste(toplot2$year, toplot2$week, sep = "-W")
              toplot2$time_order <- as.numeric(as.character(toplot2$year)) * 100 + as.numeric(toplot2$week)
              weeks_all <- unique(toplot2$time)
              weeks_sorted <- weeks_all[order(toplot2$time_order[match(weeks_all, toplot2$time)])]
              toplot2$time <- match(toplot2$time,  weeks_sorted)
            }
            g <- ggplot() + 
                  geom_ribbon(data = toplot2, aes(x = time, ymin = lower, ymax = upper), 
                              fill = "#fdae61", color = "#fdae61", alpha = 0.5) + 
                  geom_hline(yintercept = 0, color = "#404040", linetype = 2) + 
                  geom_point(data = toplot2, aes(x = time, y = excess), color = "#d7191c", alpha = 0.7) + 
                  geom_line(data = toplot2, aes(x = time, y = excess), color = "#d7191c") + 
                  ylab("Excess Deaths") +
                  theme_bw()+ 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            if(month_or_week == "Weekly"){
              breaks <- seq(1, length(weeks_sorted), by = round(length(weeks_sorted) / 20))
              values <- weeks_sorted[breaks]
              g <- g + scale_x_continuous(breaks = breaks, labels = values)
            }else{
              g <- g + scale_x_date(breaks = "month", expand = c(0.01, 0.01), date_labels = "%Y-%m")
            }
                   
       }


       g <- g  + 
          xlab(ifelse(month_or_week == "Monthly", "Month", "Week")) + 
          ggtitle(title) 
       return(g)
}