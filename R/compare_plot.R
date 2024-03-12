#' Function to plot multiple excess mortality estimates
#' 
#' @param model output from base_model
#' @param timeCol column specifying time variable
#' @param by character variable for "By Sex", "By Age", or "By Sex and Age"
#' @param month_or_week character variable for monthly or weekly input
#' @param plot_show character variable for which plot to show
#' @return a ggplot object
#' @export
#' 
#' @examples
#' data(SampleInput3)
#' out <- base_model(time_case = "Weekly", T = 53, 
#' 			  years = c(2015:2021), morData = SampleInput3, 
#' 			  sexCol = "sex", ageCol = "age", 
#' 			  popCol = "population", timeCol = "week")
#' compare_plot(model = out, by = "By Sex",timeCol = "week",
#' 			  month_or_week = "Weekly", 
#'            plot_show = "Death Counts")
#' compare_plot(model = out, by = "By Age",timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Death Counts")
#' compare_plot(model = out, by = "By Sex and Age", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Death Counts")
#' compare_plot(model = out, by = "By Sex", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts")
#' compare_plot(model = out, by = "By Age", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts")
#' compare_plot(model = out, by = "By Sex and Age", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts")
#' compare_plot(model = out, by = "By Sex", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts (Overlay)")
#' compare_plot(model = out, by = "By Age", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts (Overlay)")
#' compare_plot(model = out, by = "By Sex and Age", timeCol = "week",
#'            month_or_week = "Weekly", 
#'            plot_show = "Excess Death Counts (Overlay)")

compare_plot <- function(model, by, month_or_week, plot_show, timeCol = "timeCol"){
	   lower <- upper <- deaths <- year <- excess <- time <- plot.group <- NULL 

        
       if(plot_show == "Death Counts"){
            toplot <- model$base
       }else{
            toplot <- model$excess
       }

       toplot1 <- NULL
       if(by == "By Sex"){
            s <- names(toplot)
            s <- s[s != "All"]
            for(sex in s){
                tmp <- toplot[[sex]][["All"]]
                tmp$plot.group <- sex
                toplot1 <- rbind(toplot1, tmp)
            }
       }else if(by == "By Age"){
            aa <- names(toplot[["All"]])
            aa <- aa[aa != "All"]
            for(age in aa){
                tmp <- toplot[["All"]][[age]]
                tmp$plot.group <- age 
                toplot1 <- rbind(toplot1, tmp)
            }

       }else if(by == "By Sex and Age"){
            s <- names(toplot)
            s <- s[s != "All"]
            aa <- names(toplot[["All"]])
            aa <- aa[aa != "All"]
            for(sex in s){
                for(age in aa){
                    tmp <- toplot[[sex]][[age]]
                    tmp$plot.group.sex <- sex
                    tmp$plot.group.age <- age
                    toplot1 <- rbind(toplot1, tmp)
                }
            }
            toplot1$plot.group <- paste0(toplot1$plot.group.age, ", ",  toplot1$plot.group.sex)
       }

	   toplot1$year <- factor(toplot1$year)
       if(month_or_week == "Monthly"){
          toplot1$month <- toplot1[, timeCol]
          timeLabel = "month"
       }else{
          toplot1$week <- toplot1[, timeCol]
          timeLabel = "week"
       }
  
       # plotly ribborn requires no NA values
       toplot1 <- subset(toplot1, !is.na(lower))    
       
    
       if(plot_show == "Death Counts"){
            title <- paste0("Observed Deaths by ", ifelse(month_or_week == "Monthly", "Month", "Week"), 
                           " Compared to Expected Deaths")
             colors <- grDevices::colorRampPalette(c('#fd8d3c', '#b10026'))(length(unique(toplot1$year)))

            if(month_or_week == "Monthly"){
              toplot1$time <- paste(toplot1$year, toplot1$month, 1, sep = "-")
              toplot1$time <- lubridate::ymd(toplot1$time)
              # toplot1$time <- lubridate::ceiling_date(toplot1$time, "month") - lubridate::days(1)
            }else{
              # remove any week 53 that cannot be calculated
              toplot1$time <- paste(toplot1$year, toplot1$week, sep = "-W")
              toplot1$time_order <- as.numeric(as.character(toplot1$year)) * 100 + as.numeric(toplot1$week)
              weeks_all <- unique(toplot1$time)
              weeks_sorted <- weeks_all[order(toplot1$time_order[match(weeks_all, toplot1$time)])]
              toplot1$time <- match(toplot1$time,  weeks_sorted)
            }
            if(month_or_week == "Weekly"){
                toplot1$time_label <- weeks_sorted[toplot1$time]
            }else{
                toplot1$time_label <- toplot1$time
            }   
            g <- ggplot(toplot1) + aes(x = time, ymin = lower, ymax = upper, group = plot.group, 
                                text = paste0("Time Period: ", time_label, 
                                          "<br>Sub-population: ", plot.group, 
                                          "<br>Observed Deaths: ", deaths, 
                                          "<br>Expected Deaths: ", round(mean), 
                                          "<br>Expected Lower Bound: ", round(lower), 
                                          "<br>Expected Upper Bound: ", round(upper)
                                            )) + 
                  geom_ribbon(fill = "#a6cee3", color = NA, alpha = 0.5) + 
                  geom_line(aes(y = mean, color = "Expected Deaths")) + 
                  geom_line(aes(y = deaths, color = 'Observed Deaths'), linewidth = 0.9) + 
                  ylab("Deaths") +  
                  theme_bw()+ 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
                  scale_color_manual(name = "",
                                     values = c("Expected Deaths" = "#1f78b4", 
                                                "Observed Deaths" = "#d95f02"))
            if(by == "By Sex and Age"){
                g <- g +  facet_grid(plot.group.age ~ plot.group.sex, scales = "free") 
            }else if(by == "By Sex"){
                g <- g + facet_wrap(~plot.group, ncol = 2) 
            }else if(by == "By Age"){
                g <- g + facet_wrap(~plot.group, ncol = 2, scales = "free") 
            }


            if(month_or_week == "Weekly"){
              breaks <- seq(1, length(weeks_sorted), by = round(length(weeks_sorted) / 20))
              values <- weeks_sorted[breaks]
              g <- g + scale_x_continuous(breaks = breaks, labels = values)
            }else{
              g <- g + scale_x_date(breaks = "3 month", expand = c(0.01, 0.01), date_labels = "%Y-%m")
            }

       }


       if(grepl("Excess", plot_show)){
            title <- paste0("Excess Deaths by ", ifelse(month_or_week == "Monthly", "Month", "Week"))
            if(month_or_week == "Monthly"){
              toplot1$time <- paste(toplot1$year, toplot1$month, 1, sep = "-")
              toplot1$time <- lubridate::ymd(toplot1$time)
              # toplot1$time = lubridate::ceiling_date(toplot1$time, "month") - lubridate::days(1)
			}else{
			  # remove any week 53 that cannot be calculated
			  toplot1 <- subset(toplot1, !is.na(excess))	
              toplot1$time <- paste(toplot1$year, toplot1$week, sep = "-W")
              toplot1$time_order <- as.numeric(as.character(toplot1$year)) * 100 + as.numeric(toplot1$week)
              weeks_all <- unique(toplot1$time)
              weeks_sorted <- weeks_all[order(toplot1$time_order[match(weeks_all, toplot1$time)])]
              toplot1$time <- match(toplot1$time,  weeks_sorted)
            }
            if(month_or_week == "Weekly"){
                toplot1$time_label <- weeks_sorted[toplot1$time]
            }else{
                toplot1$time_label <- toplot1$time
            }   
            if(plot_show == "Excess Death Counts"){
                g <- ggplot(toplot1) + aes(
                            x = time, ymin = lower, ymax = upper, y = excess, group = plot.group,
                            text = paste0("Time Period: ", time_label, 
                                          "<br>Sub-population: ", plot.group, 
                                          "<br>Excess: ", round(excess), 
                                          "<br>Lower Bound: ", round(lower), 
                                          "<br>Upper Bound: ", round(upper)
                                            )) + 
                      geom_ribbon(fill = "#fdae61", color = "#fdae61", alpha = 0.5) + 
                      geom_hline(yintercept = 0, color = "#404040", linetype = 2) + 
                      geom_point(color = "#d7191c", alpha = 0.7) + 
                      geom_line(color = "#d7191c", linewidth = 0.9) + 
                      ylab("Excess Deaths") +
                      theme_bw()+ 
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

                if(by == "By Sex and Age"){
                    g <- g +  facet_grid(plot.group.age ~ plot.group.sex, scales = "free") 
                }else if(by == "By Sex"){
                    g <- g + facet_wrap(~plot.group, ncol = 2) 
                }else if(by == "By Age"){
                    g <- g + facet_wrap(~plot.group, ncol = 2, scales = "free") 
                }

            }else{
                if(by == "By Sex and Age"){
                    toplot1$plot.group <- paste0(toplot1$plot.group.age, ", ",  toplot1$plot.group.sex)
                }
                if(month_or_week == "Weekly"){
                    toplot1$time_label <- weeks_sorted[toplot1$time]
                }else{
                    toplot1$time_label <- toplot1$time
                }   
                g <- ggplot(toplot1) + aes(fill = plot.group, color = plot.group, x = time, ymin = lower, ymax = upper, y = excess, group = plot.group, 
                            text = paste0("Time Period: ", time_label, 
                                          "<br>Sub-population: ", plot.group, 
                                          "<br>Excess: ", round(excess), 
                                          "<br>Lower Bound: ", round(lower), 
                                          "<br>Upper Bound: ", round(upper)
                                            )) +
                      geom_ribbon(color = NA, alpha = 0.2) +  
                      geom_hline(yintercept = 0, color = "#404040", linetype = 2) + 
                      geom_line(alpha = 0.9, linewidth = 0.9) + 
                      ylab("Excess Deaths") +
                      theme_bw()+ 
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
                      scale_color_brewer("", palette = "Dark2") + 
                      scale_fill_brewer("", palette = "Dark2")

            }               


            if(month_or_week == "Weekly"){
              breaks <- seq(1, length(weeks_sorted), by = round(length(weeks_sorted) / 20))
              values <- weeks_sorted[breaks]
              g <- g + scale_x_continuous(breaks = breaks, labels = values)
            }else{
              g <- g + scale_x_date(breaks = "month", expand = c(0.01, 0.01), date_labels = "%Y-%m")
            }    
       }


       g <- g  + xlab("")+
          # xlab(ifelse(month_or_week == "Monthly", "Month", "Week")) + 
          ggtitle(title) 
       return(g)
}