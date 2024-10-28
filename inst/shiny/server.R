

function(input, output, session) {


  observeEvent(input$instruction_link, {
    updateNavbarPage(session, inputId = "navbar_id", selected = "How To Use The App")
  })

  ## Read in data
  rvFile <- reactiveValues(clear = 0)
  getData <- reactive({
    morData <- input$readIn
    if (is.null(morData)) {
      return (NULL)
    }
    read.csv(input$readIn$datapath, stringsAsFactors = FALSE)
  })
  observeEvent(input$month_or_week, {
    rv$base <- NULL
    rv$excess <- NULL
  })

  # observeEvent(input$takeDataset1, {
  #   data(SampleInput1)
  # })

  output$downloadDataset1 <- downloadHandler(
    filename = function(){"SampleInput.csv"},
    content = function(file){
      data(SampleInput1)
      options(scipen = 100)
      write.csv(SampleInput1, file, row.names = FALSE)    
    } 
  )
  output$downloadDataset2 <- downloadHandler(
    filename = function(){"SampleInput_by_age.csv"},
    content = function(file){
      data(SampleInput2)
      out1 <- aggregate(deaths ~ year + month + age, data = SampleInput2, FUN = sum)
      out2 <- aggregate(population ~ year + month + age, data = SampleInput2, FUN = sum)
      out <- merge(out1, out2)
      out <- out[with(out, order(year, month, age)), ]
      options(scipen = 100)
      write.csv(out, file, row.names = FALSE)    
    } 
  )
  output$downloadDataset3 <- downloadHandler(
    filename = function(){"SampleInput_by_sex.csv"},
    content = function(file){
      data(SampleInput2)
      out1 <- aggregate(deaths ~ year + month + sex, data = SampleInput2, FUN = sum)
      out2 <- aggregate(population ~ year + month + sex, data = SampleInput2, FUN = sum)
      out <- merge(out1, out2)
      out <- out[with(out, order(year, month, sex)), ]
      options(scipen = 100)
      write.csv(out, file, row.names = FALSE)    
    } 
  )
  output$downloadDataset4 <- downloadHandler(
    filename = function(){"SampleInput_by_age_sex.csv"},
    content = function(file){
      data(SampleInput2)
      options(scipen = 100)
      write.csv(SampleInput2, file, row.names = FALSE)    
    } 
  )  
  output$downloadDataset5 <- downloadHandler(
    filename = function(){"SampleInput_weekly.csv"},
    content = function(file){
      data(SampleInput3)
      out1 <- aggregate(deaths ~ year + week, data = SampleInput3, FUN = sum)
      out2 <- aggregate(population ~ year + week, data = SampleInput3, FUN = sum)
      out <- merge(out1, out2)
      out <- out[with(out, order(year, week)), ]
      options(scipen = 100)
      write.csv(out, file, row.names = FALSE)    
    } 
  )  
  output$downloadDataset6 <- downloadHandler(
    filename = function(){"SampleInput_by_sex_weekly.csv"},
    content = function(file){
      data(SampleInput3)
      out1 <- aggregate(deaths ~ year + week + sex, data = SampleInput3, FUN = sum)
      out2 <- aggregate(population ~ year + week + sex, data = SampleInput3, FUN = sum)
      out <- merge(out1, out2)
      out <- out[with(out, order(year, week, sex)), ]
      options(scipen = 100)
      write.csv(out, file, row.names = FALSE) 
    } 
  )  
  output$downloadDataset7 <- downloadHandler(
    filename = function(){"SampleInput_by_age_weekly.csv"},
    content = function(file){
      data(SampleInput3)
      out1 <- aggregate(deaths ~ year + week + age, data = SampleInput3, FUN = sum)
      out2 <- aggregate(population ~ year + week + age, data = SampleInput3, FUN = sum)
      out <- merge(out1, out2)
      out <- out[with(out, order(year, week, age)), ]
      options(scipen = 100)
      write.csv(out, file, row.names = FALSE) 
    } 
  )  
  output$downloadDataset8 <- downloadHandler(
    filename = function(){"SampleInput_by_age_sex_weekly.csv"},
    content = function(file){
      data(SampleInput3)
      options(scipen = 100)
      write.csv(SampleInput3, file, row.names = FALSE)    
    } 
  )  


  output$fileUploaded <- reactive({
    updateSelectInput(session, "raw_data_population", 
                  choices = c("None (assumed no change)", colnames(getData())))
    if("population" %in% colnames(getData())){
      updateSelectInput(session, "raw_data_population", selected = "population")
    }
    updateSelectInput(session, "raw_data_sex", choices = c("None", colnames(getData())))
    if("sex" %in% colnames(getData())){
      updateSelectInput(session, "raw_data_sex", selected = "sex")
    }
    updateSelectInput(session, "raw_data_age", choices = c("None", colnames(getData())))
    if("age" %in% colnames(getData())){
      updateSelectInput(session, "raw_data_age", selected = "age")
    }
    return(!is.null(getData()))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

rv <- reactiveValues()
rv[["processed"]] <- FALSE

observeEvent(input$processMe, {
       morData <- getData()
       if(input$month_or_week == ""){
         output$message_file_upload <- renderText("Error: Please select a time scale.\n")
         return(NULL)
       }
       colnames(morData) <- tolower(colnames(morData))
       if("death" %in% colnames(morData) && "deaths" %in% colnames(morData) == FALSE){
          colnames(morData)[colnames(morData) == "death"] <- "deaths"
       }
       morData$deaths <- as.numeric(morData$deaths)
       if(sum(is.na(morData$deaths)) > 0){
         output$message_file_upload <- renderText("Error: 'deaths' column contains non-numerical values or NAs. Please check the input death counts.")
       }

       time_case <- input$month_or_week
       if(time_case == "Monthly"){
          T <- 12
          colnames(morData)[colnames(morData) == "month"] <- "timeCol"
       }else{
          T <- 53
          colnames(morData)[colnames(morData) == "week"] <- "timeCol"
       }

       if(!"timeCol" %in% colnames(morData)){
         if(time_case == "Monthly"){
              output$message_file_upload <- renderText("Error: 'month' column does not exist in the input data. Check your data file or the time scale is selected correctly.")
          }else{
              output$message_file_upload <- renderText("Error: 'week' column does not exist in the input data. Check your data file or the time scale is selected correctly.")
          }
          rv$processed <- FALSE
          return(NULL)
       }else{
          output$message_file_upload <- NULL
       }
       if(sum(is.na(as.numeric(morData$timeCol))) > 0){
           output$message_file_upload <- renderText("Error: Month or week in the input data include non-numerical values. Check your data file.")
            return(NULL)
       }else{
          output$message_file_upload <- NULL
       }
       if(min(as.numeric(morData$timeCol), na.rm = TRUE) < 1){
           output$message_file_upload <- renderText("Error: Month or week in the input data do not start from 1. Check your data file.")
            return(NULL)
       }else{
          output$message_file_upload <- NULL
       }
       if(max(as.numeric(morData$timeCol), na.rm = TRUE) > T){
        if(time_case == "Monthly"){
              output$message_file_upload <- renderText("Error: More than 12 months are found in the input data. Check your data file or the time scale is selected correctly.")
          }else{
              output$message_file_upload <- renderText("Error: More than 53 weeks are found in the input data. Check your data file or the time scale is selected correctly.")
          }
          return(NULL)
       }else{
          output$message_file_upload <- NULL
       }
    
 
       if(input$raw_data_population == "None (assumed no change)"){
        morData$popCol <- NA
       }else{
        colnames(morData)[which(colnames(morData) == tolower(input$raw_data_population))] <- "popCol"
        morData$popCol <- as.numeric(morData$popCol)
        if(sum(is.na(morData$popCol)) > 0){
         output$message_file_upload <- renderText(paste0("Error: ", input$raw_data_population, " column contains non-numerical values or NAs. Please check the input population size."))
        }
       }
       if(input$raw_data_sex == "None"){
        morData$sexCol <- "All"
       }else{
        colnames(morData)[which(colnames(morData) == tolower(input$raw_data_sex))] <- "sexCol"
       }
       if(input$raw_data_age == "None"){
        morData$ageCol <-  "All"
       }else{
        colnames(morData)[which(colnames(morData) == tolower(input$raw_data_age))] <- "ageCol"
       }
       # Change max history to 10 years
       morData <- subset(morData, year >= 2010)
       print(dim(morData))
       if(dim(morData)[1] == 0 ){
            output$message_file_upload <- renderText("Error: no data after 2010 in the input. Check the formatting of the 'year' column to make sure it is numeric.")
             return(NULL)
       }
       rv[['cleanData']] <- morData
       years <- sort(unique(morData$year))

       ## ---------------------------------------------------------------------------------- ##
       ##  Create summary tables
       ## ---------------------------------------------------------------------------------- ##
       updateSelectInput(session, "baseline_show_sex", choices = c("All", unique(rv$cleanData$sexCol)))
       updateSelectInput(session, "baseline_show_age", choices = c("All", unique(rv$cleanData$ageCol)))
       updateSelectInput(session, "table_show_sex", choices = c("All", unique(rv$cleanData$sexCol)))
       updateSelectInput(session, "table_show_age", choices = c("All", unique(rv$cleanData$ageCol)))
       updateSelectInput(session, "compare_plot_by", 
            choices = c(
                ifelse(is.null(unique(rv$cleanData$sexCol)), NULL, "By Sex"),
                ifelse(is.null(unique(rv$cleanData$ageCol)), NULL, "By Age"),
                ifelse(is.null(unique(rv$cleanData$sexCol)) || is.null(unique(rv$cleanData$sexCol)), NULL, "By Sex and Age")
            )
        )

       ## ---------------------------------------------------------------------------------- ##
       ##  Compute baseline and excess
       ## ---------------------------------------------------------------------------------- ##

       rv[['cleanTab']] <- summary_table(time_case, T, years, morData)
       print(rv[['cleanTab']])
       if(input$which_model == "Simple Baseline"){
         rv[['excess']] <- base_model(time_case, T, years, morData, "sexCol", "ageCol", "popCol", "timeCol", use.rate = FALSE)        
       }else{

        show_modal_spinner(text = "Fitting the Excess Mortality Model") # show the spinner
        rv[['excess']] <- smooth_model(time_case = time_case, T = T, years = years, morData = morData, sexCol = "sexCol", ageCol = "ageCol", popCol = "popCol", timeCol = "timeCol", use.rate = TRUE)
        remove_modal_spinner() # hide the spinner
       }
       rv[["processed"]] <- TRUE


}
)


  output$baselinePlot <- renderPlotly({
     req(input$processMe) 
     tryCatch({
       if(rv$processed) ggplotly(mortality_plot(rv$excess, input$baseline_show_sex, input$baseline_show_age, input$month_or_week, input$plot_show), tooltip = "text") %>% layout(legend = list(orientation = "v", x = 0.02, y = 0.95))
      }, error = function(warn){
        return(NULL)
      })
  })

  output$download_baseplot = downloadHandler(
    filename = function() {
      paste0(input$plot_show, "_", input$month_or_week, "_Sex_", input$baseline_show_sex, "_Age_", input$baseline_show_age, '.pdf')
    }, 
    content = function(file) {
      ggsave(file, plot = mortality_plot(rv$excess, input$baseline_show_sex, input$baseline_show_age, input$month_or_week, input$plot_show), width = 8, height = 5)
    })

  output$comparePlot <- renderPlotly({
     req(input$processMe) 
     tryCatch({
       if(rv$processed) {
        g <- compare_plot(rv$excess, input$compare_plot_by, input$month_or_week, input$compare_plot_show)
        ndim <- wrap_dims(length(unique(ggplot_build(g)$data[[1]]$PANEL)))
        if(ndim[1] == 1) ndim[1] <- 1.5
        if(ndim[2] == 1) ndim[2] <- 2
        gg <- ggplotly(g, tooltip = "text") 
        # hack to fix plotly legend change with multiple aes
        # for (i in 1:length(gg$x$data)){
        #     if (!is.null(gg$x$data[[i]]$name)){
        #       if(substr(gg$x$data[[i]]$name, 1, 1) == "(" && substr(gg$x$data[[i]]$name, nchar(gg$x$data[[i]]$name) - 2, nchar(gg$x$data[[i]]$name)) == ",1)"){
        #           gg$x$data[[i]]$name =  substr(gg$x$data[[i]]$name, 2, nchar(gg$x$data[[i]]$name)- 3)    
        #     }
        #     }
        # }
        ww <- ifelse(input$month_or_week == "Monthly", 300, 500)
        gg %>% layout(height = 360 * ndim[1], width = ww * ndim[2] + 200) 
      }
      }, error = function(warn){
        return(NULL)
      })
  })

  output$download_compareplot = downloadHandler(
    filename = function() {
      paste0(input$compare_plot_show, "_", input$month_or_week, "_", input$compare_plot_by, '.pdf')
    }, 
    content = function(file) {
      ggsave(file, plot = compare_plot(rv$excess, input$compare_plot_by, input$month_or_week, input$compare_plot_show), width = 8)
    })

  output$baselineTab <- DT::renderDataTable({
     req(input$processMe) 
     tryCatch({
       if(rv$processed){
         tab <- rv$excess$excess[[input$baseline_show_sex]][[input$baseline_show_age]]
          if(input$month_or_week == "Monthly"){
              tab$Month <- tab$timeCol
              timeLabel = "Month"
           }else{
              tab$Week <- tab$timeCol
              timeLabel = "Week"
           }

           tab <- tab[, c("year", timeLabel, "deaths", "excess", "lower", "upper")]
           colnames(tab)[1] <- "Year"
           colnames(tab)[3] <- "Actual Deaths"
           colnames(tab)[4] <- "Excess Deaths"
           # colnames(tab)[5] <- "Standard Error of Excess"
           colnames(tab)[5] <- "Lower limit of Excess (95% CI)"
           colnames(tab)[6] <- "Uower limit of Excess (95% CI)"
           # tab <- round(tab, 0)
           if(input$month_or_week == "Monthly"){
              tab <- tab[with(tab, order(Year, Month)), ]
           }else{
              tab <- tab[with(tab, order(Year, Week)), ]
           }
           rownames(tab) <- NULL
           tab <- tab %>% 
                DT::datatable(
                      extensions = 'Buttons', 
                      options = list(dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'print'), 
                      pageLength = 20)) %>% 
                formatRound(columns = 3:6, digits = 0) 
           return(tab)
         }
       }, error = function(warn){
          return(NULL)
        })
  })


  output$tableSummary <- renderTable(rv$cleanTab[[input$table_show_type]][[input$table_show_sex]][[input$table_show_age]], digits = 0, align = "c")

  output$linePlotSummary <- renderPlotly({
    tab <- rv$cleanTab[[input$table_show_type]][[input$table_show_sex]][[input$table_show_age]]
    x <- colnames(tab)[1]
    tab[,1] <- factor(tab[,1], levels = tab[,1])
    years <- as.numeric(colnames(tab)[-1])
    colors <- c(grDevices::colorRampPalette(c('#6baed6', '#084594'))(length(years[years < 2020
      ])), grDevices::colorRampPalette(c('#fd8d3c', '#b10026'))(length(years[years >= 2020
      ])))
    if(input$month_or_week != "Monthly"){
        ww <- " week"
        xlab <- "Week"
    }else{
        ww <- ""
        xlab <- "Month"
    }
    if(input$table_show_type == "Death Counts"){
      ylab <- "Death Count"
    }else{
      ylab <- "Death Rate"
    }
    fig <- plot_ly(tab, x = ~.data[[x]], name = x) %>%
                  layout(xaxis = list(title = xlab), 
                         yaxis = list(title = ylab), 
                         title = input$table_show_type)
    for(j in 2:ncol(tab)){
      tmp <- data.frame(x = tab[, 1], y = tab[, j])
      fig <- fig %>% add_trace(data = tmp, x = ~x, y = ~ y, name = colnames(tab)[j],
                               color =I(colors[which(years == colnames(tab)[j])]), 
                               type = "scatter", mode = 'lines+markers', 
                               hovertemplate = paste0(colnames(tab)[j], ww, ' %{x}: %{y}<extra></extra>')) 
    }
    fig
  })

}