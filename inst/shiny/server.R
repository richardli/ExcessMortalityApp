

function(input, output, session) {

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
      write.csv(out, file, row.names = FALSE)    
    } 
  )
  output$downloadDataset4 <- downloadHandler(
    filename = function(){"SampleInput_by_age_sex.csv"},
    content = function(file){
      data(SampleInput2)
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
      write.csv(out, file, row.names = FALSE) 
    } 
  )  
  output$downloadDataset8 <- downloadHandler(
    filename = function(){"SampleInput_by_age_sex_weekly.csv"},
    content = function(file){
      data(SampleInput3)
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
       colnames(morData) <- tolower(colnames(morData))
       if("death" %in% colnames(morData) && "deaths" %in% colnames(morData) == FALSE){
          colnames(morData)[colnames(morData) == "death"] <- "deaths"
       }

       ## TODO: check column names and return warning message

       time_case <- input$month_or_week
       if(time_case == "Monthly"){
          T <- 12
          colnames(morData)[colnames(morData) == "month"] <- "timeCol"
       }else{
          T <- 53
          colnames(morData)[colnames(morData) == "week"] <- "timeCol"
       }
       if(input$raw_data_population == "None (assumed no change)"){
        morData$popCol <- NA
       }else{
        colnames(morData)[which(colnames(morData) == tolower(input$raw_data_population))] <- "popCol"
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
       rv[['cleanData']] <- morData
       years <- sort(unique(morData$year))

       ## ---------------------------------------------------------------------------------- ##
       ##  Create summary tables
       ## ---------------------------------------------------------------------------------- ##
       updateSelectInput(session, "baseline_show_sex", choices = c("All", unique(rv$cleanData$sexCol)))
       updateSelectInput(session, "baseline_show_age", choices = c("All", unique(rv$cleanData$ageCol)))
       updateSelectInput(session, "table_show_sex", choices = c("All", unique(rv$cleanData$sexCol)))
       updateSelectInput(session, "table_show_age", choices = c("All", unique(rv$cleanData$ageCol)))

       ## ---------------------------------------------------------------------------------- ##
       ##  Compute baseline and excess
       ## ---------------------------------------------------------------------------------- ##

       rv[['cleanTab']] <- summary_table(time_case, T, years, morData)
       rv[['excess']] <- base_model(time_case, T, years, morData, "sexCol", "ageCol", "popCol", "timeCol", use.rate = FALSE)
       rv[["processed"]] <- TRUE
}
)


  # population excess
  output$baselinePlot <- renderPlotly({
     req(input$processMe) 
     tryCatch({
       mortality_plot(rv$excess, input$baseline_show_sex, input$baseline_show_age, input$month_or_week, input$plot_show)
      }, error = function(warn){
        return(NULL)
      })
  })

  output$baselineTab <- DT::renderDataTable({
     req(input$processMe) 
     tryCatch({

       tab <- rv$excess$excess[[input$baseline_show_sex]][[input$baseline_show_age]]
        if(input$month_or_week == "Monthly"){
            tab$Month <- tab$timeCol
            timeLabel = "Month"
         }else{
            tab$Week <- tab$timeCol
            timeLabel = "Week"
         }

         tab <- tab[, c("year", timeLabel, "deaths", "excess", "se", "lower", "upper")]
         colnames(tab)[1] <- "Year"
         colnames(tab)[3] <- "Actual Deaths"
         colnames(tab)[4] <- "Excess Deaths"
         colnames(tab)[5] <- "Standard Error of Excess"
         colnames(tab)[6] <- "Lower limit of Excess (95% CI)"
         colnames(tab)[7] <- "Uower limit of Excess (95% CI)"
         # tab <- round(tab, 0)
         if(input$month_or_week == "Monthly"){
            tab <- tab[with(tab, order(Year, Month)), ]
         }else{
            tab <- tab[with(tab, order(Year, Week)), ]
         }
         rownames(tab) <- NULL
         tab <- tab %>% 
              DT::datatable(extensions = 'Buttons', 
                    options = list(dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                    pageLength = 20)) %>% 
              formatRound(columns = 3:7, digits = 0)
         return(tab)
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
    if(input$table_show_type == "Death Count"){
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