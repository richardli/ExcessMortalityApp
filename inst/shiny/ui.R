require("plotly")
require("DT")
require("ExcessMortalityApp")
ui <- fluidPage(

  tags$head(
    tags$style(HTML(".shiny-output-error-validation {
                    color: #ff0000;
                    font-weight: bold;}"))),
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel("Excess Mortality Calculator"),
  p("Developed by the openVA team", a(href="https://openva.net", "(https://openVA.net)."), "This work is supported by Vital Strategies",
    HTML("&emsp;"),
    tags$a(img(src="vital_strategies.png", style="width:100px"), href="https://www.vitalstrategies.org/")),
  hr(),
  shinyjs::useShinyjs(),

  sidebarLayout(
    sidebarPanel(
    selectInput(inputId="month_or_week", label="Select Time Scale:",
                  choices=c("Monthly" = "Monthly",
                            "Weekly" = "Weekly"),
                  width="150px"),
      br(),
      h4("Data Input"),
      fileInput("readIn",
                "Upload your own data here (CSV file)",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      h5("Details about the input data format are summarized in ",  tags$a(href="https://github.com/richardli/ExcessMortalityApp/blob/main/README.md", "this document.")),
      br(),
      h4("Options"),
      selectInput("raw_data_population", "Select Column specifying Population Counts:", choices=c()),
      selectInput("raw_data_sex", "Select Column specifying Sex:", choices=c()),
      selectInput("raw_data_age", "Select Column specifying Age:", choices=c()),
      br(),
      conditionalPanel("output.fileUploaded", actionButton("processMe", h4("Analyze my data!")), align = "center"),
      h4("Example input data (monthly):"),
      # actionButton(
      #     "takeDataset1", "Use example dataset", class = "btn-block"
      # ),
      # br(),
      downloadLink(
          "downloadDataset1", "Download monthly example dataset"
      ),
      br(),
      downloadLink(
          "downloadDataset3", "Download monthly example dataset by sex"
      ),
      br(),
      downloadLink(
          "downloadDataset2", "Download monthly example dataset by age"
      ),
      br(),
      downloadLink(
          "downloadDataset4", "Download monthly example dataset by sex and age"
      ),
      br(),
      h4("Example input data (weekly):"),
      downloadLink(
          "downloadDataset5", "Download weekly example dataset"
      ),
      br(),
      downloadLink(
          "downloadDataset6", "Download weekly example dataset by sex"
      ),
      br(),
      downloadLink(
          "downloadDataset7", "Download weekly example dataset by age"
      ),
      br(),
      downloadLink(
          "downloadDataset8", "Download weekly example dataset by sex and age"
      ),
      br(),      
      br(),
      br()
    ),
    ## Outputs
    mainPanel(
      textOutput("csvCheck"),
      navbarPage(title = NULL,
                 h4(textOutput("message_file_upload")),
                 tabPanel(title = "Excess Mortality",
                          fluidRow(
                            column(3, 
                                selectInput("plot_show", h4("Plot type"), choices=c("Death Counts", "Excess Death Counts"), width="400px"), 
                            ), 
                            column(3, 
                                selectInput("baseline_show_sex", h4("Sex"), choices=c(), width="400px"), 
                            ), 
                            column(3, 
                                selectInput("baseline_show_age", h4("Age"), choices=c(), width="400px")
                            )
                          ),
                          plotlyOutput("baselinePlot"),
                          DT::dataTableOutput("baselineTab")),
                 tabPanel(title = "Historical Data Explorer",
                          fluidRow(
                            column(3,
                              selectInput("table_show_type", h4("Historical data"), choices=c("Death Counts", "Death Rate (number of deaths per 100,000 population)"), width="400px")
                            ), 
                            column(3,
                                selectInput("table_show_sex", h4("Sex"), choices=c(), width="400px"), 
                            ),
                            column(3,
                                selectInput("table_show_age", h4("Age"), choices=c(), width="400px"),
                            )
                          ),
                          plotlyOutput("linePlotSummary"),
                          tableOutput("tableSummary"))
      )
    )
  )
)
