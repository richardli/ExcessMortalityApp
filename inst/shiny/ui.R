require("plotly")
require("DT")
require("ExcessMortalityApp")
ui <- fluidPage(
  use_busy_spinner(spin = "fading-circle"),

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
      selectInput("raw_data_population", "Select Column Specifying Population Counts:", choices=c()),
      selectInput("raw_data_sex", "Select Column Specifying Sex:", choices=c()),
      selectInput("raw_data_age", "Select Column Specifying Age:", choices=c()),
      selectInput("which_model", "Select Model:", choices=c("Poisson regression", "Simple Baseline")),
      br(),
  
      conditionalPanel("output.fileUploaded", 
        shinyWidgets::actionBttn("processMe", 
          label = "Analyze my data",
          size = "md",
          color = "primary",
          style = "jelly",
          icon = icon("sliders"),
          block = TRUE), 
        align = "center"),
      h4("Example input data:"),
      downloadLink(
          "downloadDataset1", "Download monthly example dataset"
      ),
      br(),
      # downloadLink(
      #     "downloadDataset3", "Download monthly example dataset by sex"
      # ),
      # br(),
      # downloadLink(
      #     "downloadDataset2", "Download monthly example dataset by age"
      # ),
      # br(),
      downloadLink(
          "downloadDataset4", "Download monthly example dataset by sex and age"
      ),
      br(),
      # h4("Example input data (weekly):"),
      # downloadLink(
      #     "downloadDataset5", "Download weekly example dataset"
      # ),
      # br(),
      # downloadLink(
      #     "downloadDataset6", "Download weekly example dataset by sex"
      # ),
      # br(),
      # downloadLink(
      #     "downloadDataset7", "Download weekly example dataset by age"
      # ),
      # br(),
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
                          conditionalPanel("input.processMe", 
                            downloadButton("download_baseplot", "Download Plot")
                          ),
                          br(),
                          br(),
                          DT::dataTableOutput("baselineTab")),
              tabPanel(title = "Comparison by Age and Sex",
                          fluidRow(
                            column(3, 
                                selectInput("compare_plot_show", h4("Plot type"), choices=c("Side by Side", "Overlay"), width="400px"), 
                            ), 
                            column(3, 
                                selectInput("compare_show", h4("Comparison"), choices=c(), width="400px"), 
                            )
                          ),
                          plotlyOutput("comparePlot"),
                          conditionalPanel("input.processMe", 
                            downloadButton("download_compareplot", "Download Plot")
                          )
                ),
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
