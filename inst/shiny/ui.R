require("plotly")
require("DT")
require("ExcessMortalityApp")
require("markdown")
require("INLA")

ui <- fluidPage(
  use_busy_spinner(spin = "fading-circle"),

  tags$head(
    tags$style(HTML(".shiny-output-error-validation {
                    color: #ff0000;
                    font-weight: bold;}"))),
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel(
      title = div("Excess Mortality Calculator", 
      tags$a(img(src="vital_strategies.JPG", style="width:200px; position: relative; top: -1px;"), href="https://www.vitalstrategies.org/")),
      windowTitle =  "Excess Mortality Calculator"
    ),
  p("Developed by the openVA team", a(href="https://openva.net", "(https://openVA.net)."), "This work is supported by Vital Strategies as part of the Bloomberg Philanthropies Data for Health Initiative."),
  hr(),
  shinyjs::useShinyjs(),

  sidebarLayout(
    sidebarPanel(
      h4("Data Input"),
      fileInput("readIn",
                "Upload your own data here (CSV file)",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      div(style = "margin-top: -20px"),
      actionLink("instruction_link", label = "Have questions? Read the instructions on how to use the App. You can also refer to the Example input data at the bottom of this section."),
      br(),
      br(),
      h4("Select Time Scale"),
      selectizeInput(
          inputId="month_or_week", label=NULL,
          choices=c("Monthly" = "Monthly",
                    "Weekly" = "Weekly"),
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width="300px"
      ),
      
      # selectInput(inputId="month_or_week", label=NULL,
      #             choices=c("Monthly" = "Monthly",
      #                       "Weekly" = "Weekly"),
      #             selected = NULL,
      #             width="200px"),
      h4("Select Model"),
      selectInput("which_model", NULL, choices=c("Poisson Regression", "Simple Baseline"), width="300px"),
      br(),      
      h4("Select Variables"),
      selectInput("raw_data_population", "Select Column Specifying Population Counts:", choices=c()),
      selectInput("raw_data_sex", "Select Column Specifying Sex:", choices=c()),
      selectInput("raw_data_age", "Select Column Specifying Age:", choices=c()),

  
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
                                selectInput("plot_show", h4("Plot type"), choices=c("Death Counts", "Death Counts (Y-axis Starting From 0)", "Excess Death Counts"), width="400px"), 
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
                            downloadBttn("download_baseplot", "Download Plot", 
                                    size = "sm", 
                                    style = 'unite', 
                                    color = 'primary')
                          ),

                          br(),
                          br(),
                          DT::dataTableOutput("baselineTab")),
              tabPanel(title = "Comparison By Age And Sex",
                          fluidRow(
                            column(3, 
                                selectInput("compare_plot_show", h4("Plot type"), choices=c("Death Counts", "Excess Death Counts", "Excess Death Counts (Overlay)"), width="600px"), 
                            ), 
                            column(3, 
                                selectInput("compare_plot_by", h4("Comparison"), choices=c(), width="400px"), 
                            ),
                            column(5, 
                                conditionalPanel("input.processMe", 
                                  br(),
                                  br(),
                                  downloadBttn("download_compareplot", "Download Plot", 
                                    size = "sm", 
                                    style = 'unite', 
                                    color = 'primary')
                                ) 
                            )
                          ),
                          plotlyOutput("comparePlot")

                ),
              tabPanel(title = "Historical Data Explorer",
                          fluidRow(
                            column(3,
                              selectInput("table_show_type", h4("Historical Data"), choices=c("Death Counts", "Death Rate Number of Deaths Per 100,000 Population)"), width="400px")
                            ), 
                            column(3,
                                selectInput("table_show_sex", h4("Sex"), choices=c(), width="400px"), 
                            ),
                            column(3,
                                selectInput("table_show_age", h4("Age"), choices=c(), width="400px"),
                            )
                          ),
                          plotlyOutput("linePlotSummary"),
                          tableOutput("tableSummary")), 
              tabPanel(title = "Methodology",
                withMathJax(),
                tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                'HTML-CSS': {
                      fonts: ['TeX'],
                      styles: {
                        scale: 110,
                        '.MathJax': { padding: '1em 0.1em', color: 'royalblue ! important' }
                      }
                    }
                });
                </script>
                ")),
                withMathJax(includeMarkdown("method.rmd"))
              ),
              tabPanel(title = "How To Use The App",
                withMathJax(includeMarkdown("instruction.rmd"))
              ), 
        id = "navbar_id"
      )
    )
  )
)
