
library(shiny)
library(shinythemes)
library(benford.analysis)


ui <- navbarPage("Benford Analysis",
  theme = shinytheme("yeti"),
  tabPanel("English",
   sidebarLayout(
      sidebarPanel(
         fileInput("records", strong("load dataset (.csv)"), multiple = FALSE),
         numericInput("column", strong("column of dataset:"), 1),
         numericInput("digits", strong("number of digits to test:"), 1),
         numericInput("ndigits", strong('number of suspicious groups:'), 3),
         submitButton("Apply Changes"), br(),
         downloadButton("report", "Download report")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Instructions", br(), includeMarkdown('instructions.Rmd'), textOutput('teste')),
          tabPanel("Distribution of Digits",
                  column(12,
                         h5("The first graph concerns the observation count with respect to its first two digits, comparing it with the value expected by the Benford's Law."),
                         h5("The second graph concerns the chi-squared statistics consisting of deviations of observed values from expected values.")
                   ),
                   column(12, align = "center" , plotOutput('plot_digits', click = "digit_click"))
                   ),
          tabPanel("Second Order Test",
                   column(12,
                          h5("The graph concerns the count for the ordered date difference")
                   ),
                   column(12, align = "center" , plotOutput('plot_sec_ord'))
          ),
          tabPanel("Summation Test",
                   column(12,
                          h5("The first graph concerns the deviations of summation values from expected values.")
                   ),
                   column(12, align = "center" , plotOutput('plot_summ_dist'))),
          tabPanel("Suspect Records",
                   h5('leading digits by decreasing order of discrepancies (absolute differences)'),
                   column(12, align = "center" , dataTableOutput('leading_digits')),
                   h5("observations of the most suspicious groups with leading digits not complying with Benford's Law"),
                   column(12, align = "center" , dataTableOutput('suspect_records'))
          ),
          tabPanel("More...", verbatimTextOutput('results_benf'))
        )

      )
   )
)
)


server <- function(input, output) {
  
  input_data <- reactive({
       if(is.null(input$records$name)){
         data("corporate.payment")
         data <- corporate.payment$Amount
         }else{
       data <- read.csv(input$records$datapath, header = TRUE, sep = ";")
       data[, input$column]}
    })
  
  results_benford <- reactive({
    benford(input_data(), input$digits)
    })
  
  resul <- reactive({input$digit_click})
  
  output$teste <- renderText({
    resul()
  })
  
  output$plot_digits <- renderPlot({
    plot(results_benford(), except=c("second order", "summation", "mantissa", "abs diff", "ex summation"), multiple=T)
  })
  
  output$plot_sec_ord <- renderPlot({
    plot(results_benford(), except=c("digits", "summation", "mantissa","abs diff", "chi squared", "ex summation"), multiple=T)
  })
  
  output$plot_summ_dist <- renderPlot({
    plot(results_benford(), except=c("digits", "second order", "mantissa", "chi squared",  "ex summation"), multiple=T)
  })
  
  output$results_benf <- renderPrint({
    results_benford()
  })
  
  output$leading_digits <- renderDataTable({
    head(suspectsTable(results_benford()), input$ndigits)
  })
  
  output$suspect_records <- renderDataTable({
    input_data <- extract.digits(input_data(), number.of.digits = input$digits)
    getSuspects(results_benford(), input_data, how.many = input$ndigits)
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(results_benford = results_benford())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}


shinyApp(ui = ui, server = server)

