
library(shiny)
library(shinythemes)
library(benford.analysis)
library(knitr)
library(dplyr)
library(kableExtra)

# see both graphs in the same panel

ui <- navbarPage("Benford Analysis",
  theme = shinytheme("yeti"),
  tabPanel("English",
   sidebarLayout(
      sidebarPanel(width = 3,
         fileInput("records", strong("Load dataset (.csv)"), multiple = FALSE),
         uiOutput('data_column'),
         sliderInput("digits", strong("Number of digits to test:"), 1, 3, 1),
         radioButtons("sign", strong("Sign:"),
                      c("Positive" = "positive",
                        "Negative" = "negative",
                        "Both" = "both"), inline = T),
         fluidRow(
           #column(12, align = "center" , submitButton("Refresh"), br()),
           #column(12, align = "center" , actionButton("reflesh", "Action button", class = "btn-primary")), br(),
           column(12, align = "center" , downloadButton("report", "Download report")))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Instructions",
                   column(12, includeMarkdown('instructions.Rmd'))),
          tabPanel("Distribution of Digits",
                  column(12, br(), h5("The first graph concerns the observation count with respect to its first two digits, comparing it with the value expected by the Benford's Law.\n
                                      The second graph concerns the chi-squared statistics consisting of deviations of observed values from expected values.")),
                   column(12, align = "center" ,
                          plotOutput('plot_digits', click = "digit_click", height = "500px"),
                          radioButtons("which_plot_digit", "",
                                       c("Digits" = "pdigit",
                                         "Rootogram" = "proot_digit",
                                         "Chi-Squared Difference" = "psquared"), inline = T),
                          checkboxInput("option", "See all graphs in the same panel (não implementado)"),
                          dataTableOutput('verclick')
                          )
                   ),
          tabPanel("Formal Tests",br(),
                   tabsetPanel(tabPanel("First Order Test",
                                        h5('The MAD ranges here\n chi-squared results...')
                                        ),
                               
                               tabPanel("Second Order Test",
                                        column(12, h5("The graph concerns the count for the ordered date difference")),
                                        column(12, align = "center" , plotOutput('plot_sec_ord', height = "500px"),
                                               radioButtons("which_plot_sec_ord", "",
                                                            c("Second Order" = "psec",
                                                              "Rootogram Second Order" = "proot_sec"), inline = T))                
                                        ),
                               tabPanel("Summation Test",
                                        column(12, h5("The first graph concerns the deviations of summation values from expected values.")),
                                        column(12, align = "center" ,
                                               plotOutput('plot_summ_dist', height = "500px"),
                                               radioButtons("which_plot_summ", "",
                                                            c("Summation" = "psum",
                                                              "Summation Difference" = "psumdif"), inline = T))
                                        ),
                               tabPanel("Last Two Digits Test",
                                       'Not Implemented'),
                               tabPanel("Mantissa Arc Test",
                                      column(12, align = "center" ,
                                      plotOutput('plot_mantissa', height = "500px")),
                                      verbatimTextOutput("mantissa_test")
                               )
                               )
                  ),
          tabPanel("Suspect Records",
                   numericInput("ndigits", strong('Number of suspicious groups:'), 3),
                   h5('leading digits by decreasing order of discrepancies (absolute differences)'),
                   column(12, align = "center" , tableOutput('leading_digits')),
                   h5("observations of the most suspicious groups with leading digits not complying with Benford's Law"),
                   column(12, align = "center" , dataTableOutput('suspect_records'))
          ),
          tabPanel("More", verbatimTextOutput('results_benf'))
        )

      )
   )
)
)


server <- function(input, output) {
      
  input_data <- reactive({
       if(is.null(input$records$name)){
         data("corporate.payment")
         corporate.payment
         }else{
        as.data.frame(read.csv(input$records$datapath, header = TRUE, sep = ";"))
       }
    })
  
  output$data_column <- renderUI({
    selectInput("column", strong("Column of dataset:"), names(input_data()), selected = "Amount")
  })
  
  records <- reactive({
    input_data()[, input$column]
  })
  
  results_benford <- reactive({
    benford(records(), input$digits, input$sign)
  })
  
  output$verclick <- renderDataTable({
    d1 <- min(results_benford()[["bfd"]]$digits)
    dn <- max(results_benford()[["bfd"]]$digits)
    d_click <- round(d1 + (input$digit_click$x)*(dn - d1), 0)
    getDigits(results_benford(), input_data(), d_click)
    # kable(align = "c") %>%
    # kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = F)
  })

  
  output$plot_digits <- renderPlot({
    switch(input$which_plot_digit,
                   pdigit = plot(results_benford(), except = c("rootogram digits","second order","rootogram second order", "summation","chi squared", "mantissa", "abs diff", "ex summation", "legend"), multiple=F),
                   proot_digit = plot(results_benford(), except = c("digits","second order", "rootogram second order", "summation","chi squared", "mantissa", "abs diff", "ex summation", "legend"), multiple=F),
                   psquared = plot(results_benford(), except = c("digits","second order", "rootogram second order", "summation", "mantissa", "abs diff", "ex summation", "legend"), multiple=F))
  })
  

  output$plot_sec_ord <- renderPlot({
    plot(results_benford(), except = c("digits","rootogram digits", "rootogram second order", "summation", "mantissa","abs diff", "chi squared", "ex summation", "legend"), multiple=T)
    switch(input$which_plot_sec_ord,
           psec = plot(results_benford(), except = c("digits","rootogram digits","rootogram second order", "summation","chi squared", "mantissa", "abs diff", "ex summation", "legend"), multiple=F),
           proot_sec = plot(results_benford(), except = c("digits","second order", "second order", "summation","chi squared", "mantissa", "abs diff", "ex summation", "legend"), multiple=F))
  })
  
  
  output$plot_summ_dist <- renderPlot({
    switch(input$which_plot_summ,
           psum = plot(results_benford(), except = c("digits","rootogram digits", "second order","rootogram second order", "mantissa","abs diff", "chi squared",  "ex summation", "legend"), multiple=T),
           psumdif = plot(results_benford(), except = c("summation", "digits","rootogram digits", "second order","rootogram second order","abs diff",  "mantissa", "chi squared", "legend"), multiple=T))
  })
  
  output$mantissa_test <- renderPrint({
    results_benford()$stats$mantissa.arc.test
  })
  
  output$plot_mantissa <- renderPlot({
    plot(results_benford(), except = c("digits","rootogram digits","second order","rootogram second order", "summation",'ex summation', "chi squared", "abs diff","legend"))
  })
  
  
  output$results_benf <- renderPrint({
    results_benford()
  })
  
  output$leading_digits <- function(){
      st <- head(suspectsTable(results_benford()), input$ndigits)
      names(st) <- c("Digits", "Absolute Difference")
      st%>%
        kable(align = "c") %>%
        kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = F)
      
  }
  
  output$suspect_records <-  renderDataTable({
    records <- extract.digits(records(), number.of.digits = input$digits)
    sp <- getSuspects(results_benford(), input_data(), how.many = input$ndigits)
    sp
    # kable(align = "c") %>%
    # kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = F)
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

