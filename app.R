
library(shiny)
library(shinythemes)
# install the GitHub version
# devtools::install_github("carloscinelli/benford.analysis", build_vignettes = TRUE)
library(benford.analysis)
library(knitr)
library(dplyr)
library(kableExtra)

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
           column(12, align = "center" , downloadButton("report", "Download report")))
      ),
      
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel("Instructions",
                   column(12, includeMarkdown('instructions.Rmd'))),
          tabPanel("Distribution of Digits",
                  column(12, br(), h5("The first graph concerns the observation count with respect to its first two digits, comparing it with the value expected by the Benford's Law.\n
                                      The second graph concerns the chi-squared statistics consisting of deviations of observed values from expected values.")),
                   column(10, align = "center" ,
                          plotOutput('plot_digits', click = "digit_click", height = "500px"),
                          #verbatimTextOutput('tt'),
                          radioButtons("which_plot_digit", "",
                                       c("Digits" = "pdigit",
                                         "Rootogram" = "proot_digit",
                                         "Chi-Squared Difference" = "psquared"), inline = T),
                          dataTableOutput('selected_data')
                          ),
                  column(2, 
                         'Plot Options',
                         selectInput("col.by", strong("Color Plot Acoording:"), choices = c("Default", "Chi-Squared", "Z-statistic", "Absolute Diff.", "Difference")),
                         numericInput("nclasses", "Number of classes:", value = 2, step = 1,min = 2),
                         #checkboxInput("option", "See graphs in the same panel"),
                         checkboxInput("err.bound", "Error bounds"),
                         numericInput("alpha", strong("Significance level:"), value = 0.05, step = 0.01, min = 0, max = 1),
                         downloadButton(outputId = "down", label = "Download the plot"))
                   ),
          tabPanel("Auxiliary Tests",br(),
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
                   numericInput("ndigits", strong('Number of suspicious groups:'), 3, min = 1),
                   selectInput('metric', strong("Metric:"), choices = c("Absolute Difference", "Difference", "Chi-Squared", "Absolute Excess Summation")),
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

  d_click <- reactive({
    d1 <- min(results_benford()[["bfd"]]$digits)
    dn <- max(results_benford()[["bfd"]]$digits)
    round(d1 + (input$digit_click$x)*(dn - d1)*1/(input$digit_click$domain$right - input$digit_click$domain$left), 0)
  })
  
  output$selected_data <- renderDataTable({
    getDigits(results_benford(), input_data(), d_click())
  })

  # col.bar <- reactive({
  #   ndigts <- length(results_benford()[["bfd"]]$digits)
  #   col.b <- rep("green", ndigts)
  #   col.b[!(results_benford()[["bfd"]]$digits%in%d_click())] <- "lightblue"
  #   col.b
  # })
  # 
  # output$tt=renderText({
  #   col.bar()
  #  })
  
  color_bars <- function(){
    scale1 <- colorRampPalette(c("lightblue", "blue"))
    scale2 <- colorRampPalette(c("red","orange","lightblue", "blue"))
    switch(input$col.by,
           "Default" = "lightblue",
           "Absolute Diff." = {
             absolute.diff <- results_benford()[["bfd"]]$absolute.diff
             absolute.diff.classes <- cut(absolute.diff, breaks = seq(min(absolute.diff), max(absolute.diff), length.out = input$nclasses + 1), include.lowest = T)   
             as.character(factor(absolute.diff.classes, labels = scale1(input$nclasses), levels = levels(absolute.diff.classes)))},
           "Difference" = {
             difference <- results_benford()[["bfd"]]$difference
             difference.classes <- cut(difference, breaks = seq(min(difference), max(difference), length.out = input$nclasses + 1), include.lowest = T)   
             as.character(factor(difference.classes, labels = scale2(input$nclasses), levels = levels(difference.classes)))},
           "Chi-Squared" = {
             squared.diff <- results_benford()[["bfd"]]$squared.diff
             squared.diff.classes <- cut(squared.diff, breaks = seq(min(squared.diff), max(squared.diff), length.out = input$nclasses + 1), include.lowest = T)   
             as.character(factor(squared.diff.classes, labels = scale1(input$nclasses), levels = levels(squared.diff.classes)))},
           "Z-statistic" ={
             z.statistic <- results_benford()[["bfd"]]$z.statistic
             z.statistic.classes <- cut(z.statistic, breaks = seq(min(z.statistic), max(z.statistic), length.out = input$nclasses + 1), include.lowest = T)   
             as.character(factor(z.statistic.classes, labels = scale2(input$nclasses), levels = levels(z.statistic.classes)))
           }
    )
  }
  
  p_digits <- function(){
    # ndigts <- length(results_benford()[["bfd"]]$digits)
    # col.b <- rep("green", ndigts)
    # col.b[!(results_benford()[["bfd"]]$digits%in%d_click())] <- "lightblue"
    
    switch(input$which_plot_digit,
           pdigit = plot(results_benford(), select = "digits", col.bar = color_bars(), err.bounds = input$err.bound, alpha = input$alpha),
           proot_digit = plot(results_benford(), select = "rootogram digits", col.bar = color_bars(), err.bounds = input$err.bound, alpha = input$alpha),
           psquared = plot(results_benford(), select = "chi squared"))
  }
  
  output$plot_digits <- renderPlot({
    p_digits()
  })
  
  p_sec_ord <- function(){
    switch(input$which_plot_sec_ord,
           psec = plot(results_benford(), select = "second order"),
           proot_sec = plot(results_benford(), select = "rootogram second order"))
  }
  
  output$plot_sec_ord <- renderPlot({
    p_sec_ord()
  })
  
  p_summ_dist <- function(){
    switch(input$which_plot_summ,
           psum = plot(results_benford(), select = "summation"),
           psumdif = plot(results_benford(), select = "ex summation"))
  }
  
  output$plot_summ_dist <- renderPlot({
    p_summ_dist()
  })
  
  output$mantissa_test <- renderPrint({
    marc(results_benford())
  })
  
  p_mantissa <- function(){
    plot(results_benford(), select = "mantissa")
  }
  
  output$plot_mantissa <- renderPlot({
    p_mantissa()
  })
  
  output$results_benf <- renderPrint({
    results_benford()
  })
  
  output$leading_digits <- function(){
    by.metric <- switch(input$metric,
                        "Absolute Difference" = "absolute.diff",
                        "Difference" = "difference",
                        "Chi-Squared" = "squared.diff",
                        "Absolute Excess Summation" = "abs.excess.summation")
      st <- head(suspectsTable(results_benford(), by = by.metric), input$ndigits)
      names(st) <- c("Digits", input$metric)
      st%>%
        kable(align = "c") %>%
        kable_styling(bootstrap_options = c("striped", "bordered", "hover"), full_width = F)
      
  }

  output$suspect_records <-  renderDataTable({
    records <- extract.digits(records(), number.of.digits = input$digits)
    by.metric <- switch(input$metric,
                        "Absolute Difference" = "absolute.diff",
                        "Difference" = "difference",
                        "Chi-Squared" = "squared.diff",
                        "Absolute Excess Summation" = "abs.excess.summation")
    sp <- getSuspects(results_benford(), input_data(), by = by.metric, how.many = input$ndigits)
    sp
  })
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("plot", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file, width = 20, bg = "transparent")
      p_digits()
      dev.off()
    } 
  )
  
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

