# ui.R
# ANOVA demo

library(shiny)



navbarPage("ANOVA Demo",
           tabPanel("One Way ANOVA",
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        wellPanel(
                          numericInput("ssize", "Common Sample Sizes:", 10, min = 5, max = 1000)
                        ),
                        wellPanel(
                          numericInput("mu1", "Group 1 mean:", 100, min = 1, max = 1000),
                          numericInput("sd1", "Group 1 stdev:", 10, min = 10, max = 100)
                        ),
                        wellPanel(
                          numericInput("mu2", "Group 2 mean:", 150, min = 1, max = 1000),
                          numericInput("sd2", "Group 2 stdev:", 10, min = 10, max = 100)
                        ),
                        wellPanel(
                          numericInput("mu3", "Group 3 mean:", 200, min = 1, max = 1000),
                          numericInput("sd3", "Group 3 stdev:", 10, min = 10, max = 100)
                        )

                      ),  # end sidebarPanel
                    
                      mainPanel(
                        plotOutput("datainitplot"),
                        plotOutput("dataanovaplot"),
                        hr(),
                        
                        h4("ANOVA Model Output"),
                        verbatimTextOutput('anovaresults'),
                        hr(),
                        
                        h4("Posthoc Comparisons"),
                        verbatimTextOutput('tukeyfit'),
                        # hr(),
                        # h4("Summary of entire demo data set"),
                        # verbatimTextOutput('datainitsummary'),
                        hr(),
                        
                        h4("Summary of subset of data used for ANOVA"),
                        verbatimTextOutput('dataanovasummary'),
                        hr(),
                        
                        h4("The raw data"),
                        DT::dataTableOutput('datatableanova')
                      )
                      
                    )  # end sidebarLayout
                    
           ),  # end tabPanel ANOVA demo
           
           
           tabPanel("One Way ANOVA Diagnostics",
                    fluidPage(

                        plotOutput("plotfitresids"),
                        hr(),
                        
                        h4("Nomarlity of Residuals"),
                        verbatimTextOutput('shapirowilk'),
                        plotOutput("plotnormresids")
                      
                    )  # end fluidPage
                    
           )  # end tabPanel ANOVA diagnostics
           
)  # end navbarPage


# END CODE ####################################################################