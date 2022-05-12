#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # themeSelector(),
    # Application title
    titlePanel(h1("Target-driven Response Adaptive Randomization Strategies", 
                  style = "font-family:'times'")), # , style = "font-size:200%"
    fluidRow(class = "R1",
             tabsetPanel(type = "pills",
                         ######## Calculate Sample Size Panel ############################################
                          tabPanel(h3("Calculate Sample Size", style = "font-family:'times'"), # , style = "font-size:100%"
                                   sidebarLayout(
                                     ### input controls
                                     sidebarPanel(
                                       h3("Parameters Input", style = "font-family:'times'"), # , style = "font-size:100%"
                                       wellPanel(
                                         ## Power
                                         textInput(inputId = "Pow", #width = "300px",
                                                   label = "Power :",
                                                   value = 0.8),
                                         ## alpha_star
                                         textInput(inputId = "alpha_star", #width = "300px",
                                                   label = HTML("Significant level, &alpha; :"),
                                                   value = 0.05),
                                         ## beta1
                                         textInput(inputId = "beta1", #width = "300px",
                                                   label = HTML(paste0("Recurrence rate ratio, exp(&beta;", tags$sub("R"), ") :")),
                                                   value = 1),
                                         ## beta2
                                         textInput(inputId = "beta2", #width = "300px",
                                                   label = HTML(paste0("Hazard rate ratio for death, exp(&beta;",tags$sub("T"), ") :")),
                                                   value = 1.5),
                                         ## theta
                                         textInput(inputId = "theta", #width = "300px",
                                                   label = HTML("Frailty variance, &theta; > 0 :"),
                                                   value = 1),
                                         ## r0
                                         textInput(inputId = "r0", #width = "300px",
                                                   label = HTML(paste0("Baseline rate function, r",tags$sub("0"), " > 0 :")),
                                                   value = 0.05),
                                         ## lam0
                                         textInput(inputId = "lam0", #width = "300px",
                                                   label = HTML(paste0("Baseline hazard function, &lambda;",tags$sub("0"), " > 0 :")),
                                                   value = 0.005),
                                         actionButton("start", span("Start!"), style = "font-size:20px;")#,
                                         # helpText(tags$h4("Press Quit to exit the application")), 
                                         # actionButton("quit", "Quit", 
                                         #              # style = "font-size:150%", 
                                         #              class ="btn btn-warning btn-lg")
                                         ) # end wellPanel
                                       ),
                                     
                                     ### outputs
                                     mainPanel(h3("Result", # style = "font-size:200%",
                                                  class = "alert alert-dismissible alert-info", 
                                                  style = "font-family:'times'"),
                                               div(tableOutput("SampleSizeResult"), style = "font-family:'times'"), # ,style = "font-size:100%"
                                               tags$head(tags$style("#SampleSizeResult table {background-color: white; }", media="screen", type="text/css")),
                                               div(textOutput(outputId = "text"), style = "font-family:'times'")#,
                                               # p("Press Quit to", span("exit", style = "color:blue"), "the application",
                                               #   style = "font-family: 'times'"),
                                               # h3("The required total sample size was calculated for a specified parameters based on the composite endpoint.", 
                                               #    style = "font-family: 'times'")
                                               ) # end mainPanel
                                     )
                                   ),
                         ######## Calculate Calculate Allocation Rule ############################################
                          tabPanel(h3("Calculate Allocation Rule", style = "font-family:'times'"), # , style = "font-size:100%"
                                   #div(style = "height:800px; background-color: yellow;", "This is an example")
                                   sidebarLayout(
                                     ### input controls
                                     sidebarPanel(
                                       h3("Parameters Input", style = "font-family:'times'"), # , style = "font-size:100%"
                                       wellPanel(
                                         ## beta1
                                         textInput(inputId = "allobeta1", #width = "300px",
                                                   label = HTML(paste0("Recurrence rate ratio, exp(&beta;",tags$sub("R"), ") :")),
                                                   value = 1),
                                         ## beta2
                                         textInput(inputId = "allobeta2", #width = "300px",
                                                   label = HTML(paste0("Hazard rate ratio for death, exp(&beta;",tags$sub("T"), ") :")),
                                                   value = 1.5),
                                         ## theta
                                         textInput(inputId = "allotheta", #width = "300px",
                                                   label = HTML("Frailty variance, &theta; > 0 :"),
                                                   value = 1),
                                         ## r0
                                         textInput(inputId = "allor0", #width = "300px",
                                                   label = HTML(paste0("Baseline rate function, r",tags$sub("0"), " > 0 :")),
                                                   value = 0.05),
                                         ## lam0
                                         textInput(inputId = "allolam0", #width = "300px",
                                                   label = HTML(paste0("Baseline hazard function, &lambda;",tags$sub("0"), " > 0 :")),
                                                   value = 0.005),
                                         actionButton("start2", span("Start!"), style = "font-size:20px;")#, 
                                         # helpText(tags$h4("Press Quit to exit the application")), 
                                         # actionButton("quit2", "Quit", 
                                         #              # style = "font-size:100%", 
                                         #              class ="btn btn-warning btn-lg")
                                       ) # end wellPanel
                                     ),
                                     
                                     ### outputs
                                     mainPanel(h3("Result", style = "font-family:'times'", 
                                                  # style = "font-size:200%",
                                                  class = "alert alert-dismissible alert-info"),
                                               div(tableOutput("allocationResult"), style = "font-family:'times'"), # ,style = "font-size:100%"
                                               tags$head(tags$style("#allocationResult table {background-color: white; }", media="screen", type="text/css")),
                                               h3("Rule 1 : Maximize the test power.", 
                                                  style = "font-family: 'times'"),
                                               h3("Rule 2 : Minimize the total number of recurrent events.", 
                                                  style = "font-family: 'times'"),
                                               h3("Rule 3 : Minimize the total hazard rate of terminal events.", 
                                                  style = "font-family: 'times'"),
                                               h3("Note : Allocation probability is favor the better treatment based on the current responses and achieve a patient benefit objective.", 
                                                  style = "font-family: 'times'")) 
                                   )
                                   )
                         )  # end tabsetPanel
              
    ), tags$head(tags$style(".R1{background-color: lightgray; font-size:150%}"))
    
  )  # fluidPage
)  # end shinyUI
