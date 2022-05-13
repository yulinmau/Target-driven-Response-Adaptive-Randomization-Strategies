#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI
shinyUI(
  fluidPage(
    # Application title
    titlePanel(h1("Target-driven Response Adaptive Randomization Strategies", style = "font-family:'times'")), 
    fluidRow(class = "R1",
             tabsetPanel(type = "pills",
                         ######## Calculate Sample Size Panel ############################################
                         tabPanel(h3("Calculate Sample Size", style = "font-family:'times'"),
                                  sidebarLayout(
                                    ### input controls
                                   sidebarPanel(
                                     h3("Parameters Input", style = "font-family:'times'"), 
                                     wellPanel(
                                       ## Power
                                       textInput(inputId = "Pow",
                                                 label = "Power :",
                                                 value = 0.8),
                                       ## alpha_star
                                       textInput(inputId = "alpha_star",
                                                 label = HTML("Significant level, &alpha; :"),
                                                 value = 0.05),
                                       ## beta1
                                       textInput(inputId = "beta1",
                                                 label = HTML(paste0("Recurrence rate ratio, exp(&beta;", tags$sub("R"), ") :")),
                                                 value = 1),
                                       ## beta2
                                       textInput(inputId = "beta2",
                                                 label = HTML(paste0("Hazard rate ratio for death, exp(&beta;",tags$sub("T"), ") :")),
                                                 value = 1.5),
                                       ## theta
                                       textInput(inputId = "theta",
                                                 label = HTML("Frailty variance, &theta; > 0 :"),
                                                 value = 1),
                                       ## r0
                                       textInput(inputId = "r0",
                                                 label = HTML(paste0("Baseline rate function, r",tags$sub("0"), " > 0 :")),
                                                 value = 0.05),
                                       ## lam0
                                       textInput(inputId = "lam0",
                                                 label = HTML(paste0("Baseline hazard function, &lambda;",tags$sub("0"), " > 0 :")),
                                                 value = 0.005),
                                       actionButton("start", span("Start!"), style = "font-size:20px;")
                                       ) # end wellPanel
                                     ),
                                     
                                     ### outputs
                                     mainPanel(h3("Result",
                                                  class = "alert alert-dismissible alert-info", 
                                                  style = "font-family:'times'"),
                                               div(tableOutput("SampleSizeResult"), 
                                                   style = "font-family:'times'"),
                                               tags$head(tags$style("#SampleSizeResult table {background-color: white; }", media="screen", type="text/css")),
                                               div(textOutput(outputId = "text"), style = "font-family:'times'")
                                               ) # end mainPanel
                                   ) # end sidebarLayout
                                  ), # end tabPanel
                         
                         ######## Calculate Calculate Allocation Rule ############################################
                          tabPanel(h3("Calculate Allocation Rule", style = "font-family:'times'"),
                                   sidebarLayout(
                                     ### input controls
                                     sidebarPanel(
                                       h3("Parameters Input", style = "font-family:'times'"),
                                       wellPanel(
                                         ## beta1
                                         textInput(inputId = "allobeta1",
                                                   label = HTML(paste0("Recurrence rate ratio, exp(&beta;",tags$sub("R"), ") :")),
                                                   value = 1),
                                         ## beta2
                                         textInput(inputId = "allobeta2",
                                                   label = HTML(paste0("Hazard rate ratio for death, exp(&beta;",tags$sub("T"), ") :")),
                                                   value = 1.5),
                                         ## theta
                                         textInput(inputId = "allotheta",
                                                   label = HTML("Frailty variance, &theta; > 0 :"),
                                                   value = 1),
                                         ## r0
                                         textInput(inputId = "allor0",
                                                   label = HTML(paste0("Baseline rate function, r",tags$sub("0"), " > 0 :")),
                                                   value = 0.05),
                                         ## lam0
                                         textInput(inputId = "allolam0",
                                                   label = HTML(paste0("Baseline hazard function, &lambda;",tags$sub("0"), " > 0 :")),
                                                   value = 0.005),
                                         actionButton("start2", span("Start!"), style = "font-size:20px;")
                                         ) # end wellPanel
                                       ), # end sidebarPanel
                                     
                                     ### outputs
                                     mainPanel(h3("Result", style = "font-family:'times'",
                                                  class = "alert alert-dismissible alert-info"),
                                               div(tableOutput("allocationResult"), style = "font-family:'times'"),
                                               tags$head(tags$style("#allocationResult table {background-color: white; }", media="screen", type="text/css")),
                                               h3("Rule 1 : Maximize the test power.", 
                                                  style = "font-family: 'times'"),
                                               h3("Rule 2 : Minimize the total number of recurrent events.", 
                                                  style = "font-family: 'times'"),
                                               h3("Rule 3 : Minimize the total hazard rate of terminal events.", 
                                                  style = "font-family: 'times'"),
                                               h3("Note : Allocation probability is favor the better treatment based on the current responses and achieve a patient benefit objective.", 
                                                  style = "font-family: 'times'"))
                                     ) # end sidebarLayout
                                   ), # end tabPanel
                         
                         ######## GitHub ############################################
                         tabPanel(h3("Reference", style = "font-family:'times'"),
                                  h3("You can see the source code on github.", style = "font-family:'times'", class = "alert alert-dismissible alert-info"),
                                  wellPanel("https://github.com/yulinmau/Target-driven-Response-Adaptive-Randomization-Strategies.git", style = "font-family:'times'")
                                  )
                         )  # end tabsetPanel
              
    ), tags$head(tags$style(".R1{background-color: lightgray; font-size:150%}"))
    
  )  # fluidPage
)  # end shinyUI
