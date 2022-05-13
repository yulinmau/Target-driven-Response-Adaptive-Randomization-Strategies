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

)  # end shinyUI
