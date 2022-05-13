#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  ### Sample Size function ################
  SampleSizefunction = reactive({
    if (input$start == 0)
      return(NULL)
    
    if (input$start > 0){
      Pow <- isolate(input$Pow) %>% as.numeric()
      beta1 <- isolate(input$beta1) %>% as.numeric()
      beta2 <- isolate(input$beta2) %>% as.numeric()
      theta <- isolate(input$theta) %>% as.numeric()
      r0 <- isolate(input$r0) %>% as.numeric()
      lam0 <- isolate(input$lam0) %>% as.numeric()
      alpha_star <- isolate(input$alpha_star) %>% as.numeric()
      
      SampleSize <- CalculateSampleSize(Pow = Pow,
                                        beta1 = log(beta1),
                                        beta2 = log(beta2),
                                        theta = theta,
                                        r0 = r0,
                                        lam0 = lam0,
                                        alpha_star = alpha_star,
                                        tau = 100) %>% ceiling()
      if(SampleSize %% 2 != 0){
        SampleSize <- SampleSize + 1
      }
      
      Result <- data.frame("Total sample size" = sprintf("%.0f", SampleSize))
      Result
    }
  })
  
  ### Sample Size result ################
  output$SampleSizeResult <- renderTable({
    if (input$start == 0)
      return(NULL)
    
    if (input$start > 0){
      withProgress(message = "Calculating...",
                   value = 0, {
                     setProgress(1)
                     SampleSizefunction()
                     })
      }
    }, align = "c",
    sanitize.text.function = function(x){x})
  
  output$text <- renderText({
    if (input$start == 0)
      return(NULL)
    
    if (input$start > 0){
      paste0("The required total sample size was ", SampleSizefunction()[1,1],
             " for a specified parameters based on the composite endpoint.")
    }
  })
  
  ### Allocation function ################
  Allocationfunction = reactive({
    if (input$start2 == 0)
      return(NULL)
    
    if (input$start2 > 0){
      # Calculate allocation Rule
      allobeta1 <- isolate(input$allobeta1) %>% as.numeric()
      allobeta2 <- isolate(input$allobeta2) %>% as.numeric()
      allotheta <- isolate(input$allotheta) %>% as.numeric()
      allolam0 <- isolate(input$allolam0) %>% as.numeric()
      allor0 <- isolate(input$allor0) %>% as.numeric()
      
      Adap <- function(p){
        E_VarF(beta1 = log(allobeta1), 
               beta2 = log(allobeta2), 
               the = allotheta, 
               lamD = allolam0, 
               lamR = allor0, p)  
      }
      Adap2 <- function(p) 1/Adap(p)*(p*allor0 + (1-p)*allor0*exp(log(allobeta1)))
      Adap3 <- function(p) 1/Adap(p)*(p*allolam0 + (1-p)*allolam0*exp(log(allobeta2)))
      
      alloI <- optimize(Adap, c(0.1, 0.9), maximum = T)[[1]]
      AllocationI <- data.frame(Rule = "Rule 1",
                                "Allocation probability" = paste0("<strong>", sprintf("%.3f", alloI), "</strong>"),
                                check.names = FALSE)
      
      alloII <- optimize(Adap2, c(0.1, 0.9), maximum = F)[[1]]
      AllocationII <- data.frame(Rule = "Rule 2",
                                 "Allocation probability" = paste0("<strong>", sprintf("%.3f", alloII), "</strong>"),
                                 check.names = FALSE)
      
      alloIII <- optimize(Adap3, c(0.1, 0.9), maximum = F)[[1]]
      AllocationIII <- data.frame(Rule = "Rule 3",
                                  "Allocation probability" = paste0("<strong>", sprintf("%.3f", alloIII), "</strong>"),
                                  check.names = FALSE)
      
      AllocationResult <- rbind(AllocationI, AllocationII, AllocationIII)
      AllocationResult
    }
  })
  
  ### Allocation result ################
  output$allocationResult <- renderTable({
    if (input$start2 == 0)
      return(NULL)
    
    if (input$start2 > 0){
      withProgress(message = "Calculating...",
                   value = 0, {
                     setProgress(1)
                     Allocationfunction()
                   })
    }
  }, align = "c",
  sanitize.text.function = function(x){x})
})
