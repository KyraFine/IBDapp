#app for blood biomarker based diagnosis and activity tracking

#load libraries and model
library(shiny)
library(bslib)
library(tidyverse)
library(tidymodels)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#load(file = 'activity_tracker.Rda')
#rsconnect::writeManifest()
#ui
ui <- fluidPage(
  actionButton("predict", "Calcuate Probable Diagnosis"),
  numericInput('neut',
               label = 'Neutrophils',
               value = 0
  ),
  numericInput('lymph',
               label = 'Lymphocytes',
               value = 0
  ),
  numericInput('mono',
               label = 'Monocytes',
               value = 0
  ),
  numericInput('eosin',
               label = 'Eosinophils',
               value = 0
  ),
  numericInput('baso',
               label = 'Basophils',
               value = 0
  ),
  numericInput('hb',
               label = 'Hemoglobin',
               value = 0
  ),
  numericInput('plt',
               label = 'Platelets',
               value = 0
  ),
  numericInput('age',
               label = 'Age',
               value = 0
  ),
  textOutput("actprob"),
  textOutput("remprob"),
  textOutput("bestprob")
  
)

#server
server <- function(input, output) {
  
  patient_values <- eventReactive(input$predict, {
    as.tibble(data.frame(
      status = NA,
      neut = (((input$neut)-5.044868)/2.521903),
      lymph = (((input$lymph)-2.156628)/1.05681),
      mono = (((input$mono)-0.619986)/0.2746403),
      eosin = (((input$eosin)-0.2466767)/0.2668758),
      baso = (((input$baso)-0.04055256)/0.03872633),
      age = (((input$age)-32.51449)/17.42219),
      hb = (((input$hb)-129.4782)/17.08963),
      plt = (((input$plt)-313.8753)/116.1361)
    ))
  })
  
  
  predicted <- eventReactive(input$predict, {
    patient_values <- patient_values()
    predict(diff_model, new_data = patient_values, type = "prob")  
  })
  pred_act <- eventReactive(input$predict, {
    predicted <- predicted()
    predicted[1, 2]
  })
  pred_rem <- eventReactive(input$predict, {
    predicted <- predicted()
    predicted[1, 1]
  })
  pred_best <- eventReactive(input$predict, {
    pred_act <- pred_act()
    pred_rem <- pred_rem()
    ifelse(pred_act<pred_rem, "in remission", "active")
    #assigned active as winning the tie if the values are equal because of the danger of not acting on IBD if it is active
  }) 
  output$actprob  <- renderText({
    paste("probability of active IBD is", pred_act())
  })
  
  output$remprob <- renderText({
    paste("probability of remission IBD", pred_rem())
  })
  output$bestprob <- renderText({
    paste("The patient's IBD is most likely", pred_best())
  })
  
}

#app
shinyApp(ui = ui, server = server)