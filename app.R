#app for blood biomarker based diagnosis and activity tracking

#load libraries and model
library(shiny)
library(bslib)
library(tidyverse)
library(tidymodels)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load(file = 'activity_tracker.Rda')
load(file = 'IBSvIBD.Rda')
#rsconnect::writeManifest()

#activity tracker
#ui
ui = tagList(
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "IBD assessment helper",
    tabPanel("activity tracker",
             fluidPage(
  actionButton("track_predict", "Calcuate Probable Diagnosis"),
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
  textOutput("trackactprob"),
  textOutput("trackremprob"),
  textOutput("trackbestprob")
  
),
),






#IBSvIBD

  #ui
tabPanel("IBS vs IBD diagnosis",
         fluidPage(
    actionButton("ibs_predict", "Calcuate Probable Diagnosis"),
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
    textOutput("ibsactprob"),
    textOutput("ibsremprob"),
    textOutput("ibsbestprob")
    
  )
)
)
)
  
  #server
  server <- function(input, output) {
    
    track_patient_values <- eventReactive(input$track_predict, {
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
    
    
    track_predicted <- eventReactive(input$track_predict, {
      track_patient_values <- track_patient_values()
      predict(diff_model, new_data = track_patient_values, type = "prob")  
    })
    track_pred_act <- eventReactive(input$track_predict, {
      track_predicted <- track_predicted()
      track_predicted[1, 2]
    })
    track_pred_rem <- eventReactive(input$track_predict, {
      track_predicted <- track_predicted()
      track_predicted[1, 1]
    })
    track_pred_best <- eventReactive(input$track_predict, {
      track_pred_act <- track_pred_act()
      track_pred_rem <- track_pred_rem()
      ifelse(track_pred_act<track_pred_rem, "in remission", "active")
      #assigned active as winning the tie if the values are equal because of the danger of not acting on IBD if it is active
    }) 
    output$trackactprob  <- renderText({
      paste("probability of active IBD is", track_pred_act())
    })
    
    output$trackremprob <- renderText({
      paste("probability of remission IBD", track_pred_rem())
    })
    output$trackbestprob <- renderText({
      paste("The patient's IBD is most likely", track_pred_best())
    })
    

    
    ibs_patient_values <- eventReactive(input$ibs_predict, {
      as.tibble(data.frame(
        status = NA,
        neut = (((input$neut)-5.651213)/2.81442),
        lymph = (((input$lymph)-2.091166)/0.9486037),
        mono = (((input$mono)-0.6273751)/0.2845463),
        eosin = (((input$eosin)-0.2078273)/0.2491543),
        baso = (((input$baso)-0.03772737)/0.03768921),
        age = (((input$age)-39.52566)/18.25781),
        hb = (((input$hb)-128.8111)/18.20674),
        plt = (((input$plt)-299.2203)/117.2642)
      ))
    })
    
    
    ibs_predicted <- eventReactive(input$ibs_predict, {
      ibs_patient_values <- ibs_patient_values()
      predict(IBSvIBD_model, new_data = ibs_patient_values, type = "prob")  
    })
    ibs_pred_act <- eventReactive(input$ibs_predict, {
      ibs_predicted <- ibs_predicted()
      ibs_predicted[1, 2]
    })
    ibs_pred_rem <- eventReactive(input$ibs_predict, {
      ibs_predicted <- ibs_predicted()
      ibs_predicted[1, 1]
    })
    ibs_pred_best <- eventReactive(input$ibs_predict, {
      ibs_pred_act <- ibs_pred_act()
      ibs_pred_rem <- ibs_pred_rem()
      ifelse(ibs_pred_act>ibs_pred_rem, "IBS", "IBD")
    }) 
    output$ibsactprob  <- renderText({
      paste("probability of IBS", ibs_pred_act())
    })
    
    output$ibsremprob <- renderText({
      paste("probability of IBD", ibs_pred_rem())
    })
    output$ibsbestprob <- renderText({
      paste("The patient most likely has", ibs_pred_best())
    })
    
  }
  



#app
shinyApp(ui = ui, server = server)