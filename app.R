# Load necessary libraries
library(shiny)
library(flexsurv)
library(knitr)

#----Data setup---

study_data <- data.frame(
  Study = c("CamPalGN (UK)", "NYPUM (Sweden)", "PICNICS (UK)", "PINE (UK)"),
  "Number of patients, n"= c(129, 124, 270, 200),
  "Age (years), mean (SD)" = c("70.2 (9.7)", "70.7 (9.6)", "68.6 (9.7)", "72.6 (10.3)"),
  "Male, n (%)" = c("73 (56.6%)", "77 (62.1%)", "165 (61.1%)", "123 (61.5%)"),
  "Hoehn and Yahr Scale, median (IQR)" = c("2.0 (1.5,2.5)", "2.0 (2.0,2.5)", "2.0 (1.0,2.0)", "2.5 (2.0,3.0)"),
  "MDS-UPDRS III, median (IQR)" = c("32.9 (22.1,42.5)", "34.1 (25.1,43.0)", "29 (22.0,38.0)", "31.1 (21.5,40.7)"),
  "MMSE, median (IQR)" = c("28.0 (27.0,29.0)", "29.0 (28.0,30.0)", "29.0 (28.0,30.0)", "29.0 (28.0,30.0)"),
  check.names = FALSE
)

incidence_data <- data.frame(
  Study = c("CamPalGN (UK)", "NYPUM (Sweden)", "PICNICS (UK)", "PINE (UK)"),
  `Incidence rate (per 100 person-years)` = c(2.8, 6.2, 3.3, 5.0),
  `Median time of follow-up (years)` = c(10, 9.22, 10.00, 9.43),
  check.names = FALSE
)

#--- UI ---

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    .baseline-table {
      width: 100%;
      table-layout: fixed;
      word-wrap: break-word;
    }
    .baseline-table th,
    .baseline-table td {
      text-align: center !important;
      vertical-align: middle !important;
      white-space: normal !important;
      word-break: break-word;
    }
    .shiny-table {
      margin-bottom: 10px !important;
    }
    .custom-panel {
      margin-top: 20px;
      padding-top: 10px;
    }
  "))
  ),
  
  tags$h2("Predictions of Institutionalisation in Parkinson's Disease",
          style = "font-weight: bold; margin-bottom: 20px;"),
  
  div(style = "font-size:16px; margin-bottom: 30px;",
      HTML("<ul>
             <li style='color:blue;'> The tables below show the baseline characteristics and the incidence rates of institutionalisation for each study.</li>
             <li style='color:blue;'> These models are designed to calculate the predicted risk for newly diagnosed Parkinson's patients.</li>
             
             <li> <strong> Please select the study that most closely matches your patient population. </strong></li>
             <li> <strong> Enter the patient’s information. </strong></li>
             <li> <strong> Choose the prediction time (in years). </strong></li>
             
             <li style='color:red;'>Models for CamPalGN<sup>1</sup>, NYPUM<sup>2</sup>, and PINE<sup>3</sup> are only validated at the 10-year time point. The model for PICNICS<sup>4</sup> is only validated at the 7-year time point. If you use these models to predict other time points, the results may not be reliable. We recommend that researchers further validate these models before applying them to other time points.</li>
           </ul>")
  ),
  
  div(style = "font-size:16px; font-weight:bold; margin-top:30px; margin-bottom:10px;", 
      "Baseline characteristics of each study"),uiOutput("studyTable"),

  div(style = "font-size:16px; font-weight:bold; margin-top:10px;",
      "Incidence rates of institutionalisation and the median follow-up time for each study"), uiOutput("allIncidenceSummary"),
  
  # Calculator and results
  div(class = "custom-panel",
      sidebarLayout(
        sidebarPanel(
          selectInput("study", "Select Study Dataset", choices = c("CamPalGN", "NYPUM", "PICNICS", "PINE")),
          numericInput("age", "Age", value = 70, min = 30, max = 100),
          selectInput("sex", "Gender", choices = c("Male" = 0, "Female" = 1)),
          numericInput("updrs3", "MDS-UPDRS III", value = 30, min = 0, max = 132),
          numericInput("hy", "Hoehn and Yahr Scale", value = 2, min = 0, max = 5),
          numericInput("mmse", "MMSE", value = 27, min = 0, max = 30),
          uiOutput("yearInput"),
          uiOutput("yearWarning")
        ),
        mainPanel(
          div(style = "font-size:18px; font-weight:bold; margin-top:20px;",
              textOutput("riskEstimate"))
        )
      )
  ),
  
  # References section moved below calculator
  div(style = "font-size:14px; margin-top:30px;",
      HTML("<strong>References:</strong><br>
            1: Williams-Gray CH, Mason SL, Evans JR, et al. <i>The CamPaIGN study of Parkinson's disease: 10-year outlook in an incident population-based cohort</i>. J Neurol Neurosurg Psychiatry 2013;84(11):1258-1264.<br>
            2: Linder J, Stenlund H, Forsgren L. <i>Incidence of Parkinson's disease and parkinsonism in northern Sweden: a population-based study</i>. Mov Disord 2010;25(3):341-348.<br>
            3: Caslake R, Taylor K, Scott N, et al. <i>Age-, gender-, and socioeconomic status-specific incidence of Parkinson's disease and parkinsonism in northeast Scotland: the PINE study</i>. Parkinsonism Relat Disord 2013;19(5):515-521.<br>
            4: Breen DP, Evans JR, Farrell K, Brayne C, Barker RA. <i>Determinants of delayed diagnosis in Parkinson's disease</i>. J Neurol 2013;260(8):1978-1981.")
  )
)

#--- Server ---


server <- function(input, output, session) {
  
  library(flexsurv)  # needed for basis()
  
  model_list <- list(
    CamPalGN = list(
      gamma = c(-11.44,1.63, -0.97),
      beta = c(age = 1.19, sex = 0.05, updrs3 = 0.16, hy = 0.35, mmse = -0.16),
      knots = c(0.14, 1.82, 2.2)
    ),
    NYPUM = list(
      gamma = c(-10.14,1.51, -0.73),
      beta = c(age = 1.43, sex = -0.01, updrs3 = 0.15, hy = 0.50, mmse = -0.23),
      knots = c(-0.11, 1.83, 2.25)
    ), 
    PICNICS = list(
      gamma = c(-12.93,1.40, -0.33),
      beta = c(age = 1.14, sex = -0.21, updrs3 = 0.16, hy = 0.41, mmse = -0.11),
      knots = c(-2.24, 1.51, 1.93)
    ),
    PINE = list(
      gamma = c(-16.35,0.76, -0.55),
      beta = c(age = 1.41, sex = 0.08, updrs3 = 0.11, hy = 0.44, mmse = -0.07),
      knots = c(-2.24, 1.87, 2.30)
    )
  )
  
  #Reactive wrapper to fetch selected model
  
  params <- reactive({
    req(input$study)
    model_list[[input$study]]
  })
  
  #Dynamic max prediction year
  
  max_years <- reactive({
    if (input$study == "PICNICS(UK)") 7 else 10
  })
  
  #Dynamic prediction year input with max limit
  
  output$yearInput <- renderUI({
    numericInput("year", "Prediction year", 
                 value = min(10, max_years()), min = 1, max = max_years())
  })
  
  # Dynamic warning based on selected study and year input
  
  output$yearWarning <- renderUI({
    if (!is.null(input$year) && input$year > max_years()) {
      div(style = "color:red; font-size:14px; margin-top:10px;",
          paste0("Note: The ", input$study, 
                 " model was developed for predictions up to ", max_years(), 
                 " years. Predictions beyond this may be unreliable."))
    }
  })
  
  #Calculate predicted risk  
  
  output$riskEstimate <- renderText({
    req(input$year)
    
    #Correct use of reactive object
    
    p <- params()
    
    # Calculate linear predictor (LP)
    lp <- (input$age / 10) * p$beta["age"] +
      as.numeric(input$sex) * p$beta["sex"] +
      (input$updrs3 / 10) * p$beta["updrs3"] +
      input$hy * p$beta["hy"] +
      input$mmse * p$beta["mmse"]
    
    # Basis spline for log(time)
    log_time <- log(input$year)
    basis_vals <- basis(p$knots, log_time)
    
    # Make sure gamma and basis are compatible
    gamma_term <- sum(p$gamma * basis_vals)
    
    # Calculate log cumulative hazard: intercept + sum(gamma * basis values) + LP
    log_ch <- gamma_term + lp
    
    #survival
    su<-exp(-exp(log_ch))
    
    #risk
    risk<-1-su
    
    paste0("Predicted risk of institutionalisation at ", input$year, " years: ", round(risk * 100, 1), "%")
    
  })
  
  
#output$studyTable <- renderTable({
#    study_data
#  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
  

  output$studyTable <- renderUI({
    div(style = "width: 60%;",
    HTML(knitr::kable(study_data, format = "html", 
                      table.attr = "class='table table-bordered table-striped baseline-table'", 
                      escape = FALSE))
    )
  })
  
#output$allIncidenceSummary <- renderTable({
 # incidence_data
#}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
  
  output$allIncidenceSummary <- renderUI({
    div(style = "width: 60%;",  
        HTML(knitr::kable(
          incidence_data, 
          format = "html", 
          table.attr = "class='table table-bordered table-striped baseline-table'", 
          escape = FALSE
        ))
    )
  })

}

# Run app
shinyApp(ui = ui, server = server)
