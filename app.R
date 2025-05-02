#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




#install.packages("shiny")

library("shiny")

#install.packages("flexsurv")


library("flexsurv")



study_data <- data.frame(
  Study = c("CamPalGN", "NYPUM", "PICNICS", "PINE"),
  "Number of patients, n"= c(129, 124, 270, 200),
  "Age (years), mean (SD)" = c("70.2 (9.7)", "70.7 (9.6)", "68.6 (9.7)", "72.6 (10.3)"),
  "Male,n(%)" = c("73 (56.6%)", "77 (62.1%)", "165 (61.1%)", "123 (61.5%)"),
  "Hoehn and Yahr Scale,median (IQR)" = c("2.0 (1.5,2.5)", "2.0 (2.0,2.5)", "2.0 (1.0,2.0)", "2.5 (2.0,3.0)"),
  "MDS-UPDRS III,median (IQR)" = c("32.9 (22.1,42.5)", "34.1 (25.1,43.0)", "29 (22.0,38.0)", "31.1 (21.5,40.7)"),
  "MMSE,median (IQR)" = c("28.0 (27.0,29.0)", "29.0 (28.0,30.0)", "29.0 (28.0,30.0)", "29.0 (28.0,30.0)"),
  check.names = FALSE
)

incidence_data <- data.frame(
  Study = c("CamPalGN", "NYPUM", "PICNICS", "PINE"),
  `Incidence rate (per 100 person-years)` = c(2.8, 6.2, 3.3, 5.0),
  `Median time of follow-up (years)` = c(10, 9.22, 10.00, 9.43),
  check.names = FALSE
)









ui <- fluidPage(
  
  # Big main title
  tags$h2(
    "Individual's Predicted Risk of Institutionalisation (Royston-Parmar Model)",
    style = "font-weight: bold; margin-bottom: 20px;"
  ),
  
  # Instructions
  div(style = "font-size:16px; margin-bottom: 30px;",
      HTML("<ol>
             <li>Please select the study that most closely matches your patient population. The characteristics of the selected study population will be shown in the table.</li>
             <li>Enter the patient’s clinical information.</li>
             <li>Choose the prediction time (in years).</li>
           </ol>")
  ),
  
  
  div(style = "font-size:16px; font-weight:bold; margin-top:30px; margin-bottom:10px;", 
      textOutput("selectedStudy")
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study", "Select Study Dataset", choices = c("CamPalGN", "NYPUM", "PICNICS", "PINE")),
      numericInput("age", "Age", value = 70, min = 30, max = 100),
      selectInput("sex", "Gender", choices = c("Male" = 0, "Female" = 1)),
      numericInput("updrs3", "MDS-UPDRS III", value = 60, min = 0, max = 132),
      numericInput("hy", "Hoehn and Yahr Scale", value = 2, min = 0, max = 5),
      numericInput("mmse", "MMSE", value = 27, min = 0, max = 30),
      numericInput("year", "Prediction year", value = 10, min = 0, max = 10)
    ),
    
    mainPanel(
      # Larger, bold output
      div(
        style = "font-size:18px; font-weight:bold; margin-top:20px;",
        textOutput("riskEstimate")
      ),
      hr(),
      div(style = "font-size:16px; font-weight:bold; margin-top:20px;", 
          textOutput("selectedStudy")
      ),
      tableOutput("studySummary"),
      
      br(),
      div(style = "font-size:16px; font-weight:bold; margin-top:20px;", 
          "Incidence Rate Summary"
      ),
      tableOutput("incidenceSummary")
    )
  )
)







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
    
    paste0("Predicted risk of institutionalisation at ", input$year, " years: ", round(risk * 100, 2), "%")
    
  })
  
  # (new) render selected study name
  output$selectedStudy <- renderText({
    paste("You selected:", input$study)
  })
  
  # (new) render study summary info
  output$studySummary <- renderTable({
    study_data[study_data$Study == input$study, ]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$incidenceSummary <- renderTable({
    incidence_data[incidence_data$Study == input$study, ]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
}







shinyApp(ui = ui, server = server)



#install.packages("rsconnect")

library(rsconnect)
rsconnect::setAccountInfo(name='yan61726',
                          token='32ACCB2536BE4ED96E13DC03DE199F65',
                          secret='EpqhV2tIw7xb2ZtBhGV1Z7DKUcHvjLRHapTiO7kj')

# Deploy app from your R project folder
rsconnect::deployApp()

