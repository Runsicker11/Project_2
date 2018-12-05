library(shiny)

# UI
ui <- fluidPage(
  
  #App title
  titlePanel("Payment Performance"),
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      checkboxGroupInput("var", "Variable Options",
                         choices = names(df[2:15]),
                         select = c("Charges", "DateDiff", "paperedi.E", "paperedi.P", "CCtier.Mid",
                                    "CCtier.Top", "CarrierType.Commercial", "CarrierType.Medicaid",
                                    "CarrierType.BlueCross.BlueShield", "CarrierType.Medicare",
                                    "CarrierType.Other", "CarrierType.Other Federal Program",
                                    "CarrierType.Tricare", "PatientBalPres", "paycode.CO", "paycode.PR")),
      br(),
      
      sliderInput("perc",
                  "Train/Test Split",
                  value = 75,
                  min= 1,
                  max = 100),
      
      actionButton(inputId = "run_model", label = "Run Model")
      
    ),
    # Outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Model          
                  tabPanel(title = "Model",
                           verbatimTextOutput("results")),
                  # Summary
                  tabPanel(title = "Summary", br(),
                           includeMarkdown("shiny_text.Rmd"))
      )
    )
  )
)

