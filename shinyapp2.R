library(shiny)
library(tidyverse)
library(mlbench)
library(tidyverse)
library(caret)
library(ROCR)
library(e1071)
library(dataPreparation)
library(Boruta)
library(mctest)
library(GGally)
library(plotROC)
setwd("/Users/riley.unsicker/Documents/1_school/Analytics Applications/Project 2")

df <- read_csv("shinydatapre_two.csv")

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



server <- function(input, output) {

  
  #Run the model on Results
  
  result <- eventReactive(input$run_model, label = "run_the_model", {
    selectedDF <- select(df, one_of(unlist(input$var)))
    paidvar <- df[1:1]
    selectedDF <- cbind(paidvar, selectedDF)
    inTraining <- createDataPartition(selectedDF$paid, p = input$perc/100, list = FALSE)
    training <- selectedDF[ inTraining,]
    testing  <- selectedDF[-inTraining,]
    
    # Train the Model
    fitControl <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats =10,
      summaryFunction=twoClassSummary,
      classProbs = TRUE
    )
    
    training$paid <- factor(training$paid)
    testing$paid <- factor(testing$paid)
     levels(training$paid) <- make.names(levels(factor(training$paid)))
     levels(testing$paid) <- make.names(levels(factor(testing$paid)))
     # levels(training$Charges) <- make.names(levels(factor(training$Charges)))
     # levels(testing$Charges) <- make.names(levels(factor(testing$Charges)))
     # logFit <- train(form = paid ~ .,
     #                 data=training,
     #                 method="glm",
     #                 family="binomial",
     #                 trControl = fitControl,
     #                 metric = "ROC",
     #                 preProcess = c("scale", "center")
     # )
    
    logFit <- glm(paid ~ ., data = training, family=binomial(link='logit'))

    #testing$pred <- predict(logFit, newdata = testing, type = "response")
    # 
    # roc_d <- as.data.frame(cbind(testing$pred, testing$paid))
    # 
    # long_roc <- melt_roc(roc_d, testing$paid, testing$pred )
    # 
    # ggplot(long_roc, aes(d=D, m=M)) +
    #   geom_roc() + style_roc(xlab = "False Positive Rate",
    #                          ylab = "True Positive Rate") +
    #   annotate("text", x= 0.75, y = 0.18, label = paste("AUC =",
    #                                                     round(calc_auc())))
      
    
    testing$pred <- predict(logFit, newdata = testing, type = "response")
    roc_object <- roc(testing$paid, testing$pred)
    area_under_curve <- auc(roc_object)
     
    testing$pred <- ifelse(testing$pred > 0.5, 1, 0)
    testing$pred <- as.factor(testing$pred)
    levels(testing$pred) <- make.names(levels(factor(testing$pred)))
    levels(testing$paid) <- make.names(levels(factor(testing$paid)))
    confusion_matrix <- confusionMatrix(testing$pred, testing$paid, positive = "X1")
    
    
    print(confusion_matrix)
    
    print(paste0("AIC: ", round(AIC(logFit)), digits = 9))
    
    print(area_under_curve)
    
    print(summary(logFit))
 
    
  })
  

  # Render Results
  
  output$results <- renderPrint({
    result()
  })
  
  
}

shinyApp(ui = ui, server = server)
  