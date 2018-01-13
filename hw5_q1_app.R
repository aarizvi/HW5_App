library(shiny)
library(shinythemes)
library(rmarkdown)
library(caret)
library(lattice)
library(ggplot2)
library(e1071)


## Modify the code below for inputs
RESULTS.df<-read.csv("Lecture10Data.csv")

# user interface
ui <- navbarPage(theme<-"Performance Summary",
                 tabPanel('Test Data',
                          mainPanel(
                                  tabsetPanel(
                                          tabPanel("glmnet",verbatimTextOutput("GLM.TEST")),
                                          tabPanel("pls",verbatimTextOutput("PLS.TEST")),
                                          tabPanel("pam",verbatimTextOutput("PAM.TEST"))
                                  )
                          )
                 ),
                 tabPanel('Val Data',
                          mainPanel(
                                  tabsetPanel(
                                          tabPanel("glmnet",verbatimTextOutput("GLM.VAL")),
                                          tabPanel("pls",verbatimTextOutput("PLS.VAL")),
                                          tabPanel("pam", verbatimTextOutput("PAM.VAL"))
                                  )
                          )
                 )
)

# Data Manipulation
PAM.VAL<-subset(RESULTS.df,TrainTestVal=='Val'&Method=="pam")
GLM.VAL <- subset(RESULTS.df, TrainTestVal=='Val' & Method == "glmnet")
PLS.VAL <- subset(RESULTS.df, TrainTestVal=='Val' & Method == "pls")

PAM.TEST <-subset(RESULTS.df,TrainTestVal=='Test'&Method=="pam")
GLM.TEST <- subset(RESULTS.df, TrainTestVal=='Test' & Method == "glmnet")
PLS.TEST <- subset(RESULTS.df, TrainTestVal=='Test' & Method == "pls")


# server
server <- function(input, output){
        output$GLM.VAL<-renderPrint({confusionMatrix(GLM.VAL$PredResp,GLM.VAL$Resp)})
        output$PAM.VAL<-renderPrint({confusionMatrix(PAM.VAL$PredResp,PAM.VAL$Resp)})
        output$PLS.VAL<-renderPrint({confusionMatrix(PLS.VAL$PredResp,PLS.VAL$Resp)})
        
        output$GLM.TEST<-renderPrint({confusionMatrix(GLM.TEST$PredResp,GLM.TEST$Resp)})
        output$PAM.TEST<-renderPrint({confusionMatrix(PAM.TEST$PredResp,PAM.TEST$Resp)})
        output$PLS.TEST<-renderPrint({confusionMatrix(PLS.TEST$PredResp,PLS.TEST$Resp)})
}

shinyApp(ui = ui, server = server)


