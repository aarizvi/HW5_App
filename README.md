---
title: "Homework 5"
author: "Abbas Rizvi"
date: "November 20, 2017"
output: github_document
---

```{r setup, include=F}
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)
library(shiny)
library(caret)
library(lattice)
library(ggplot2)
library(e1071)
library(dplyr)
library(forcats)
```

**Note:**   
**GOOD NEWS** You do not need to knit your HW5, just submit the this .Rmd file!!

**BAD NEWS** You will also have to host your HW5 online, Q3 will explain how to do this!!

#### Max points = 100  

There are a few more details regarding RShiny and Rmarkdown, but rather than tell you about them, I am going to use this homework to force you to figure them out yourself.

It is possible to create an interactive Rmarkdown document, this is like a cross between an RShiny app and an Rmarkdown (A summary on how to do this is in your markdown-cheatsheet). I personally find this a little less complicated than making a full RShiny app. 

I have created an interactive Rmarkdown document that I would like you to recreate, click the [HW5_APP](https://tomall01.shinyapps.io/HW5_APP/) link to see it.

### Question 1 {40 pts}.

In the first section of **HW5_APP** there are several tabs, clicking on each tab will give a performance summary of our *glmnet*, *pls* and *pam* models in either **test** or **validation** data (the results of the models we trained in Lecture 10). Try to recreate these tabs and their contents using the *navbarPage()* and *tabPanel()* functions, I have included some code below to help you get started. The file *Lecture10Data.csv* contains the output (RESULTS.df) of our Lecture10 models. The code for the AsthmaApp.R in Lecture9 might be helpful to you, also the shiny-cheatsheet might be helpful. 

NOTE: You can test your app is working correctly by the clicking on **Run Document** in RStudio, or by typing **rmarkdown::run** into the console. Try it now to see what the app looks like, it will run!  
  
```{r echo=F}
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

```  
\pagebreak 
 
### Question 2 {40 pts}.

The second section of **HW5_APP** contains a slider and a plot. The plot displays the Mean value for Symptoms.Past30D for each state, with the error bars representing the standard error of the mean. The slider controls which month (Jan=1, Dec=12) the data corresponds to. The dataset is the AsthmaChild.Ozone data from lecture9. See if you can recreate the slider and plot, I have provided some code to help you.
  
```{r echo=F}
X<-read.csv("AsthmaChild.Ozone.2006_2007.Sample.csv")
 
ui <- mainPanel(
        sliderInput(inputId="month_slider",
            label=h5("Month: Jan=1, Dec=12"),
            min = 1,
            max = 12,
            value=1,
            step=1),
        plotOutput("plot")
        )


month.lookup = c("January"=1,
                 "February"=2,
                 "March"=3,
                 "April"=4,
                 "May"=5,
                 "June"=6,
                 "July"=7,
                 "August"=8,
                 "September"=9,
                 "October"=10,
                 "November"=11,
                 "December"=12)


server <- function(input, output){
        output$plot <- renderPlot({
                X %>%
                        arrange(Symptoms.Past30D) %>%
                        filter(Month==input$month_slider) %>% 
                        ggplot(aes(x = fct_rev(reorder(STATE, Symptoms.Past30D, FUN=mean)), y= Symptoms.Past30D)) +
                        stat_summary(fun.data = mean_se, geom = "errorbar") +
                        stat_summary(fun.y=mean, geom="point", size=2) + 
                        coord_flip() +
                        xlab("STATE") +
                        ylab("Mean Symptoms.Past30D (SEM)") +
                        ggtitle(names(month.lookup[input$month_slider]))
                
        })
  
}

shinyApp(ui = ui, server = server)


```    
\pagebreak    
   
   
### Question 3 {20 pts}.  

ShinyApps and interactive Rmarkdown can be hosted publically online, just like the HW5_APP I made for this homework. 

One such hosting service called [ShinyApps.io](http://www.shinyapps.io/). I want you to visit ShinyApps website and follow the instructions to set up an account and then host your Homework 5 app online.

Once you have hosted your app, paste the url below, the closer your app is to mine, the more points you will get.

**Answer**  
https://aarizvi.shinyapps.io/hw5_app/
\pagebreak   


