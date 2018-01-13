library(shiny)
library(shinythemes)
library(rmarkdown)
library(caret)
library(lattice)
library(ggplot2)
library(e1071)

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