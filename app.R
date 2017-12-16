library(shiny)
library(mdsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

## SuperBowl
SuperBowl_2010 <- train %>%
  filter(Date == '2010-02-12') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

SuperBowl_2011 <- train %>%
  filter(Date == '2011-02-11') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

SuperBowl <- inner_join(SuperBowl_2010, SuperBowl_2011, by = "Dept")
SuperBowl <- SuperBowl %>%
  mutate(prediction_sales = (avg_sales.x + avg_sales.y)/2)

## Labor Day
LaborDay_2010 <- train %>%
  filter(Date == '2010-09-10') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

LaborDay_2011 <- train %>%
  filter(Date == '2011-09-09') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

LaborDay <- inner_join(LaborDay_2010, LaborDay_2011, by = "Dept")
LaborDay <- LaborDay %>%
  mutate(prediction_sales = (avg_sales.x + avg_sales.y)/2)

## Thanksgiving
Thanksgiving_2010 <- train %>%
  filter(Date == '2010-11-26') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

Thanksgiving_2011 <- train %>%
  filter(Date == '2011-11-25') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

Thanksgiving <- inner_join(Thanksgiving_2010, Thanksgiving_2011, by = "Dept")
Thanksgiving <- Thanksgiving %>%
  mutate(prediction_sales = (avg_sales.x + avg_sales.y)/2)

## Christmas
Christmas_2010 <- train %>%
  filter(Date == '2010-12-31') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

Christmas_2011 <- train %>%
  filter(Date == '2011-12-30') %>%
  group_by(Dept) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  arrange(desc(avg_sales))

Christmas <- inner_join(Christmas_2010, Christmas_2011, by = "Dept")
Christmas <- Christmas %>%
  mutate(prediction_sales = (avg_sales.x + avg_sales.y)/2) 


ui <-
  fluidPage(
    titlePanel(strong("Wal-Smart")),
    em("Monica Cardenas & Kelly Yang"),
    br(), br(),
    sidebarLayout(
      sidebarPanel(
        selectInput("Holiday",
                    "Select the Holiday: ",
                    choices = c("SuperBowl", "Labor Day", "Thanksgiving", "Christmas"))
      ),
      
      mainPanel(
        plotOutput("Plot")
      )
    )
  )


server <-
  function(input,output){
    output$Plot <- renderPlot({
      if(input$Holiday == "SuperBowl") {
        ggplot(SuperBowl, aes(x= as.factor(Dept), y = prediction_sales, fill=as.factor(Dept))) + geom_bar(stat = 'identity') + ggtitle("SuperBowl Average Sales of 2010 & 2011") + xlab("Department") + ylab("Average Sales") 
      } else if(input$Holiday == "Labor Day") {
        ggplot(LaborDay, aes(x= as.factor(Dept), y = prediction_sales, fill=as.factor(Dept))) + geom_bar(stat = 'identity') + ggtitle("Labor Day Average Sales of 2010 & 2011") + xlab("Department") + ylab("Average Sales")
        
      } else if(input$Holiday == "Thanksgiving") {
        ggplot(Thanksgiving, aes(x= as.factor(Dept), y = prediction_sales, fill=as.factor(Dept))) + geom_bar(stat = 'identity') + ggtitle("Thanksgiving Average Sales of 2010 & 2011") + xlab("Department") + ylab("Average Sales")
        
      } else{
        ggplot(Christmas, aes(x= as.factor(Dept), y = prediction_sales, fill=as.factor(Dept))) + geom_bar(stat = 'identity') + ggtitle("Christmas Average Sales of 2010 & 2011") + xlab("Department") + ylab("Average Sales")
      }
    }
    )
  }


shinyApp(ui = ui, server = server)
