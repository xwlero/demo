library(shiny)
library(shinydashboard)
library(lattice)
library(ggplot2)
library(ggthemes)
library(scales)
library(recharts)
library(survival)
library(Formula)
library(plyr)
library(Hmisc)
library(reshape2)
ui <- dashboardPage(
  dashboardHeader(title = "xwl"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("第一页",tabName="first"),
      menuItem("第二页",tabName="second")
    ),
    selectInput("dataset", "Choose a dataset:", choices = c("rock", "pressure", "cars"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="first","第一页的内容",
            fluidRow(
              box(title="标题一",solidHeader = TRUE,width=6,height=500,status="primary" ,
                  tabsetPanel(type = "tabs",
                              tabPanel("Plot", eChartOutput('myChart')),
                              tabPanel("Summary",verbatimTextOutput("summary")),
                              tabPanel("Table")
                  )
                  ),
              box(title="标题二",solidHeader = TRUE,width=6,height=500,status="primary" ,eChartOutput('myChart1') ),
              box(title="标题三",solidHeader = TRUE,width=6,height=500,status="primary" ),
              box(title="标题四",width=6,height=500,status="primary"  )
              )),
      tabItem(tabName="second","第二页的内容",
              fixedRow(
                box(title="标题一",width=6,height=500,status="primary" ,eChartOutput('myChart2')),
                box(height=500)
              )
              )
    )
  )
  )
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)})
  #summary
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  #chart
  dtcars <- mtcars
  dtcars$car <- row.names(dtcars)
  dtcars$transmission <- as.factor(dtcars$am)
  levels(dtcars$transmission) <- c("Automatic","Manual")
  dtcars$cylinder <- as.factor(dtcars$cyl)
  dtcars$carburetor <-as.factor(dtcars$carb)
  chart <- echartR(dtcars, x = ~cylinder,  y = ~car, type='rose',
                   palette='colorblind', title='Number of Cylinders',
                   subtitle = '(source: mtcars)')
  output$myChart <-  renderEChart({chart})
  #chart1
  hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
  hair_eye_male[,1] <- paste0("Hair",hair_eye_male[,1])
  hair_eye_male[,2] <- paste0("Eye",hair_eye_male[,2])
  chart1 <- echartR(data = hair_eye_male, x = Hair, y = ~Freq,  series = ~Eye,
                    type = 'bar', palette='fivethirtyeight',
                    xlab = 'Hair', ylab = 'Freq')
  output$myChart1 <-  renderEChart({chart1})
  #chart2
  chart2 <- echartR(dtcars, x = ~cylinder,  y = ~car, type='rose',
                   palette='colorblind', title='Number of Cylinders',
                   subtitle = '(source: mtcars)')
  output$myChart2 <-  renderEChart({chart2})
}
shinyApp(ui, server)
