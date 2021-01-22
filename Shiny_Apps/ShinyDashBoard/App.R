library(shiny)
library(ggplot2)
library(tidyverse)
library(tidycells)
library(shinytest)
library(lubridate)
library(prophet)



BuoyData <- read_csv("R_DataViz_Forecasts/BuoyData_2_2.csv", col_types = cols(DATE_TIME = col_datetime(format = "%m/%d/%Y %H:%M")))

theme_set(theme_minimal())


ui <- fluidPage(
  #Create input for domain (single) and variable (multiple)
  selectInput("domain", "Domain", choices = ""),
  selectInput("varSelection", "Choose 2 or 3 variables to Compare", multiple = T, choices = ""),
  selectInput("varfcast", "Choose a Variable to Forcast", multiple = F, choices = ""),
  
  #Set plot for output
  plotOutput("myPlot")
  ,plotOutput("fcast")
)

boutyserver <- function(input, output, session) {

  #Update the domain input according to the data
  updateSelectInput(session, "domain", choices = sort(unique(BuoyData$DEPTH_m)))
  
  #Update the variable list (assumed all but d and year are variables of interest)
  updateSelectInput(session, "varSelection", 
                    choices = colnames(BuoyData %>% select(-DEPTH_m, -DATE_TIME)))
  
  #Update the variable list (assumed all but d and year are variables of interest)
  updateSelectInput(session, "varfcast", 
                    choices = colnames(BuoyData %>% select(-DEPTH_m, -DATE_TIME)))
  
  #Load the chart function
  draw_chart <- function(df, listv, dom){
    df2 <- df %>%
      gather("variable", "value", 3:12)
    df3 <- df2 %>%
      filter(variable %in% listv)
    df4 <- df3 %>%
      group_by(DEPTH_m, year(DATE_TIME)) %>%
      summarise(value = mean(value)) %>%
      mutate(variable = "m")
    
    df5 <- bind_rows(df3, df4) 
    
    df5 <- df5 %>%
      mutate(year = year(DATE_TIME))
    df5 <- df5 %>%
      filter(DEPTH_m == dom)
    
    ggplot(df5, aes(x= DATE_TIME , y=value)) +
      geom_line(aes(color = variable, linetype = variable)) + 
      xlab("")
    
  }
  
  draw_forecast <- function(dff, var, dom){
    stats <- dff %>%
      filter(DEPTH_m == dom) %>%
      select(DATE_TIME, var)
    colnames(stats) <- c("ds", "y")
    
    m <- prophet(stats)
    
    future <- make_future_dataframe(m, periods = (2*365))
    el_nino <- tibble(
      holiday = 'el_nino',
      ds = as.Date(c('2017-09-17', '2017-09-16', '2017-09-15', '2017-09-14', '2017-09-13')),
      lower_window = 0,
      upper_window = 20
    )
    future2 <- future %>% 
      filter(as.numeric(format(ds, "%m")) > 3) %>%
      filter(as.numeric(format(ds, "%m")) != 12 ) 
    fcst <- predict(m, future2)
    plot(m, fcst)
    
  }
  #Render the plot
  output$myPlot = renderPlot({
    #Only render if there are 2 or 3 variables selected
    #req(between(length(input$varSelection), 2, 3))
    draw_chart(BuoyData, input$varSelection, input$domain)
  })
  #Render the plot
  output$fcast = renderPlot({
    #Only render if there are 2 or 3 variables selected
    #req(between(length(input$varSelection), 2, 3))
    draw_forecast(BuoyData, input$varfcast, input$domain)
  })
  
}

shinyApp(ui, boutyserver)
