library(shinydashboard)
library(shiny)
library(ggthemes)
library(scales)
library(tidyverse) # MUST GO LAST!!!!

neighborhood_data <- read_csv('Nate_Data.csv')

lookup_columns <- c('Median Earnings $' = names(neighborhood_data)[2],
                    'Bachelors Degree %' = names(neighborhood_data)[3],
                    'No HS Degree %' = names(neighborhood_data)[4],
                    'Uninsured %' = names(neighborhood_data)[5],
                    'Population' = names(neighborhood_data)[6],
                    'Low Income %' = names(neighborhood_data)[7],
                    'Unemployed %' = names(neighborhood_data)[8],
                    'Black %' = names(neighborhood_data)[9],
                    'Low Income Children' = names(neighborhood_data)[10],
                    'Life Expectancy' = names(neighborhood_data)[13],
                    'Child Low Income' = names(neighborhood_data)[14],
                    'z-score Low Income' = names(neighborhood_data)[15],
                    'z-score Unemployed'  = names(neighborhood_data)[16], 
                    'z-score No HS Degree' = names(neighborhood_data)[17],
                    'z-score Uninsured' = names(neighborhood_data)[18],
                    'z-score Overall' = names(neighborhood_data)[19]) 

make_scatter<-function(x, y){
  neighborhood_data$x_axis <- unlist(neighborhood_data[x])
  neighborhood_data$y_axis <- unlist(neighborhood_data[y])
  ggplot(neighborhood_data, aes(x=x_axis, y=y_axis)) + 
    geom_smooth(method = 'lm', se = F) +
    geom_label(aes(label = sc_abbr))+
    labs(x = names(lookup_columns)[which(lookup_columns == x)], 
         y = names(lookup_columns)[which(lookup_columns == y)], 
         title = paste0(names(lookup_columns)[which(lookup_columns == x)],' ~ ', names(lookup_columns)[which(lookup_columns == y)]))
}

make_bar<-function(y){
  neighborhood_data$y_axis <- unlist(neighborhood_data[y])
  data_r <- mutate(neighborhood_data, abbr = factor(abbr, levels = abbr[order(y_axis)]))
  ggplot(data_r, aes(x=abbr, y=y_axis)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 90))+
    labs(x='Neighborhood', y = y, title=paste0('Neighborhoods by ',names(lookup_columns)[which(lookup_columns == y)]))
}

ui<-dashboardPage(
  dashboardHeader(title = 'Louisville Neighborhoods',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('plotType', 'Plot Type', c('Scatter' = 'scatter',
                                           'Bar' = 'bar')),
    conditionalPanel(condition = "input.plotType == 'scatter'",
      selectInput('x_axis', 'Select X-Axis', names(lookup_columns), selected = 'median_earning'),
      selectInput('y_axis', 'Select Y-Axis', names(lookup_columns), selected = 'Bachelors Degree %')
    ),
    conditionalPanel(condition = "input.plotType == 'bar'",
      selectInput('y_axis_b', 'Select Y-Axis', names(lookup_columns), selected = 'median_earning')
    )
  ),
  dashboardBody(
    tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),
    plotOutput('plot')
  )
)

server<-function(input,output){
  output$plot<-renderPlot({
    if(input$plotType == 'scatter'){
      make_scatter(lookup_columns[which(names(lookup_columns) == input$x_axis)], 
                   lookup_columns[which(names(lookup_columns) == input$y_axis)])
    }else if(input$plotType == 'bar'){
      make_bar(lookup_columns[which(names(lookup_columns) == input$y_axis_b)])
    }
  })
}

shinyApp(ui, server)