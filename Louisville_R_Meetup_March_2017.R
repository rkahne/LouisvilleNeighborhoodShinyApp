library(shinydashboard)
library(shiny)
library(scales)
library(tidyverse) # MUST GO LAST!!!!

neighborhood_data <- read_csv('Nate_Data.csv')


make_scatter<-function(x, y){
  neighborhood_data$x_axis <- unlist(neighborhood_data[x])
  neighborhood_data$y_axis <- unlist(neighborhood_data[y])
  ggplot(neighborhood_data, aes(x=x_axis, y=y_axis)) + 
    geom_smooth(method = 'lm', se = F) +
    geom_label(aes(label = sc_abbr))
}

data_r <- mutate(data, abbr = factor(abbr, levels = abbr[order(bachelors_degree)]))
ggplot(data_r, aes(x=abbr, y=bachelors_degree)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90))

