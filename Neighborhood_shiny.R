library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(gpclib)
library(maptools)
library(raster)
library(broom)
library(scales)
library(tidyverse) # MUST GO LAST!!!!

gpclibPermit()

neighborhoods <- readOGR(dsn = './input data/Shapefiles/NeighborhoodAreas', layer = 'NeighborhoodAreas2010_ll')
neighborhoods@data$id <- rownames(neighborhoods@data)
neighborhoods.points <- tidy(neighborhoods, region = 'id')
neighborhoods.df <- inner_join(neighborhoods.points, neighborhoods@data, by = 'id') %>% 
  mutate(region = Neighbor)


tract_neigh<-read_csv("./input data/Neighborhood IDs/census tract neighborhoods.csv")
#tract_neigh<-read.csv("./input data/Neighborhood IDs/tract_to_market_area.csv",header=T)
#tract_neigh = tract_neigh %>% rename(Id2 = GEOID10, Neighborhood = MARKET_AREA)

##Now each data set is read in, transformed, and neighborhood values calculated. 

#education
n_ed_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_B23006_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id, Id2, Neighborhood, Geography, population = `Estimate; Total:`, 
         less_than_hs = `Estimate; Less than high school graduate:`,
         bachelors_up = `Estimate; Bachelor's degree or higher:`) %>% 
  mutate(bach_percent = bachelors_up/population,
         less_than_hs_percent = less_than_hs/population) %>% 
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population), 
         bach_pct = bachelors_up/population,
         hs_pct = less_than_hs/population,
         bach_wgt = bach_pct*wgt,
         hs_wgt = hs_pct*wgt) %>%
  summarise(bach_nh = sum(bach_wgt),
            hs_nh = sum(hs_wgt))

##earnings
n_earn_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_S2001_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id2, Geography, Neighborhood, population = `Total; Margin of Error; Population 16 years and over with earnings`,
         earnings = `Total; Estimate; Median earnings (dollars)`) %>% 
  mutate(num_earnings = as.numeric(as.character(earnings))) %>% 
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         earnings_wgt=num_earnings*wgt)%>%
  summarise(earnings_nh = sum(earnings_wgt))

##insurance
n_health_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_S2701_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id2, Geography, Neighborhood, population = `Total; Estimate; Total civilian noninstitutionalized population`,
         uninsured = `Percent Uninsured; Estimate; Total civilian noninstitutionalized population`) %>% 
  mutate(uninsured = as.numeric(as.character(uninsured))) %>% 
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         uninsured_wgt = uninsured*wgt)%>%
  summarise(uninsured_nh = sum(uninsured_wgt),
            population = sum(population)) #this is the one data set where I keep population

##low income
n_low_inc_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_C17002_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id2, Geography, Neighborhood, population = `Estimate; Total:`, 
         less_than_50 = `Estimate; Total: - Under .50`,
         less_than_1 = `Estimate; Total: - .50 to .99`,
         less_than_1.25 = `Estimate; Total: - 1.00 to 1.24`,
         less_than_1.5 = `Estimate; Total: - 1.25 to 1.49`) %>%
  mutate(under_150 = (less_than_50+less_than_1+less_than_1.25+less_than_1.5)/population) %>% 
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         low_inc_wgt = under_150*wgt)%>%
  summarise(low_inc_nh = sum(low_inc_wgt))

##unemployment
n_unemp_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_S2301_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id2, Geography, Neighborhood, 
         population = `Total; Estimate; Population 16 years and over`,
         unemployed = `Unemployment rate; Estimate; Population 16 years and over`) %>% 
  mutate(unemployed = as.numeric(as.character(unemployed))) %>% 
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         unemp_wgt = unemployed*wgt) %>%
  summarise(unemp_nh = sum(unemp_wgt))

##race
n_race_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_B02001_with_ann.csv", skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  select(Id2, Geography, Neighborhood, population = `Estimate; Total:`,
         black_population = `Estimate; Total: - Black or African American alone`) %>%
  mutate(pct_black = black_population/population) %>% 
  group_by(Neighborhood)%>%
  mutate(wgt = population/sum(population),
         pct_black_wgt = pct_black*wgt)%>%
  summarise(pct_black_nh = sum(pct_black_wgt))

##child poverty
n_cp_data<-read_csv("./input data/JC Tracts/ACS_14_5YR_B17024_with_ann.csv",skip=1) %>% 
  full_join(tract_neigh, by = "Id2") %>% 
  mutate(li_6_pop = `Estimate; Under 6 years: - Under .50` + 
           `Estimate; Under 6 years: - .50 to .74` +
           `Estimate; Under 6 years: - .75 to .99` +
           `Estimate; Under 6 years: - 1.00 to 1.24` +
           `Estimate; Under 6 years: - 1.25 to 1.49`,
         li_11_pop = `Estimate; 6 to 11 years: - Under .50` +
           `Estimate; 6 to 11 years: - .50 to .74` +
           `Estimate; 6 to 11 years: - .75 to .99` +
           `Estimate; 6 to 11 years: - 1.00 to 1.24` +
           `Estimate; 6 to 11 years: - 1.25 to 1.49`, 
         li_17_pop = `Estimate; 12 to 17 years: - Under .50` +
           `Estimate; 12 to 17 years: - .50 to .74` +
           `Estimate; 12 to 17 years: - .75 to .99` +
           `Estimate; 12 to 17 years: - 1.00 to 1.24` +
           `Estimate; 12 to 17 years: - 1.25 to 1.49`,
         li_child_pop = li_6_pop + li_11_pop + li_17_pop,
         child_pop = `Estimate; Under 6 years:` + 
           `Estimate; 6 to 11 years:` +
           `Estimate; 12 to 17 years:`,
         child_li_rate = li_child_pop/child_pop) %>%
  select(Id2, Neighborhood, child_li_rate, child_pop) %>% 
  group_by(Neighborhood)%>%
  mutate(wgt = child_pop/sum(child_pop),
         child_li_rate_wgt = child_li_rate*wgt)%>%
  summarise(child_li_nh = sum(child_li_rate_wgt))

#Life expectancy data comes from the Louisville Center for Health Equity.
#I transcribed it to a csv file from their report PDF
#This will not work with the alternate neighborhood areas, no life expectancy data is available
n_life_expectancy = read_csv(file = "./input data/neighborhood abbreviations and life expectancy.csv")

##using z-scores to identify MPI neighborhoods
NormZ<-function(x){
  (x-mean(x, na.rm=T))/sd(x, na.rm=T)
}

options(scipen = 999)

##joining into one database
data <- full_join(n_earn_data, n_ed_data, by = "Neighborhood") %>% 
  full_join(n_health_data, by = "Neighborhood") %>% 
  full_join(n_low_inc_data, by = "Neighborhood") %>% 
  full_join(n_unemp_data, by = "Neighborhood") %>% 
  full_join(n_race_data, by = "Neighborhood") %>% 
  full_join(n_cp_data, by = "Neighborhood") %>% 
  full_join(n_life_expectancy, by = "Neighborhood") %>% 
  rename(low_income = low_inc_nh, unemployed = unemp_nh, no_hs_degree = hs_nh, 
         bachelors_degree = bach_nh, uninsured = uninsured_nh, 
         median_earnings = earnings_nh, percent_black = pct_black_nh)%>%
  mutate(low_income = low_income*100, bachelors_degree = bachelors_degree*100, 
         no_hs_degree = no_hs_degree*100, percent_black = percent_black*100,
         child_low_income = child_li_nh*100) %>% 
  filter(Neighborhood != "Airport") %>%
  mutate(z_low_income = NormZ(low_income),
         z_unemployed = NormZ(unemployed),
         z_no_hs_degree = NormZ(no_hs_degree),
         z_uninsured = NormZ(uninsured),
         z_overall = (z_low_income+z_unemployed+z_no_hs_degree+z_uninsured)/4)%>%
  arrange(-z_overall)
# data <- rbind(data, c('Airport', rep(0,ncol(data)-1)))

neighborhoods@data<-left_join(neighborhoods@data, data, by = c('Neighbor' = 'Neighborhood'))


neighborhood_leaflet<-function(text, n_format){
  pal <- colorQuantile('YlGn', NULL, n = 7)
  vec <-neighborhoods[[which(names(neighborhoods) == text)]]
  
  if(n_format == 'dollar') pop<-paste0('<strong>',neighborhoods$Neighbor,':</strong> ',dollar(neighborhoods[[which(names(neighborhoods) == text)]]))
  else if(n_format == 'percent') pop<-paste0('<strong>',neighborhoods$Neighbor,':</strong> ',paste0(round(neighborhoods[[which(names(neighborhoods) == text)]],2),'%'))
  else if(n_format == 'comma') pop<-paste0('<strong>',neighborhoods$Neighbor,':</strong> ',comma(neighborhoods[[which(names(neighborhoods) == text)]]))
  else pop<-paste0('<strong>',neighborhoods$Neighbor,':</strong> ',round(neighborhoods[[which(names(neighborhoods) == text)]],4))
  
  leaflet(data=neighborhoods) %>% 
    addTiles() %>% 
    setView(lng = -85.7585, lat = 38.2527, zoom = 10) %>% 
    addPolygons(data = neighborhoods, 
                stroke = F, 
                smoothFactor = 0.2, 
                fillOpacity = 0.75, 
                fillColor = ~pal(vec),
                color ="#BDBDC3",
                popup = pop) %>% 
    addPolylines(weight = 2, color='black')
}

lookup_columns <- c('Median Earnings $' = names(data)[2],
                    'Bachelors Degree %' = names(data)[3],
                    'No HS Degree %' = names(data)[4],
                    'Uninsured %' = names(data)[5],
                    'Population' = names(data)[6],
                    'Low Income %' = names(data)[7],
                    'Unemployed %' = names(data)[8],
                    'Black %' = names(data)[9],
                    'Low Income Children' = names(data)[10],
                    'Life Expectancy' = names(data)[13],
                    'Child Low Income' = names(data)[14],
                    'z-score Low Income' = names(data)[15],
                    'z-score Unemployed'  = names(data)[16], 
                    'z-score No HS Degree' = names(data)[17],
                    'z-score Uninsured' = names(data)[18],
                    'z-score Overall' = names(data)[19]) 

ui<-dashboardPage(
  dashboardHeader(title = 'Louisville Neighborhoods',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('stat', 'Select Metric', names(lookup_columns), selected = 'median_earning')
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput('map')
  )
)

server<-function(input,output){
  scale_type<-reactive({
    text_select<-which(lookup_columns[which(names(lookup_columns) == input$stat)] == names(data))
    if(text_select == 2) scale <- 'dollar'
    else if(text_select == 3 | text_select == 4 | text_select == 5 | text_select == 7 | text_select == 8 | text_select == 9| text_select == 10 | text_select == 14) scale <- 'percent'
    else if(text_select == 6 | text_select == 13) scale <- 'comma'
    else scale<-'none'
    scale
  })
  output$map<-renderLeaflet({
    neighborhood_leaflet(lookup_columns[which(names(lookup_columns) == input$stat)], scale_type())
  })
}

shinyApp(ui, server)