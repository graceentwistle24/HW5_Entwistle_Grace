library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(maps)
library(ggthemes)
library(DT)
library(knitr)
library(kableExtra)

#I utilized data from NASA's GeoCoded Disaster dataset from 1960 - 2018, which can be found at this URL: https://sedac.ciesin.columbia.edu/data/set/pend-gdis-1960-2018#:~:text=The%20Geocoded%20Disasters%20(GDIS)%20Dataset,the%20years%201960%20to%202018.
#I have volunteered for disaster relief in the past, and am very interested in the rise of natural disasters that have occurred recently. My dataset includes natural disasters that occurred across 200 countries from 1960 to 2018. In my plot below, I specifically look at flood and storm counts across all countries to see how the counts of each floods compare to reported storms. I had assumed that there would be more storms than floods, because storms lead to floods, but not all storms cause floods. However, as you can see in the chart below, there are more floods than storms in many years.
#I am creating this dashboard to provide awareness of the increase in natural disasters that have occurred over the years. I created multiple pages for the user to review data in a variety of ways, maps, tables, and charts. Each of the tables provide searchable functions, and the maps are all interactive.
#I created a map of the World to show which countries had the most disasters overall from 1960-2018. The totals are calculated by adding up each of the disasters, regardless of type, for each country. I am utilizing a plotly map for the interactivity of showing the country and total number of disasters when you hover over an area.
#I created two tables so one can see the total number of natural disasters, by type, for each country and statestate has had from 1960 - 2018. I used DT package for both tables because I like the search bar, as well as the tab format to view the different country/states and find which one you are looking for.
#Additionally, I mapped the United States specifically to show with markers where each of the disasters have occurred. The markers are each shown by a different color which designates the type of disaster that occurred at that location. Additionally, if you hover over each marker, you see the year that disaster occurred and the type of disaster.
#I also created graphs of the trends of each disaster type across the 58 year timeframe. Each map includes a single disaster type (i.e., floods, droughts, earthquakes, etc.). I utilized ggplot2 package to create each of these line graphs.

##read in data
disasters <- read.csv("World Disaster Data 1960-2018.csv")

disasters["country"][disasters["country"] == "United States"] <- "USA"
disasters["country"][disasters["country"] == "Republic Of Congo"] <- "Democratic Republic of the Congo"
disasters["country"][disasters["country"] == "Republic Of The Congo"] <- "Democratic Republic of the Congo"

disasters <- disasters %>% 
  subset(disastertype != "mass movement (dry)")

total.disasters <- disasters %>% 
  group_by(country) %>% 
  summarise(
    "TotalNum" = n()
  )
worldmap  <- map_data("world")

mapdata <- inner_join(total.disasters, worldmap, by = c("country" = "region"))

world_map <- ggplot() +
  geom_polygon(data = worldmap, 
               mapping = aes(x = long, y = lat, group = group), 
               color="black", fill=NA) + theme_minimal() 

world_map <- world_map + 
  geom_polygon(data = mapdata, 
               mapping = aes(x = long, y = lat, group = group, fill = TotalNum, 
                             text = paste("Country :", country,
                                          "<br> Total Disasters :", TotalNum))) + 
  scale_fill_viridis_c(option="magma", direction = -1)

world.map <- ggplotly(world_map, tooltip = "text")

by.country <- disasters %>% 
  group_by(country, disastertype) %>% 
  summarise(TotalNum = n())
by.country <- pivot_wider(by.country, names_from = disastertype, values_from = TotalNum)
by.country[is.na(by.country)] <- 0


world.table <- by.country %>%
  datatable(colnames = c("Country", "Droughts", "Earthquakes", "Extreme Temperatures", "Floods", "Landslides", "Storms", "Volcanic Activity"))



US.disasters <- disasters %>% 
  subset(country == "USA")

US.disasters$disastertype <- as.factor(US.disasters$disastertype)


# Function to assign colors
make_color <- function(US.disasters) {
  sapply(US.disasters$disastertype, function(disastertype) {
    if(disastertype == "storm") {
      "green"
    } else if(disastertype == "flood") {
      "blue"
    }  else if(disastertype == "drought") {
      "orange"
    } else if(disastertype == "earthquake") {
      "purple"
    }  else if(disastertype == "extreme temperature") {
      "red"
    }  else if(disastertype == "landslide") {
      "pink"
    }  else if(disastertype == "volcanic activity") {
      "yellow"
    } else {
      "white"
    } })
}

# create icon format
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'fa',   
  markerColor = make_color(US.disasters)
)

US.map <- leaflet(US.disasters) %>%
  addTiles() %>% 
  addAwesomeMarkers(~longitude, ~latitude,
                    label = paste("Type :", US.disasters$disastertype,
                                  " Year :", US.disasters$year),
                    icon=icons)
US.map

by.state <- US.disasters %>% 
  group_by(adm1, disastertype) %>% 
  summarise(TotalNum = n())
by.state <- pivot_wider(by.state, names_from = disastertype, values_from = TotalNum)
by.state[is.na(by.state)] <- 0

state.tbl <- by.state %>%
  datatable(colnames = c("State", "Droughts", "Earthquakes", "Extreme Temperatures", "Floods", "Landslides", "Storms", "Volcanic Activity"))
state.tbl

by.disaster <- disasters %>% 
  group_by(disastertype, year) %>% 
  summarise("TotalNum" = n())

#by.disaster$year <- as.Date(paste(by.disaster$year, "-01-01", sep = ""))

flood <- by.disaster %>% 
  subset(disastertype == "flood")

flood.chart <- ggplot(data = flood, mapping = aes(x = year, y = TotalNum)) 
flood.chart <- flood.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Floods Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number") +
  scale_x_continuous(breaks = seq(1960, 2018, 5))

drought <- by.disaster %>% 
  subset(disastertype == "drought")

drought.chart <- ggplot(data = drought, mapping = aes(x = year, y = TotalNum)) 
drought.chart <- drought.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Droughts Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number")+
  scale_x_continuous(breaks = seq(1960, 2018, 5))

earthquake <- by.disaster %>% 
  subset(disastertype == "earthquake")

earthquake.chart <- ggplot(data = earthquake, mapping = aes(x = year, y = TotalNum)) 
earthquake.chart <- earthquake.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Earthquakes Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number") +
  scale_x_continuous(breaks = seq(1960, 2018, 5))

extremetemp <- by.disaster %>% 
  subset(disastertype == "extreme temperature ")

extremetemp.chart <- ggplot(data = extremetemp, mapping = aes(x = year, y = TotalNum)) 
extremetemp.chart <- extremetemp.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Extreme Temperatures Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number") +
  scale_x_continuous(breaks = seq(1960, 2018, 5))

landslide <- by.disaster %>% 
  subset(disastertype == "landslide")

landslide.chart <- ggplot(data = landslide, mapping = aes(x = year, y = TotalNum)) 
landslide.chart <- landslide.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Landslides Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number") +
  scale_x_continuous(breaks = seq(1960, 2018, 5))

volcanicact <- by.disaster %>% 
  subset(disastertype == "volcanic activity")

volcanicact.chart <- ggplot(data = volcanicact, mapping = aes(x = year, y = TotalNum)) 
volcanicact.chart <- volcanicact.chart +
  geom_line() +
  theme_tufte(base_size = 14) +
  ggtitle("Volcanic Activity Across the World from 1960 to 2018") +
  xlab("Year") + ylab("Total Number") +
  scale_x_continuous(breaks = seq(1960, 2018, 5))

ui <- dashboardPage(
  
  # format
  skin="blue",
  
  # define the title
  dashboardHeader(
    title="Natural Disasters"
  ),
  
  # define the sidebar
  dashboardSidebar(
    # set sidebar menu  
    sidebarMenu(
      menuItem("World Map", tabName = "WorldMap"),
      menuItem("World Data Table", tabName = "worldTable"),
      menuItem("US Map", tabName = "USMap"),
      menuItem("US Data", tabName = "UStable"),
      menuItem("Trends By Disaster", tabName = "trends")
    )
  ),
  
  # define the body
  dashboardBody(
    tabItems(
      # first page
      tabItem("WorldMap",
              h2("Number of Natural Disasters Per Country"),
              box(plotlyOutput("world.map"), width = 600)
      ),
      # second page
      tabItem("worldTable",
              h2("Total Number of Natural Disasters Per Country"),
              box(dataTableOutput("world.table"), width= 500)
      ),
      # third page
      tabItem("USMap",
              h2("Natural Disasters in United States", inline=TRUE),
              box(leafletOutput("US.map"), width= 500)
      ),
      # fourth page
      tabItem("UStable",
              h2("Total Number of Natural Disasters Per US State", inline=TRUE),
              box(dataTableOutput("state.tbl"), width= 500)
      ),
      # fifth page
      tabItem("trends",
              h2("Trends of Natural Disasters Each Year"),
              h3("Trend of Floods from 1960 - 2018"),
              box(plotOutput("flood.chart"), width= 500),
              h3("Trend of Droughts from 1960 - 2018"),
              box(plotOutput("drought.chart"), width= 500),
              h3("Trend of Earthquakes from 1960 - 2018"),
              box(plotOutput("earthquake.chart"), width= 500),
              h3("Trend of Extreme Temperatures from 1960 - 2018"),
              box(plotOutput("extremetemp.chart"), width= 500),
              h3("Trend of Landslides from 1960 - 2018"),
              box(plotOutput("landslide.chart"), width= 500),
              h3("Trend of Volcanic Activity from 1960 - 2018"),
              box(plotOutput("volcanicact.chart"), width= 500)
      )
    )
  )
  
)

server <- function(input, output) { 
  
  # --------------------------------------------------
  # WorldMap
  # --------------------------------------------------
  output$world.map <- renderPlotly(
    
    world.map 
  )
  
  # --------------------------------------------------
  # WorldTable
  # --------------------------------------------------
  output$world.table <- renderDataTable(
    
    world.table 
  )
  
  # --------------------------------------------------
  # USMap
  # --------------------------------------------------
  output$US.map <- renderLeaflet(
    
    US.map 
  )
  # --------------------------------------------------
  # US Table
  # --------------------------------------------------
  output$state.tbl <- renderDataTable(
    
    state.tbl 
  )
  
  # --------------------------------------------------
  # Flood Trends
  # --------------------------------------------------
  output$flood.chart <- renderPlot(
    
    flood.chart 
  )
  
  # --------------------------------------------------
  # Drought Trends
  # --------------------------------------------------
  output$drought.chart <- renderPlot(
    
    drought.chart 
  )
  
  # --------------------------------------------------
  # Earthquake Trends
  # --------------------------------------------------
  output$earthquake.chart <- renderPlot(
    
    earthquake.chart 
  )
  
  # --------------------------------------------------
  # Extreme Temperature Trends
  # --------------------------------------------------
  output$extremetemp.chart <- renderPlot(
    
    extremetemp.chart 
  )
  
  # --------------------------------------------------
  # Landslide Trends
  # --------------------------------------------------
  output$landslide.chart <- renderPlot(
    
    landslide.chart 
  )
  
  # --------------------------------------------------
  # Volcanic Activity Trends
  # --------------------------------------------------
  output$volcanicact.chart <- renderPlot(
    
    volcanicact.chart 
  )
  
  
}

shinyApp(ui, server)