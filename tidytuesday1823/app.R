####Shinyapp 1-8 tidytuesday

library(shiny)
library(shiny.fluent)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(stringr)
library(DT)
library(sf)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(ggrepel)
library(bslib)
library(htmltools)



states <- read.csv("states.csv")
state_name_etymology <- read.csv("state_name_etymology.csv")

pols <- read_sf("us-state-boundaries.shp")

states$Año <- year(states$admission)


data <- inner_join(x = states , y = pols, by = c("state" = "name" ))
data <- st_as_sf(data)


bins <- c(0, 1000000, 3000000, 5000000, 10000000, 30000000, 50000000)
pal <- colorBin("YlOrRd", bins = bins)


selan <- sort(unique(states$Año))

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Calibri"
)



# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Tidytuesday: 2023-08-01",
  theme = custom_theme,
  ####Capítulo-1 ----
  tabPanel("US State Names",
           fluidRow(
             column(9,
                    card(height = 650,full_screen = TRUE,
                         h2("Map"),
                         leafletOutput("mapa",
                                       width = "100%",
                                       height = 550))),
             column(3,
                    card(height = 650,full_screen = TRUE,
                         h2("Population and admission"),
                         DT::dataTableOutput("tabla")))),
           fluidRow(column(3,
                           card(height = 200,
                                h2("Years"),
                                sliderInput("dateSel", "",
                                            min = 1787,
                                            max = 1959,
                                            value = 1787,
                                            step = 5,
                                            animate = animationOptions(interval = 1000, loop = FALSE),
                                            width = '100%'))
           ))
  )
)



server <- function(input, output) {
  
  
  
  #### Mapa proyecciones poblacionales ----
  
  output$mapa <- renderLeaflet({
    
    
    data <- inner_join(x = states , y = pols, by = c("state" = "name" ))
    data <- st_as_sf(data)
    
    bins <- c(0, 1000000, 3000000, 5000000, 10000000, 30000000, 50000000)
    pal <- colorBin("YlOrRd", bins = bins)
    
    
    selan <- sort(unique(states$Año))
    
    datar <- data %>% filter(Año <= input$dateSel)
    lg <- length(datar$state)
    
    if (lg >= 50) {
      leaflet() %>%
        addTiles() %>%
        setView(-100.68376379795953,37.20651781478685, 3) %>%
        addPolygons(data = datar,
                    weight = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal(datar[[6]]),
                    group = "`Año`",
                    popup = ~ state) %>%
        addLegend(pal = pal, values = bins, opacity = 1.0,
                  title = "Population")
    }else{
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = datar,
                    weight = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal(datar[[6]]),
                    group = "`Año`",
                    popup = ~ state) %>%
        addLegend(pal = pal, values = bins, opacity = 1.0,
                  title = "States population")
    }
    
    
  })
  
  #### Tabla states data ----
  output$tabla <- renderDataTable({
    datar <- states[,c(1,6,5)]
    datar <- rename(datar, "State" = "state")
    datar <- rename(datar, "Population_2020" = "population_2020")
    datar <- rename(datar, "Admission" = "admission")
    
    DT::datatable(
      datar[order(datar$Population_2020,decreasing = TRUE),],
      extensions = 'FixedColumns',
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list(className = 'dt-center', 
                               targets = "_all"))
      )
    )
    
  })
  
  
}


shinyApp(ui,server)

