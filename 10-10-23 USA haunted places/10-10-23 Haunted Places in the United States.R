library(pacman)
p_load(tidyverse,tidytuesdayR,leaflet,htmlwidgets,webshot)


tuesdata <- tidytuesdayR::tt_load('2023-10-10')



df <- tuesdata$haunted_places


dfr <- df


dfr$location <- toupper(dfr$location)

dfr <- dfr |> mutate(Location_spec = 
                       case_when(grepl("UNIVERSITY", location) ~ 'University',
                                 grepl("SCHOOL", location) ~ 'School',
                                 grepl("COLLEGE", location) ~ 'College',
                                 .default = "No")
)

####Check haunted places by states
res <- dfr |> group_by(state) |>
  summarise(Tot = n()) |>
  arrange(-Tot)

####Filter na coords and university,school and college
c <- dfr |>
  filter(!is.na(longitude) | !is.na(latitude)) |>
  filter(!Location_spec == "No")

  

locr <- unique(c$Location_spec)
pals <- c("#FBE426","#FF00B6","#00FFBE")

lpal <- paste(locr,pals,sep = ",")

####Mapa


style <- htmltools::HTML(
  "
  <style>
    @import url('https://fonts.googleapis.com/css2?family=Playball&display=swap');

    h2 {
    font-size: 30px;
    font-family: 'Playball';
    line-height: 35px;
    margin-bottom: 10px;
    }
    
    h3 {
    font-size: 24px;
    font-family: 'Playball';
    line-height: 25px;
    margin-bottom: 10px;
    }

    #legend {
    position: fixed;
    border-radius: 10px;
    top:10px;
    left:400px;
    margin: 10px;
    width: 800px;
    background-color: #990000;
    padding: 10px 20px;
    z-index: 1002;
    }

    .content {
    margin-bottom: 20px;
    }

    .row {
    height: 12px;
    width: 100%;
    }


    .label {
    width: 30%;
    display: inline-block;
    text-align: center;
    font-family: 'Playball';
    }
    </style>
  "
)

legend <- htmltools::HTML(
  '
<div id="legend">
<div class="content">
<h2>Haunted Universities, Colleges and Schools in the USA.</h2>
<h3>California is the state with the most haunted places followed by Texas and Pennsylvania. On the other hand, the states with the fewest haunted places are Washington DC
, Alaska and Delaware.</h3>

</div>
</div>
  '
)


  
mp <- leaflet() |>
  setView(lng = -104.52898029491769, lat = 39.210175260752344, zoom = 4) |>
  addProviderTiles(provider = "CartoDB.DarkMatter")

for (i in lpal) {
  t <- unlist(strsplit(i,","))
  p <- t[1]
  pl <- t[2]
  
  cr2 <- c |> filter(Location_spec == p)
  
  
  mp <- mp |>
    addCircleMarkers(data = cr2, lng = ~ longitude, lat = ~ latitude,
                     color = pl, radius = 1.5, popup = ~ location,
                     weight = 1.5, opacity = 1, fillOpacity = 1)


  
}


mapa <- mp |>
  addLegend(data = c, "bottomright",
            colors = c("#FBE426","#FF00B6","#00FFBE"), 
            labels = c("University","College","School"),
            title = "Ref.",
            opacity = 1) |>
  htmlwidgets::prependContent(style) |>
  htmlwidgets::appendContent(legend)




saveWidget(mapa, file="map.html")
webshot("map.html","map.png", vwidth = 1350, vheight = 800)


mapa


