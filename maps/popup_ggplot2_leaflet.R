# 
# Author:   Cristian E. Nuno
# Purpose:  Add ggplot2 plots as popups within polygons in a leaflet map
# Date:     December 24, 2018
#

# load necessary packages ----
library(leaflet)
library(mapview)
library(sf)
library(tidyverse)

# load necessary data ----

# note: cca stands for chicago community area
cca.boundaries <-
  read_sf("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON")

affordable.rental.units <-
  read_csv("https://data.cityofchicago.org/api/views/s6ha-ppgi/rows.csv?accessType=DOWNLOAD")

# count how many affordable rental units per cca
cca.aru.count <-
  affordable.rental.units %>%
  mutate(community = str_to_upper(`Community Area Name`)) %>%
  count(community)

# join the count onto the sf object
cca.boundaries <-
  cca.boundaries %>%
  left_join(cca.aru.count, by = "community") %>%
  mutate(n = if_else(is.na(n), 0L, n)
         , community = str_to_title(community)
         , community = if_else(community == "Ohare"
                               , "O'Hare"
                               , community))

# make a function that will make a custom plot for each cca ----
CustomPlot <- function(x){
  # Inputs:
  # * x: a character vector used to subset cca.boundaries
  #
  # Output:
  # a ggplot that visualizes the distribution of affordable rental units per Chicago community area
  # that highlights the value in 'x'
  require(ggplot2)
  require(dplyr)
  
  cca.boundaries %>%
    ggplot(aes(x = "", y = n)) +
    geom_jitter(width = 0.15, height = 0, color = "#515253") +
    geom_point(data = function(i) filter_(i, ~ community == x)
               , color = "#046afd"
               , size = 3) +
    ylim(c(0, 40)) +
    xlab("Chicago community areas") +
    ylab("Number of affordable rental units") +
    labs(title = "Distribution of affordable\nrental units in Chicago, 2018"
         , subtitle = paste0(cca.boundaries$community[cca.boundaries$community == x]
                             , " has "
                             , cca.boundaries$n[cca.boundaries$community == x]
                             , " affordable rental units")
         , caption = "Source: City of Chicago Open Data Portal (https://tinyurl.com/ycl2nxb6)") +
    theme_minimal() +
    theme(panel.grid = element_blank()
          , plot.subtitle = element_text(color = "#046afd"))
  
}

# store custom plots in a list ---
# note: this takes awhile to create (~15 seconds)
list.of.custom.plots <-
  cca.boundaries$community %>%
  set_names() %>%
  map(.f = CustomPlot) %>%
  # note: svg produces a higher quality image than png and loads faster
  #       while png produces a lower quality image and takes longer to load than svg.
  #       however, png graphs can be saved as images from the browser
  popupGraph(type = "svg"
             , width = 300
             , height = 400)

# map data values to colors
pal <- 
  colorNumeric(palette = "viridis"
               , domain = NULL)

# create leaflet map ----
leaflet(data = cca.boundaries) %>%
  addTiles() %>%
  addPolygons(popup = list.of.custom.plots
              , stroke = TRUE
              , color = "#CCCCCC"
              , weight = 2
              , smoothFactor = 0.05
              , fillOpacity = 1
              , fillColor = ~pal(n)
              , label = ~community
              , labelOptions = labelOptions(textsize = "25px"
                                            , textOnly = TRUE
                                            , style = list(
                                              "color" = "white"
                                              , "font-family" = "Ostrich Sans Black black"
                                              , "font-weight" =  "bold"
                                              , "text-shadow" = "-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black"
                                            ))
              , highlightOptions = highlightOptions(color = "white"
                                                    , weight = 5
                                                    , opacity = 0.75)) %>%
  addLegend(pal = pal
            , values = ~n
            , opacity = 1.0
            , position = "bottomright"
            , title = "Number of affordable rental units, 2018")


# end of script #