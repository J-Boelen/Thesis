# Libraries
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("maptools")
library("KernSmooth")
library("raster")
library("MASS")
library("RColorBrewer")
library("shiny")
library("leaflet.extras")


# Read in data (tourists_geo.csv for twitter, tourists_geo_attachments.csv for attachments
# FlickrTourists_geo.csv for Flickr, Tourists_Photo.csv for combined)
#data <- read.csv("/Users/Jody/PycharmProjects/Thesis/CSV/FlickrTourists_geo.csv", header = TRUE)

# Get the suggested bandwidth --> twitter
#bandwidth_Lat = bandwidth.nrd(data[, "geo_lat"])
#bandwidth_Lon = bandwidth.nrd(data[, "geo_lon"])

# Get the suggested bandwidth --> flickr
#bandwidth_Lat = bandwidth.nrd(data[, "latitude"])
#bandwidth_Lon = bandwidth.nrd(data[, "longitude"])

# Create KDE --> the smaller the bandwidth, the more detailed the kde.
#kde <- bkde2D(data, bandwidth = c(0.002, 0.002), gridsize = c(3000,3000))
#kde2 <- kde2d(data[, 'geo_lon'], data[, 'geo_lat'], h = c(0.002, 0.002), n = 3000, lims = c(3, 5, 50, 55))
# Create raster from KDE 
#KernelDensityRaster <- raster(list(x=kde$x1, y=kde$x2, z=kde$fhat))

# Remove low density cells 1 for Flickr, 5 for twitter
#KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 10)] <- NA

# Create function to color the raster
#palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent", reverse = TRUE, sort(rexp(16)))

# Binned raster
#palRaster <- colorBin("Blues", bins = c(0, 5, 8, 10, 100, 1000, 2000, 3500, 5000, 7500), domain = KernelDensityRaster@data@values, na.color = "transparent", reverse = FALSE, sort(rexp(16)))

# Leaflet map with raster
#leaflet() %>% addTiles() %>%
  #addProviderTiles(providers$CartoDB.Positron) %>% 
  #fitBounds(4.35, 51.80, 4.7, 52)%>% 
  #addRasterImage(KernelDensityRaster,
                 #colors = palRaster,
                 #opacity = 0.7) %>%
  #addGraticule(
    #interval = 1/100, style = list(color = "rgb(8,81,156)", weight = 0.8)) #%>% 
  #addLegend(#pal = palRaster,
            #colors = c("rgb(107,174,214)", "rgb(66,146,198)", "rgb(33,113,181)", "rgb(8,81,156)", "rgb(8,48,107)"),
            #labels= c("<100", "1000","2000","3500",">5000"),
            #values = KernelDensityRaster@data@values,
            #title = "Photos", opacity = 1)
  
# Interactive map for getting coordinates
ui <- fluidPage(
  leafletOutput("mymap",height=850)
)

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      #addRasterImage(KernelDensityRaster,
                     #colors = palRaster,
                     #opacity = 0.6) %>%
      #addLegend(pal = palRaster,
                #values = KernelDensityRaster@data@values,
                #title = "Photos", opacity = 1) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  })
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    
  })
  
  
}

shinyApp(ui, server)

