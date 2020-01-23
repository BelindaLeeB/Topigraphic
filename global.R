# R Libraries
require(shiny)
require(shinydashboard)

require(readr)
require(dplyr)
require(data.table)



require(plotly)
require(ggplot2)

require(rgdal)
require(leaflet)
require(overlap)
require(magick)

source("helpers.R")

#Load data files - meta-data file fields are used to set Shiny application parameters

CameraTraps <- fread(paste("data", "Camera_Trap_Metadata.csv",sep = "/"))

Species <- fread(paste("data", "Pop_Unit_Metadata.csv",sep = "/"))

CaptureImages <- fread(paste("data",  "Pop_Unit_Images.csv",sep = "/"))

CaptureTimestamps <- fread(paste("data", "Activity_Data.csv",sep = "/"))

#LandscapeVariableDetail <- fread(paste("data", "Landscape_Var_Metadata.csv",sep = "/"))
#LandscapeVariableValues <- fread(paste("data", "Landscape_Variables.csv",sep = "/"))

#PUVarMeta <- fread(paste("data", "Pop_Unit_Var_Metadata.csv",sep = "/"))
#PUVar <- fread(paste("data", "Pop_Unit_Variables.csv",sep = "/"))
#PredVar <- fread(paste("data", "Predation_Variables.csv",sep = "/"))

# Load landscape feature shapefiles and transform coordinates

#RiverShapefiles <- readOGR(paste("data", "map_layers","MainRivAnna", sep = "/"), "mainrivers")
#proj4string(RiverShapefiles) <- CRS("+init=epsg:21036")
#RiverShapefiles <- spTransform(RiverShapefiles, CRS("+init=epsg:4326"))

RiverShapefiles <- readOGR(paste("data", "map_layers","MainRivers", sep = "/"), "RiverShapefiles")
  
#KopjeShapefiles <- readOGR(paste("data", "map_layers","KopjesOrig", sep = "/"), "V3_Kopjes_ARC1960")
#KopjeShapefiles <- spTransform(KopjeShapefiles,CRS("+init=epsg:4326"))
#writeOGR(KopjeShapefiles, "./data/map_layers/Kopjes", "KopjeShapefiles", driver="ESRI Shapefile")

KopjeShapefiles <- readOGR(paste("data", "map_layers","Kopjes", sep = "/"), "KopjeShapefiles")

# Initialized values
init_location= "Serengeti"
init_highlighted_camera_trap = "I07"
init_selected_prey  = "topi"
init_selected_predator = "lion"
init_selected_season = "Wet"
init_trait_list = list(label = c("Scientific name", "Trophic level", "Weight", "Maximum speed"), 
                       var = c("species_latin_name", "species_trophic_level","species_weight_range","species_max_speed"))


CameraTraps[, c("include", "highlight") := .(TRUE, site_id==init_highlighted_camera_trap),]

CaptureImages[Species, species_label := species_label, on = .(species)]
CaptureTimestamps[Species, species_label := species_label, on = .(species)]

Species[, c("include_species", "include_prey") := .(species %in% c(init_selected_predator,init_selected_prey), species%in% c(init_selected_prey))]

#Do once calcTimeDensity better scoped
init_capture_timedensity_plot <- calcTimeDensity("gazelleThomsons", CameraTraps, "Wet")
#init_capture_timedensity_plot <- max(CameraTraps[, max(calcTimeDensity(unique(Species$species),.SD,c("wet","dry"))[[1]]$y)])
#init_capture_timedensity_plot <- max(vapply(seq_along(Species$species), function(i) {max(calcTimeDensity(Species$species,CameraTraps,c("wet","dry"))[[i]]$y)}))

