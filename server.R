
####### SERVER FUNCTION ####### 
shinyServer(function(input, output, session, clientData) {
  
  # SIDEBAR OUTPUT
  
  #Species Traits
  observe({
    trait_detail <- paste0(mapply(function(label,var){paste0(tags$b(paste0(label,":  ")), Species[species == input$select_prey_species,c(var), with = FALSE][[1]],tags$br())},
                  init_trait_list$label, init_trait_list$var), 
           collapse = "")
    
    output$detail_species <- renderUI({
      div(style = "padding: 10px;",HTML(trait_detail))
    })
  
  }) 
  
  # Season Description
  output$detail_season_wetdry <- renderUI({
    if(input$radio_season_wetdry == "Wet") {
      div(style = "padding: 10px;","Rain drives the great migrations of the Serengeti. 
      The 'wet' season begins in November and ends in May, and during that period millions of animals march across the Serengeti plain, 
          grazing over its grasslands for nourishment as they go.")
    } else {
      div(style = "padding: 10px;","By June, the rains have gone, the grasslands have become parched, the migration has moved on, 
      and only a fraction of the animal remain to see the 'dry' season through to its end in October.")
    }
  })
  
  # Landscape Feature Description
 #output$detail_landscape_var_type <- renderUI(LandscapeVariableDetail[land_var_type == input$select_landscape_variable]$var_type_description)
  
  
 # SELECTIZED MAP
  
  # Map Title
  output$map_title <- renderText({paste(init_location, "Camera Trap Locations", sep = " ")})
 
  # Render initial base map layers
  output$selectized_cameratrap_map <- renderLeaflet({
    
    camera_trap_map <- leaflet(CameraTraps) %>%
      setView(lng = 34.94, lat = -2.513417, zoom = 11) %>%
      addMapPane(name = "Landscape", zIndex = 1000) %>% 
      addMapPane(name = "Occupancy", zIndex = 2000) %>%
      addMapPane(name = "TrapMarkers", zIndex = 3000)
   
    
    if(init_selected_season == "Wet") {
      camera_trap_map <- camera_trap_map %>%
        addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(noWrap = T, maxZoom = 13, zIndex = 1), group = "Season") %>%
        addPolylines(data = RiverShapefiles, color = "cornflowerblue", weight = 4, group = "Season",options = leafletOptions(pane = "Landscape"))
   
    } else {
      camera_trap_map <- camera_trap_map %>%
        addProviderTiles(providers$Esri.WorldShadedRelief, options = tileOptions(noWrap = T, maxZoom = 13, zIndex = 1), group = "Season") %>%
        addPolylines(data = RiverShapefiles, color = "lightblue", weight = 4, group = "Season", options = leafletOptions(pane = "Landscape"))
    }
    
    camera_trap_map <- camera_trap_map %>%
      addPolygons(data = KopjeShapefiles, weight = 6, color = "peru", fillColor = "darktan", fillOpacity = 0.7, group = "Kopje", options = leafletOptions(pane = "Landscape")) %>%
      addCircles(data = filter(CameraTraps, highlight == TRUE), lng = ~lon, lat = ~lat, radius = 1000, weight = 3, color = "goldenrod", fill = FALSE, group = "Camera Traps",options = leafletOptions(pane = "TrapMarkers")) %>%
      addCircleMarkers(data = CameraTraps, layerId = ~site_id, lng = ~lon, lat = ~lat, radius = 3.5, weight = 2, color = "black", fill = ~include, fillOpacity = 0.7, label = ~site_id, labelOptions = labelOptions(textOnly = TRUE),group = "Camera Traps",options = leafletOptions(pane = "TrapMarkers")) %>%
      addLayersControl(overlayGroups = c("Kopje","Water","Camera Traps"), options = layersControlOptions(collapsed = FALSE))
    
    camera_trap_map
  })
  
  # Reactive dataframe/value for tracking activated/deactivated and highlighted camera trap sites
  reactive_CT <- reactiveValues(data = data.frame(copy(CameraTraps)), highlight = init_highlighted_camera_trap)
  
  # Render updated seasonal map features 
  observe({
    
    if(input$radio_season_wetdry == "Wet") {
      
      leafletProxy("selectized_cameratrap_map", data = isolate(reactive_CT$data)) %>%
        clearGroup("Season") %>%
        addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(noWrap = T, maxZoom = 13, zIndex = 1), group = "Season") %>%
        addPolylines(data = RiverShapefiles, color = "cornflowerblue", weight = 4, group = "Season", options = leafletOptions(pane = "Landscape"))
    } else {
      leafletProxy("selectized_cameratrap_map", data = isolate(reactive_CT$data)) %>%
        clearGroup("Season") %>%
        addProviderTiles(providers$Esri.WorldShadedRelief, options = tileOptions(noWrap = T, maxZoom = 13, zIndex = 1), group = "Season") %>%
        addPolylines(data = RiverShapefiles, color = "lightblue", weight = 4, group = "Season", options = leafletOptions(pane = "Landscape"))
    }
    
})
  
  #Render updated activated/deactivated and highlighted camera trap site markers map layers
  
  observe({  
    leafletProxy("selectized_cameratrap_map", data = reactive_CT$data) %>%
      clearGroup("Camera Traps") %>%
      addCircles(data = filter(reactive_CT$data, highlight == TRUE), lng = ~lon, lat = ~lat, radius = 1000, weight = 3, color = "goldenrod", fill = FALSE, group = "Camera Traps", options = leafletOptions(pane = "TrapMarkers")) %>%
      addCircleMarkers(data = reactive_CT$data, layerId = ~site_id, lng = ~lon, lat = ~lat, radius = 3.5, weight = 2, color = "black", fill = ~include, fillOpacity = 0.7, label = ~site_id, labelOptions = labelOptions(textOnly = TRUE), group = "Camera Traps", options = leafletOptions(pane = "TrapMarkers"))
    
  })
  
  #Map_marker_click observer for changing activation/highlight status of camera trap sites
  
  observeEvent(input$selectized_cameratrap_map_marker_click, {
    map_click <- input$selectized_cameratrap_map_marker_click
    
    if(is.null(map_click)) {return()}
    
    if(map_click$id != reactive_CT$highlight) {
      
      reactive_CT$data <- reactive_CT$data %>% 
        mutate(highlight = ifelse(site_id == map_click$id, TRUE, FALSE))
      
      reactive_CT$highlight = map_click$id
      
    } else {
      
      if(map_click$id %in% filter(reactive_CT$data, include == TRUE)$site_id) {
        
        update_include <- filter(reactive_CT$data, include == TRUE & site_id != map_click$id)$site_id
        reactive_CT$data <- reactive_CT$data %>%
          mutate(include = ifelse(site_id %in% update_include, TRUE, FALSE))
        
      } else {
        
        update_include <- filter(reactive_CT$data, include == TRUE | site_id == map_click$id)$site_id
        reactive_CT$data <- reactive_CT$data %>%
          mutate(include = ifelse(site_id %in% update_include, TRUE, FALSE))
      }
    }
  })
  
# Reactive value and action button observer for activating/deactivating ALL camera trap sites simultaneously
  switch_include_exclude <- reactiveValues(include_all = F)
  
  observeEvent(input$button_include_all_sites,{
    
    reactive_CT$data <- reactive_CT$data %>%
      mutate(include = ifelse(switch_include_exclude$include_all, TRUE, FALSE))
    
    switch_include_exclude$include_all <- !switch_include_exclude$include_all
    
  })
  
# SNAPSHOT IMAGE SLIDESHOW
  
  #Slideshow titles
  output$snapshot_title_site <- renderText({paste0("Snapshots (Site ", reactive_CT$highlight, ")")})
  output$snapshot_title_prey <- renderText({Species[species == input$select_prey_species]$species_label})
  output$snapshot_title_lion <- renderText({"Lion"})
  
  #Slideshow image temp directory
  #session_directory <- paste0("A",session$token)
  #dir.create(paste("www",session_directory, sep = "/"))
  #session_directory_path <- paste("www",session_directory, sep = "/")

# Reactive population-unit-to-species coversion object (for compatibility with legacy data and potential expansion)
  #reactive_SP <- reactive({unique(Species[species == input$select_prey_species]$species)})
  
  #Sampled images specific to species season and site
  reactive_IM_prey <- reactive({
    
    sampleValidImages(highlighted_site = reactive_CT$highlight, focal_species = input$select_prey_species, season = input$radio_season_wetdry)
    
    })
  
  reactive_IM_lion <- reactive({
    
    sampleValidImages(highlighted_site = reactive_CT$highlight, focal_species = "lion", season = input$radio_season_wetdry)
  
    })
  
 
# Observers for updating the image selection slider to reflect the correct number of images in slideshow
  observe({
    n_images <- nrow(reactive_IM_prey())
    
    updateSliderInput(session, "sliding_image_number_prey", max = ifelse(n_images == 0, 1, n_images), value = 1)
    
    #remove old full-size image files in species directory using file.remove
    #lapply(Species[species_type == "Prey"]$species, 
    #       function(sp_prey) {file.remove(paste(paste(session_directory_path,sp_prey, sep = "/"),list.files(paste(session_directory_path,sp_prey, sep = "/"))), sep = "/")})
  })
  
  observe({
    n_images <- nrow(reactive_IM_lion())
  
    updateSliderInput(session, "sliding_image_number_lion", max = ifelse(n_images == 0, 1, n_images), value = 1)
    
    #lapply(Species[species_type == "Prey"]$species, 
    #       function(sp_prey) {file.remove(paste(paste(session_directory_path,sp_prey, sep = "/"),list.files(paste(session_directory_path,sp_prey, sep = "/"))), sep = "/")})
  })
  
  # Temp file paths
  #reactive_image_file_path_prey <- reactive({
    
  #  findImageFile(IM_i = input$sliding_image_number_prey, 
  #                IM = reactive_IM_prey(), 
  #                site_id = reactive_CT$highlight, 
  #                species = input$select_prey_species, 
  #                sess_dir = session_directory)
  #  })
  
  #reactive_image_file_path_lion <- reactive({
  #  
  #  findImageFile(IM_i = input$sliding_image_number_lion, 
  #                IM = reactive_IM_lion(), 
  #                site_id = reactive_CT$highlight, 
  #                species = "lion", 
  #                sess_dir = session_directory)
  #  })
  
  
  
# Render image selected from reactive dataframe of image names using slider value as row index   
  output$snapshot_image_prey <- renderImage({
    
    #resized_src <- retrieveResizedImage(reactive_image_file_path_prey(), 
    #                                    final_width = clientData$output_snapshot_image_prey_width, 
    #                                    final_height = clientData$output_snapshot_image_prey_width,
    #                                    focal_species = isolate(input$select_prey_species))
    
    # Generate image path to "empty" site image if there are no prey images)
    if(nrow(reactive_IM_prey()) == 0)
    {
      image_path <- paste0("www/BlankSiteImages/", reactive_CT$highlight, "_Blank1.JPG")
      
    } else {
      image_path <- paste0("https://snapshotserengeti.s3.msi.umn.edu/", reactive_IM_prey()[input$sliding_image_number_prey, ]$url)
    }
    
    image_file <- image_read(image_path)
    
    # Get current image output dimensions from UI and resize image
    expected_width  <- min(clientData$output_snapshot_image_prey_width)
    expected_height <- min(clientData$output_snapshot_image_prey_height)
    
    actual_width <- min(image_info(image_file)$width[1])
    actual_height <- min(image_info(image_file)$height[1])
    
    min_aspect_ratio <- min(actual_width/expected_width*100, actual_height/expected_height*100,100)
    
    image_file <- image_resize(image_file, geometry_size_percent(min_aspect_ratio))
    
    image_write(image_file, path = "www/Prey_Img.JPG", format = "jpeg")
    
    # Return file src data
    #list(
    #  src = resized_src,
    #  width = clientData$output_snapshot_image_prey_width,
    #  height = clientData$output_snapshot_image_prey_width,
    #  contentType = "image/jpeg",
    #  alt = "Copyright U of M Lion Center"
    #)
    
    # Return file src data
    list(
      src = "www/Prey_Img.JPG",
      width = expected_width,
      height = expected_height,
      contentType = "image/jpeg",
      alt = "Copyright U of M Lion Center"
    )
    
  }, deleteFile = TRUE)
  
 
  output$snapshot_image_lion <- renderImage({
    
    # Generate image path (to "empty" site image if there are no prey images)
    if(nrow(reactive_IM_lion()) == 0)
    {
      image_path <- paste0("www/BlankSiteImages/", reactive_CT$highlight, "_Blank1.JPG")
      
    } else {
      image_path <- paste0("https://snapshotserengeti.s3.msi.umn.edu/", reactive_IM_lion()[input$sliding_image_number_lion, ]$url)
    }
    
    image_file <- image_read(image_path)
    
    # Get current image output dimensions from UI
    expected_width  <- min(clientData$output_snapshot_image_lion_width)
    expected_height <- min(clientData$output_snapshot_image_lion_height)
    
    actual_width <- min(image_info(image_file)$width[1])
    actual_height <- min(image_info(image_file)$height[1])
    
    min_aspect_ratio <- min(actual_width/expected_width*100, actual_height/expected_height*100,100)
    
    image_file <- image_resize(image_file, geometry_size_percent(min_aspect_ratio))
    
    image_write(image_file, path = "www/Lion_Img.JPG", format = "jpeg")
    
    # Return file src data
    list(
      src = "www/Lion_Img.JPG",
      width = expected_width,
      height = expected_height,
      contentType = "image/jpeg",
      alt = "Copyright U of M Lion Center"
    )
  }, deleteFile = TRUE)
  
# TIME OF DAY CAMPTURE DENSITY
  
  output$capture_timedensity_plot <- renderPlot({
    
    # Check if there are enough camera trap sites selected
    if(nrow(filter(reactive_CT$data, include == TRUE)) > 2) {
      
      timedensity_plot_list <- lapply(as.list(c(input$select_prey_species,"lion")), 
                              function(sp){calcTimeDensity(sp, reactive_CT$data, input$radio_season_wetdry)})
    } else {
      timedensity_plot_list <- list()
    }
    
    # Set limit of y axis from initialized value
    timedensity_plot_ymax <- max(init_capture_timedensity_plot[[1]]$y)
    
    # Set up graph axes and background
    timedensity_plot <- ggplot(data = init_capture_timedensity_plot[[1]], aes(x = x)) + 
      ylab("Relative Capture Frequency") + 
      xlab("") + 
      scale_x_continuous(breaks = c(0.5, 6, 12, 18, 23), labels = c("Midnight", "Morning", "Midday", "Evening", "Midnight"), limits=c(0,24), expand = c(0,0)) +
      scale_y_continuous(limits = c(0, timedensity_plot_ymax), breaks = NULL, expand = c(0,0)) + 
      theme(axis.line.x = element_line(), axis.line.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size = 12), axis.ticks.length = unit(2,"mm"), axis.title.y = element_text(size = 14)) +
      geom_rect(aes(xmin = 0 , xmax = 6, ymin = 0, ymax = timedensity_plot_ymax), fill = "grey86", alpha = 0.5) + 
      geom_rect(aes(xmin = 18 , xmax = 24, ymin = 0, ymax = timedensity_plot_ymax), fill = "grey86", alpha = 0.5)
    
    # Draw activity curves
    if(length(timedensity_plot_list) != 0) {
      for(i in seq_along(timedensity_plot_list)) {
        timedensity_plot <- timedensity_plot +
          geom_area(data = timedensity_plot_list[[i]][[1]], aes(y = y), color = timedensity_plot_list[[i]][[2]], fill = timedensity_plot_list[[i]][[2]], alpha = 0.2)
      }}  
    
    # Overlay day/night delimeters
    timedensity_plot <- timedensity_plot + 
      geom_vline(xintercept = 6, linetype = 3) +
      geom_vline(xintercept = 18, linetype = 3)
    
    # Render activity plot output
    timedensity_plot
    
  })
  
 # session$onSessionEnded(function() {
#    system(paste("rm -r", session_directory))
#  })
  
 # output$var.plot <- renderPlot({
    
    # arrage data to plot
#    pop.unit.list <- c(input$ckbx.prey,input$ckbx.pride)
    
 #   x.var <- PlotData %>% 
#      filter(season == input$radio.season) %>%
#      filter(tod %in% ToD.Selection.List()) %>%
#      filter(pop.unit.name %in% pop.unit.list) %>%
#      filter(var.name == input$radio.plot.x.var) %>%
#      mutate(x.value = value) 
    
#    y.var <- PlotData %>% 
#      filter(season == input$radio.season) %>%
#      filter(tod %in% ToD.Selection.List()) %>%
#      filter(pop.unit.name %in% pop.unit.list) %>%
#      filter(var.name == input$radio.plot.y.var) %>%
#      mutate(y.value = value) %>%
#      select(pop.unit.name, tod, y.value)
    
#    plot.data <- merge(x.var, y.var, by=c("pop.unit.name", "tod"))
    
#    ggplot(data = plot.data, aes(x = x.value)) +
#      geom_point(aes(y = y.value, color = pop.unit.name, shape = tod)) + 
#      theme_bw() +
#      xlab(input$radio.plot.x.var) +
#      ylab(input$radio.plot.y.var) +
#      theme(legend.position = "bottom")
    
#  })

  })
