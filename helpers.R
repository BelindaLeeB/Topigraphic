#GET SAMPLE SUBSET OF VALID IMAGE RECORDS

sampleValidImages <- function(highlighted_site, focal_species, season) {
  
  valid_IM <- CaptureImages[site_id == highlighted_site
                            ][species == focal_species
                              ][wet_dry == season]
  
  #Calculate image subset size 
  n_images <- valid_IM[,.N]
  subset_size <- ifelse(n_images == 0, 0, min(2*(round(log(n_images)) + 1), n_images))
  
  #Return subset
  if(n_images != 0) {
    valid_IM <- sample_n(valid_IM, subset_size, replace = FALSE)
  } else {
    valid_IM <- valid_IM[FALSE, ]
  }
  
  return(data.frame(valid_IM))
}

#GENERATE TIME-OF-DAY CAPTURE DENSITY PLOT DATA
calcTimeDensity <- function(included_sp, react_CT_data, season) {
  
  color_sp <- unique(filter(Species, species == included_sp)$species_color)
  included_ct <- unique(filter(react_CT_data, include == TRUE)$site_id)
  capture_timeofday_rad <- filter(CaptureTimestamps, (species == included_sp) & (site_id %in% included_ct) & (wet_dry %in% season))$tod_radians
  
  sp_timedensity_dataframe <- as.data.frame(densityPlot(capture_timeofday_rad, extend = NULL))
  
  list(sp_timedensity_dataframe, color_sp)
}

# FIND OR DOWNLOAD SNAPSHOT IMAGE FILE IF ANY AVAILABLE
#findImageFile <- function(IM_i, IM, site_id, species, sess_dir){
#  
#  if(nrow(IM) == 0) {
#    
#    image_path <- paste0("www/BlankSiteImages/", site_id, "_Blank1.JPG")
#    
#  } else {
#    
#    image_path <- paste("www",sess_dir,species,paste0("Img_",IM_i,".JPG"), sep = "/")
#    
#    if(!file.exists(image_path)){
#      
#      if(!dir.exists(paste("www",sess_dir,species, sep = "/"))){dir.create(paste("www",sess_dir,species, sep = "/"))}
#      
#      image_file <- image_read(paste0("https://snapshotserengeti.s3.msi.umn.edu/", IM[IM_i, ]$url))
#      
#      image_write(image_file, path = image_path, format = "jpeg")
#      
#    } 
#  }
#  return(image_path)
#}

# FIND AND RESIZE DOWNLOADED IMAGE
#retrieveResizedImage <- function(full_image_file_path, final_width, final_height, focal_species) {
#  
#  image_file <- image_read(full_image_file_path)
#  
#  min_aspect_ratio <- min(image_info(image_file)$width[1]/final_width*100, image_info(image_file)$height[1]/final_height*100,100)
#  
#  image_file <- image_resize(image_file, geometry_size_percent(min_aspect_ratio))
#  
#  temp_image_file_path <- paste0("www/",focal_species,"_Current_Image.JPG")
#  
#  image_write(image_file, path = temp_image_file_path, format = "jpeg")
  
#  return(temp_image_file_path)
#}
