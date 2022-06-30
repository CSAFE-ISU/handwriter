server <- function(input, output, session) {
  
  #Tooltips
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  
  #========================================================
  #=================== GLOBAL SET UP ======================
  #========================================================
  
  #Disable buttons
  shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop"); shinyjs::disable("save_mask"); shinyjs::disable("reset_mask"); shinyjs::disable("undo_mask")
  
  global <- reactiveValues(datapath = getwd())
  
  #Create Reactive Values
  values <- reactiveValues()
  
  #Read in sample image & Set up basic values
  image <- image_read("images/samplewriting.png")
  values$upload_path <- "images/samplewriting.png"
  values$current_path <- "images/samplewriting.png"
  values$image_name <- 'images/samplewriting.png'
  
  info <- image_info(image)
  mask_list_df <- data.frame(matrix(ncol=6, nrow=0))
  colnames(mask_list_df) <- c('xmin', 'xmax', 'ymin', 'ymax', 'xrange', 'yrange')
  
  values$image <- image
  values$uploaded_image <- image
  values$info <- info
  
  values$crop_list <- list(image)
  values$mask_list_df <- mask_list_df
  
  values$dimensions <- paste0(info$width, 'x', info$height)
  
  
  #========================================================
  #================ SOURCE SERVER CODE ====================
  #========================================================
  
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  source(file.path("server", "preProcess.R"), local = TRUE)$value
  source(file.path("server", "plot.R"), local = TRUE)$value
  source(file.path("server", "extractFeatures.R"), local = TRUE)$value
  source(file.path("server", "clustering.R"), local = TRUE)$value
  source(file.path("server", "triangleDecomposition.R"), local = TRUE)$value
  source(file.path("server", "slr.R"), local = TRUE)$value
  
}
