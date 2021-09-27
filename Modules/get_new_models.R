# Create a list of models created from the app and stored as .rds objects in a 
# given location

get_new_models <- function(models_path){
  
  models <- list.files(path = models_path)
  
  all_models <- data.frame(species = as.character(),
                           suit_factor = as.character(),
                           model = as.character())
  
  for(model in models){
    
    model_name <- as.character(model)
    
    suit_factor <- stringr::str_extract(model_name, "(?<=\\_)\\w*")
    
    species <- stringr::str_extract(model_name, "\\w*\\s\\w*(?=\\_)")
    
    model <- readRDS(file = file.path(models_path, model))
    
    model_df <- data.frame(species = species,
                           suit_factor = suit_factor,
                           model = I(list(model)))
    
    all_models <- rbind(all_models, model_df)
    
  }
  
  return(all_models)
  
}
