main <- function(){
  my_folder <- "gdp"
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data%>%
    rename_country()%>%
    gen_gdp_per_capita()
    
  basics$save_interim(ready_data, my_folder, extension = "ready")
}

rename_country <- function(data_input){
  
  data_output <- data_input %>%
    dplyr::mutate(country = dplyr::if_else(country == "United States", "USA", "JPN"))
  
  return(data_output)
}


gen_gdp_per_capita <- function(data_input){
  
  data_output <- data_input%>%
    dplyr::mutate(gdp_per_capita = GDP/population) 
  return(data_output)
}

box::use(`functions`/basics)
main()
