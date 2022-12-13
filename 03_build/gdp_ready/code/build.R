main <- function(){
  my_folder <- "gdp"
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data%>%
    gen_gdp_per_capita
    
  basics$save_interim(ready_data, my_folder, extension = "ready")
}


gen_gdp_per_capita <- function(data_input){
  data_output <- data_input%>%
    dplyr::mutate(gdp_per_capita = data_input$GDP/data_input$population) 
  return(data_output)
}

box::use(`functions`/basics)
main()
