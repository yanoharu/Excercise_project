main <- function(){
  inequality_data <- basics$read_interim("inequality", extension = "ready")
  gdp_data <- basics$read_interim("gdp", extension = "ready")
  
  master_data <- prep_merge(
    inequality_data,
    gdp_data) 
  
  basics$save_interim(master_data, "master")
}

prep_merge <- function(inequality_data, gdp_data){
  data_output <- inequality_data %>%
    dplyr::left_join(gdp_data,by=c("year","country"))
  
  return(data_output)
}

box::use(`functions`/basics)
main()
