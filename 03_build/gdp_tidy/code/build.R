main <- function(){
  my_folder <- "gdp"
  country_list <- c("United States", "Japan")
  
  tidy_data <- country_list %>%
    purrr::map(read_country, my_folder) %>%
    dplyr::bind_rows()
    
  basics$save_interim(tidy_data,my_folder,extension = "tidy")
}


read_country <- function(country_name, my_folder){
  
    file_name <- paste0(country_name, ".csv")
    output_data <- read.csv(here::here("02_raw", my_folder, "data", file_name)) %>%
      dplyr::mutate(country = country_name, .before = year)
    
    return(output_data)
}
      

box::use(`functions`/basics)
main()
