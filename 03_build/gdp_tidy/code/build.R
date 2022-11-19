main <- function(){
  my_folder <- "gdp"
  country_list <- c("United States","Japan")
  raw_data <- read_raw(my_folder,country_list)
  tidy_data <- raw_data
  
  basics$save_interim(tidy_data,my_folder,extension="tidy")
}

read_raw <- function(my_folder, country_list){
  data_list <- country_list %>% 
    purrr::map(function(country) here::here("02_raw",my_folder,"data",paste0(country, ".csv"))) %>%
    purrr::map(readr::read_csv)
  
  data_output <- data_list %>%
    dplyr::bind_rows(.id = "country_number") %>%
    dplyr::mutate(country = country_list[as.numeric(country_number)])%>%
    dplyr::select(-country_number)
  
  return(data_output)
}

box::use(`functions`/basics)
main()
