main <- function(){
  my_folder <- "inequality"
  file_name <- "Gini.xlsx"
  
  raw_data <- read_raw(my_folder,file_name)
  
  tidy_data <- prep_long_table(raw_data)
    
  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}



read_raw <- function(my_folder, file_name){
  
  file_path <- here::here("02_raw", my_folder, "data", file_name)
  data_output <- readxl::read_xlsx(file_path, skip = 1, col_names = FALSE)
  
  return(data_output)
}


prep_long_table <- function(data_input){
  
  data_na_replaced <- data_input %>%
    dplyr::na_if("missing") %>%
    t() %>%
    tidyr::as_tibble()
  
  colnames(data_na_replaced) <- c("country", "year", "gini")
  
  long_data <- data_na_replaced %>%
    dplyr::relocate(country, .after = gini) %>%
    dplyr::filter(country %in% c("USA", "JPN"))
  
  data_output <- long_data %>%
    dplyr::mutate(gini = round(as.numeric(gini), digits = 4))
    
  return(data_output)
}


box::use(`functions`/basics)
main()
