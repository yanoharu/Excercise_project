main <- function(){
  my_folder <- "inequality"
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  
  years_for_US = seq(1974, 2019, 1)
  years_for_JP = seq(1990, 2017, 1)
  
  
  ready_data <- tidy_data %>% 
    complete_linearly(years_for_US=years_for_US,
                      years_for_JP=years_for_JP) %>% 
    prep_fill_na() %>%
    prep_select_years()
  
  basics$save_interim(ready_data, my_folder, extension = "ready")  
}


complete_linearly <- function(data_input, years_for_US, years_for_JP){
  
  year_columns_US <- tibble::tibble(year = years_for_US)
  year_columns_JP <- tibble::tibble(year = years_for_JP)
  
  data_output_US <- select_country(data_input, 
                                  year_column = year_columns_US,
                                  country_name = "USA")
  data_output_JP <- select_country(data_input, 
                                  year_column = year_columns_JP,
                                  country_name = "JPN")
  
  data_output <- data_output_US %>%
    dplyr::bind_rows(data_output_JP)
  
  
  return(data_output)
}

select_country <- function(data_input, year_column, country_name){
  
  data_table <- data_input %>%
    dplyr::filter(country == country_name)
  
  data_table$year <- as.numeric(data_table$year)
  
  data_output <- year_column %>%  
    dplyr::left_join(data_table,by = c("year"))
  
  data_output <- data_output %>%
    dplyr::mutate(country = country_name) %>% 
    dplyr::arrange(year)
  
  return(data_output)
  
}



prep_fill_na <- function(data_input){
  
  data_table_US <- data_input %>%
    dplyr::filter(country == "USA")
  data_table_JP <- data_input %>%
    dplyr::filter(country == "JPN")
  
  data_output_US <- data_table_US %>%
    dplyr::mutate(gini = approx(seq_along(data_table_US$gini)[!is.na(data_table_US$gini)],
                              gini[!is.na(data_table_US$gini)],seq_along(data_table_US$gini))$y)
  data_output_JP <- data_table_JP %>%
    dplyr::mutate(gini = approx(seq_along(data_table_JP$gini)[!is.na(data_table_JP$gini)],
                              gini[!is.na(data_table_JP$gini)],seq_along(data_table_JP$gini))$y)
  data_output <- data_output_US %>%
    dplyr::bind_rows(data_output_JP)
  
  return(data_output)
}


prep_select_years <- function(data_input){
  data_output <- data_input %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(endsWith(year, "5")|endsWith(year, "0")) %>%
    dplyr::mutate(year = as.numeric(year))
  
  return(data_output)
}


box::use(`functions`/basics)
main()
