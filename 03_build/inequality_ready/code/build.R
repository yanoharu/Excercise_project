main <- function(){
  my_folder <- "inequality"
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  gen_years_for_US=seq(1974,2019,1)
  gen_years_for_JP=seq(1990,2017,1)
  
  ready_data <- tidy_data %>% 
    complete_linearly(years_for_US=gen_years_for_US, years_for_JP=gen_years_for_JP)%>%
    prep_fill_na() %>%
    prep_select_years()
  
  basics$save_interim(ready_data, my_folder, extension = "ready")  
}


complete_linearly <- function(data_input, years_for_US, years_for_JP){
  year_columns_US<- tibble::tibble(year=years_for_US)
  year_columns_JP<- tibble::tibble(year=years_for_JP)
  
  data_table_US<-data_input%>%
    dplyr::filter(country=="United States")
  data_output_US <- year_columns_US%>%  
    dplyr::left_join(data_table_US,by=c("year"))
  data_output_US<- data_output_US%>%
    dplyr::mutate(country=dplyr::if_else(is.na(country),"United States",country))%>%
    dplyr::arrange(year)
  
  data_table_JP<-data_input%>%
    dplyr::filter(country=="Japan")
  data_output_JP <- year_columns_JP%>%  
    dplyr::left_join(data_table_JP,by=c("year"))
  data_output_JP<- data_output_JP%>%
    dplyr::mutate(country=dplyr::if_else(is.na(country),"Japan",country))%>%
    dplyr::arrange(year)
  
  data_output<-data_output_US%>%
    dplyr::bind_rows(data_output_JP)
    
  
  return(data_output)
  }


prep_fill_na <- function(data_input){
  data_table_US<-data_input%>%
    dplyr::filter(country=="United States")
  data_table_JP<-data_input%>%
    dplyr::filter(country=="Japan")
  
  data_output_US <- data_table_US%>%
    dplyr::mutate(gini=approx(seq_along(data_table_US$gini)[!is.na(data_table_US$gini)],
                              gini[!is.na(data_table_US$gini)],seq_along(data_table_US$gini))$y)
  data_output_JP <- data_table_JP%>%
    dplyr::mutate(gini=approx(seq_along(data_table_JP$gini)[!is.na(data_table_JP$gini)],
                              gini[!is.na(data_table_JP$gini)],seq_along(data_table_JP$gini))$y)
  data_output<-data_output_US%>%
    dplyr::bind_rows(data_output_JP)
  
  return(data_output)
}


prep_select_years <- function(data_input){
  data_output<- data_input %>%
    dplyr::mutate(year=as.character(year))%>%
    dplyr::filter(endsWith(year,"5")|endsWith(year,"0"))%>%
    dplyr::mutate(year=as.numeric(year))
  
  return(data_output)
}


box::use(`functions`/basics)
main()
