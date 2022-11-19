main <- function(){
  my_folder <- "inequality"
  tidy_data <- basics$read_interim(my_folder, extension = "tidy")
  
  country_list <- c("United States","Japan")
  period <- 5
  ready_data <- tidy_data %>% 
    prep_fill_na() %>%
    prep_years(period,country_list)
  
  basics$save_interim(ready_data, my_folder, extension = "ready")  
}

prep_fill_na <- function(data_input){
  data_output <- data_input%>%
    transform(gini=approx(seq_along(data_input$gini)[!is.na(data_input$gini)],
                          gini[!is.na(data_input$gini)],seq_along(data_input$gini))$y)
  
  return(data_output)
}


prep_years <- function(data_input,period,country_list){
  
  prep_select_year<-function(data_input){
    year_list <- c()
    for(i in seq(dplyr::first(data_input$year),dplyr::last(data_input$year),period)){
      year_list <- append(year_list,i)
    }
    data_output <- data_input %>% dplyr::filter(year %in% year_list)
    return(data_output)
  }
  
  data_list<-list()
  for (country_name in country_list) {
    data_list<-data_input%>%
      dplyr::filter(data_input$country==country_name)%>%
      prep_select_year()%>%
      list()%>%
      append(data_list)
  }
  
  data_output<-data_list%>%
    dplyr::bind_rows()
  return(data_output)
}



box::use(`functions`/basics)
main()
