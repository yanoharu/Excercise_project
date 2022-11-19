main<-function(){
  my_folder <- "inequality"
  file_name <- "Gini.xlsx"
  raw_data <- read_raw(my_folder,file_name)
  tidy_data <- raw_data %>% 
    prep_long_table%>%
    prep_rename_country_value
  
  basics$save_interim(tidy_data,my_folder,extension = "tidy")
}



read_raw<- function(my_folder, file_name){
  file_path <- here::here("02_raw",my_folder,"data",file_name)
  data_output <- readxl::read_xlsx(file_path,skip=1)
  return(data_output)
}


prep_long_table<-function(data_input){
  data_output <- data_input%>%
    dplyr::select(-country)%>%
    dplyr::na_if("missing")
  data_output <- tibble::tibble(country=as.character(data_output %>% names()),
                                year=as.numeric(data_output %>% dplyr::slice(1)),
                                gini=as.numeric(data_output %>% dplyr::slice(2)))
  
  return(data_output)
}

prep_rename_country_value<-function(data_input){
  new_value<-data_input%>%
    dplyr::select(country)%>%
    dplyr::transmute(country=dplyr::if_else(stringr::str_detect(data_input$country,"USA"),"United States","Japan"))
  data_output<-data_input%>%
    dplyr::select(-country)%>%
    dplyr::bind_cols(new_value)
  
  return(data_output)
}

box::use(`functions`/basics)
main()
