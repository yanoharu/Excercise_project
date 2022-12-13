main <- function(){
  my_folder <- "inequality_tidy"
  my_data <- basics$read_interim(my_folder)
  
  var_vector <- c("country")
  
  checks$check_categorical(my_data,
                           var_vector,
                           my_folder)
  
  var_quantitative <- c("year",
                        "gini")
  
  checks$check_quantitative(my_data,
                            var_quantitative,
                            my_folder)
}

box::use(`functions`/basics)
box::use(`functions`/checks)
main()