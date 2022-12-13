main <- function(){
  my_folder <- "gdp_ready"
  my_data <- basics$read_interim(my_folder)
  
  var_quantitative <- c("GDP",
                        "population",
                        "gdp_per_capita")
  
  checks$check_quantitative(my_data,
                            var_quantitative,
                            my_folder)
}

box::use(`functions`/basics)
box::use(`functions`/checks)
main()
