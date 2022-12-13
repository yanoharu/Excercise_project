main <- function(){
  my_folder <- "kuznets_curve_study"
  data_master <- basics$read_interim("master")
  
  main_varnames <- c('gdp_per_capita' = 'GDP per Capita',
                     'I(gdp_per_capita^2)'='(GDP per Capita)^2',
                     '(Intercept)' = 'Constant')
  
  data_master %>%
    run_regressions() %>% 
    format_and_save_table(
      my_file = "initial_reg",
      my_title = "Initial regressions",
      my_varnames = main_varnames,
      my_folder = my_folder)
  
  data_master %>%
    lay_kuznets_curve(
      x_var = gdp_per_capita,
      y_var = gini,
      group_var = country) %>%
    basics$save_my_plot(var_name = "correlation_gini_GDPperCapita", folder = my_folder)
}



run_regressions <- function(data_input){
  estimates_list <- list(
    "OLS" = estimatr::lm_robust(
      gini ~ gdp_per_capita+I(gdp_per_capita^2),
      clusters = country, se_type = "stata",
      data = data_input
    ),
    "FE" = estimatr::lm_robust(
      gini ~ gdp_per_capita+I(gdp_per_capita^2),
      fixed_effects = ~ country, clusters = country, se_type = "stata",
      data = data_input
    )
  )
  return(estimates_list)
}

format_and_save_table <- function(estimates_lists, my_file_name, 
                                  my_title, my_varnames, my_folder){
  my_file_tex0 <- paste0(my_file_name, ".tex")  
  my_file_html0 <- paste0(my_file_name, ".html")  
  my_file_tex <- here::here("04_analyze", my_folder, "table", my_file_tex0)
  my_file_html <- here::here("04_analyze", my_folder, "table", my_file_html0)
  
  my_content <- "^R2$|Std.Errors"
  
  my_fmt <- "%.2f"
  my_rows <- tibble::tribble(~term,  ~'OLS',  ~'FE',
                             'Clustering', 'Y', 'Y')
  attr(my_rows, 'position') <- 5
  
  table_tex <- modelsummary::msummary(
    estimates_lists, gof_omit = my_content, fmt = my_fmt, title = my_title, 
    coef_map = my_varnames, add_rows = my_rows,
    output = "latex", booktabs = TRUE) %>%
    format_table() %>%
    add_double_lines_latex() 
  writeLines(table_tex, my_file_tex)
  
  table_image <- modelsummary::msummary(
    estimates_lists, gof_omit = my_content, fmt = my_fmt, title = my_title,
    coef_map = my_varnames, add_rows = my_rows,
    output = "html") %>%
    format_table() %>% 
    cat(., file = my_file_html)
}

format_table <- function(table_input){
  table_output <- table_input %>% 
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
    kableExtra::add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1)) %>%
    kableExtra::kable_classic_2(full_width = F) %>% 
    kableExtra::footnote(general = "Heteroskedasticity-robust standard errors clustered at students level are reported in the parenthesis.",
                         threeparttable = T) 
  return(table_output)
}

add_double_lines_latex <- function(table_input){
  table_output <- table_input %>% 
    sub("\\\\toprule", "\\\\midrule\\\\midrule", .) %>%
    sub("\\\\bottomrule", "\\\\midrule\\\\midrule", .)
  return(table_output)
}



lay_kuznets_curve <- function(data_input, x_var, y_var, group_var){
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  group_var <- rlang::enquo(group_var)
  
  require(ggplot2)
  
  plot_output <- ggplot(
    data = data_input,
    mapping = aes(x = !!x_var,
                  y = !!y_var,
                  group = !!group_var,
                  color = !!group_var)) +
    geom_point(mapping = aes(color = country, 
                             shape = country),
               size = 4) +
    geom_line(mapping = aes(color = country,
                            group = country),
              size = 1)+
    scale_color_manual(values = c("United States"="orange",
                                  "Japan"="dark blue"))+
    labs(title = "Gini coefficient and GDP per capita of US and Japan",
         #subtitle = "Based on Peanuts Data",
         x = "GDP per Capita",
         y = "Gini Coefficient",
         caption = "Source  Gini:data bank/GDP per Capita: macrotrends")+
    theme(legend.position = "left")
  
  return(plot_output)
}

box::use(`functions`/basics)
main()