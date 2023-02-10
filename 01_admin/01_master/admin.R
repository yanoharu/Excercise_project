main <- function(){
  preamble()
  build()
  analyze()
  report()
  postamble()
}

preamble <- function(){
  lets('set', 'preamble')
}


build <- function(){
   lets('build','inequality_tidy')
   lets('build','gdp_tidy')
   lets('build','inequality_ready')
   lets('build','gdp_ready')
   lets('build','master')
}


analyze <- function(){
  lets('analyze','gdp_tidy_check')
  lets('analyze','gdp_ready_check')
  lets('analyze','inequality_tidy_check')
  lets('analyze','inequality_ready_check')
  lets('analyze','kuznets_curve_study')
}


report <- function(){
  lets('report', 'checks')
}


postamble <- function(){
  lets('set', 'postamble')
}

lets <- function(verb_name, object_name){
  if (verb_name == 'set' && object_name == 'preamble'){
    source(here::here('01_admin', '02_preamble', 'R', 'admin.R'))
  }
  
  else if (verb_name == 'build'){
    source(here::here('03_build', object_name, 'code', 'build.R'))
  }
  
  else if (verb_name == 'analyze'){
    source(here::here('04_analyze', object_name, 'code', 'analyze.R'))
  }
  
  else if (verb_name == 'report'){
    rmarkdown::render(here::here('05_report', 
                                 object_name, 'text', 'report.Rmd'),
                      output_dir = here::here('05_report', 
                                              object_name, 'output')) 
  }
  
  else if (verb_name == 'set' && object_name == 'postamble'){
    source(here::here('01_admin', '03_postamble', 'admin.R'))
  }
  
}

main()
