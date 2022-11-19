main <- function(){
  preamble()
  build()
  analyze()
  report()
  postamble()
}

source("01_admin/initialize/admin.R")
       
       here::here('04_analyze', object_name, 'code', 'analyze.R'))

preamble <- function(){
  lets('set', 'preamble')
}


build <- function(){

}


analyze <- function(){
  lets('analyze', 'initial')
}


report <- function(){
  lets('report', 'initial')
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