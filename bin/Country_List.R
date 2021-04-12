Country_List <- function(){
  
  #'@title Country_List: finding countries for COVID-19 plotting in R.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that returns a list of countries
  #' 
  #'@usage Country_List()
  #'
  #'@details This function contains no arguments. The user simply types the function as 
  #'outlined above under Usage, and recieves a list of all countries in the world that can 
  #'be used with other subsequent functions.
  
  # Make package list.
  packages <- c('tidyverse')    
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE)) {
    
    install.packages(packages[!installed_packages], repos = c(CRAN = "https://cran.rstudio.com"))
    
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Total Global Cases Data
  x <-read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  
  # Total global cases data in long format.
  COVID_Global_Long_Date <- gather(x, date, cases, 5:ncol(x), factor_key=TRUE)
  
  # compute all unique countries and return a list.
  return(unique(COVID_Global_Long_Date$`Country/Region`))
  
}