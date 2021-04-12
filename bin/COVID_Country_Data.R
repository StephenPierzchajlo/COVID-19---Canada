COVID_Country_Data <- function(country){
  
  #'@title COVID_Country_Data: raw COVID-19 cases and deaths.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that returns raw COVID-19 cases and deaths for a specific country.
  #' 
  #'@usage COVID_Country_Data(country)
  #'
  #'@param country character. Name of country
  #'
  #'@details This function is used to get a numerical output of cases and deaths for a specific 
  #'country. Use Country_List() for a list of country names that can be passed to this function.
  
  # Make package list.
  packages <- c('tidyverse', 'plyr')    
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE)) {
    
    install.packages(packages[!installed_packages], repos = c(CRAN = "https://cran.rstudio.com"))
    
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Adds quotes to country argument which is needed for the subset function.
  country <- rlang::quo_name(rlang::enquo(country))
  
  # Total Global Cases Data
  COVID_Global_Wide<-read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  
  # Total Global Deaths Data
  COVID_Global_Deaths_Wide <-read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
  
  # Total global cases data in long format.
  COVID_Global_Long_Date <- gather(COVID_Global_Wide, date, cases, 5:ncol(COVID_Global_Wide), factor_key=TRUE)
  
  # Total global deaths data in long format
  COVID_Global_Deaths_Long_Date <- gather(COVID_Global_Deaths_Wide, date, cases, 5:ncol(COVID_Global_Deaths_Wide), factor_key=TRUE)
  
  # Combine death count to cases dataframe.
  COVID_Global_Long_Date$deaths <- COVID_Global_Deaths_Long_Date$cases
  
  # Subset Canadian provinces from total cumulative global cases data.
  Covid <- subset(COVID_Global_Long_Date, `Country/Region` == country)
  
  ### Some countries have provinces, others don't. The strategy I am using to make the dataframes for entire country
  ### won't work in both instances. So we need to apply averaging differently.
  
  # If Province/State is empty:
  if(is.na(Covid$`Province/State`) == TRUE){
    
    # Calculate cases per day by subtracting current count in each row by total cases up until that point.
    Covid$DailyCases <- ave(Covid$cases,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
    
    # Calculate deaths per day by subtracting current count in each row by total deaths up until that point.
    Covid$DailyDeaths <- ave(Covid$deaths,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
    
  }
  
  # If Province/State has values:
  else{
    
    # Calculate cases per day by subtracting current count in each row by total cases up until that point.
    Covid$DailyCases <- ave(Covid$cases,Covid$`Province/State`,  FUN=function(x) c(0, diff(x)))
    
    # Calculate deaths per day by subtracting current count in each row by total deaths up until that point.
    Covid$DailyDeaths <- ave(Covid$deaths,Covid$`Province/State`,  FUN=function(x) c(0, diff(x)))
    
  }
  
  # Make total cases/deaths chart.
  Chart <- Covid %>%
    ddply(c("`Country/Region`"), summarise,
          cases = sum(DailyCases),
          deaths = sum(DailyDeaths)) %>%
    gather(Category, Number, cases:deaths)
  
  # Return chart.
  return(Chart)
  
}