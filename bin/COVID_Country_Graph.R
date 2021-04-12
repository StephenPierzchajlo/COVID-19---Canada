COVID_Country_Graph <- function(country){
  
  #'@title COVID_Country_Graph: Plot total COVID-19 cases and deaths.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots the total COVID-19 cases and deaths in a specific country.
  #' 
  #'@usage COVID_Country_Graph(country)
  #'
  #'@param country character. Name of country
  #'
  #'@details This function returns a single graph containing total COVID-19 cases 
  #'and deaths per day. Use Country_List() for a list of country names that can 
  #'be passed to this function.
  
  # Make package list.
  packages <- c('tidyverse', 'plyr', 'ggthemr')    
  
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
  
  # Set Theme.
  ggthemr("flat")
  
  # average data in dataframe for graphing.
  a <- Covid %>%
    ddply(c("`Country/Region`"), summarise,
          cases = sum(DailyCases),
          deaths = sum(DailyDeaths)) %>%
    gather(Category, Number, cases:deaths) %>%
    
    # Graph total cases and deaths per province, tilt sideways, stack cases/deaths, display cases count on x-axis/death count above each bar.
    ggplot(aes(x = `Country/Region`, y = Number, fill = factor(Category, levels = c("deaths", "cases")))) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_colour_ggthemr_d() +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  +
    labs(x = "Country",
         y = "COVID-19 Cases And Deaths",
         title = paste0("Total COVID-19 Cases In ", country),
         caption = paste0("Data source: Government of ", country),
         fill = "Case Type") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = Number), position = position_dodge(width=0.9), vjust = -.2)
  
  return(a)
  
}