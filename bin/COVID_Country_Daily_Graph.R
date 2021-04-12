COVID_Country_Daily_Graph <- function(country){
  
  #'@title COVID_Country_Daily_Graph: plot daily COVID-19 cases and deaths.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots daily COVID-19 cases and deaths in a specific country.
  #' 
  #'@usage COVID_Country_Daily_Graph(country)
  #'
  #'@param country character. Name of country
  #'
  #'@details This function returns 2 graphs, each containing either COVID-19 cases per day 
  #'or COVID-19 deaths per day. Use Country_List() for a list of country names that can 
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
  
  # Filter canadian provincial data to only include data starting from March 1st.
  Covid <- COVID_Global_Long_Date[352:nrow(COVID_Global_Long_Date), ]
  
  # Subset Canadian provinces from total cumulative global cases data.
  Covid <- subset(COVID_Global_Long_Date, `Country/Region` == country)
  
  # Convert date column to date format for cases dataframe.
  Covid$date <- as.Date(Covid$date, format = "%m/%d/%Y")
  
  ### Some countries have provinces, others don't. The strategy I am using to make the dataframes for entire country
  ### won't work in both instances. So we need to apply averaging differently.
  
  # If Province/State is empty:
  if(is.na(Covid$`Province/State`) == TRUE){
    
    # Calculate cases per day by subtracting current count in each row by total cases up until that point.
    Covid$DailyCases <- ave(Covid$cases,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
    
    # Calculate deaths per day by subtracting current count in each row by total deaths up until that point.
    Covid$DailyDeaths <- ave(Covid$deaths,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
    
    Covid <- Covid %>%
      ddply(c("`date`"), summarise,
            cases = sum(DailyCases),
            deaths = sum(DailyDeaths))
    
  }
  
  # If Province/State has values:
  else{
    
    # Calculate cases per day by subtracting current count in each row by total cases up until that point.
    Covid$DailyCases <- ave(Covid$cases,Covid$`Province/State`,  FUN=function(x) c(0, diff(x)))
    
    # Calculate deaths per day by subtracting current count in each row by total deaths up until that point.
    Covid$DailyDeaths <- ave(Covid$deaths,Covid$`Province/State`,  FUN=function(x) c(0, diff(x)))
    
    Covid <- Covid %>%
      ddply(c("`date`"), summarise,
            cases = sum(DailyCases),
            deaths = sum(DailyDeaths))
    
  }
  
  # Set Theme.
  ggthemr("flat")
  
  # Daily Cases Graph.
  cases <- ggplot(Covid, aes(x = date, y = cases)) +
    geom_bar(stat = "identity") +
    labs(x = paste0(head(format(Covid$date, "%B %d %y"), n = 1)," to Present"),
         y = "Cases Per Day",
         title = paste0("Daily Coronavirus Cases In ", country),
         caption = c(paste0("Data source: Government of ", country), paste0("Today's Date: ", tail(format(Covid$date, "%B %d %y"), n = 1)))) +
    theme(plot.caption = element_text(hjust=c(0, 1))) +
    scale_colour_ggthemr_d() +
    scale_x_date(date_breaks = '1 month', date_labels = '%B') +
    theme(axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  # Daliy Deaths Graph.
  deaths <- ggplot(Covid, aes(x = date, y = deaths)) +
    geom_bar(stat = "identity") +
    labs(x = paste0(head(format(Covid$date, "%B %d %y"), n = 1)," to Present"),
         y = "Deaths Per Day",
         title = paste0("Daily Coronavirus Deaths In ", country),
         caption = c(paste0("Data source: Government of ", country), paste0("Today's Date: ", tail(format(Covid$date, "%B %d %y"), n = 1)))) +
    theme(plot.caption = element_text(hjust=c(0, 1))) +
    scale_colour_ggthemr_d() +
    scale_x_date(date_breaks = '1 month', date_labels = '%B') +
    theme(axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  outputs <- list(cases, deaths)
  
  return(outputs)
  
}