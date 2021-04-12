COVID_Country_Cumulative_Graph <- function(country){
  
  #'@title COVID_Country_Cumulative_Graph: cumulative COVID-19 cases and deaths.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots cumulative COVID-19 cases and deaths in a specific country.
  #' 
  #'@usage COVID_Country_Cumulative_Graph(country)
  #'
  #'@param country character. Name of country
  #'
  #'@details This function returns 2 graphs, each containing either cumulative COVID-19 cases 
  #'or cumulative COVID-19 deaths. Use Country_List() for a list of country names that can 
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
  
  # Calculate cases per day by subtracting current count in each row by total cases up until that point.
  Covid$DailyCases <- ave(Covid$cases,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
  
  # Calculate deaths per day by subtracting current count in each row by total deaths up until that point.
  Covid$DailyDeaths <- ave(Covid$deaths,Covid$`Country/Region`,  FUN=function(x) c(0, diff(x)))
  
  # Set Theme.
  ggthemr("flat")
  
  # Cumulative Cases Graph.
  cases <- Covid %>% 
    ggplot(aes(x = date, y = cases, color = `Country/Region`, group = `Country/Region`, fill = `Country/Region`)) +
    geom_area() +
    labs(x = paste0(head(format(Covid$date, "%B %d %y"), n = 1)," to Present"),
         y = "Cumulative Cases",
         title = paste0("Cumulative Coronavirus Cases In ", country),
         caption = c(paste0("Data source: Government of ", country), paste0("Today's Date: ", tail(format(Covid$date, "%B %d %y"), n = 1)))) +
    scale_colour_ggthemr_d() +
    scale_x_date(date_breaks = '1 month', date_labels = '%B') +
    theme(axis.text.x = element_text(face = "bold", size = 9, angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  deaths <- Covid %>% 
    ggplot(aes(x = date, y = deaths, color = `Country/Region`, group = `Country/Region`, fill = `Country/Region`)) +
    geom_area() +
    labs(x = paste0(head(format(Covid$date, "%B %d %y"), n = 1)," to Present"),
         y = "Cumulative Cases",
         title = paste0("Cumulative Coronavirus Deaths In ", country),
         caption = c(paste0("Data source: Government of ", country), paste0("Today's Date: ", tail(format(Covid$date, "%B %d %y"), n = 1)))) +
    scale_colour_ggthemr_d() +
    scale_x_date(date_breaks = '1 month', date_labels = '%B') +
    theme(axis.text.x = element_text(face = "bold", size = 9, angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  output <- list(cases, deaths)
  
  return(output)
  
}