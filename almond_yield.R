
#define a function that takes in climate data and returns the almond yield anomaly for each year
almond_yield <- function(climate_data){
  
  #filter climate data for minimum temperature for February of each year
  temp_data <- climate_data |> 
    filter(month == 2) |> 
    select(year, tmin_c) |> 
    group_by(year) |> 
    summarize(min_temp = min(tmin_c))
  
  #filter climate data for total precipitation for January of each year
  precip_data <- climate_data |> 
    filter(month == 1) |> 
    select(year, precip) |> 
    group_by(year) |> 
    summarize(total_p = sum(precip, na.rm = TRUE))
  
  #merge temperature and precipitation data
  function_data <- merge(temp_data, precip_data, by = 'year', all.x=TRUE)
  
  #create empty vectors to store year and yield data
  yr <- c()
  yield <- c()
  
#calculate the yield anomaly for each year using temperature and precipitation data
for (i in 1:nrow(function_data)){
  min_temp <- function_data$min_temp[i]
  total_p <- function_data$total_p[i]
  y = -0.015*min_temp - 0.0046*(min_temp)**2 - 0.07*total_p +
    0.0043*(total_p**2) + 0.28
  
  #add year and yield data to the vectors
  yr <- c(yr, function_data$year[i])
  yield <- c(yield, round(y, 4))
}
  
  #combine year and yield data into a final dataframe
  final_df <- data.frame(year = yr, almond_yield = yield)
  
  #return the final data frame
  return(final_df)
}

