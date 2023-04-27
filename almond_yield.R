
# Function that calculates almond yield anomaly based on climate data
#
# Args:
#   climate_data: A data frame containing climate data with columns 'year', 'month', 'tmin_c', and 'precip'
#   min_temp_co, min_temp_co_2, total_p_co, total_p_co_2, yint: Coefficients for the almond yield anomaly calculation
#
# Returns:
#   A data frame containing year and almond_yield columns

almond_yield <- function(climate_data, min_temp_co = -.015, min_temp_co_2 = 0.0046, total_p_co = 0.07,total_p_co_2 = 0.0043, yint = 0.28){
  
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
  y = min_temp_co*min_temp - min_temp_co_2*(min_temp)**2 - total_p_co*total_p +
    total_p_co_2*(total_p**2) + yint
  
  #add year and yield data to the vectors
  yr <- c(yr, function_data$year[i])
  yield <- c(yield, round(y, 4))
}
  
  #combine year and yield data into a final dataframe
  final_df <- data.frame(year = yr, almond_yield = yield)
  
  #return the final data frame
  return(final_df)
}

# Function that calculates profit based on almond yield anomaly
#
# Args:
#   almond_yield: A numeric value representing the almond yield anomaly
#   profit_anom: A numeric value representing the profit anomaly, default is 100
#   baseline_profit: A numeric value representing the baseline profit, default is 1000
#
# Returns:
#   A numeric value representing the adjusted profit
profit_function <- function(almond_yield, profit_anom = 100, baseline_profit = 1000) {
  adjusted_profit <- almond_yield * profit_anom + baseline_profit
  return(adjusted_profit)
}


# Wrapper function that calculates profit based on almond yield anomaly and climate data
#
# Args:
#   climate_data: A data frame containing climate data with columns 'year', 'month', 'tmin_c', and 'precip'
#   baseline_profit: A numeric value representing the baseline profit, default is 1000
#   profit_anom: A numeric value representing the profit anomaly, default is 100
#   min_temp_co, min_temp_co_2, total_p_co, total_p_co_2, yint: Coefficients for the almond yield anomaly calculation
#
# Returns:
#   A list with two elements:
#     - final_df: A data frame containing year, almond_yield, and profit columns
#     - mean: A numeric value representing the mean profit across all years
profit_wrapper <- function(climate_data, baseline_profit = 1000, profit_anom = 100, min_temp_co = -0.015, min_temp_co_2 = 0.0046, total_p_co = 0.07, total_p_co_2 = 0.0043, yint = 0.28) {
  
  # Call almond_yield function with custom arguments
  almond_yield_df <- almond_yield(climate_data, min_temp_co, min_temp_co_2, total_p_co, total_p_co_2, yint)
  
  # Calculate profit for each year
  profit <- sapply(almond_yield_df$almond_yield, profit_function, baseline_profit, profit_anom)
  
  # Combine almond_yield_df and profit data into a final dataframe
  final_df <- cbind(almond_yield_df, profit)
  
  # Return the final data frame
  return(list(final_df = final_df[,c("year", "almond_yield", "profit")], mean = mean(final_df$profit)))
  
}






