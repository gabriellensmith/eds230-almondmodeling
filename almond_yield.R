
almond_yield <- function(climate_data){
  
  temp_data <- climate_data |> 
    filter(month == 2) |> 
    select(year, tmin_c) |> 
    group_by(year) |> 
    summarize(min_temp = min(tmin_c))
  
  precip_data <- climate_data |> 
    filter(month == 1) |> 
    select(year, precip) |> 
    group_by(year) |> 
    summarize(total_p = sum(precip, na.rm = TRUE))
  
  function_data <- merge(temp_data, precip_data, by = 'year', all.x=TRUE)
  
  yr <- c()
  yield <- c()
  
for (i in 1:nrow(function_data)){
  min_temp <- function_data$min_temp[i]
  total_p <- function_data$total_p[i]
  y = -0.015*min_temp - 0.0046*(min_temp)**2 - 0.07*total_p +
    0.0043*(total_p**2) + 0.28
  yr <- c(yr, function_data$year[i])
  yield <- c(yield, round(y, 4))
}
  
  final_df <- data.frame(year = yr, almond_yield = yield)
  return(final_df)
}

