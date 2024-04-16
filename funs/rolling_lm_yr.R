# Function to calculate rolling linear regression using year values
#' Title
#'
#' @param data the dataframe to calculate the rolling index
#' @param window_size the number of years in the window
#'
#' @return
#' @export
#'
#' @examples

rolling_lm_yr <- function(data, window_size) {
  
  years <- c(data$year)
  
  coef_df <- data.frame()
  
  for(start_year in seq(1, nrow(data), by = window_size)) {
    
    start_date <- years[start_year]
    
    end_year <- start_date + window_size
    
    window_data <- data %>%
      filter(year >= start_date & year <= end_year)
    
    # Check if there are any NA values in DepthToWater_m
    # if (any(is.na(window_data$DepthToWater_m))) {
    #   next
    # }
    
    lm_results <- lm(DepthToWater_m ~ year, 
                     data = window_data)
    
    slope <- summary(lm_results)$coefficients[2] # get the slope
    
    df <- data.frame(start_date = start_date, 
                     end_date = end_year, 
                     slope = slope) 
    
    coef_df <- rbind(coef_df, df)
  }
  
  return(coef_df)
}