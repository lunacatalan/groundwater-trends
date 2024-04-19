# Function to calculate rolling linear regression using index values
#' Title
#'
#' @param data the dataframe to calculate the rolling index
#' @param window_size the number of years in the window
#'
#' @return
#' @export
#'
#' @examples

rolling_lm_index <- function(data, window_size) {
  
  data <- data %>% 
    mutate(index = row_number())
  
  coef_df <- data.frame()
  
  for(position in seq(1, nrow(data), by = window_size)) {
    
    start_position <- position
    
    end_position <- start_position + window_size
    
    window_data <- data %>%
      filter(index >= start_position & index <= end_position)
    
    # Check if there are any NA values in DepthToWater_m
     if (any(is.na(window_data$DepthToWater_m))) {
       next
     }

    lm_results <- lm(DepthToWater_m ~ index, 
                     data = window_data)
    
    slope <- summary(lm_results)$coefficients[2] # get the slope
    
    df <- data.frame(index = position,
                     start_date = data$year[start_position], 
                     end_date = data$year[end_position], 
                     slope = slope) 
    
    coef_df <- rbind(coef_df, df)
  }
  
  return(coef_df)
}
