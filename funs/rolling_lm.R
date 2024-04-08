# Function to calculate rolling linear regression
#' Title
#'
#' @param data the dataframe to calculate the rolling index
#' @param window_size the number of years in the window
#'
#' @return
#' @export
#'
#' @examples

rolling_lm <- function(data, window_size) {
  
  results <- data %>%
    mutate(rolling_index = row_number()) %>%
    group_by(rolling_index) 
  
  coef_df <- data.frame()
  
  for(start_year in seq_along(results$year)) {
    
    start_date <- results$year[start_year]
    end_year <- start_date + window_size
    window_data <- subset %>%
      filter(year >= start_year & year <= end_year)
    lm_results <- lm(DepthToWater_m ~ year, data = window_data)
    slope <- summary(lm_results)$coefficients[2] # get the slope
    df <- data.frame(start_date = start_date, 
                     end_date = end_year, slope) 
    
    coef_df <- rbind(coef_df, df)
    
  }
  
  return(coef_df)
}