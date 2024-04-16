#' Title
#'
#' @param data 
#' @param window_size 
#'
#' @return A dataframe of the identified breakpoints
#' @export
#'
#' @examples

breakpoint_fun <- rolling_lm_index <- function(data, window_size) {
  
  data <- data %>% 
    mutate(index = row_number())
  
  coef_df <- data.frame()
  
  for(position in seq(1, nrow(data), by = window_size)) {
    
    start_position <- position
    
    end_position <- start_position + window_size
    
    window_data <- data %>%
      filter(index >= start_position & index <= end_position)
    
    # Check if there are any NA values in DepthToWater_m
    # if (any(is.na(window_data$DepthToWater_m))) {
    #   next
    # }
    
    lm_results <- lm(DepthToWater_m ~ index, 
                     data = window_data)
    
    slope <- summary(lm_results)$coefficients[2] # get the slope
    
    df <- data.frame(index = position,
                     start_date = data$year[start_position], 
                     end_date = data$year[end_position], 
                     slope = slope) 
    
    coef_df <- rbind(coef_df, df)
  }
  
  breakpoint_df <- coef_df %>% 
    # calculate the change in slope
    mutate(change = c(NA, diff(slope)),
           
           # calculate the sign change
           sign_change = c(0, diff(sign(slope))),
           
           start_date = as.numeric(start_date),
           end_date = as.numeric(end_date)) %>%  
    
    # identify when the magnitude change is greater than 0.5 and the sign changes from pos to neg
    mutate(flag = ifelse(abs(change) >= 0.5 & sign_change == -2,
                         "breakpoint",
                         NA)) %>% 
    filter(flag == "breakpoint")
  
  return(breakpoint_df)
}
