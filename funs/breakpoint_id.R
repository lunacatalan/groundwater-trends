#' Creates two lists that identifies stations with breakpoints and without breakpoints. 
#'
#' @param df input dataframe with more than 10 observations for each well
#'
#' @return a list of wells with no breakpoints identified, and a dataframe of wells with breakpoints, and the year range of the breakpoint
#' @export
#'
#' @examples
#' 
breakpoint_id <- function(df) {
  
  # create list of unique IDs
  id_list <- df$StnID %>% 
    unique()
  
  
  no_bp <- data.frame()
  bp <- data.frame()
  
  name <- "StnID"
  
  
  for (id in seq(id_list)) {
    
    # filter for only a single station
    sub <- df %>% 
      filter(StnID == id_list[id])
    
    ID = id_list[id]
    
    # run the breakpoint function on it
    bp_df <- breakpoint_fun(sub, window_size = 5) %>% 
      mutate(StnID = ID)
    
    ifelse(nrow(bp_df) < 1,
           no_bp <- rbind(no_bp, ID),
           bp <- rbind(bp, bp_df)) # save the whole dataframe 
    
  }
  
  colnames(no_bp) <- name
  
  # return a list of dataframe
  return(list(bp = bp, non = no_bp))
  
}