
# function to convert heterogeneous measurement units to a common scale. 
# used for experiment 1 conversion of height and lateness scales 
expt1_unit_to_common_scale <- function(unit){
  
  # 1. handle 'late' -- `norm_unit` is minutes relative to 9am
  if (grepl(":", unit)){
    
    the_time <- setNames(strsplit(unit, split=":")[[1]], c("hr","min"))
    
    # assert that the hour is either 8 or 9
    stopifnot(the_time["hr"] %in% c("8","9"))
    
    if (the_time["hr"]=="8"){
      return(as.numeric(the_time["min"])-60) 
    } else {
      return(as.numeric(the_time["min"]))
    }
  }
  
  # 2. handle 'tall' -- `norm_unit` is (height-70)*3 (note 70in is mean height)
  if (grepl("^\\d{2}$", unit)){
    
    the_inches <- as.numeric(unit)
    
    # assert that height is between 63in and 82in 
    stopifnot(the_inches %in% 63:82)
    
    return((the_inches-70)*3)
  }
  
  # if we get here, there's a problem
  message("couldn't convert `", unit, "` into a common scale point!")
}



# convert a string of integers (unix timestamp) to date or time string or both
parse_unix_timestamp <- Vectorize(function(digits, epoch="1970-01-01"){
  digits <- as.numeric(digits) / 1e3
  return(as.character(as.POSIXct(digits, origin=epoch)))
}, USE.NAMES=FALSE)


