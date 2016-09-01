


get.locations <- function(lengths){
  k <- length(lengths)
  
  cosis <- c()
  sinis <- c()
  alts <- 0:(k - 1)
  angle <- alts * ((360/k) * pi)/180
  cosis <- cos(angle)
  sinis <- sin(angle)
  
  # locations of ray ends, relative to center
  x.locs <- lengths * cosis
  y.locs <- lengths * sinis
  
  return(cbind(x.locs,y.locs))
}


