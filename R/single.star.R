
single.star <- function(lengths, name, sub, labels, factor, control = star.ctrl()){

  # get all arguments from control
  lwd.circle <- control$lwd.circle
  col.circle <- control$col.circle
  lty.circle <- control$lty.circle
  col.fill <- control$col.fill
  lwd.star <- control$lwd.star
  cex.main <- control$cex.main
  cex.labels <- control$cex.labels
  col.main <- control$col.main
  col.labels <- control$col.labels
  col.star <- control$col.star
  dist.labels <- control$dist.labels
  font.labels <- control$font.labels
  radius <- control$radius
  ################################
  
  par(xpd=TRUE, mar = rep(2,4))
  # number of categories
  k <- length(lengths)
  
  # global shrinkage
  factor <- factor*0.8  

  
  # additive distance for label locations
  dist.add <-(max(lengths)-1)*0.2+0.3
  
  # x and y locations of labels
  locations <- get.locations(lengths+dist.add) * dist.labels * factor 


  
  # nullcolor is used to make first star invisible
  oldcolor <- par()$col
  nullcolor <- "white"
  if(par()$bg != "transparent"){nullcolor <- par()$bg}

  
  # star to set the scale, without (possible) factor
  #par(col = nullcolor)
  stars(matrix(lengths,nrow=1), add = FALSE, locations = c(0,0), len = 1,
        scale = FALSE, draw.segments = FALSE, main = "", 
        lwd = lwd.star, bg = par("col"))

  
  # circle to make first star invisible
  symbols(0,0, circles = max(lengths)*1.02, 
          add = TRUE, inches = FALSE, fg = nullcolor, bg = nullcolor)
  #par(col = oldcolor)
  
  # add correct circle
  symbols(0,0, circles = radius*factor, 
          add = TRUE, inches = FALSE, fg = col.circle, bg = col.fill, lwd = lwd.circle,
          lty = lty.circle)
  
  # add correct star
oldcolor <- par()$fg
par(fg = col.star)
  stars(matrix(lengths,nrow=1), add = TRUE, locations = c(0,0), len = factor,
        scale = FALSE, draw.segments = FALSE, main = "", lwd = lwd.star,
        col.lines = col.star, fg = col.star)
par(fg = oldcolor)

  # add labels
  text(x = locations[,1], y = locations[,2], labels, cex = cex.labels, col = col.labels, 
       font = font.labels)

  # add main 
  upper.line <- (par()$mar)[3]
  title(main = name, line = upper.line-1.2, cex.main = cex.main, col = col.main)
  
  # add sub main
  if(!is.na(sub)){
    title(main = sub, line = upper.line-1.2-cex.main, font.main = 1, 
          cex.main = cex.main*0.8, col = col.main)
  }
  

}

