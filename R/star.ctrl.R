#' Control function for effect stars.
#' 
#' @aliases star.ctrl
#' @export 
#' @description Control function to set graphical parameters for method \code{\link{effectstars}}.
#' 
#' @param lwd.circle Line width of circle.
#' @param col.circle Color of circle, possibly a vector with one value per covariate.
#' @param lty.circle Line type of circle.
#' @param col.fill Color to fill the circle, possibly a vector with one value per covariate.
#' @param lwd.star Line width for effect star.
#' @param cex.main Size of mains.
#' @param cex.labels Size of labels.
#' @param col.main Color of mains. 
#' @param col.labels Colors of labels. Can be a vector (one value/color per category) 
#' or a even matrix (one column per category, one row per star).
#' @param col.star Color of effect star.
#' @param dist.labels Tuning parameter for distance of labels 
#' from effect star. Default is 1, higher values increase the distance of the 
#' labels to effect stars. Can also be specified as a vector, containing one value per star.
#' @param font.labels Font type of labels. Can be a vector (one value/color per category) 
#' or a even matrix (one column per category, one row per star).
#' @param radius Radius for circle. Can also be specified as a vector, containing one value per star.
#' @seealso \code{\link{effectstars}}
#' @author Gunther Schauberger \cr
#' \email{g.schauberger@tum.de} \cr \cr
#' \email{gunther@stat.uni-muenchen.de} \cr \cr
#' \url{https://www.researchgate.net/profile/Gunther_Schauberger2}\cr \cr
#' \url{http://www.semsto.statistik.uni-muenchen.de/personen/doktoranden/schauberger/index.html}
#' 
#' @references  Tutz, G. and Schauberger, G. (2013): \emph{Visualization of Categorical Response Models -
#' from Data Glyphs to Parameter Glyphs}, Journal of Computational and Graphical Statistics 22(1), 156--177.
#' \cr \cr  Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press


#' @examples \dontrun{
#' data(election) 
#' library(VGAM)
#' 
#' m_elect <- vglm(Partychoice ~ Gender + West + Age + Union + Highschool + Unemployment
#' + Pol.Interest + Democracy + Religion, family = multinomial(), data = election)
#' 
#' ctrl <- star.ctrl(col.labels = c("black","red2","yellow2","green2","darkred"), 
#' col.star = "darkgray", col.fill = "lightblue", col.circle = "darkgray", 
#' cex.labels = 1.1)
#' 
#' effectstars(m_elect, control = ctrl)
#' }
#' 
star.ctrl <- function(lwd.circle = 1, col.circle = "yellowgreen", lty.circle = "solid", 
                      col.fill = "yellowgreen", lwd.star = 1.5,
                      cex.main = 1.5, cex.labels = 1, 
                      col.main = "black", col.labels = "black", col.star = "black",
                      dist.labels = 1, font.labels = 1, radius = 1){
  
  RET <- list(lwd.circle = lwd.circle, col.circle = col.circle,
              lty.circle = lty.circle, col.fill = col.fill, lwd.star = lwd.star,
              cex.main = cex.main, cex.labels = cex.labels, 
              col.main = col.main, col.labels = col.labels, col.star = col.star,
              dist.labels = dist.labels, font.labels = font.labels,
              radius = radius)
  
  RET
}
