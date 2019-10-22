#' @export
effectstars <- function(x, ...)
  UseMethod("effectstars")



#' Plot effect stars.
#' 
#' @aliases effectstars
#' @export 
#' @importFrom graphics layout par stars symbols text title
#' @importFrom stats coefficients pnorm terms
#' @description Plots effect stars for grouped coefficients. Effect stars are applicable if the
#' parameters of a model are grouped in some sense. For example, the parameters of a multinomial 
#' logit models are grouped by the covariates, i.e. per covariate there is one estimate per response category.
#' But also in many other models, the parameters can have a fixed grouping structure. All estimates have to be positive, 
#' typically the exponentials of the estimates are plotted. Every effect star comes with a circle of radius 1.
#' This circle represents the case of no effect, i.e. \code{exp(0)=1}.
#' 
#' @param x A matrix containing all coefficients to plot, one column per group/covariate, 
#' one row per category. If the arguments \code{names} and \code{labels} are not specified, 
#' the \code{colnames} and \code{rownames} of \code{x} are used.
#' @param names A vector containing all group/covariate names, will be used as titles of single 
#' effect stars. If NULL, \code{colnames} of \code{x} is used.
#' @param subs A vector containing all subtitles, one per group/covariate.
#' @param labels A vector or a matrix containing labels of the categories. If labels is a matrix, 
#' it needs to have the same dimensions as \code{x}. Otherwise, \code{labels} is a vector with 
#' length equal to the number of categories, i.e. number rows of \code{x}. If NULL, \code{rownames}
#' of \code{x} is used.
#' @param control Control argument (to set graphical parameters) for method \code{\link{effectstars}}, see \code{\link{star.ctrl}}.
#' @param cols Number of columns for arranging effect stars
#' @param fixed If \code{TRUE}, all circles have the same radius. If \code{FALSE}, every star is scaled so that 
#' the length of the longest ray is equal for all stars.
#' @param scale Global factor to increase (\code{scale}>1) or decrease (\code{scale}<1) the size of the stars.
#' @param ... possible further arguments
#' @seealso \code{\link{star.ctrl}}, \code{\link{effectstars.vglm}}, \code{\link{effectstars.DIFlasso}}, \code{\link{effectstars.DIFboost}}
#' @author Gunther Schauberger \cr
#' \email{gunther.schauberger@tum.de} \cr \cr
#' \url{https://www.sg.tum.de/epidemiologie/team/schauberger/}
#' 
#' @references  Tutz, G. and Schauberger, G. (2013): \emph{Visualization of Categorical Response Models -
#' from Data Glyphs to Parameter Glyphs}, Journal of Computational and Graphical Statistics 22(1), 156--177.
#' \cr \cr  Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press


#' @examples \dontrun{
#' #####################
#' ### Simple example for basic effectstars function
#' p <- 4; k <- 5
#' coefs <- matrix(exp(rnorm(p*k,sd=0.5)),ncol=k)
#' rownames(coefs) <- paste("Variable",1:p)
#' colnames(coefs) <- paste("Cat",1:k)
#' effectstars(coefs)
#' 
#' #####################
#' ### Example for effect stars for a multivariate logit model
#' data(xs.nz, package = "VGAMdata")
#' xs.nz$age <- scale(xs.nz$age)
#' library(VGAM)
#' 
#' cats_dogs <- vglm(cbind(cat, dog) ~ age +  sex + marital, 
#'                   data = xs.nz, family = binom2.or(zero = NULL))
#' 
#' summary(cats_dogs)
#' 
#' ## quick and dirty
#' effectstars(exp(coef(cats_dogs, matrix = TRUE)))
#' 
#' 
#' ## make it pretty
#' # create the effects matrix you want to plot, name rows and columns
#' effects <- exp(coef(cats_dogs, matrix = TRUE))
#' colnames(effects) <- c("cat", "dog", "OR")
#' rownames(effects) <- c("Intercept", "Age", "Gender", rep("Marital", 3))
#' 
#' # create subtitles containing category labels of predictors
#' subs <- c(rep("",2), "(male)", "(married)", "(separated/divorced)", "(widowed)")
#' 
#' # create labels containing the response categories and all p-values
#' p_values <- formatC(summary(cats_dogs)@coef3[,4], format="f", digits=3)
#' labels <- matrix(paste0(rep(c("cat", "dog", "OR"), nrow(effects)), "\n(", p_values, ")"),
#' byrow = TRUE, ncol = 3)
#' 
#' # plot effectstars
#' effectstars(effects, labels = labels, subs = subs)
#' 
#' 
#' #####################
#' ## Example for method effectstars.vglm for a multinomial logit model calculated in VGAM
#' data(election) 
#' library(VGAM)
#' m_elect <- vglm(Partychoice ~ Gender + West + Age + Union + Highschool + Unemployment
#' + Pol.Interest + Democracy + Religion, family = multinomial(), data = election)
#' effectstars(m_elect)
#' }



effectstars.default <- function(x, names = NULL, subs = NULL, labels = NULL, control = star.ctrl(),
                               cols = NULL, fixed = FALSE, scale = 1, ...){
  
  
  # keep old par settings
  old.par <- par(no.readonly = TRUE) 
  
  # number of groups/covariates
  p <- nrow(x)
  
  # number of categories
  k <- ncol(x)
  
  ########################
  # several stop commands
  if((!is.null(names)) & length(names)!=p)
    stop("names must be a vector with length equal to the number of groups 
         (i.e. equal to the number of rows in x)")
  
  if(!is.null(labels)){
    if(is.matrix(labels)){
      if(ncol(labels) != ncol(x) | nrow(labels) != nrow(x) )
        stop("If labels is a matrix, it must have the same dimension as x")
    }  
    if(is.vector(labels)){
      if(length(labels) != k)
        stop("If labels is a vector, it must be a vector with length equal to the number of categories 
           (i.e. equal to the number of columns in x)")
    }
  }
  
  if((!is.null(subs)) & length(subs)!=p)
    stop("subs must be a vector with length equal to the number of groups 
         (i.e. equal to the number of rows in x)")
  
  if(!is.logical(fixed))
    stop("fixed must be logical")
  
  if(sum(x<=0)>0){
    stop("Negative effects can not be displayed!")
  }
  
  ########################
  
  
  # get names and labels
  if(is.null(names)){
    names <- rownames(x)
  }
  if(is.null(labels)){
    labels <- colnames(x)
  }
  if(is.null(subs)){
    subs <- rep(NA,p)
  }
  
  # make a label matrix if only vector
  if(length(labels)==k){
    labels <- matrix(rep(labels,each=p),nrow=p)
  }

  ############# create layout matrix
  if (is.null(cols)){
    if (floor(sqrt(p)) == sqrt(p)) {
      cols <- sqrt(p)
    }
    else {
      cols <- (floor(sqrt(p)) + 1)
    }
  }
  
  rows <- ceiling(p/cols)
  
  n.nulls <- cols*rows - p

  mat.layout <- matrix(c(1:p,rep(0,n.nulls)),ncol=cols,nrow=rows,byrow = TRUE)
  
  layout(mat.layout, respect = TRUE, widths = rep(1,cols), heights = rep(1,rows))

  
  ###################################
  if(!fixed){
    factors <- rep(1,p)*scale
  }else{
    factors <- apply(x,1,max)/max(x)*scale
  }


  # plot all stars
  for(i in 1:p){
    control_i <- control

    if(is.matrix(control$col.labels)){
      control_i$col.labels = control$col.labels[i,]
    }
    
    if(is.matrix(control$font.labels)){
      control_i$font.labels = control$font.labels[i,]
    }
    
    if(length(control$dist.labels)==p){
      control_i$dist.labels = control$dist.labels[i]
    }
    
    if(length(control$radius)==p){
      control_i$radius = control$radius[i]
    }
    
    if(length(control$col.fill)==p){
      control_i$col.fill = control$col.fill[i]
    }

    if(length(control$col.circle)==p){
      control_i$col.circle = control$col.circle[i]
    }
    
    single.star(x[i,], name = names[i], sub = subs[i], labels = labels[i,],
                factor = factors[i], control = control_i)
  }
  
  # set old par settings 
  par(old.par)
}
