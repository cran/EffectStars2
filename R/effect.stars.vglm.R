#' Plot effect stars for vglm objects.
#' 
#' @aliases effectstars.vglm
#' @export
#' @export effectstars.vglm
#' @importFrom VGAM coef
#' @importFrom miscTools insertCol
#' @description Plots effect stars for \code{\link[VGAM]{vglm}}-objects. In particular, the method
#' works for multinomial logit models created by family \code{\link[VGAM]{multinomial}} and for 
#' models with ordinal response like \code{\link[VGAM]{sratio}}, \code{\link[VGAM]{cratio}},
#' \code{\link[VGAM]{cumulative}} or \code{\link[VGAM]{acat}}. \cr
#' 
#' For more details on plotting effect stars see \code{\link{effectstars}}.
#' 
#' @param x A \code{\link[VGAM]{vglm}}-object.
#' @param p.values Should the p-values of the single coefficients be included in the labels? Default 
#' is \code{FALSE}. 
#' @param symmetric Should the parameters be transformed to parameters with symmetric (sum-to-zero) 
#' side constraints instead of using reference levels. Default is \code{TRUE} for 
#' \code{\link[VGAM]{multinomial}}-models. If the \code{\link[VGAM]{multinomial}}-model 
#' contains object-specific covariates (\code{xij} argument from \code{\link[VGAM]{vglm.control}}) symmetric side 
#' constraints are not possible. In ordinal response models, 
#' no side constraints are needed and the option is obsolete. 
#' @param plot.parallel Should parallel parameters (equal over all response categories) be 
#' represented by effect stars. Default is \code{FALSE}.
#' @param ... further arguments for generic function \code{\link{effectstars}}.
#' @seealso \code{\link{effectstars}} \code{\link{effectstars.DIFlasso}}
#' @author Gunther Schauberger \cr
#' \email{gunther.schauberger@tum.de} \cr \cr
#' \url{https://www.sg.tum.de/epidemiologie/team/schauberger/}
#' 
#' @references  Tutz, G. and Schauberger, G. (2013): \emph{Visualization of Categorical Response Models -
#' from Data Glyphs to Parameter Glyphs}, Journal of Computational and Graphical Statistics 22(1), 156--177.
#' \cr \cr  Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press
#' @examples
#' \dontrun{
#' ############################################
#' ### Examples for multinomial logit model
#' ############################################
#' 
#' ### German election data
#' data(election) 
#' library(VGAM)
#' m_elect <- vglm(Partychoice ~ Gender + West + Age + Union + Highschool + Unemployment
#' + Pol.Interest + Democracy + Religion, family = multinomial(), data = election)
#' 
#' effectstars(m_elect)
#' 
#' # include p.values
#' effectstars(m_elect, p.values = TRUE)
#' 
#' ### German election data with category-specific covariates
#' 
#' data(election)
#' election[,13:16] <- election[,13:16] - election[,12]
#' election[,18:21] <- election[,18:21] - election[,17]
#' election[,23:26] <- election[,23:26] - election[,22]
#' election[,28:31] <- election[,28:31] - election[,27]
#' 
#' election$Social <- election$Social_SPD
#' election$Immigration <- election$Immigration_SPD
#' election$Nuclear <- election$Nuclear_SPD
#' election$Left_Right <- election$Left_Right_SPD
#' 
#' m.all <- vglm(Partychoice ~ Social + Immigration + Nuclear + Left_Right + Age + 
#'                 Religion + Democracy + Pol.Interest + Unemployment + Highschool + Union + West + 
#'                 Gender, data = election, 
#'                 family = multinomial(parallel = TRUE~-1 + Social + Immigration + 
#'                 Nuclear + Left_Right, refLevel = 1),
#'               xij = list(Social ~ Social_SPD + Social_FDP + Social_Greens + Social_Left,
#'                          Immigration ~ Immigration_SPD + Immigration_FDP + 
#'                          Immigration_Greens + Immigration_Left,
#'                          Nuclear ~ Nuclear_SPD + Nuclear_FDP + 
#'                          Nuclear_Greens + Nuclear_Left,
#'                          Left_Right ~ Left_Right_SPD + Left_Right_FDP + 
#'                          Left_Right_Greens + Left_Right_Left),
#'               form2 = ~Social + Immigration + Nuclear + Left_Right + Age + 
#'                 Religion + Democracy + Pol.Interest + Unemployment + Highschool + Union + West + 
#'                 Gender + Social_SPD + Social_FDP + Social_Greens + Social_Left +
#'                 Immigration_SPD + Immigration_FDP + Immigration_Greens + Immigration_Left +
#'                 Nuclear_SPD + Nuclear_FDP + Nuclear_Greens + Nuclear_Left +
#'                 Left_Right_SPD + Left_Right_FDP + Left_Right_Greens + Left_Right_Left
#' )
#' 
#' effectstars(m.all, symmetric = FALSE, p.values = TRUE)
#' summary(m.all)
#' 
#' 
#' ### Chilean plebiscite data
#' data(plebiscite)
#' m_chile <- vglm(Vote ~ ., family = multinomial(), data = plebiscite)
#' effectstars(m_chile)
#' 
#' # choose fixed circle sizes and use reference category instead of symmetric side constraints
#' effectstars(m_chile, symmetric = FALSE, fixed = TRUE)
#' 
#' ############################################
#' ### Examples for ordinal data
#' ############################################
#' 
#' ### Munich insolvency data
#' data(insolvency)
#' insolvency$Age <- scale(insolvency$Age)
#' 
#' my_formula <- Insolvency ~ Age + Gender
#' 
#' m_acat <- vglm(my_formula, data = insolvency,family = acat())
#' m_cratio <- vglm(my_formula, data = insolvency,family = cratio())
#' m_sratio <- vglm(my_formula, data = insolvency,family = sratio())
#' m_cumulative <- vglm(my_formula, data = insolvency,family = cumulative())
#' 
#' summary(m_acat)
#' effectstars(m_acat, p.values = TRUE)
#' 
#' summary(m_cratio)
#' effectstars(m_cratio, p.values = TRUE)
#' 
#' summary(m_sratio)
#' effectstars(m_sratio, p.values = TRUE)
#' 
#' summary(m_cumulative)
#' effectstars(m_cumulative, p.values = TRUE)
#' }





effectstars.vglm <- function(x, p.values = FALSE, symmetric = TRUE, plot.parallel = FALSE, ...){

  if(!(attributes(x@family)$vfamily[1] %in% c("multinomial","cumulative","sratio","acat","cratio"))){
    stop("Currently effect stars are are only implemented for family multinomial 
         and for the ordinal families cumulative, sratio, cratio and acat")
  }

  if(attributes(x@family)$vfamily[1] == "multinomial"){
    # check if xij is specified
    if(!all(dim(x@Xm2)==c(0,0))){
      # check if symmetric=TRUE is specified
      if(symmetric){
        warning("If argument xij was specified in vglm, symmetric side constraints can not be used. 
              Option 'symmetric' is set 'symmetric = FALSE'!")
        symmetric <- FALSE
      }
    }

    coef.se <- coef.se.multinomial(x = x, symmetric = symmetric)
  }else{
    coef.se <- coef.se.ordinal(x = x)
  }
  
  coefs <- coef.se$coefficients
  se <- coef.se$stderr
  which.parallel <- coef.se$which.parallel
  is_parallel <- coef.se$is_parallel
  
  p <- nrow(coefs)
  k <- ncol(coefs)
  
  # calculate pvalues
  pvalues <- 2*( 1 - pnorm(abs(coefs/se)))
  # create printable pvalues    
  pval.print <- pvalues
  is.NAN <- is.nan(pvalues)
  pval.print[pvalues < 5e-04] <- 0
  pval.print[pvalues >= 5e-04 & pvalues < 0.001] <- 0.001
  pval.print <- format(pval.print, digits = 1, nsmall = 3)
  
  # are labels in command?
  plot_labels <- NULL
  if(!is.null(match.call()$labels)){
    labels <- plot_labels <- eval(match.call()$labels)
  }else{
   # category names as labels for coefs, se, and pvalues
   labels <- plot_labels <- (x@misc$ynames)[1:k]
  }
  
  # rownames of coefficients, including categories and intercations
  complete.names <- x@misc$colnames.x
  complete.names[complete.names == "(Intercept)"] <- "Intercept"
  
  
  # allnames contains all names of all covariates
  if(all.vars(x@misc$formula)[2]=="."){
    allnames <- attributes(terms(x@misc$formula,data=get(x@misc$dataname)))$term.labels
  }else{
    allnames <- attributes(terms(x@misc$formula))$term.labels
  }
  if(all.vars(x@misc$formula)[2]=="."){
    allnames <- c("Intercept",allnames)}
  else{
    if(attributes(terms(x@misc$formula))$intercept==1){
      allnames <- c("Intercept",allnames)
    }
  }
  
  allnames <- unique(c(allnames,unlist(strsplit(allnames,":"))))
  
  ##############
  # initialize factor_cat, contains all factor levels of covariates if covariates are factors
  factor_cat <- complete.names
  
  # after deleting covariate names from complete.names only the categories are left
  for(i in 1:length(allnames)){
    factor_cat <-  sub(allnames[i],"",factor_cat,fixed = TRUE)
  }
  for(i in 1:length(factor_cat)){
    if(substr(factor_cat[i], 1, 1)==":"){
      factor_cat[i] <- substr(factor_cat[i], 2,nchar(factor_cat[i])) 
    }
    if(substr(factor_cat[i], nchar(factor_cat[i]), nchar(factor_cat[i]))==":"){
      factor_cat[i] <- substr(factor_cat[i], 1,nchar(factor_cat[i])-1) 
    }}
  
  # create plot_subs, either by subs from call or with factor levels from factor_cat
  if(is.null(match.call()$subs)){
    if(!all(factor_cat==rep("",p))){
      plot_subs <- factor_cat
      plot_subs[factor_cat!=""] <- paste0("(",factor_cat[factor_cat!=""],")")
    }
  }else{
    plot_subs <- match.call()$subs
  }

  # create plot_names, either by names from call or by removing factor_cat from complete.names
  if(!is.null(match.call()$names)){
    plot_names <- match.call()$names
  }else{
    plot_names <- complete.names
    # get all factor labels, also from interactions
    individ.facs <- unlist(strsplit(factor_cat,":"))
    unique_facs <- c(factor_cat, individ.facs)
    for(i in 1:length(unique_facs)){
      if(substr(unique_facs[i], 1, 1)==":"){
        unique_facs[i] <- substr(unique_facs[i], 2,nchar(unique_facs[i])) 
      }
      if(substr(unique_facs[i], nchar(unique_facs[i]), nchar(unique_facs[i]))==":"){
        unique_facs[i] <- substr(unique_facs[i], 1,nchar(unique_facs[i])-1) 
      }
    }
    unique_facs <- unique(unique_facs[unique_facs!=""])
    # remove everything from unique.facs from the covariate names
    if(length(unique_facs)>0){
    unique_facs <- unique_facs[order(nchar(unique_facs),decreasing = TRUE)]
    for(i in 1:length(unique_facs)){
      plot_names <- sub(unique_facs[i],"",plot_names,fixed = TRUE)
    }
    }
  }

  # name cols and rows of coefs, se and pvalues
  colnames(coefs) <- colnames(se) <- colnames(pvalues) <- labels
  rownames(coefs) <- rownames(se) <- rownames(pvalues) <- complete.names
  
  # if p.values, create a label matrix containing pvalues
  if(p.values){
    plot_labels <- rep(plot_labels,each = p)
    plot_labels[c(!is.NAN)] <- paste0(plot_labels[!is.NAN], "\n (", pval.print[!is.NAN], ")")
    plot_labels <- matrix(plot_labels,ncol=k)
  }
  

  
  ## remove everything corresponding to parallel covariates if necessary
  plot_coefs <- coefs
  if(!plot.parallel){
    if(!all(!is_parallel)){
      if(is.null(match.call()$names)){
        plot_names <- plot_names[-which.parallel]
      }
      plot_coefs <- plot_coefs[-which.parallel,,drop=FALSE]
      if(is.matrix(plot_labels)){
        plot_labels <- plot_labels[-which.parallel,,drop=FALSE]
      }
      plot_subs <- plot_subs[-which.parallel]
    }
  }
  
# create a list of arguments for function effect stars
  arg.list <- list(x = exp(plot_coefs), names = plot_names, subs = plot_subs, labels = plot_labels, 
                   control = star.ctrl(), cols = NULL, fixed = FALSE, scale = 1)
  if(!is.null(match.call()$control)){
    arg.list$control <- match.call()$control
  }
  if(!is.null(match.call()$cols)){
    arg.list$cols <- match.call()$cols
  }
  if(!is.null(match.call()$fixed)){
    arg.list$fixed <- match.call()$fixed
  }
  if(!is.null(match.call()$scale)){
    arg.list$scale <- match.call()$scale
  }

  # plot effect stars
  do.call(effectstars, arg.list)

  # return invisible stuff
  invisible(list(model = x, coefficients = coefs, stderr = se, pvalues = pvalues))
  
  
}
