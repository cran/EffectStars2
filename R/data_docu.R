
#' Election Data
#' 
#' The data set contains data from the German Longitudinal Election Study. The
#' response categories refer to the five dominant parties in Germany. The
#' explanatory variables refer to the declarations of single voters.
#' 
#' 
#' @name election
#' @docType data
#' @format A data frame with 816 observations on the following 30 variables.
#' \describe{ \item{Age}{Standardized age of the voter}
#' \item{AgeOrig}{Unstandardized age of the voter}
#' \item{Partychoice}{Party Choice with levels \code{CDU}, \code{SPD},
#' \code{FDP}, \code{Greens} and \code{Left Party}}
#' \item{Gender}{Gender with levels \code{female} and \code{male}}
#' \item{West}{Regional provenance (West-Germany or East-Germany) with
#' levels \code{east} and \code{west}} \item{Union}{Member of a Union
#' with levels \code{no member} and \code{member}}
#' \item{Highschool}{Educational level with levels \code{no highschool}
#' and \code{highschool}} \item{Unemployment}{Unemployment with levels
#' \code{not unemployed} and \code{unemployed}}
#' \item{Pol.Interest}{Political Interest with levels \code{very
#' interested} and \code{less interested}}
#' \item{Democracy}{Satisfaction with the functioning of democracy with
#' levels \code{satisfied} and \code{not satisfied}}
#' \item{Religion}{Religion with levels \code{evangelical},
#' \code{catholic} and \code{other religion}}
#' \item{Social_CDU}{Difference in attitude towards the socioeconomic
#' dimension of politics between respondent and CDU}
#' \item{Social_SPD}{Difference in attitude towards the socioeconomic
#' dimension of politics between respondent and SPD}
#' \item{Social_FDP}{Difference in attitude towards the socioeconomic
#' dimension of politics between respondent and FDP}
#' \item{Social_Greens}{Difference in attitude towards the
#' socioeconomic dimension of politics between respondent and the Greens}
#' \item{Social_Left}{Difference in attitude towards the socioeconomic
#' dimension of politics between respondent and the Left party}
#' \item{Immigration_CDU}{Difference in attitude towards immigration of
#' foreigners between respondent and CDU}
#' \item{Immigration_SPD}{Difference in attitude towards immigration of
#' foreigners between respondent and SPD}
#' \item{Immigration_FDP}{Difference in attitude towards immigration of
#' foreigners between respondent and FDP}
#' \item{Immigration_Greens}{Difference in attitude towards immigration
#' of foreigners between respondent and the Greens}
#' \item{Immigration_Left}{Difference in attitude towards immigration
#' of foreigners between respondent and the Left party}
#' \item{Nuclear_CDU}{Difference in attitude towards nuclear energy
#' between respondent and CDU} \item{Nuclear_SPD}{Difference in
#' attitude towards nuclear energy between respondent and SPD}
#' \item{Nuclear_FDP}{Difference in attitude towards nuclear energy
#' between respondent and FDP} \item{Nuclear_Greens}{Difference in
#' attitude towards nuclear energy between respondent and the Greens}
#' \item{Nuclear_Left}{Difference in attitude towards nuclear energy
#' between respondent and the Left party}
#' \item{Left_Right_CDU}{Difference in attitude towards the positioning
#' on a political left-right scale between respondent and CDU}
#' \item{Left_Right_SPD}{Difference in attitude towards the positioning
#' on a political left-right scale between respondent and SPD}
#' \item{Left_Right_FDP}{Difference in attitude towards the positioning
#' on a political left-right scale between respondent and FDP}
#' \item{Left_Right_Greens}{Difference in attitude towards the
#' positioning on a political left-right scale between respondent and the
#' Greens} \item{Left_Right_Left}{Difference in attitude towards the
#' positioning on a political left-right scale between respondent and the Left
#' party} }
#' @references German Longitudinal Election Study (GLES)
#' @keywords datasets multinomial response
#' @examples
#' 
#' data(election) 
#' library(VGAM)
#' m_elect <- vglm(Partychoice ~ Gender + West + Age + Union + Highschool + Unemployment
#' + Pol.Interest + Democracy + Religion, family = multinomial(), data = election)
#' 
#' effectstars(m_elect)
#' 
NULL





#' Insolvency data
#' 
#' The data set originates from the Munich founder study. The data were
#' collected on business founders who registered their new companies at the
#' local chambers of commerce in Munich and surrounding administrative
#' districts. The focus was on survival of firms measured in 7 categories, the
#' first six represent failure in intervals of six months, the last category
#' represents survival time beyond 36 months.
#' 
#' 
#' @name insolvency
#' @docType data
#' @format A data frame with 1224 observations on the following 16 variables.
#' \describe{ 
#' \item{Insolvency}{Survival of firms in ordered categories
#' with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6}
#' < \code{7}} 
#' \item{Sector}{Economic Sector with levels
#' \code{industry}, \code{commerce} and \code{service industry}}
#' \item{Legal}{Legal form with levels \code{small trade}, \code{one
#' man business}, \code{GmBH} and \code{GbR, KG, OHG}}
#' \item{Location}{Location with levels \code{residential area} and
#' \code{business area}} 
#' \item{New_Foundation}{New Foundation or
#' take-over with levels \code{new foundation} and \code{take-over}}
#' \item{Pecuniary_Reward}{Pecuniary reward with levels \code{main} and
#' \code{additional}} 
#' \item{Seed_Capital}{Seed capital with levels
#' \code{< 25000} and \code{> 25000}} 
#' \item{Equity_Capital}{Equity capital with levels \code{no} and \code{yes}}
#' \item{Debt_Capital}{Debt capital with levels \code{no} and
#' \code{yes}} 
#' \item{Market}{Market with levels \code{local} and
#' \code{national}} 
#' \item{Clientele}{Clientele with levels \code{wide
#' spread} and \code{small}} 
#' \item{Degree}{Educational level with
#' levels \code{no A-levels} and \code{A-Levels}} 
#' \item{Gender}{Gender with levels \code{female} and \code{male}}
#' \item{Experience}{Professional experience with levels \code{< 10
#' years} and \code{> 10 years}} 
#' \item{Employees}{Number of employees
#' with levels \code{0 or 1} and \code{> 2}} 
#' \item{Age}{Age of the founder at formation of the company} }
#' @references Bruederl, J. and Preisendoerfer,
#' P. and Ziegler, R. (1996): \emph{Der Erfolg neugegruendeter Betriebe: 
#' eine empirische Studie zu den Chancen und Risiken von
#' Unternehmensgruendungen}, Duncker & Humblot.
#' @source Muenchner Gruender Studie
#' @keywords datasets ordinal response
#' @examples
#' 
#' \dontrun{
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
#' 
NULL



#' Chilean Plebiscite
#' 
#' The data origin from a survey refering to the plebiscite in Chile 1988. The
#' chilean people had to decide, wether Augusto Pinochet would remain president
#' for another ten years (voting yes) or if there would be presidential
#' elections in 1989 (voting no).
#' 
#' 
#' @name plebiscite
#' @docType data
#' @format A data frame with 2431 observations on the following 7 variables.
#' \describe{ \item{Gender}{Gender with levels \code{female} and
#' \code{male}} 
#' \item{Education}{Educational level with levels
#' \code{low} and \code{high}} 
#' \item{SantiagoCity}{Respondent from
#' Santiago City with levels \code{no} and \code{yes}}
#' \item{Income}{(Standardized) Monthly Income in Pesos}
#' \item{Population}{(Standardized) Population size of respondent's community}
#' \item{Age}{(Standardized) Age in years} 
#' \item{Vote}{Response with levels
#' \code{Abstention}, \code{No}, \code{Undecided} and \code{Yes}} }
#' @references  Fox, J. (2008):
#' \emph{Applied Regression Analysis and Generalized Linear Models}, Second
#' Edition.
#' @source R package carData: \code{\link[carData]{Chile}}
#' @keywords datasets multinomial response
#' @examples
#' 
#' \dontrun{
#' data(plebiscite)
#' m_chile <- vglm(Vote ~ ., family = multinomial(), data = plebiscite)
#' 
#' effectstars(m_chile)
#' }
#' 
NULL


