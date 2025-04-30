#' Compute Attributable Measures for DLNMs
#'
#' This function computes estimates of attributable risk (fractions and numbers)
#' from a distributed lag non-linear model (DLNM).
#'
#' This function computes the attributable fraction or number for a specific exposure
#' scenario and associated cases, given an estimated exposure-lag-response association
#' defined by a DLNM. Either \emph{forward} or \emph{backward} versions of attributable
#' risk measures are available in this setting. The method is described by Gasparrini
#' and Leone (2014), see references below. The function works in combination with
#' other functions in the package \pkg{dlnm}, which is assumed to be available.
#' The exposure and cases are provided by the arguments \code{x} and \code{cases},
#' respectively. The original cross-basis and fitted model containg it used for
#' estimation are provided by the arguments \code{basis} and \code{model}, respectively.
#' Alternatively, the user can provide estimated coefficients and (co)variance matrix
#' with \code{coef} and \code{vcov}. The function works both with time series and
#' non-time series data. In a time series setting, both \code{x} and \code{cases}
#' represent a complete series of ordered observations. More generally, the user
#' can apply this function for any kind of data: in this case \code{x} must be a
#' matrix of lagged exposures when \code{dir="back"}, and \code{cases} must be a
#' matrix of future cases when \code{dir="forw"}. The function can compute the total
#' attributable risk (if \code{tot=TRUE}, the default) or the contribution for each
#' observation. The argument \code{cen} defines the value used as counterfactual
#' scenario. If \code{sim=TRUE}, the function computes samples of the attributable
#' risk measures by simulating from the assumed normal distribution of the estimated
#' coefficients (only implemented for total estimates). These samples can be used
#' to defined empirical confidence intervals.
#'
#' An updated version of the function can be found at: https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata.
#'
#' @references Gasparrini, Antonio, and Michela Leone. “Attributable Risk from Distributed Lag Models.” *BMC Medical Research Methodology* 14, no. 1 (2014): 55. https://doi.org/10.1186/1471-2288-14-55.
#'
#' @param x An exposure vector or matrix of exposure histories for which the attributable risk needs to be computed.
#' @param basis An object of class `"crossbasis"` used for fitting the model.
#' @param cases The vector of cases or matrix of future cases corresponding to \code{x}.
#' @param model The fitted model, to be defined with a log link function.
#' @param coef,vcov,model.link  The user-provided coefficients, (co)variance matrix and model link for the prediction.
#' @param type The measure of attributable risk, either \code{"af"} (fraction, default) or \code{"an"} (number).
#' @param dir The direction used for computing the attributable risk, either \code{"back"} (backward, default) or \code{"forw"} (forward).
#' @param tot A logical. If \code{TRUE} (default), the total estimate is returned.
#' @param cen A numeric scalar. The reference value used as counterfactual scenario.
#' @param range The range of exposure. If \code{NULL}, the whole range is used.
#' @param sim A logical. If \code{TRUE}, simulation samples are returned.
#' @param nsim The number of simulation samples (only for \code{nsim=TRUE}).
#'
#' @returns By default, a numeric scalar representing the total attributable fraction
#' or number. If \code{sim=TRUE}, a vector of the simulated samples with length
#' \code{nsim}. If \code{tot=FALSE}, a vector with contributions for all the observations
#' (see Note below). These quantities are defined versus a counterfactual scenario
#' defined through the argument \code{cen}.
#'
#' @note The function handles missing values in both the \code{x} and \code{cases}
#' objects, excluding incomplete observations (also due to lagging) accordingly.
#' However, the total attributable number is rescaled to match the fraction using
#' as denominator the total observed number in \code{cases}. This approach uses
#' the all the available information even in the presence of missing values in \code{x}.
#' All of this under the assumption that the missing mechanism is unrelated with
#' both exposure and cases values. The functions can be also used with estimates
#' from DLNMs reduced to the overall cumulative exposure-response through the function
#' \code{crossreduce} in the package \pkg{dlnm}. In this case, the modified coefficients
#' and (co)variance matrix of the reduced cross-basis in \code{basis} must be passed
#' using the arguments \code{coef} and \code{vcov}, together with the information
#' on the link function in \code{model.link}. This option can be useful when the
#' original estimates from the full cross-basis are not available any more, for
#' example following a meta-analysis. Given the lag-specific estimates are not available
#' in this case, only the forward version of attributable risk (\code{dir="forw"})
#' can be computed. See Gasparrini and Leone (2014) for further info.
#'
#' @author Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}
#'
#' @export
#'
#' @examples
#' # load the package dlnm and the function attrdl
#' library(dlnm)
#'
#' # define the cross-basis and fit the model
#' cb <- crossbasis(chicagoNMMAPS$temp,
#'                  lag=30,
#'                  argvar=list(fun="bs", knots=c(-10,3,18)),
#'                  arglag=list(knots=c(1,3,10)))
#'
#' library(splines)
#' model <- glm(death ~  cb + ns(time, 7*14) + dow,
#'              family=quasipoisson(), chicagoNMMAPS)
#'
#' # global backward attributable risk of temperature (number and fraction)
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,type="an",cen=21)
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21)
#'
#' # global forward attributable fraction
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,dir="forw",cen=21)
#'
#' # empirical confidence intervals
#' afsim <- attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,
#'                 sim=TRUE,nsim=1000)
#' quantile(afsim,c(2.5,97.5)/100)
#'
#' # attributable fraction component due to heat and cold
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,range=c(21,100))
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,range=c(-100,21))
#'
#' # daily attributable deaths in the second month
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,type="an",
#'        tot=FALSE,cen=21)[31:60]
#'

################################################################################
attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,model.link=NULL,
                   type="af",dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000) {
  ################################################################################
  #
  # CHECK VERSION OF THE DLNM PACKAGE
  if(utils::packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")
  #
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
  #
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
  #
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
  #
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel:::Lag(x,seq(lag[1],lag[2])) else
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1)
      stop("dimension of 'x' not compatible with 'basis'")
  }
  #
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases,na.rm=TRUE)
    if(dir=="forw")
      cases <- rowMeans(as.matrix(tsModel:::Lag(cases,-seq(lag[1],lag[2]))))
  }
  #
  ################################################################################
  #
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- dlnm:::getlink(model,model.class)
    if(!model.link %in% c("log","logit"))
      stop("'model' must have a log or logit link function")
  }
  #
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
  #
  ################################################################################
  #
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else dlnm:::seqlag(lag)
  #
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- dlnm:::mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(dlnm::onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }
  #
  # CHECK DIMENSIONS
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
  #
  ################################################################################
  #
  # COMPUTE AF AND AN
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
  an <- af*cases
  #
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an)
    af <- sum(an[!isna])/sum(cases[!isna])
    an <- af*den
  }
  #
  ################################################################################
  #
  # EMPIRICAL CONFIDENCE INTERVALS
  if(!tot && sim) {
    sim <- FALSE
    warning("simulation samples only returned for tot=T")
  }
  if(sim) {
    # SAMPLE COEF
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(stats::rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    # RUN THE LOOP
    # pre_afsim <- (1 - exp(- Xpredall %*% coefsim)) * cases # a matrix
    # afsim <- colSums(pre_afsim,na.rm=TRUE) / sum(cases[!isna],na.rm=TRUE)
    afsim <- apply(coefsim,2, function(coefi) {
      ani <- (1-exp(-drop(Xpredall%*%coefi)))*cases
      sum(ani[!is.na(ani)])/sum(cases[!is.na(ani)])
    })
    ansim <- afsim*den
  }
  #
  ################################################################################
  #
  res <- if(sim) {
    if(type=="an") ansim else afsim
  } else {
    if(type=="an") an else af
  }
  #
  return(res)
}
