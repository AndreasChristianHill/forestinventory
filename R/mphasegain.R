# --------------------------------------------------- #
# Takes as input an esttable - object and returns a validation of
# which multiphase-methods performed BEST in comparison to the onephase
# (baseline) std-error.
#
# We define the "gain" as the reduction ("+") or possibly also the increase ("-")
# of the best multiphase-standard-error compared to the onephase-std-error
#
# We also give the percentage of the best multiphase-standard-error
# compared to the onephase std-error AND the relative efficiency
#
# --------------------------------------------------- #

#' mphase.gain
#'
#' \code{mphase.gain} takes as input an object created by the \code{\link{estTable}} function
#' and returns a validation of which multiphase method and estimator performed best in comparison
#' to the onephase estimation (baseline) in terms of estimation precision.
#'
#'
#' @param esttable.obj an object of class \code{esttable} created by the \code{\link{estTable}} function
#'
#' @param pref.vartype preferred type of multiphase variance that should be compared to the \code{onephase} variance,
#'                     if more then one variance type has been calculated in the multiphase estimation object(s) stored in
#'                     \code{esttable}. Valid input values are \code{"g_variance"} (default) and \code{"ext_variance"}.
#'
#' @param exclude.synth \code{logical}. If set to \code{TRUE} (default), synthetic estimations are not considered in the validation.
#'
#'
#' @return \code{mphase.gain} returns a \code{data.frame} containing the following components:
#'
#'  \itemize{
#'     \item \code{area:} in case of small area estimation: the name of the small area
#'     \item \code{std_onephase:} standard error of the \code{\link{onephase}} estimation
#'     \item \code{std_multiphase:} smallest standard error among the (set of) multiphase estimations stored in \code{esttable.obj}
#'     \item \code{estimator:} multiphase estimator with the smallest standard error
#'     \item \code{method:} estimation Method of the multiphase estimator with the smallest standard error
#'     \item \code{gain:} the \emph{gain} is the reduction (if value is positive) or possibly also the increase (if value is negative)
#'                        in standard error when applying the multiphase as alternative to the onephase estimation
#'     \item \code{perc.of.onephase:} ratio between the smallest multiphase standard error and the onephase standard error
#'     \item \code{rel.eff:} the \emph{relative efficiency} defined as the ratio between the onephase standard error and the multiphase standard error
#'  }
#'
#' @note
#'
#' The standard error is defined as the square root of the estimated variance
#'
#' The \emph{gain} can be interpreted as: "The multiphase estimation procedure leads a xx % reduction in standard error compared to the
#' onephase procedure".
#'
#' The \emph{relative efficiency} can be interpreted as: "The terrestrial sample size would have to be 'rel.eff' times larger in order to achieve the
#' mutiphase estimation precision (in terms of standard error) with the onephase estimation procedure".
#'
#'
#' @example examples/example_mphasegain.R
#'
#' @import plyr
#' @export


# -----------------------------------------------------------------------------#
# FUNCTION STARTS HERE:


mphase.gain<- function(esttable.obj, pref.vartype = "g_variance", exclude.synth = TRUE){

  # check input:
  if(!class(esttable.obj)[2] == "esttable"){stop("'mphase.gain()' expects an 'esttable' object created by 'estTable()'")}

  # convert esttable-object into data.frame:
  esttable.obj<- as.data.frame(esttable.obj)

  # -------- #
  # closure:
  prec.gain<- function(est.tab){

    ind.1ph<- est.tab$vartype %in% "variance"

    if(exclude.synth){
      ind.not.1ph<- est.tab$vartype %in% c(pref.vartype) & !(est.tab$estimator %in% c("psynth", "synth"))
    } else {
      ind.not.1ph<- est.tab$vartype %in% c(pref.vartype)
    }

    std.1ph<- est.tab[ind.1ph, "std"]
    est.tab.not1ph<- est.tab[ind.not.1ph,]

    ind.best.multiph<- which.min(est.tab.not1ph[["std"]])[1]

    best.mphase<- est.tab.not1ph[ind.best.multiph, c("estimator", "method", "std", "vartype")]

    if(all(!ind.1ph)){ # if no onephase is there

      d<- data.frame(std_onephase = NA, std_multiphase = best.mphase$std,
                     estimator = best.mphase$estimator, method = best.mphase$method,
                     gain = NA, perc.of.onephase = NA)
    } else {

      # gain:
      red.to.1ph<- round(100* (1 - (best.mphase[["std"]] / std.1ph)), digits = 1)

      perc.of.1phstd<- round(100*(best.mphase[["std"]] / std.1ph), digits = 1)

      rel.eff<- std.1ph / best.mphase[["std"]]

      d<-
        data.frame(std_onephase = std.1ph, std_multiphase = best.mphase$std,
                   estimator = best.mphase$estimator, method = best.mphase$method,
                   gain = red.to.1ph, perc.of.onephase = perc.of.1phstd, rel.eff = rel.eff)
    }

  }
  # -------- #


  # apply closure:
  if (any(colnames(esttable.obj) %in% "area")){

    return(ddply(esttable.obj,"area", prec.gain))

  } else {

    return(prec.gain(esttable.obj))

  }


} # end of function
