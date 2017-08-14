# ------------------------------------------------------------ #
#
# Function takes a list of estimation-objects from forestinventory
# and "glues"  them into one dataframe
# --> using
#
# format = "long" or "wide"
# key = "variance" or "ci"
# ------------------------------------------------------------


#' estTable
#'
#' \code{estTable} is used to calculate estimations based on double sampling under the
#' \emph{model-assisted Monte Carlo approach}. A \emph{first phase} of auxiliary information
#' (e.g. taken from remote sensing data) is used to generate model predictions based on multiple linear
#' regression  using the method of ordinary least squares. A subsample of the first phase comprises
#' the \emph{second phase} which contains terrestrial observations (i.e. the \emph{local densities}
#' of the ground truth) that is used to correct for bias in the design-based sense.
#' The estimation method is available for \emph{simple} and \emph{cluster sampling} and includes
#' the special case where the first phase is based on an \emph{exhaustive} sample (i.e. a census).
#' \emph{Small-area applications} are supported for synthetic estimation as well as two varieties
#' of bias-corrected estimators: the traditional small-area estimator and an asymptotically
#' equivalent version derived under Mandallaz's extended model approach.
#'
#' @param est.list
#' @param key.var
#' @param sae
#' @param format
#' @param vartypes
#'
#' @details Here come the details

#' @return \code{estTable} returns ....
#'
#' An object of class \code{"twophase"} returns a \code{list} of the following components:
#'
#'  \item{input}{a \code{list} containing the function's inputs}
#'  \item{estimation}{a data frame containing the following components:
#'                   \itemize{
#'                    \item \code{area:} the domain (only present if argument \code{areas} has been used)
#'                    \item \code{estimate:} the point estimate
#'                    \item \code{ext_variance:} the external variance of the point estimate that doesn't account for
#'                                               fitting the model from the current inventory
#'                    \item \code{g_variance:} the internal (g-weight) variance that accounts for
#'                                               fitting the model from the current inventory
#'                    \item \code{n1} the first phase sample size of plots
#'                    \item \code{n2} the second phase (i.e. terrestrial) sample size of plots
#'                    \item \code{n1G} the first phase sample size in the small area
#'                    \item \code{n2G} the second phase (i.e. terrestrial) sample size in the small area
#'                    \item \code{r.squared} the R squared of the linear model
#'                    }}
#'  \item{samplesizes}{a \code{\link[base]{data.frame}} summarizing all samplesizes: in case of cluster sampling both,
#'                     the number of individual plots and the nuber of clusters is reported.}
#'  \item{coefficients}{the linear model coefficients}
#'  \item{cov_coef}{the design-based covariance matrix of the model coefficients}
#'  \item{Z_bar_1G}{the estimated auxiliary means of \code{formula} based on the first phase.
#'                  If the first phase is exhaustive, these are the true auxiliary means specified in the input-argument \code{exhaustive}.}
#'  \item{cov_Z_bar_1G}{the covariance matrix of \code{Z_bar_1G}}
#'  \item{Rc_x_hat_G}{the small-area residuals at either the plot level or cluster level depending on the call}
#'  \item{Rc_x_hat}{the residuals at either the plot level or cluster level depending on the call}
#'  \item{Yx_s2G}{the local densities in the small area}
#'  \item{Mx_s2G}{the cluster weights in the small area}
#'  \item{mean_Rc_x_hat_G}{the mean residual (weighted mean in the case of cluster sampling) in the small area}
#'  \item{mean_Rc_x_hat}{the mean residual (weighted mean in the case of cluster sampling)}
#'  \item{warn.messages}{logical indicating if warning messages were issued}
#'
#' @note
#' An estimation object of class \code{onephase} is mandatory
#' @example examples/example_twophase_estimations_long.R
#'
#' @import tidyr
#' @import stats
#' @import utils
#' @export


# -----------------------------------------------------------------------------#
# FUNCTION STARTS HERE:

estTable<- function(est.list, key.var = "variance", sae=FALSE, format = "long",
                     vartypes=c("variance", "ext_variance", "g_variance")){

  # check input:
  inputclasses<- sapply(est.list, FUN = function(x){class(x)})
  if (!c("onephase") %in% inputclasses){stop("'onephase'-object missing for function estTable()")}
  if (!all(unlist(inputclasses) %in% c("onephase", "twophase", "global", "smallarea"))){
    ndf<- unlist(inputclasses)[!unlist(inputclasses) %in% c("onephase", "twophase", "global", "smallarea")]
    stop(paste("class '", ndf, "' not valid as input for function estTable()", sep = ""))}

  # get number of input objects:
  n.obj<- length(est.list)

  # ---------------------------- #
  # add domain (global or sae), estimator and method as columns to each estimation table:
  for (j in 1:n.obj){

    if (length(class( est.list[[j]])) > 1){
      est.list[[j]]$estimation$domain<- as.factor(paste(as.character(class( est.list[[j]])[1])))
    } else {
      est.list[[j]]$estimation$domain<- as.factor("global")
    }

    if (length(class( est.list[[j]])) > 1){
      est.list[[j]]$estimation$estimator<- as.factor(paste(as.character(class( est.list[[j]])[2])))
    } else{
      est.list[[j]]$estimation$estimator<- as.factor(paste(as.character(class( est.list[[j]])[1])))
    }

    est.list[[j]]$estimation$method <- as.factor(est.list[[j]]$input$method)

  }

  # ---------------------------- #

  if(format == "wide"){

    for (i in 2:n.obj){

      if (i == 2) {dn<- est.list[[1]]$estimation}

      dn<- full_join(x = dn, y = est.list[[i]]$estimation, by = "area")

    }

    return(dn)

  } # end of "wide"-formatting



  # ---------------------------- #
  # restructure dataset for small area objects:

  if(sae & format == "long"){

    for (i in 2:n.obj){

      if (i == 2) {
        dn<- est.list[[1]]$estimation

        if(key.var=="ci"){dn<- merge(dn, confint(est.list[[1]])[["ci"]][,-2], by="area")}
      }

      # calc. confint if requested:
      if(key.var=="ci"){est.list[[i]]$estimation<- merge(est.list[[i]]$estimation, confint(est.list[[i]])[["ci"]][,-2], by="area")}

      # which cols of the  appending d2 do exist in dn?
      ind.dat2<- colnames(est.list[[i]]$estimation) %in% colnames(dn)

      # extend dn by the missing columns:
      if (sum(!ind.dat2) > 0) {
        for (m in 1:sum(!ind.dat2)){
          eval(parse(text=paste("dn$" ,colnames(est.list[[i]]$estimation)[which(!ind.dat2)[m]],"<- NA", sep = "")))
        }
      }

      ind.dat1<- match(colnames(est.list[[i]]$estimation),colnames(dn))

      # expand d2 to rbind to dn:
      d<- data.frame(matrix(data = NA, ncol= ncol(dn), nrow = nrow(est.list[[i]]$estimation)))
      d[, ind.dat1]<- est.list[[i]]$estimation
      colnames(d)<- colnames(dn)

      dn<- rbind(dn, d)

    }

    # Restructure dataframe:
    if(key.var=="variance"){
      cdat<- gather(data = dn, key = vartype, value = variance, ext_variance, g_variance, variance)
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$variance), ]
      # calculate and add estimation error in [%]:
      cdat$std<- sqrt(cdat$variance)
      cdat$error<- cdat$std / cdat$estimate
      cdat$error<- round(100*cdat$error, digits = 2)

      # Reihenfolge (sieht schöner aus):
      my.order<- c("area", "estimate", "vartype", "variance", "std", "error", "domain", "estimator", "method",
                   "n2", "n2G", "n1", "n1G", "n0", "n0G", "r.squared", "r.squared_reduced", "r.squared_full")

      if("area" %in% colnames(cdat)){
        cdat<- cdat[order(cdat$area, cdat$estimator), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      } else {
        cdat<- cdat[order(cdat$estimator), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      }
    }


    if(key.var=="ci"){
      cdat<-  dn %>%
        gather(key, value, ci_lower_op, ci_upper_op, ci_lower_ext, ci_upper_ext, ci_lower_g, ci_upper_g) %>%
        extract(key, c("question", "vartype"), "(c._.....)\\_(.*)") %>%
        spread(question, value)
      cdat$vartype[cdat$vartype=="op"]<- "variance"
      cdat$vartype[cdat$vartype=="ext"]<- "ext_variance"
      cdat$vartype[cdat$vartype=="g"]<- "g_variance"
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$ci_lower), ]
    }

    # round r-squares to 2 digits:
    cdat[,grep("r.squared",colnames(cdat))]<- round(cdat[,grep("r.squared",colnames(cdat))], digits = 4)

    # restrict to certain variance-types:
    cdat<- cdat[cdat$vartype %in% vartypes,]

    class(cdat)<- c("list", "esttable", "smallarea")

    return(cdat)

  } # end of "long"-formatting sae-objects



  # ---------------------------- #
  # ---------------------------- #


  if(!sae & format == "long"){

    for (i in 2:n.obj){

      if (i == 2) {
        dn<- est.list[[1]]$estimation

        if(key.var=="ci"){dn<- cbind(dn, confint(est.list[[1]])[["ci"]][,-1])}
      }

      # calc. confint if requested:
      if(key.var=="ci"){est.list[[i]]$estimation<- cbind(est.list[[i]]$estimation, confint(est.list[[i]])[["ci"]][,-1])}

      # which cols of the  appending d2 do exist in dn?
      ind.dat2<- colnames(est.list[[i]]$estimation) %in% colnames(dn)

      # extend dn by the missing columns:
      if (sum(!ind.dat2) > 0) {
        for (m in 1:sum(!ind.dat2)){
          eval(parse(text=paste("dn$" ,colnames(est.list[[i]]$estimation)[which(!ind.dat2)[m]],"<- NA", sep = "")))
        }
      }

      ind.dat1<- match(colnames(est.list[[i]]$estimation),colnames(dn))

      # expand d2 to rbind to dn:
      d<- data.frame(matrix(data = NA, ncol= ncol(dn), nrow = nrow(est.list[[i]]$estimation)))
      d[, ind.dat1]<- est.list[[i]]$estimation
      colnames(d)<- colnames(dn)

      dn<- rbind(dn, d)

    }

    # Restructure dataframe:
    if(key.var=="variance"){
      cdat<- gather(data = dn, key = vartype, value = variance, ext_variance, g_variance, variance)
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$variance), ]
      # calculate and add estimation error in [%]:
      cdat$std<- sqrt(cdat$variance) / cdat$estimate
      cdat$error<- round(100*cdat$std, digits = 2)

      # Reihenfolge (sieht schöner aus):
      my.order<- c("area", "estimate", "vartype", "variance", "std", "error", "domain", "estimator", "method",
                   "n2", "n2G", "n1", "n1G", "n0", "n0G", "r.squared", "r.squared_reduced", "r.squared_full")

      if("area" %in% colnames(cdat)){
        cdat<- cdat[order(cdat$area, cdat$estimator), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      } else {
        cdat<- cdat[order(cdat$estimator), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      }
    }

    if(key.var=="ci"){
      cdat<-  dn %>%
        gather(key, value, ci_lower_op, ci_upper_op, ci_lower_ext, ci_upper_ext, ci_lower_g, ci_upper_g) %>%
        extract(key, c("question", "vartype"), "(c._.....)\\_(.*)") %>%
        spread(question, value)
      cdat$vartype[cdat$vartype=="op"]<- "variance"
      cdat$vartype[cdat$vartype=="ext"]<- "ext_variance"
      cdat$vartype[cdat$vartype=="g"]<- "g_variance"
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$ci_lower), ]
    }

    # round r-squares to 2 digits:
    cdat[,grep("r.squared",colnames(cdat))]<- round(cdat[,grep("r.squared",colnames(cdat))], digits = 4)

    # restrict to certain variance-types:
    cdat<- cdat[cdat$vartype %in% vartypes,]

    class(cdat)<- c("list", "esttable", "global")

    return(cdat)

  } # end of "long"-formatting global-objects




} # end of function


# -------------------------------------------------------- #
