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
#' \code{estTable} can be used to compare the results of \code{\link{onephase}} to multiphase estimations
#' (\code{\link{twophase}}, \code{\link{threephase}}). It restructures the estimation results into a table that can
#' be used to plot the estimation results and provides the basis for further analysis.
#'
#'
#' @param est.list a \code{\link[base]{list}} object containing at least one multiphase estimation object created by
#'        the \code{\link{twophase}} or code{\link{threephase}} function and the respective \code{\link{onephase}} estimation object.
#'
#' @param key.var Names of key and value columns to create in output. Can be set to \code{"variance"} (default) or \code{"ci"} for additionally
#'        adding the confidence intervals of the estiamtes.
#'
#' @param sae an object of type \code{\link[base]{logical}}. Has to be set to \code{TRUE} if results of small area estimations are passed to \code{Esttable}.
#'        Defaults to \code{FALSE}.
#'
#' @param format Specifying whether the output table should be in \code{"long"} or \code{"wide"} format.
#'
#' @param vartypes Specifiying the variances that should be included in the estimation table. Has to be specified as a \code{character} vector. The full set is
#'        \code{c("variance", "ext_variance", "g_variance")}.
#'
#'
#' @return \code{estTable} returns a \code{list} of the following components:
#'
#'  \itemize{
#'     \item \code{area:} in case of small area estiamtions: the name of the small area
#'     \item \code{estimate:} the point estimates
#'     \item \code{vartype:} the type of variance
#'     \item \code{variance:} the variance values
#'     \item \code{std:} the standard errors (square root of variance values)
#'     \item \code{error:} the estimation errors defined as \eqn{\frac{standard error}{point estimate}}
#'     \item \code{domain:} indicating if current row belongs to a \code{smallarea} or \code{global} estimation
#'     \item \code{estimator:} the estimator that that was applied
#'     \item \code{method:} the estimation method that was applied
#'     \item \code{n2:} terrestrial sample size in entire inventory area
#'     \item \code{n1:} first phase sample size in entire inventory area
#'     \item \code{n0:} in case of \code{threephase} estimations: zero phase sample size in entire inventory area
#'     \item \code{n1:} first phase sample size in entire inventory area
#'     \item \code{n0:} in case of \code{threephase} estimations: zero phase sample size in entire inventory area
#'     \item \code{n2G:} terrestrial sample size in small area
#'     \item \code{n1G:} first phase sample size in small area
#'     \item \code{n0G:} in case of \code{threephase} estimations: zero phase sample size in small area
#'     \item \code{r.squared: coefficient of determination of regression model}
#'     \item \code{r.squared_reduced:} in case of \code{threephase} estimations: coefficient of determination of reduced regression model
#'     \item \code{r.squared_full:} in case of \code{threephase} estimations: coefficient of determination of full regression model
#'     \item \code{ci_lower:} if \code{key.var="ci"}: lower confidence limit
#'     \item \code{ci_upper:} if \code{key.var="ci"}: upper confidence limit
#'     \item \code{ci_upper:} if \code{key.var="ci"}: upper confidence limit
#'  }
#'
#' @note
#' An estimation object of class \code{onephase} as input is mandatory
#'
#' @example examples/example_estTable.R
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
  if (!all(unlist(inputclasses) %in% c("onephase", "twophase", "threephase", "global", "smallarea"))){
    ndf<- unlist(inputclasses)[!unlist(inputclasses) %in% c("onephase", "twophase", "threephase", "global", "smallarea")]
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
      est.list[[j]]$estimation$method<- as.factor(paste(as.character(class( est.list[[j]])[2])))
    } else{
      est.list[[j]]$estimation$method<- as.factor(paste(as.character(class( est.list[[j]])[1])))
    }

    est.list[[j]]$estimation$estimator <- as.factor(est.list[[j]]$input$method)

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

  ## SAE
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
      cdat<- gather_(data = dn, key_col = "vartype", value_col="variance", gather_cols=c("ext_variance", "g_variance", "variance"))
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$variance), ]
      # calculate and add estimation error in [%]:
      cdat$std<- sqrt(cdat$variance)
      cdat$error<- cdat$std / cdat$estimate
      cdat$error<- round(100*cdat$error, digits = 2)

      # Reihenfolge (sieht schöner aus):
      my.order<- c("area", "estimate", "vartype", "variance", "std", "error", "domain", "method", "estimator",
                   "n2", "n2G", "n1", "n1G", "n0", "n0G", "r.squared", "r.squared_reduced", "r.squared_full")

      if("area" %in% colnames(cdat)){
        cdat<- cdat[order(cdat$area, cdat$method), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      } else {
        cdat<- cdat[order(cdat$method), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      }
    }


    if(key.var=="ci"){
      cdat<- gather_(data = dn, key_col = "vartype", value_col="value",
                     gather_cols=c("ci_lower_op", "ci_upper_op", "ci_lower_ext", "ci_upper_ext", "ci_lower_g", "ci_upper_g"))
      cdat<- extract_(data=cdat, col= "vartype", into=c("question","vartype"), regex="(c._.....)\\_(.*)")
      cdat<- spread_(data = cdat, key_col="question", value_col = "value")

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

  ## NOT SAE
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
      cdat<- gather_(data = dn, key_col = "vartype", value_col="variance", gather_cols=c("ext_variance", "g_variance", "variance"))
      cdat$vartype<- as.factor(cdat$vartype)
      cdat<- cdat[!is.na(cdat$variance), ]
      # calculate and add estimation error in [%]:
      cdat$std<- sqrt(cdat$variance) / cdat$estimate
      cdat$error<- round(100*cdat$std, digits = 2)

      # Reihenfolge (sieht schöner aus):
      my.order<- c("area", "estimate", "vartype", "variance", "std", "error", "domain", "method", "estimator",
                   "n2", "n2G", "n1", "n1G", "n0", "n0G", "r.squared", "r.squared_reduced", "r.squared_full")

      if("area" %in% colnames(cdat)){
        cdat<- cdat[order(cdat$area, cdat$method), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      } else {
        cdat<- cdat[order(cdat$method), na.omit(match( my.order, names(cdat)))]
        rownames(cdat)<- seq(1:nrow(cdat))
      }
    }

    if(key.var=="ci"){
      cdat<- gather_(data = dn, key_col = "vartype", value_col="value",
                     gather_cols=c("ci_lower_op", "ci_upper_op", "ci_lower_ext", "ci_upper_ext", "ci_lower_g", "ci_upper_g"))
      cdat<- extract_(data=cdat, col= "vartype", into=c("question","vartype"), regex="(c._.....)\\_(.*)")
      cdat<- spread_(data = cdat, key_col="question", value_col = "value")

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








