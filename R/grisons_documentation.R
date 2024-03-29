#' Data from a multiphase forest inventory in the canton of Grisons, Switzerland
#'
#' A dataset containing observations of 306 systematically arranged sample plots.
#' Auxiliary information for all 306 plots is provided in the form of LiDAR canopy height metrics.
#' For a systematic subsample of 67 out of the 306 plots, terrestrial information of the timber volume
#' is provided from a terrestrial survey in the year 2007. Originally the inventory was carried out as a twophase inventory and
#' has been artificially extended to a threephase inventory for demonstration purposes.
#'
#'\itemize{
#'  \item{\code{phase_id_2p} phase-membership of each observation for the twophase inventory.
#'                           The large phase is indicated by \code{1}, the terrestrial phase by \code{2}.}
#'  \item{\code{phase_id_3p} phase-membership of each observation for the threephase inventory,
#'                           i.e. the largest phase (\code{0}), the large phase (\code{1})
#'                           and terrestrial phase (\code{2}). \emph{Note:} The threephase sample scheme
#'                           was artificially created for demonstration purposes of the
#'                           \code{\link{threephase}}-functions.}
#'  \item{\code{boundary_weights} proportion of analysis-window for auxiliary information lying within the forest.}
#'  \item{\code{mean} mean canopy height at the sample location based on the LiDAR canopy height model.}
#'  \item{\code{stddev} standard deviation of the LiDAR canopy height model at the sample location.}
#'  \item{\code{max} maximum value of the LiDAR canopy height model at the sample location.}
#'  \item{\code{q75} 75\%-Quantile of the LiDAR canopy height model at the sample location.}
#'  \item{\code{smallarea} smallarea-indicator for each observation.}
#'  \item{\code{tvol} terrestrial timber volume from field survey. Use for \code{\link{twophase}}-inventory.}
#'  \item{\code{tvol.3p} terrestrial timber volume from field survey. Use for \code{\link{threephase}}-inventory.}
#'}
#'
#'
#'@note There are additional columns in \code{grisons} to demonstrate the function-behaviors
#'      for special cases which might occur in a forest inventory
#'\itemize{
#'  \item{\code{phase_id_3p_nG0} one of the smallareas does not contain any terrestrial observation.}
#'  \item{\code{phase_id_3p_nG1} one of the smallareas does contain only a single terrestrial observation.}
#'  \item{\code{tvol.3p_nG0} Use as response variable to test \code{phase_id_3p_nG0} for \code{\link{threephase}}-inventory.}
#'  \item{\code{tvol.3p_nG1} Use as response variable to test \code{phase_id_3p_nG1} for \code{\link{threephase}}-inventory.}
#'}
#'
#' We leave testing these special cases to the user.
#'
#' @format data frame with 306 rows and 14 columns
#' @source The terrestrial data are kindly provided by the forest service of the canton grisons.
#'
#'         The dataset was created and used within the framework of the publications listed under \emph{References}.
#'
#'@references Mandallaz, D., Breschan, J., & Hill, A. (2013). \emph{New regression estimators in forest inventories
#'           with two-phase sampling and partially exhaustive information:
#'           a design-based monte carlo approach with applications to small-area estimation.}
#'           Canadian Journal of Forest Research, 43(11), 1023-1031.
#'
#'           Hill, A., Breschan, J., & Mandallaz, D. (2014). \emph{Accuracy assessment of timber
#'           volume maps using forest inventory data and LiDAR canopy height models.}
#'           Forests, 5(9), 2253-2275.
#'
"grisons"
