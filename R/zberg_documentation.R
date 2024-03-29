#' Data from a multiphase forest inventory at the zurichberg (zurich), switzerland
#'
#' A dataset from 1991 containing 1203 sample plots observations from a forest inventory using cluster-sampling.
#' The large phase comprises 298 clusters. Terrestrial information of the stem number as well as the basal area
#' is available for a systematic subsample of 73 clusters. Auxiliary information at all 2103 sample plots were
#' derived by stand maps. Originally the inventory was carried out as a twophase inventory and
#' has been artificially extended to a threephase inventory for demonstration purposes.
#'
#'
#'\itemize{
#'  \item{\code{cluster}     cluster identification. Maximum number of sample plots per cluster is 5.}
#'  \item{\code{phase_id_2p} phase-membership of each observation for the twophase inventory.
#'                           The first phase is indicated by \code{1}, the second (i.e. terrestrial) phase by \code{2}.}
#'  \item{\code{phase_id_3p} the phase-membership of each observation for the threephase inventory,
#'                           i.e. the first phase (\code{0}), the second phase (\code{1})
#'                           and third (terrestrial) phase (\code{2}). \emph{Note:} The threephase sample scheme
#'                           was artificially created for demonstration purposes of the
#'                           \code{\link{threephase}}-functions.}
#'  \item{\code{stade}       development stage at sample plot location based on the stand map.
#'                           Categorical variable of class \code{factor} with 4 \code{levels}.}
#'  \item{\code{melange}     degree of mixture at sample plot location based on the stand map.
#'                           Categorical variable of class \code{factor} with 2 \code{levels}.}
#'  \item{\code{couver}      crown-coverage at sample plot location based on the stand map.
#'                           Categorical variable of class \code{factor} with 2 \code{levels}.}
#'  \item{\code{stem}        stem number derived at field survey.}
#'  \item{\code{basal}       basal area derived at field survey.}
#'  \item{\code{ismallg23}   indicator for small area 2 and 3 for each observation.}
#'  \item{\code{ismallold}   indicator for small area 1 for each observation.}
#'}
#'
#'
#'
#' @format data frame with 1203 rows and 12 columns
#' @source Data provided by D.Mandallaz
#'
#'@references Mandallaz, Daniel (1991). \emph{A unified approach to sampling theory for forest inventory
#'            based on infinite population and superpopulation models.} http://dx.doi.org/10.3929/ethz-a-000585900
#'
#'            Mandallaz, Daniel (1993). \emph{Geostatistical methods for double sampling schemes. application to combined forest inventories.}
#'            Chair of Forest Inventory and Planning, Swiss Federal Institute of Technology (ETH). http://dx.doi.org/10.3929/ethz-a-000943897
#'
"zberg"






