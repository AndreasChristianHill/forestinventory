# Here, we extend the grison-dataset from a two-phase into a three-phase
# dataset
#
# 25.01.2016
# -----------------------------------------------------------------------------#
# get example datasets:


#setwd("R:\\RPackage\\Alex\\2016\\20_01_2016_update_Andreas")

setwd("E:\\20_01_2016_update_Andreas")
# setwd("/Volumes/05_aa/RPackage/Alex/2016/14_01_2016_update_Andreas")

load("example_datasets\\grisons.RData")
# load("example_datasets\\zberg.RData")

# source superfunction "twophase.R":
# source("twophase.R")


# -----------------------------------------------------------------------------#

## 1) grison-dataset for greg with exhaustive component of auxiliary information
#     (that corresponds to the three-phase estimator where the s1-phase (Alex' terminology)
#      is exhaustive)

# --> reduced model: tvol ~ mean
# --> full model:    tvol ~ tvol ~ mean + stddev + max + q75

# the true means of variable mean for the small areas are
truemeans.G<- data.frame(Intercept=rep(1, 4),mean=c(12.85, 12.21, 9.33, 10.45))
rownames(truemeans.G)<- c("A", "B", "C", "D")

# the true mean of variable mean for the global:
truemean.F<- data.frame(Intercept=1,mean=11.37)



# -----------------------------------------------------------------------------#

## 2) grison-dataset for the three-phase estimator where the s1-phase (Alex' terminology)
#      is non-exhaustive)

# We have to create the s1 phase artificially, since this dataset was not a real three-phase dataset
# from the beginning.

# 1) order dataset according to small area
grisons<- grisons[order(grisons$smallarea, grisons$phase_id), ]

# 2) *** change phase_ids to a three-phase scheme: *** #
grisons$phase_id_3p<- rep(c(1,1,2), length.out=nrow(grisons)) # all points are s1, every 3rd plot is also s2

# in order to derive a reasonable sampling fraction of a three-phase inventory,
# we have to reduce the terrestrial samples in the dataset
set.seed(666)
tvol.existing<- which(!is.na(grisons$tvol))
grisons$phase_id_3p[sample(tvol.existing, size = 0.6*67)]<- 3  # terrstrial samples reduced by 40% over entire area

# number s1, s2, s3-points for global estimation
table(grisons$phase_id_3p)
#   1   2   3
# 178  88  40

# number s1, s2, s3-points for small area estimation
table(grisons$smallarea, grisons$phase_id_3p)
#    1  2  3
# A 56 26 12
# B 47 23 11
# C 38 20  8
# D 37 19  9


grisons<- grisons[, c(1,9,2,3,4,5,6,7,8)]
colnames(grisons)[1]<- "phase_id_2p"

save(grisons, file="example_datasets\\grisons_3p.RData")


