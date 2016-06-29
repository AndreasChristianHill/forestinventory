# Here, we extend the zberg-dataset from a two-phase into a three-phase
# dataset
#
# 29.01.2016
# -----------------------------------------------------------------------------# 
# get example datasets:


#setwd("R:\\RPackage\\Alex\\2016\\20_01_2016_update_Andreas")

setwd("E:\\RPackage\\Alex\\2016\\29_01_2016_update_Andreas")
# setwd("/Volumes/05_aa/RPackage/Alex/2016/14_01_2016_update_Andreas")

load("example_datasets\\zberg.RData")

# source superfunction "twophase.R":
# source("twophase.R")


# -----------------------------------------------------------------------------# 

## 2) zberg-dataset for the three-phase estimator where the s0-phase (Alex' terminology) 
#      is non-exhaustive)

# We have to create the s0 phase artificially, since this dataset was not a real three-phase dataset 
# from the beginning.

# 1) order dataset according to small area 
zberg.<- zberg[order(zberg$ismallold, zberg$iground), ]

# get cluster ids:
clust.ids<- as.numeric(as.character(unique(zberg.$cluster)))

# make all plots to s0, every third plot s1:
phase.id.3p<- rep(c(0,0,1), length.out=length(clust.ids))

cbind(clust.ids, phase.id.3p)


# which clustids belong to the terrestrial sample?
terr<- numeric(length(clust.ids))
for (i in 1: length(clust.ids)){
  terr[i]<- ifelse(any(zberg$iground[ zberg$cluster == clust.ids[i] ] == 2), TRUE, FALSE)
}
  
aidinfo<- data.frame(cbind(clust.ids, phase.id.3p, terr))


# make (at least some) of the original terrestrial clusters to s2:
set.seed(666)
aidinfo$phase.id.3p[ sample(which(aidinfo$terr==1), size = round(0.6*length(which(aidinfo$terr==1)))) ]<- 2


aidinfo<- aidinfo[,-3]
colnames(aidinfo)<- c("cluster", "phase_id_3p")

zberg.<- merge(zberg, aidinfo, by = "cluster")


# check samplesizes:
table(zberg.$phase_id_3p)
#   0   1   2 
# 691 333 179 

table(zberg.$ismallg23, zberg.$phase_id_3p)
#     0   1   2
# 2 117  47  21
# 3 156  67  27

table(zberg.$ismallold, zberg.$phase_id_3p)
#     0   1   2
# 1  50  26  16

# --> scheint ok


save(zberg., file="example_datasets\\zberg_3p.RData")



