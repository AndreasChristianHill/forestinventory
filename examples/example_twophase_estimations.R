# -----------------------------------------------#
# ----------- global estimation------------------#
# -----------------------------------------------#

## load datasets:
data(grisons)
data(zberg)

## 1) Global two-phase estimation with non-exhaustive first-phase:
glob.nexh.2p <-  twophase(formula = tvol ~mean + stddev + max + q75,
                         data = grisons,
                         phase_id = list(phase.col="phase_id_2p", terrgrid.id=2))
summary(glob.nexh.2p)
confint(glob.nexh.2p)

## 2) Global two-phase estimation with non-exhaustive first-phase (cluster example):
glob.nexh.clust.2p <- twophase(formula=basal ~ stade + couver + melange,
                               data=zberg,
                               phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                               cluster="cluster")
summary(glob.nexh.clust.2p)
confint(glob.nexh.clust.2p)

## 3) Global two-phase estimation with exhaustive first-phase:

  #establish order for vector of true auxiliary means
colnames(lm(formula=tvol ~ mean + stddev + max + q75, data=grisons, x=TRUE)$x)

  #independently calculate auxiliary vector of true auxiliary means for the above
  #variables including potential boundary weight adjustment and save them
  #in a vector in that order
true.means <- c(1, 11.39, 8.84, 32.68, 18.03)

  #calculate two-phase estimator
glob.exh.2p <- twophase(formula=tvol ~ mean + stddev + max + q75,
                        data=grisons,
                        phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        exhaustive=true.means)
summary(glob.exh.2p)
confint(glob.exh.2p)

## 4) Global two-phase estimation with manual boundary weight adjustment (cluster example):
glob.nexh.clust.BW.2p <- twophase(formula=tvol ~ mean + stddev + max + q75,
                                  data=grisons,
                                  phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                                  boundary_weights = "boundary_weights")
summary(glob.nexh.clust.BW.2p)
confint(glob.nexh.clust.BW.2p)


# ---------------------------------------------------#
# ----------- small-area estimation------------------#
# ---------------------------------------------------#

