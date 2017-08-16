
## run onephase estimation:
op.a <- onephase(formula = tvol~1,
                 data = grisons,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D")))

## run small area twophase estimation:
sae.2p.est <- twophase(formula = tvol ~ mean + stddev + max + q75,
                       data = grisons,
                       phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                       small_area = list(sa.col = "smallarea", areas = c("A", "B","C", "D"),
                                         unbiased = TRUE))

sae.2p.est2 <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                          unbiased = TRUE),
                        psmall = TRUE)


## create estimation table:
sae.table<- estTable(est.list = list(op.a, sae.2p.est, sae.2p.est2),
                     sae = TRUE, vartypes = c("variance", "g_variance",  "ext_variance"))


# validate estimation results:
mphase.gain(sae.table)
