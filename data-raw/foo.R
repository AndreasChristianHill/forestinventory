



load("example_datasets\\grisons_3p.RData")
load("example_datasets\\zberg_3p.RData")


#  -------------------------------------------------------------------------- #


# change phase_id_3p to Daniel's Notation: 3 --> 2; 2 --> 1, 1 --> 0

# grisons$phase_id_3p[grisons$phase_id_3p==1]<- 0
# grisons$phase_id_3p[grisons$phase_id_3p==2]<- 1
# grisons$phase_id_3p[grisons$phase_id_3p==3]<- 2
# 
# grisons$tvol.3p<- grisons$tvol
# grisons$tvol.3p[grisons$phase_id_3p %in% c(0, 1)]<- NA
# 
# table(grisons$phase_id_3p)
# #   0   1   2 --> phase
# # 178  88  40 --> no plots
# 
# table(grisons$phase_id_2p)
# #   1   2  --> phase
# # 239  67  --> no plots
# 
# save(grisons, file = "example_datasets/grisons_3p.RData")

#  -------------------------------------------------------------------------- #
#  -------------------------------------------------------------------------- #

# -------------------------------------------------- #
# create case where n2G = 0 for three-phase example  #
# -------------------------------------------------- #


# *** for grisons-dataset *** #

# situation so far:
table(grisons$smallarea, grisons$phase_id_3p)
#    0  1  2 --> phases
# A 56 26 12
# B 47 23 11
# C 38 20  8
# D 37 19  9


# now turn small area D into a small area "D" that has no n2G:

# create new columns for phase_id and tvol for this special case:
grisons$tvol.3p_nG0<- grisons$tvol.3p
grisons$phase_id_3p_nG0<- grisons$phase_id_3p

# set all local densities to NA
grisons[grisons$smallarea== "D", "tvol.3p_nG0"]<- NA 

# change phase_id "2" to "0" or "1", whereas "0" is twice as likely as "1":
grisons[grisons$smallarea == "D" & grisons$phase_id_3p==2, "phase_id_3p_nG0"]<- sample(c(0,1), 
                                                                                size = nrow(grisons[grisons$smallarea == "D" & grisons$phase_id_3p==2, ]), 
                                                                                replace = T, prob = c(2,1))


# -------------------------------------------------- #
# create case where n2G = 1 for three-phase example  #
# -------------------------------------------------- #

# now turn small area D into a small area "D" that has n2G = 1:

# create new columns for phase_id and tvol for this special case:
grisons$tvol.3p_nG1<- grisons$tvol.3p
grisons$phase_id_3p_nG1<- grisons$phase_id_3p_nG0

# set all local densities to NA EXCEPT ONE:
ind.settoNA<- sample(x = which(!is.na(grisons[grisons$smallarea== "D", "tvol.3p_nG1"])), 
                     size =  length(which(!is.na(grisons[grisons$smallarea== "D", "tvol.3p_nG1"]))) - 1)
grisons[grisons$smallarea== "D", "tvol.3p_nG1"][ind.settoNA] <- NA

# change phase_id of terrestrial value to "2":
grisons[grisons$smallarea== "D", "phase_id_3p_nG1"] [!is.na(grisons[grisons$smallarea== "D", "tvol.3p_nG1"])]<- 2



grisons<- grisons[, c(1,2,12,14,3,4,5,6,7,8,9,10,11,13)]



save(grisons, file = "example_datasets/grisons_3p.RData")

#  -------------------------------------------------------------------------- #
#  -------------------------------------------------------------------------- #

# 02.03.2016

load("example_datasets\\grisons_3p.RData") # grisons. (3p)
load("example_datasets\\grisons.RData")    # grisons  (2p) 

load("example_datasets\\zberg_3p.RData")   # zberg.   (3p)
load("example_datasets\\zberg.RData")      # zberg    (2p)


grisons<- grisons.
save(grisons, file = "example_datasets/grisons.RData")


colnames(zberg.)<- tolower(colnames(zberg.))
colnames(zberg.)[4]<- "phase_id_2p"

zberg<- zberg.[,c(1,2,3,4,13,5,6,7,8,9,10, 11,12)]
save(zberg, file = "example_datasets/zberg.RData")



































