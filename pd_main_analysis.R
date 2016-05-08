############################ MAIN PART ####################
### R-Code for paper: Real-time Prediction of User Performance
###cbased on Pupillary Assessment via Eye-Tracking
### Code by Sebastian Sauer
###########################################################



#### the main part builds on externalized functions
source(pdFunctions.R)


### init the analysis

pdInit() # load libraries etc.


### set file/path names etc.





########## read and compress data ###############################

pd.lr <- vector(mode = "list", length = 2)
pd.lr.2 <- vector(mode = "list", length = 2)

pd.lr.2 <- pdReadCompress(transform.stat = "mean", filter.raw = TRUE)




### cut-out dataframes for each eye
pd.l <- as.data.frame(pd.lr[[1]])
pd.r <- as.data.frame(pd.lr[[2]])




### filter to exclude outliers ##########################

pd.l.filtered <- pdFilter(pd.l)
pd.r.filtered <- pdFilter(pd.r)


setwd(path.analysis)
save(pd.l.filtered, pd.r.filtered, file = "pd.lr.filtered.Rda")



### performance matching ##################################

pd.l.perf <- performanceMatching(pd.l.filtered)
pd.r.perf <- performanceMatching(pd.r.filtered)

#### check and save #####
save(pd.l.perf, pd.r.perf, file = "pd.lr.perf.Rda")




#### make-up data frames, calculate score-groups###########

# with 3 values as outcome variable!
pd.l.makeup <- pdMakeUp(pd.l.perf)
pd.r.makeup <- pdMakeUp(pd.r.perf)
save(pd.l.makeup, pd.r.makeup, file = "pd.lr.makeup.Rda")


# check and plot missings
pMissings <- pdDrawMissings(pd.l.makeup, pd.r.makeup)
ggsave(pMissings, file = "pMissings.jpg", width = 9, height = 6)


# check and plot pupil diameter variation (sd in pupil diameter per person)
p.PDV <- pdDrawSDs(pd.l.makeup, pd.r.makeup)
ggsave(p.PDV, file = "pPDV.jpg", width = 9, height = 6)


#z-scale data
pd.l.makeup.z <- pdZScale(pd.l.makeup)
pd.r.makeup.z <- pdZScale(pd.r.makeup)

pd.l.makeup.z.nona <- pdZScale(pd.l.makeup.nona)
pd.r.makeup.z.nona <- pdZScale(pd.r.makeup.nona)




setwd(path.project)
save(pd.l.makeup, pd.r.makeup,
     pd.l.makeup.nona,
     pd.r.makeup.nona,
     file = "pd.lr.makeup.RData")
save(pd.l.makeup.z, pd.r.makeup.z,
     pd.l.makeup.z.nona, pd.r.makeup.z.nona,
     file = "pd.lr.makeup.z.RData")


load(file = "pd.lr.makeup.z.RData")
load(file = "pd.lr.makeup.RData")





############ Predictive Model: Random Forests #############################


# left eye

# raw pupil sized ~ 3min. (classification)
rf1.l.4.err <- pdRF(pd.l.makeup, pd.n.groups = 4) # 4 groups
rf1.l.3.err <- pdRF(pd.l.makeup, pd.n.groups = 3)
rf1.l.2.err <- pdRF(pd.l.makeup, pd.n.groups = 2)

rf1.l.3.err.nona <- pdRF(pd.l.makeup.nona, pd.n.groups = 3)
rf1.l.2.err.nona <- pdRF(pd.l.makeup.nona, pd.n.groups = 2)



# z-standardized
rf1.l.z.4.err <- pdRF(pd.l.makeup.z, pd.n.groups = 4)
rf1.l.z.3.err <- pdRF(pd.l.makeup.z, pd.n.groups = 3)
rf1.l.z.2.err <- pdRF(pd.l.makeup.z, pd.n.groups = 2)

rf1.l.z.3.err.nona <- pdRF(pd.l.makeup.z.nona, pd.n.groups = 3)
rf1.l.z.2.err.nona <- pdRF(pd.l.makeup.z.nona, pd.n.groups = 2)



setwd(path.project)
save(rf1.l.4.err, rf1.l.3.err, rf1.l.2.err,
     rf1.l.z.4.err, rf1.l.z.3.err, rf1.l.z.2.err,
     rf1.l.3.err.nona,
     rf1.l.z.3.err.nona,
     rf1.l.z.2.err.nona,
     rf1.l.2.err.nona,
     file = "rf1.l.Rda")
#load("rf1.l.Rda")


# show overall error rate
mean(rf1.l.4.err[[1]], na.rm = T)
mean(rf1.l.3.err[[1]], na.rm = T)
mean(rf1.l.2.err[[1]], na.rm = T)


mean(rf1.l.z.3.err[[1]], na.rm = T)
mean(rf1.l.z.2.err[[1]], na.rm = T)





# right eye -- raw pupil size
rf1.r.4.err <- pdRF(pd.r.makeup, pd.n.groups = 4)  # 4 groups/score classes
rf1.r.3.err <- pdRF(pd.r.makeup, pd.n.groups = 3)
rf1.r.2.err <- pdRF(pd.r.makeup, pd.n.groups = 2)

rf1.r.3.err.nona <- pdRF(pd.r.makeup.nona, pd.n.groups = 3)
rf1.r.2.err.nona <- pdRF(pd.r.makeup.nona, pd.n.groups = 2)



# z-standardized
rf1.r.z.4.err <- pdRF(pd.r.makeup.z, pd.n.groups = 4)
rf1.r.z.3.err <- pdRF(pd.r.makeup.z, pd.n.groups = 3)
rf1.r.z.2.err <- pdRF(pd.r.makeup.z, pd.n.groups = 2)

rf1.r.z.3.err.nona <- pdRF(pd.r.makeup.z.nona, pd.n.groups = 3)
rf1.r.z.2.err.nona <- pdRF(pd.r.makeup.z.nona, pd.n.groups = 2)


setwd(path.project)
save(rf1.r.4.err, rf1.r.3.err, rf1.r.2.err,
     rf1.r.z.4.err, rf1.r.z.3.err, rf1.r.z.2.err,
     rf1.r.3.err.nona,
     rf1.r.z.3.err.nona,
     rf1.r.z.2.err.nona,
     rf1.r.2.err.nona,
     file = "rf1.r.Rda")
#load("rf1.r.Rda")


# save confusion matrices
save(rf1.l.z.3.err, rf1.r.z.3.err,
     file = "confusion_matrices_z_3groups.Rda")


# show overall error rate
mean(rf1.r.err.z[[1]], na.rm = T)
mean(rf1.r.z.3.err[[1]], na.rm = T)  # ~.540 error rate
mean(rf1.r.z.2.err[[1]], na.rm = T)  # ~.465 error rate




# compare error rates to outcome distribution

outcome_distribution(pd.l.makeup$sum.correct)




# RF regression ####

# raw data
rf1.l.3.regr.err <- pdRF(pd.l.makeup, pd.n.groups = 3, type = "regression")
rf1.r.3.regr.err <- pdRF(pd.r.makeup, pd.n.groups = 3, type = "regression")

# z-data
rf1.l.z.3.regr.err <- pdRF(pd.l.makeup.z, pd.n.groups = 3, type = "regression")
rf1.r.z.3.regr.err <- pdRF(pd.r.makeup.z, pd.n.groups = 3, type = "regression")


setwd(path.project)
save(rf1.l.3.regr.err, rf1.r.3.regr.err, file = "rf1.lr.3.regr.err.Rda")
save(rf1.l.z.3.regr.err, rf1.r.z.3.regr.err, file = "rf1.lr.z.3.regr.err.Rda")

load(rf1.lr.z.3.regr.err.Rda)
load(rf1.lr.3.regr.err.Rda)

############ Plot Random Forests #############################


# Raw data regression
p.rf.3.regr.comp <- pdDrawRFErr(rf1.l.3.regr.err[[1]], rf1.l.3.regr.err[[1]],
                                pd.randomness.line = .67)  # raw data regression
p.rf.3.regr.comp
ggsave(p.rf.3.regr.comp, file = "p.rf.3.regr.comp.pdf", width = 297, height = 210, units = "mm", dpi = 300)


# Raw data classification
p.rf.3.comp <- pdDrawRFErr(rf1.l.3.err[[1]], rf1.l.3.err[[1]],
                                pd.randomness.line = .67)  # raw data regression
p.rf.3.comp
ggsave(p.rf.3.comp, file = "p.rf.3.comp.jpg", width = 9, height = 6)


p.rf.3.comp.nona <- pdDrawRFErr(rf1.l.3.err.nona[[1]], rf1.r.3.err.nona[[1]],
                           pd.randomness.line = .67,
                           pdymin = .48,
                           pdymax = .54,
                           pd.title = "Groups: 3, Data: raw")  # raw data regression

p.rf.3.comp.nona
ggsave(p.rf.3.comp.nona, file = "p.rf.3.comp.nona.jpg", width = 9, height = 6)


# z-data classification
p.rf.z.3.comp <- pdDrawRFErr(rf1.l.z.3.err[[1]], rf1.r.z.3.err[[1]],
                         pd.randomness.line = .67)
p.rf.z.3.comp
ggsave(p.rf.z.3.comp, file = "p.rf.z.3.comp.jpg", width = 9, height = 6)


p.rf.z.3.comp.nona <- pdDrawRFErr(rf1.l.z.3.err.nona[[1]], rf1.r.z.3.err.nona[[1]],
                             pd.randomness.line = .67,
                             pdymin = .57,
                             pdymax = .62,
                             pd.title = "Groups: 3, Data: z-normalized")
p.rf.z.3.comp.nona
ggsave(p.rf.z.3.comp.nona, file = "p.rf.z.3.comp.nona.jpg", width = 9, height = 6)





p.rf.2.comp.nona <- pdDrawRFErr(rf1.l.2.err.nona[[1]], rf1.r.2.err.nona[[1]],
                                  pd.randomness.line = .50,
                                  pd.title = "Groups: 2, Data: raw")
p.rf.2.comp.nona
ggsave(p.rf.2.comp.nona, file = "p.rf.2.comp.nona.jpg", width = 9, height = 6)





p.rf.z.2.comp <- pdDrawRFErr(rf1.l.z.2.err[[1]], rf1.r.z.2.err[[1]],
                             pd.randomness.line = .50)
p.rf.z.2.comp
ggsave(p.rf.z.2.comp, file = "p.rf.z.2.comp.pdf", width = 9, height = 6)
ggsave(p.rf.z.2.comp, file = "p.rf.z.2.comp.jpg", width = 9, height = 6)



p.rf.z.2.comp.nona <- pdDrawRFErr(rf1.l.z.2.err.nona[[1]], rf1.r.z.2.err.nona[[1]],
                             pd.randomness.line = .50,
                            pd.title = "Groups: 2, Data: z-normalized")
p.rf.z.2.comp.nona
ggsave(p.rf.z.2.comp.nona, file = "p.rf.z.2.comp.nona.jpg", width = 9, height = 6)


# z-data regression
p.rf.z.comp.regr <- pdDrawRFErr(rf1.l.z.3.regr.err[[1]], rf1.r.z.3.regr.err[[1]],
                         pd.randomness.line = .67)  # z-data regression
p.rf.z.comp.regr
ggsave(p.rf.z.comp.regr, file = "p.rf.z.3.comp.regr.pdf", width = 297,
       height = 210, units = "mm", dpi = 300)



############ Plot Confusion Matrix for Random Forests #############################
p.Confusion.z <- pdPlotConfusion(rf1.l.z.3.err, rf1.r.z.3.err)
p.Confusion.z.nona <- pdPlotConfusion(rf1.l.z.3.err.nona, rf1.r.z.3.err.nona)
p.Confusion.nona <- pdPlotConfusion(rf1.l.3.err.nona, rf1.r.3.err.nona,
                                    pd.title = "Groups: 3; Data: raw")

ggsave(p.Confusion.z, file = "p.Confusion.z.jpg", width = 9, height = 5)
ggsave(p.Confusion.z.nona, file = "p.Confusion.z.nona.jpg", width = 9, height = 5)
ggsave(p.Confusion.nona, file = "p.Confusion.nona.jpg", width = 9, height = 5)


ggsave(p.Confusion.z, file = "p.Confusion.z.tiff", width = 9, height = 5)
ggsave(p.Confusion.z, file = "p.Confusion.z.pdf", width = 9, height = 5)

p.Confusion.z.regr <- pdPlotConfusion(rf1.l.z.3.regr.err, rf1.r.z.3.regr.err)

p.Confusion.z.2 <- pdPlotConfusion(rf1.l.z.2.err, rf1.r.z.2.err, type = "two")

p.Confusion.2.nona <- pdPlotConfusion(rf1.l.2.err.nona, rf1.r.2.err.nona,
                                   type = "two",
                                   pd.title = "Groups: 2, Data: raw")
ggsave(p.Confusion.2.nona, file = "p.Confusion.2.nona.jpg", width = 9, height = 5)

setwd(path.project)
ggsave(p.Confusion.z.2, file = "p.Confusion2.z.jpg", width = 9, height = 5)
ggsave(p.Confusion.z.2, file = "p.Confusion2.z.tiff", width = 9, height = 5)
ggsave(p.Confusion.z.2, file = "p.Confusion2.z.pdf", width = 9, height = 5)


################### Draw Time Series ######################

pdTSGrid(melt_groups = TRUE)
