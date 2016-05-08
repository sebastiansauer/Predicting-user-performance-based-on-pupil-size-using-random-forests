#### Functions of Pupil Diameter Paper(s)
#### As of: 2015 May
### Sebastian Sauer


#-------------------------------------------------------------------------------
pdInit <- function(read.raw.data = FALSE){

  library(plyr)
  library(stringr)
  library(ggplot2)
  library(reshape)
  library(randomForest)
  library(dplyr)
  library(tidyr)
  seed <- 42
  set.seed(seed)


  path.data = paste("~/Documents/OneDrive/Forschung/",
                    "Mental_Workload/HICCS_Followup_Paper/LinkedIN Versuch/",
                    "Data/Eye_tracking_data", sep = "")

  path.project = paste("~/Documents/OneDrive/Forschung/Mental_Workload/",
                       "HICCS_Followup_Paper/LinkedIN Versuch", sep = "")

  path.performance <- paste("~/Documents/OneDrive/Forschung/Mental_Workload/",
                            "HICCS_Followup_Paper/LinkedIN Versuch/Data/Performance_data",
                            sep = "")


  # name of file with performance data
  performance.file <- "task_performance_n125_v03.csv"


}


#-------------------------------------------------------------------------------
pdReadCompress <- function(transform.stat = "mean",
                           desired.length = 1000,
                           filter.raw = FALSE)
{
  # Reads pupil diameter (pd) data files (n=125)
  # and compresses the data


  require(stringr)


  # list raw data files [1.5 GB]
  file.list <- list.files(path = path.data)


  #set working directory to data path
  setwd(path.data)
  print(getwd())



  # initialize index variable
  i <- 1
  j <- 1
  x <- 1 # initialize dummy variable for pupil diameter data
  # define length of compressed data vector

  comp.vec <- vector() # vector of values which will be compressed to one value


  #pd.l <- NULL # initialize data.frame for pupil diameter data - left eye
  comp.factor <- NULL

  pdv.l.short <- as.data.frame(matrix(ncol = 1007, nrow = length(file.list)))
  pdv.r.short <- as.data.frame(matrix(ncol = 1007, nrow = length(file.list)))


  #count.na.l <- NULL

  len <- NULL # only to initialize the variable

  names(pdv.l.short)[desired.length + 1] <- "pd.mean"
  names(pdv.l.short)[desired.length + 2] <- "ID"
  names(pdv.l.short)[desired.length + 3] <- "count.na"
  names(pdv.l.short)[desired.length + 4] <- "mean.raw"
  names(pdv.l.short)[desired.length + 5] <- "md.raw"
  names(pdv.l.short)[desired.length + 6] <- "mean.sd.raw"
  names(pdv.l.short)[desired.length + 7] <- "md.sd.raw"

  names(pdv.r.short)[desired.length + 1] <- "pd.mean"
  names(pdv.r.short)[desired.length + 2] <- "ID"
  names(pdv.r.short)[desired.length + 3] <- "count.na"
  names(pdv.r.short)[desired.length + 4] <- "mean.raw"
  names(pdv.r.short)[desired.length + 5] <- "md.raw"
  names(pdv.r.short)[desired.length + 6] <- "mean.sd.raw"
  names(pdv.r.short)[desired.length + 7] <- "md.sd.raw"



  ###### read the pupil data and compress it to data frame ####

  for (i in 1:length(file.list)){
    dummy <- read.csv2(file.list[i], sep = "\t", as.is = TRUE, fill = TRUE,
                       strip.white = TRUE, skipNul = TRUE)



    # if desired, exlude values that exceed biological maxima of dilation/contraction
    if (filter.raw == TRUE) {
      dummy <- pdFilterRaw(dummy)
    }




    # double size to come from radius to diameter
    dummy$PupilSizeLeft <- dummy$PupilSizeLeft * 2
    dummy$PupilSizeRight <- dummy$PupilSizeRight * 2


    dummy$PupilSizeLeft[dummy$GazeVectorFound == 0] <- NA # set pupil size to NA if gaze vector == 0
    dummy$PupilSizeRight[dummy$GazeVectorFound == 0] <- NA

    cat(i,length(file.list),"LEFT: No of NA:",sum(is.na(dummy$PupilSizeLeft)),
        " - No of non-NA:",sum(!is.na(dummy$PupilSizeLeft)), "\n")
    cat(i,length(file.list),"RIGHT: No of NA:",sum(is.na(dummy$PupilSizeRight)),
        " - No of non-NA:",sum(!is.na(dummy$PupilSizeRight)), "\n")



    #### compression to desired length of vector - independently of original length
    # compress factor in order to arrive at vector of desired length 1000
    comp.factor[i] <- trunc(nrow(dummy) / desired.length)

    len <- trunc(nrow(dummy) / comp.factor[i]) # coerce vector into vector of given length


    # Compress values based on *median*, next few lines
    # left eye
    for (j in 1:desired.length){
      comp.vec <- dummy$PupilSizeLeft[((j-1) *
                                         comp.factor[i] + 1):(j * comp.factor[i])]
      if (transform.stat == "IQR") pdv.l.short[i,j] <- IQR(comp.vec, na.rm = TRUE)
      if (transform.stat == "median") pdv.l.short[i,j] <- median(comp.vec, na.rm = TRUE)
      if (transform.stat == "mean") pdv.l.short[i,j] <- mean(comp.vec, na.rm = TRUE)

    }

    # right eye
    for (j in 1:desired.length){
      comp.vec <- dummy$PupilSizeRight[((j-1) *
                                          comp.factor[i] + 1):(j * comp.factor[i])]
      if (transform.stat == "IQR") pdv.r.short[i,j] <- IQR(comp.vec, na.rm = TRUE)
      if (transform.stat == "median") pdv.r.short[i,j] <- median(comp.vec, na.rm = TRUE)
      if (transform.stat == "mean") pdv.r.short[i,j] <- mean(comp.vec, na.rm = TRUE)

    }

    #pd.l.short[1,]


    # add rowmean of medians for present case

    pdv.l.short[i,"pd.mean"] <- rowMeans(pdv.l.short[i,1:1000], na.rm = TRUE)
    pdv.r.short[i,"pd.mean"] <- rowMeans(pdv.r.short[i,1:1000], na.rm = TRUE)

    setwd(path.data)
    # assign name of person/ID to vector of data.frame
    pdv.l.short[i,"ID"] <- str_sub(file.list[i],1,str_locate(file.list[i],"id")[1]-3)
    pdv.r.short[i,"ID"] <- str_sub(file.list[i],1,str_locate(file.list[i],"id")[1]-3)

    # count No of NAs
    pdv.l.short[i,"count.na"] <- sum(is.na(dummy$PupilSizeLeft))
    pdv.r.short[i,"count.na"] <- sum(is.na(dummy$PupilSizeRight))


    #pd.l.short[i,j+2]

    # calculate mean,md, sd
    ## left eye
    pdv.l.short$mean.raw[i] <- mean(dummy$PupilSizeLeft, na.rm = TRUE)
    pdv.l.short$md.raw[i] <- median(dummy$PupilSizeLeft, na.rm = TRUE)

    pdv.l.short$mean.sd.raw[i] <- sd(dummy$PupilSizeLeft, na.rm = TRUE)
    pdv.l.short$md.sd.raw[i] <- sd(dummy$PupilSizeLeft, na.rm = TRUE)


    ## right eye
    pdv.r.short$mean.raw[i] <- mean(dummy$PupilSizeRight, na.rm = TRUE)
    pdv.r.short$md.raw[i] <- median(dummy$PupilSizeRight, na.rm = TRUE)

    pdv.r.short$mean.sd.raw[i] <- sd(dummy$PupilSizeRight, na.rm = TRUE)
    pdv.r.short$md.sd.raw[i] <- sd(dummy$PupilSizeRight, na.rm = TRUE)



    # print row PDV mean for present case
    cat(i, length(file.list), "LEFT: row mean of", transform.stat, "for",
        pdv.l.short[i,"ID"],"is", pdv.l.short[i,"pd.mean"],"\n")

    cat(i, length(file.list), "RIGHT: row mean of", transform.stat, "for",
        pdv.r.short[i,"ID"],"is", pdv.r.short[i,"pd.mean"],"\n")
  }


  pdv.short.list <- list(pdv.l.short,pdv.r.short)
  return(pdv.short.list)
}



#-------------------------------------------------------------------------------
pdFilter <- function(pd.data.frame, pd.max = 6, pd.replace = NA, pd.start.col = 1,
                     pd.stop.col = 1000, exclude.outliers = TRUE){
  # ## kick out all values > 6 mm
  # returns df without extreme values


  for (i in pd.start.col:pd.stop.col){
    pd.data.frame[i][pd.data.frame[i] > 6] <- NA
  }
  #pd.data.frame[pd.data.frame > 6] <- NA # this one would kill string vars!

  if (exclude.outliers == TRUE){
    pd.data.frame$pd.mean[pd.data.frame$pd.mean >
                            3*sd(pd.data.frame$pd.mean, na.rm = T)] <- NA
    pd.data.frame$mean.raw[pd.data.frame$mean.raw >
                             3*sd(pd.data.frame$mean.raw, na.rm = T)] <- NA
    pd.data.frame$mean.sd.raw[pd.data.frame$mean.sd.raw >
                                3*sd(pd.data.frame$mean.sd.raw, na.rm = T)] <- NA
    pd.data.frame$md.raw[pd.data.frame$md.raw >
                           3*sd(pd.data.frame$md.raw, na.rm = T)] <- NA
    pd.data.frame$md.sd.raw[pd.data.frame$md.sd.raw >
                              3*sd(pd.data.frame$md.sd.raw, na.rm = T)] <- NA
    pd.data.frame$pd.sd[pd.data.frame$pd.sd > 3*sd(pd.data.frame$pd.sd,
                                                   na.rm = T)] <- NA
    #     pd.data.frame$base[pd.data.frame$base >
    #                          3*sd(pd.data.frame$base, na.rm = T)] <- NA
  }


  return(pd.data.frame)
}



#-------------------------------------------------------------------------------
performanceMatching <- function(pd.data.frame)
{


  ######### Performance matching###################################################

  setwd(path.performance)
  perform <- read.csv(performance.file, sep=";")

  # convert ID from number to character
  perform$ID <- as.character(perform$ID)
  pd.data.frame$ID <- as.character(pd.data.frame$ID)



  # merge {"join/vlookup"} performance dataframe with main dataframe of pupil sizes
  pd.merge <- merge(perform,pd.data.frame, by = 'ID')

  return(pd.merge)
}



#-------------------------------------------------------------------------------
pdMakeUp <- function(pd.data.frame, pd.eye = "left") {
  # some computation to "makeup" the main data frame
  # eg., add col means

  # calculate "base" values as reference for individiual pupil size
  pd.data.frame$base <- apply(pd.data.frame[5:155], 1, min, na.rm = TRUE)


  # replace 9999 by NA
  pd.data.frame$A1[pd.data.frame$A1 == 9999] <- NA
  pd.data.frame$A2[pd.data.frame$A2 == 9999] <- NA
  pd.data.frame$A3[pd.data.frame$A3 == 9999] <- NA

  # remove missings
  pd.data.frame <-
    pd.l.makeup %>%
    mutate_each(funs(ifelse(is.na(.), pd.mean, .)), V1:V1000)


  # compute sum of correct answers
  pd.data.frame$sum.correct <- factor(rowSums(pd.data.frame[2:4], na.rm = TRUE))
  pd.data.frame$sum.correct2 <- rowSums(pd.data.frame[2:4], na.rm = TRUE)


  # as only very few individuals had a score of zero, those were coded here as
  # "1"; group "1" thus reflects zero or one point score
  pd.data.frame$sum.correct2[pd.data.frame$sum.correct == 0] <- 1
  pd.data.frame$sum.correct2 <- factor(pd.data.frame$sum.correct2)


  # compute binary score variable
  pd.data.frame$score.bin[pd.data.frame$sum.correct %in% c(0,1)] <- "low_score"
  pd.data.frame$score.bin[pd.data.frame$sum.correct %in% c(2,3)] <- "hi_score"


  # compute sd of pd per row (i.e., person)
  pd.data.frame$pd.sd <- apply(pd.data.frame[5:1004], 1,
                               function(x) sd(x, na.rm = T))


  # which eye?
  pd.data.frame$eye <- pd.eye




  return(pd.data.frame)

}



#-------------------------------------------------------------------------------
pdDrawMissings <- function(pd.l, pd.r){

  require(ggplot2)
  require(dplyr)

  left_eye_compressed <- apply(pd.l[5:1004], 1, function(x) sum(is.na(x)))
  right_eye_compressed <- apply(pd.r[5:1004], 1, function(x) sum(is.na(x)))


  na_left_df <- data.frame(
    na_raw = pd.l$count.na,
    na_compressed = left_eye_compressed,
    sum.correct = pd.l$sum.correct,
    eye = "left")

  na_right_df <- data.frame(
    na_raw = pd.r$count.na,
    na_compressed = right_eye_compressed,
    sum.correct = pd.r$sum.correct,
    eye = "right")


  na_df <- rbind(na_left_df, na_right_df)


  na_df_gathered <- gather(na_df, type, missings, 1:2)

  md_df <- data.frame(
    type = c("na_compressed", "na_raw"),
    missings = c(median(na_df_gathered$missings[na_df_gathered$type == "na_compressed"],
                        na.rm = T),
                 (median(na_df_gathered$missings[na_df_gathered$type == "na_raw"],
                         na.rm = T))),
    eye = NA
  )


  p.missings <- ggplot(na_df_gathered, aes(x = missings, color = eye, fill = eye)) +
    geom_histogram() +
    facet_wrap(~ type, scales = "free") +
    geom_vline(data = md_df, aes(xintercept = missings)) +
    geom_text(data = md_df, aes(x = missings, label = missings), y = Inf,
              vjust = "inward", hjust = 0)

  print(p.missings)

  return(p.missings)



}



#-------------------------------------------------------------------------------
pdDrawSDs <- function(pd.l, pd.r){

  require(ggplot2)
  require(dplyr)



  pd_sd_df <- data.frame(
    ID = pd.l$ID,
    left_eye = pd.l$pd.sd,
    right_eye = pd.r$pd.sd
  )


  pd_sd_df_gathered <- gather(pd_sd_df, eye, person_sd, 2:3)


  md_df <- data.frame(
    type = c("left_eye", "right_eye"),
    median_sd = c(round(median(pd_sd_df_gathered$person_sd[pd_sd_df_gathered$eye == "left_eye"],
                               na.rm = T), 2),
                  round(median(pd_sd_df_gathered$person_sd[pd_sd_df_gathered$eye == "right_eye"],
                               na.rm = T), 2)),
    eye = c("left_eye", "right_eye")
  )



  p.sd <- ggplot(pd_sd_df_gathered, aes(x = person_sd, fill = eye),
                 alpha = .6) +
    geom_histogram() +
    geom_vline(data = md_df, aes(xintercept = median_sd)) +
    geom_text(data = md_df, aes(x = median_sd, label = median_sd, eye = eye), y = Inf,
              vjust = "inward", hjust = 0) +
    facet_wrap(~eye)


  print(p.sd)

  return(p.sd)


}




#-------------------------------------------------------------------------------
pdZScale <- function(pd.df){
  # z-scales pupil diameter data
  # expects typical pupil diameter df

  require(dplyr)

  pd.df.z <- pd.df %>% select(V1:V1000) %>% t %>% scale %>%
    t %>% data.frame %>% mutate(ID = pd.df$ID) %>%
    inner_join(pd.df[c(1:4,1005:dim(pd.df)[2])], by = "ID")

  return(pd.df.z)

}




#-------------------------------------------------------------------------------
pdRF <- function(pd.df, pd.n.tree = 2000, pd.n.groups = 4, type_user = "classification"){
  # computes 1000 Random Forests based on {randomForest}
  # input:
  # pd.df: dataframe of type pd (1000 predictors)
  # pd.n.tree: ntree
  # pd.n.groups: number of values in outcome variable (sum.correct)
  # type: "classification" or "regression"

  # output:
  #   list1: OOB errors for each RF
  #   list2: confusion matrix for each RF
  #   list3: mtry
  #   list4: number of unique outcome values
  #   list5: regression or classification as specified by user
  #   list6: overall mean OOB error
  #   list7: regression or classification as reported by function randomForest
  #   list8: MSE

  ######################### Random Forests Preparation ##################


  if (type_user == "classification")
  {pd.df$sum.correct <- as.factor(pd.df$sum.correct)}
  if (type_user == "regression")
  {pd.df$sum.correct <- as.numeric(as.character(pd.df$sum.correct))}

  require(randomForest)


  # check if there are really no NAs
  #cat("sum of NAs: ", sum(is.na(pd.l.for3.nona)))


  # Rand.For. model is built on 1000 variables - each one is one measurement point
  pd.df.for <- subset(pd.df, select = c(V1:V1000))



  # create factor variable for dependent variable
  pd.df.for$sum.correct <- as.factor(pd.df$sum.correct)



  # reduce groups only if selected
  if (pd.n.groups == 3){
    options(stringsAsFactors = FALSE)
    pd.df.for$sum.correct[pd.df.for$sum.correct == 0] <- 1
    pd.df.for$sum.correct <- factor(pd.df.for$sum.correct)
  }


  if (pd.n.groups == 2){
    options(stringsAsFactors = FALSE)
    pd.df.for$sum.correct[pd.df.for$sum.correct == 0] <- 1
    pd.df.for$sum.correct[pd.df.for$sum.correct == 3] <- 2
    pd.df.for$sum.correct <- factor(pd.df.for$sum.correct)
  }






  ##################### Random Forests analysis #############



  # set random seed to allow for replication
  set.seed(42)

  # set elapsed time back to 0
  elapsed.time <- 0


  # create vector of mean accuracy
  rf.err <- vector(mode="numeric", length=1000)


  # create list for confusion matrices of length 1000 for each eye
  rf.conf <- vector(mode = "list", length = 1000)

  # create list for mtry for each RF
  rf.mtry <- vector(mode = "list", length = 1000)

  # initiallize loop variable
  i <- 1

  # create list for type of RF
  rf.typelist <- vector(mode = "list", length = 1000)

  # create list for MSE
  rf.mse <- vector(mode = "list", length = 1000)


  # rf main loop
  elapsed.time <- proc.time()
  for (i in 1:1000){
    # make 1000 rand.fors.
    dummy.rf <- randomForest(sum.correct ~ V1:V1000, data = pd.df.for,
                             ntree = pd.n.tree)
    rf.err[i] <- mean(dummy.rf$err.rate[,"OOB"], na.rm = TRUE)
    if (type_user == "regression") rf.mse[[i]] <- dummy$mse
    if (type_user == "classification") rf.conf[[i]] <- dummy.rf$confusion
    rf.mtry[[i]] <- dummy.rf$mtry
    rf.typelist[[i]] <- dummy.rf$type
    cat("mean OOB error", "Rand.For. ",i," is ",rf.err[i],"measurement points",
        i,"of 1000","\n")
  }
  cat("Random Forests including V5 to max. V1005")
  cat("elapsed time (s): ",(proc.time() - elapsed.time)/60)


  # combine the two results into one list to return it back

  if (is.factor(pd.df$sum.correct)) {RF_type = "factor"}
  else {
    RF_type = "regression"
  }

  rf.list <- list(list_of_OOB_errors = rf.err,
                  confusion_matrices = rf.conf,
                  mtry = rf.mtry,
                  n_outcome_values = length(unique(pd.df$sum.correct)),
                  RF_type_user = RF_type,
                  overall_OOB_err = mean(rf.err, na.rm = T),
                  RF_type_fnctn = rf.typelist,
                  RF_mse = rf.mse)

  # cat("Mean overall error rate: ", mean(rf.err[[1]], na.rm = T), "\n")

  return(rf.list)

}



#-------------------------------------------------------------------------------
outcome_distribution <- function(sum.correct){

  # distribution of outcome variable to calculate accuracy gain
  # it is assumed that sum.correct has four levels/unique values
  # input: outcome variable
  # output: distributions (table)


  # 4 gropus
  outcome_distrib4 <- table(sum.correct)


  # 3 gropus
  sum.correct[sum.correct == 0] <- 1
  outcome_distrib3 <- table(sum.correct)

  # 2 gropus
  sum.correct[sum.correct == 0] <- 1
  sum.correct[sum.correct == 3] <- 2

  outcome_distrib2 <- table(sum.correct)



  pd_distrib <- list(distrib_4groups = outcome_distrib4,
                     distrib_4groups_prob = prop.table(outcome_distrib4),
                     distrib_3groups = outcome_distrib3,
                     distrib_3groups_prob = prop.table(outcome_distrib3),
                     distrib_2groups = outcome_distrib2,
                     distrib_2groups_prob = prop.table(outcome_distrib3)
  )

  return(pd_distrib)
}




#-------------------------------------------------------------------------------
pdDrawRFErr <- function(rf.l.err, rf.r.err, pd.randomness.line = .67, pd.title =
                          "Mean OOB prediction error for Random Forests models",
                        annotate_vjust = 0,
                        pdymin = NULL,
                        pdymax = NULL,
                        overall_line = FALSE)
{

  require(plyr)
  require(reshape)
  require(ggplot2)



  # left eye
  rf.l.err.df <- as.data.frame(rf.l.err)
  names(rf.l.err.df)[1] <- "left.eye"

  # right eye
  rf.r.err.df <- as.data.frame(rf.r.err)
  names(rf.r.err.df)[1] <- "right.eye"

  # bind error data frame together, including "s" as count variable
  rf.lr.err.df <- cbind(rf.l.err.df, rf.r.err.df)
  rf.lr.err.df$s <- seq(1:1000)

  # melt it for ggplot
  rf.err.melt <- melt(rf.lr.err.df, id = "s", variable.name = "eye", value.name =
                        "mean.error")

  names(rf.err.melt) <- c("s", "eye", "mean.error") # assign names to columns

  pd.line <- data.frame(pd.randomness.line) # define dataframe for randomness line

  # define dataframe for grandmean line
  grand.mean.error.df <- data.frame(z = c(mean(rf.err.melt$mean.error[rf.err.melt$eye ==
                                                                        "left.eye"]),
                                          mean(rf.err.melt$mean.error[rf.err.melt$eye == "right.eye"])),
                                    eye = c("left.eye", "right.eye"))

  # define title
  my.title <- paste(pd.title)

  #   mean error left",as.character(format(round(mean(rf.err.melt$mean.error[rf.err.melt$eye
  #                                           == "left.eye"])),2), nsmall = 2))

  rf.comp.plot <- ggplot(rf.err.melt, aes(x = s, y = mean.error,
                                          group = eye)) +
    geom_line(, stat = "identity") +
    facet_grid(eye ~ .) +
    geom_hline(aes(yintercept = pd.randomness.line),
               colour = "forestgreen", linetype = "dashed",
               data = pd.line) +
    # geom_smooth() +
    ggtitle(my.title) +
    scale_x_continuous(name = "thousandths of trial time") +
    scale_y_continuous(name = "mean OOB error",
                       limits = c(pdymin, pdymax)) +
    annotate("text", x = 800,
             y = (pd.randomness.line - .001),
             label = "randomness line", color = "forestgreen",
             vjust = 1) +
    # scale_y_continuous(breaks = c(.5, round(grand.mean.error.df$z,3), .6, .7))
    geom_text(data = grand.mean.error.df,
              #aes(y = round(grand.mean.error.df$z,2) - .01),
              x = -Inf,
              y = -Inf,
              label = paste("overall OOB error: ",round(grand.mean.error.df$z,2)),
              colour = "orange3",
              vjust = -.5,
              hjust = 0)

  # if (overall_line) rf.comp.plot <- rf.comp.plot +
  #   geom_hline(aes(yintercept = z), colour = "orange3",
  #              data = grand.mean.error.df) + # line of grand error.mean

  return(rf.comp.plot)
}




#-------------------------------------------------------------------------------
pdPlotConfusion <- function(rf.left, rf.right, type = "three",
                            pd.title = ""){
  # plot confusion matrices for different (cumulated) measurement points
  # input:
  #   rf.left, rf.right: RF objects of type pd
  #   type: number of unique values in outcome variable ("three" or "two")

  require(dplyr)
  require(ggplot2)
  require(tidyr)


  if (type == "three"){

    conf_df <- data.frame(class11 = NULL,
                          class12 = NULL,
                          class13 = NULL,
                          class21 = NULL,
                          class22 = NULL,
                          class23 = NULL,
                          class31 = NULL,
                          class32 = NULL,
                          class33 = NULL,
                          eye = NULL,
                          pos = NULL)




    pos_vector <- c(.05, .1, .25, .5, 1)


    # left eye
    for (i in pos_vector){

      conf_matrices_l <- pdConfusionMatrix(rf.left, pos = i, eye = "left")

      conf_df_append <- data.frame(class1 = c(conf_matrices_l$pd_prop_conf_matrix[[1, 1]],
                                              conf_matrices_l$pd_prop_conf_matrix[[1, 2]],
                                              conf_matrices_l$pd_prop_conf_matrix[[1, 3]]),
                                   class2 = c(conf_matrices_l$pd_prop_conf_matrix[[2, 1]],
                                              conf_matrices_l$pd_prop_conf_matrix[[2, 2]],
                                              conf_matrices_l$pd_prop_conf_matrix[[2, 3]]),
                                   class3 = c(conf_matrices_l$pd_prop_conf_matrix[[3, 1]],
                                              conf_matrices_l$pd_prop_conf_matrix[[3, 2]],
                                              conf_matrices_l$pd_prop_conf_matrix[[3, 2]]),
                                   err_class = c(conf_matrices_l$pd_conf_matrix[1, 4],
                                                 conf_matrices_l$pd_conf_matrix[2, 4],
                                                 conf_matrices_l$pd_conf_matrix[3, 4]),
                                   test_result = c("class1", "class2", "class3"),
                                   eye = "left",
                                   pos = i)

      conf_df <- rbind(conf_df, conf_df_append)



    }


    # right eye
    for (i in pos_vector){

      conf_matrices_r <- pdConfusionMatrix(rf.right, pos = i, eye = "right")

      conf_df_append <- data.frame(class1 = c(conf_matrices_r$pd_prop_conf_matrix[[1, 1]],
                                              conf_matrices_r$pd_prop_conf_matrix[[1, 2]],
                                              conf_matrices_r$pd_prop_conf_matrix[[1, 3]]),
                                   class2 = c(conf_matrices_r$pd_prop_conf_matrix[[2, 1]],
                                              conf_matrices_r$pd_prop_conf_matrix[[2, 2]],
                                              conf_matrices_r$pd_prop_conf_matrix[[2, 3]]),
                                   class3 = c(conf_matrices_r$pd_prop_conf_matrix[[3, 1]],
                                              conf_matrices_r$pd_prop_conf_matrix[[3, 2]],
                                              conf_matrices_r$pd_prop_conf_matrix[[3, 2]]),
                                   err_class = c(conf_matrices_r$pd_conf_matrix[1, 4],
                                                 conf_matrices_r$pd_conf_matrix[2, 4],
                                                 conf_matrices_r$pd_conf_matrix[3, 4]),
                                   test_result = c("class1", "class2", "class3"),
                                   eye = "right",
                                   pos = i)

      conf_df <- rbind(conf_df, conf_df_append)


    }  # for-loop


    conf_df_long <- gather(conf_df, class, percent, 1:3)
    conf_df_long$correct_classified <- ifelse(conf_df_long$class ==
                                                conf_df_long$test_result,
                                              .9, .1)



    # dataframe for geom_text
    conf_df_long_annotate <- conf_df
    conf_df_long_annotate$class <- conf_df_long_annotate$test_result
    conf_df_long_annotate$percent <- 1.1
    conf_df_long_annotate$class1 <- NULL
    conf_df_long_annotate$class2 <- NULL
    conf_df_long_annotate$class3 <- NULL



    # dataframe for facet laballer
    mean_oob_err_df <- data.frame(
      id = 1:(2*length(pos_vector)),
      pos = c(pos_vector, pos_vector),
      eye = gl(2, 5, labels = c("left", "right"))
    )




    # populate oob error values per eye
    conf_df_long$mean_oob_err <- NA




    for (i in seq_along(pos_vector)){

      mean_oob_err_df$mean_oob_err[i] <- pdConfusionMatrix(rf.left,
                                                           pos = pos_vector[i],
                                                           eye = "left")$mean_oob_err

      mean_oob_err_df$mean_oob_err[i+5] <- pdConfusionMatrix(rf.right,
                                                             pos = pos_vector[i],
                                                             eye = "right")$mean_oob_err
    }




    # creat labeller variable
    mean_oob_err_df$labeller <- format(round(mean_oob_err_df$mean_oob_err, 2))


    conf_df_long_annotate <- left_join(conf_df_long_annotate,
                                       select(mean_oob_err_df, pos, eye, labeller),
                                       by = c("eye", "pos"))



    p.confusion <- ggplot(conf_df_long, aes(x = class, y = percent,
                                            fill = test_result)) +
      geom_bar(aes(alpha = correct_classified),
               position = "fill", stat = "identity") +
      facet_grid(eye ~ pos) +
      geom_text(data = conf_df_long_annotate, aes(label = round(err_class, 2)),
                size = 3) +
      scale_y_continuous(breaks = c(0, .5, 1), name = "classification rate") +
      scale_x_discrete(labels = c("1", "2", "3"), name = "class") +
      scale_alpha_continuous(range=c(0.4,1), guide = FALSE) +
      scale_fill_discrete(name = "classification result") +
      ggtitle(pd.title)



    conf_df_long_annotate$percent <- 1.2

    conf_df_long_annotate$class[conf_df_long_annotate$class == "class1"] <- NA
    conf_df_long_annotate$class[conf_df_long_annotate$class == "class3"] <- NA



    p.conf2 <- p.confusion +
      geom_text(data = conf_df_long_annotate,
                aes(label = labeller), size = 3, fontface = "bold")

    print(p.conf2)

    return(p.conf2)
  }  # end type == "three"


  # ----------------------------------------------------------------------------


  if (type == "two"){


    conf_df <- data.frame(class11 = NULL,
                          class12 = NULL,
                          class21 = NULL,
                          class22 = NULL,
                          eye = NULL,
                          pos = NULL)




    pos_vector <- c(.05, .1, .25, .5, 1)


    # left eye
    for (i in pos_vector){

      conf_matrices_l <- pdConfusionMatrix(rf.left, pos = i, eye = "left")

      conf_df_append <- data.frame(class1 = c(conf_matrices_l$pd_prop_conf_matrix[[1, 1]],
                                              conf_matrices_l$pd_prop_conf_matrix[[1, 2]]),
                                   class2 = c(conf_matrices_l$pd_prop_conf_matrix[[2, 1]],
                                              conf_matrices_l$pd_prop_conf_matrix[[2, 2]]),
                                   err_class = c(conf_matrices_l$pd_conf_matrix[1, 3],
                                                 conf_matrices_l$pd_conf_matrix[2, 3]),
                                   test_result = c("class1", "class2"),
                                   eye = "left",
                                   pos = i)

      conf_df <- rbind(conf_df, conf_df_append)



    }


    # right eye
    for (i in pos_vector){

      conf_matrices_r <- pdConfusionMatrix(rf.right, pos = i, eye = "right")

      conf_df_append <- data.frame(class1 = c(conf_matrices_r$pd_prop_conf_matrix[[1, 1]],
                                              conf_matrices_r$pd_prop_conf_matrix[[1, 2]]),
                                   class2 = c(conf_matrices_r$pd_prop_conf_matrix[[2, 1]],
                                              conf_matrices_r$pd_prop_conf_matrix[[2, 2]]),
                                   err_class = c(conf_matrices_r$pd_conf_matrix[1, 3],
                                                 conf_matrices_r$pd_conf_matrix[2, 3]),
                                   test_result = c("class1", "class2"),
                                   eye = "right",
                                   pos = i)


      conf_df <- rbind(conf_df, conf_df_append)


    }  # for-loop


    conf_df_long <- gather(conf_df, class, percent, 1:2)
    conf_df_long$correct_classified <- ifelse(conf_df_long$class ==
                                                conf_df_long$test_result,
                                              .9, .1)



    # dataframe for geom_text
    conf_df_long_annotate <- conf_df
    conf_df_long_annotate$class <- conf_df_long_annotate$test_result
    conf_df_long_annotate$percent <- 1.1
    conf_df_long_annotate$class1 <- NULL
    conf_df_long_annotate$class2 <- NULL



    # dataframe for facet laballer
    mean_oob_err_df <- data.frame(
      id = 1:(2*length(pos_vector)),
      pos = c(pos_vector, pos_vector),
      eye = gl(2, 5, labels = c("left", "right"))
    )




    # populate oob error values per eye
    conf_df_long$mean_oob_err <- NA




    for (i in seq_along(pos_vector)){

      mean_oob_err_df$mean_oob_err[i] <- pdConfusionMatrix(rf.left,
                                                           pos = pos_vector[i],
                                                           eye = "left")$mean_oob_err

      mean_oob_err_df$mean_oob_err[i+5] <- pdConfusionMatrix(rf.right,
                                                             pos = pos_vector[i],
                                                             eye = "right")$mean_oob_err
    }




    # creat labeller variable
    mean_oob_err_df$labeller <- format(round(mean_oob_err_df$mean_oob_err, 2))


    conf_df_long_annotate <- left_join(conf_df_long_annotate,
                                       select(mean_oob_err_df, pos, eye, labeller),
                                       by = c("eye", "pos"))



    p.confusion <- ggplot(conf_df_long, aes(x = class, y = percent,
                                            fill = test_result)) +
      geom_bar(aes(alpha = correct_classified),
               position = "fill", stat = "identity") +
      facet_grid(eye ~ pos) +
      geom_text(data = conf_df_long_annotate, aes(label = round(err_class, 2)),
                size = 3) +
      scale_y_continuous(breaks = c(0, .5, 1), name = "classification rate") +
      scale_x_discrete(labels = c("1", "2", "3"), name = "class") +
      scale_alpha_continuous(range=c(0.4,1), guide = FALSE) +
      scale_fill_discrete(name = "classification result") +
      ggtitle(pd.title)



    conf_df_long_annotate$percent <- 1.2

    conf_df_long_annotate$class[conf_df_long_annotate$class == "class2"] <- NA



    p.conf2 <- p.confusion +
      geom_text(data = conf_df_long_annotate,
                aes(label = labeller), size = 3, fontface = "bold")

    print(p.conf2)

    return(p.conf2)
  }  # end type == "two"




}



#-------------------------------------------------------------------------------
pdTSGrid <- function(pd.l = pd.l.makeup.nona,
                     pd.r = pd.r.makeup.nona,
                     pd.l.z = pd.l.makeup.z.nona,
                     pd.r.z = pd.r.makeup.z.nona,
                     save_parts = FALSE,
                     melt_groups = FALSE,
                     pd.melt = pd,
                     pd.z.melt = pd.z,
                     pd.mean.melt = pd.mean,
                     pd.mean.z.melt = pd.mean.z){
  # plots time series (TS) of pupil size

  require(ggplot2)
  require(gridExtra)
  require(grid)

  if (melt_groups == TRUE){
    # Meltdown Dataframes:
    pd.z.melt <- pdPrepareMeltedGroups(pd.l.z, pd.r.z,
                                       fun = "median")

    pd.melt <- pdPrepareMeltedGroups(pd.l, pd.r,
                                     fun = "median")

    pd.mean.melt <- pdPrepareMeltedGroups(pd.l, pd.r,
                                          fun = "mean")

    pd.mean.z.melt <- pdPrepareMeltedGroups(pd.l.z, pd.r.z,
                                            fun = "mean")

  }  # end melt_groups



  # Prepare Times Series (TS) plots:
  pTS.z <- pdDrawTimeSeries(pd.z.melt, pd.title = "Aggregate: median; data: z")

  pTS.mean.z <- pdDrawTimeSeries(pd.mean.z.melt,
                                 pd.title = "Aggregate: mean; data: z")


  pTS <- pdDrawTimeSeries(pd.melt,
                          pd.title = "Aggregate: median; data: raw")


  pTS.mean <- pdDrawTimeSeries(pd.mean.melt,
                               pd.title = "Aggragte: mean; data: raw")



  # Now high-scorers vs. low-scorers:
  pTS_2groups <- pdDrawTimeSeries(pd.melt,
                                  pd.title = "Aggregate: median; data: raw; groups: 2",
                                  pd.n.groups = 2)


  pTS_2groups.z <- pdDrawTimeSeries(pd.z.melt,
                                    pd.title = "Aggregate: median; data: z; groups: 2",
                                    pd.n.groups = 2)



  if (save_parts == TRUE){
    save(pd.z.melt, file = "pd.z.Rda")
    save(pd.z.melt, file = "pd.Rda")
    save(pd.mean.z.melt, file = "pd.mean.Rda")
    save(pd.mean.z.z.melt, file = "pd.mean.z.Rda")


    ggsave(pTS.z, file = "pTS.z.jpg")
    ggsave(pTS, file = "pTS.jpg")
    ggsave(pTS.mean.z, file = "pTS.mean.z.jpg")
    ggsave(pTS.mean, file = "pTS.mean.jpg")

    ggsave(pTS_2groups, file = "pTS.2groups.jpg")
    ggsave(pTS_2groups.z, file = "pTS.2groups.z.jpg")
  }



  grid_arrange_shared_legend <- function(...) {
    # source:  https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }

  grid_arrange_shared_legend(pTS,
                             pTS.z,
                             pTS.mean,
                             pTS.mean.z,
                             ncol = 2)

}







