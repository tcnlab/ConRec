#Primary script for running Analyses

#CondFolder is used for outputting figures and stats
#"ltsAll" corresponds to analyses on first repetitions
condFolder <<- "ltsAll" 
#Specifies where everything is being held
mainDir <<- '/Users/ianmbright/Desktop/Desktop/Main_Desktop/Continuous_Recognition_Analysis/'
#Changes working directory
setwd(mainDir)
#This adds the date to folders so that multiple versions can be saved
subDir <<- paste("plots_",Sys.Date(),condFolder,sep = "")
#Generates the folder to hold plots
dir.create(file.path(mainDir, subDir))

#Loads up packages used in analyses
library(nlme)
library(lme4)
library(lmerTest)
library(plotrix)

#Shortens name of length function
len<-length

#Loads up functions to make a bunch of the plots
figSrc = "ltsFig.R"
source(figSrc)


#Loads and processes datafiles into something we can work with
## dataFile: Name of datafile you're loading up
## removeMul: If set to 1, removes trials that correspond to old items that
##    have repeated more than once. Set to 0 to include.
## dropOver: Remove trials with old items at lags larger than given value
preProcess <- function(datFile = "LTS1", removeMul = 1, dropOver = 128)
{ 
  
  setwd(mainDir)
  #Sets working director to relevant folder.
  setwd(paste("Exp",substring(datFile,nchar(datFile),nchar(datFile)),"_Data",sep=""))
  # Loads file up.
  MM1data <- read.csv(paste(datFile,".csv",sep=""),header=T,stringsAsFactors = FALSE)
  
  #Saves copy of raw RT data 
  MM1data$RT2 = MM1data$RT
  
  #Deals with trials where participant does not respond
  MM1data$RT[MM1data$RT=='MAX'] = "0"
  MM1data$RT[is.na(MM1data$RT=='MAX')] = "0"
  #Converts RT's to seconds
  MM1data$RT = as.numeric(as.character(MM1data$RT))/1000 
  
  #Checks if this is the MIT dataset or BU.
  if(datFile != "LTS0")
  {
    #BU dataset
    #Numericalize lags
    MM1data$lag = as.numeric(as.character(MM1data$lag))
    
  } else
  {
    #MIT dataset
    #"Changes" column names to be congruent with BU datasets
    MM1data$sub <- MM1data$Subject 
    MM1data$trial <- MM1data$Trial
    MM1data$isResponse <- MM1data$Responded
    #Numericalize lags
    MM1data$lag = as.numeric(as.character(MM1data$nBackLength))
    #"Changes" name of column to be consistent with BU dataset
    MM1data$ImgFile <- MM1data$logicStr
    
  }
  
  
  ## Setup template DF
  MM1data$sel = rep(0,len(MM1data$sub))
  MM1data$selx = rep(0,len(MM1data$sub))
  MM1data$isOutlier = rep(0,len(MM1data$sub))
  MM1sel = MM1data[0,]
  MM1sel2 = MM1data[0,]
  
  #Saves data incase something goes wrong later on
  #While writing the function
  MM1all <- MM1data
  
  #This block determines if multiple repetitions are being removed
  # and if so, removes them.
  #Checks which experiment is being loaded up.
  if(as.numeric(substring(datFile,nchar(datFile),nchar(datFile))) <= 4)
  {
    #Its a dataset where some items were repeated more than once
    #Gets all trials that contain items repeated multiple times
    mulRep = MM1data[grepl("-mul",MM1data$ImgFile) == TRUE ,]
    #Checks if multiples are being removed
    if (removeMul==1){ 
      #Removes multiple repetitions    
      MM1data = MM1data[grepl("-mul",MM1data$ImgFile) != TRUE ,]
    }
  }else
  {
    #Dataset where items could be repeated up to five times
    #Gets all trials that contain items repeated multiple times
    mulRep = MM1data[grepl("-second",MM1data$ImgFile) == TRUE | grepl("-third",MM1data$ImgFile) == TRUE | grepl("-fourth",MM1data$ImgFile) == TRUE | grepl("-fifth",MM1data$ImgFile) == TRUE,]
    if (removeMul==1){ 
      #Removes mutiple repetitions
      MM1data = MM1data[grepl("-second",MM1data$ImgFile) != TRUE & grepl("-third",MM1data$ImgFile) != TRUE & grepl("-fourth",MM1data$ImgFile) != TRUE & grepl("-fifth",MM1data$ImgFile) != TRUE ,]
    }
  }
  
  
  #Removes repeats that span the two sessions
  MM1data <- MM1data[grepl("00-across",MM1data$ImgFile) != TRUE, ]
  
  #Removes first 5 images since some subjects make FA to noise
  MM1data = MM1data[ !(MM1data$trial %in% seq(0,4)) ,]
  
  #Gets all lags in dataset greater than dropOver 
  dropLags = sort(unique(MM1data$lag))[sort(unique(MM1data$lag)) > dropOver ]
  #Removes trials with lags greater than dropOver
  MM1data = MM1data[!(MM1data$lag %in% dropLags),]
  
  #Gets list of all subjects in dataset 
  subs = unique(MM1data$sub)
  
  #Sets thresholds for accuracy cutoffs
  #If hit rate is below hrcutoff for any single lag, participant will be removed.
  ## hrcutoff is NOT USED later on
  hrcutoff  = 0.3
  #Particpants with less than "minHits" correct responses for each lag will be removed
  minHits = 3
  
  #Gets the hit and false alarm rates for each subject
  
  #Checks which experiment is being considered, as participant responses differ
  #   between experiments
  if(as.numeric(substring(datFile,nchar(datFile),nchar(datFile))) > 3)
  {
    #Is a dataset where participants were instructed to respond every trial
    #Gets all the hits
    MM1HIT = MM1data[MM1data$keypress=="LEFT" & MM1data$isRepeat==1,]
    #Gets all the misses
    MM1Miss = MM1data[MM1data$keypress!="LEFT" & MM1data$isRepeat==1,]
    #Gets all the false alarms
    MM1FA = MM1data[MM1data$keypress=="LEFT" & MM1data$isRepeat==0,]
    #Gets all the trials where the participant doesn't respond
    MM1NR = MM1data[MM1data$keypress=="NOKEY" & MM1data$isRepeat==0,]
    #Gets all the correct rejections
    MM1CR = MM1data[MM1data$keypress=="RIGHT" & MM1data$isRepeat==0,]
   
    #Counts how may hits there were for each subject
    hitC <- aggregate(MM1HIT$RT, by=list(MM1HIT$sub), FUN=count)
    #Counts how may misses there were for each subject
    missC <- aggregate(MM1Miss$RT, by=list(MM1Miss$sub), FUN=count)
    #Counts how may false alarms there were for each subject
    faC <- aggregate(MM1FA$RT, by=list(MM1FA$sub), FUN=count)
    #Counts how may non-responses there were for each subject
    nrC <- aggregate(MM1NR$RT, by=list(MM1NR$sub), FUN = count)
    #Counts how may correct rejections there were for each subject
    crC <- aggregate(MM1CR$RT, by=list(MM1CR$sub), FUN=count)
    
    #Names columns for counts
    names(missC) = c("sub","miss")
    names(nrC) = c("sub","nr")
    #Holds the accuracy counts for trials with repeated items
    accDf = hitC
    #Names the coumns 
    names(accDf) = c("sub","hit")
    #Adds a miss column
    accDf$miss = 0
    
    #Adds miss counts to accDF
    for(subi in missC$sub ){
      accDf$miss[accDf$sub == subi] = missC$miss[missC$sub == subi]
    }
    
    #Adds false alarm counts to accDF
    accDf$fa = faC$x
    #Adds no response counts to accDF
    accDf$nr = 0
    for(subi in nrC$sub ){
      accDf$nr[accDf$sub == subi] = nrC$nr[nrC$sub == subi]
    }
    
    #Adds correct rejections counts to accDF
    accDf$cr = crC$x
    #Counts up all the responses
    accDf$tot = accDf$hit + accDf$miss + accDf$fa + accDf$nr + accDf$cr
    #Calculates the hit rate
    accDf$hr = accDf$hit / (accDf$hit+accDf$miss)
    #Calculates the false alarm rate
    accDf$far = accDf$fa / (accDf$fa+accDf$cr+accDf$nr)
    #Calculates the z-scored hit rate
    accDf$zhr = qnorm(accDf$hr)
    #Calculates the z-scored false alarm rate
    accDf$zfar = qnorm(accDf$far)
    #Calculates d-prime
    accDf$dprime = accDf$zhr - accDf$zfar
    
    #Gets list of all lags in experiment
    lagList = sort(unique(MM1data$lag))
    #Removes trials with new images
    lagList = lagList[lagList!=0]
    #Get list of names for hit rates at all lags
    namevector <- paste("hr",lagList,sep="")
    #Adds columns that will hold HR for each lag to accDF
    accDf[,namevector] <- NA
    #Gets list of names for number of hits at all lags
    namevectorHits <- paste("hits",lagList,sep="")
    #Adds columns that will hold hit counts for each lag
    accDf[,namevectorHits] <- NA
    #Gets list of names for d-prime at all lags
    namevectorDp <- paste("dp",lagList,sep="")
    #Adds columns that will hold d-prime at all algs
    accDf[,namevectorDp] <- NA
    
    #Loops through the subjects
    for(subi in subs){
      #Gets all the hit trials
      MM1HIT128 = MM1data[MM1data$keypress=="LEFT" & MM1data$isRepeat==1 & MM1data$sub==subi ,]
      #Gets all the repeat trials
      MM1all128 = MM1data[MM1data$isRepeat==1 & MM1data$sub==subi ,]
      
      #Counts number of hits per lag
      hitC128 <- aggregate(MM1HIT128$RT, by=list(MM1HIT128$lag), FUN=count)
      #Counts number of repeats by lag
      allC128 <- aggregate(MM1all128$RT, by=list(MM1all128$lag), FUN=count)
      #Stores subject HR for each lag
      accDf[accDf$sub==subi,namevector] = hitC128$x/allC128$x
      #Stores subject hit count for each lag
      accDf[accDf$sub==subi,namevectorHits] = hitC128$x
      #Stores subject d-prime for each lag
      accDf[accDf$sub==subi,namevectorDp] = qnorm(hitC128$x/allC128$x) - accDf$zfar[accDf$sub==subi]
      #Stores if all lags are above min hit rate
      accDf$hrPass[accDf$sub==subi] = all(hitC128$x/allC128$x > hrcutoff)
      #Stores if all lags are above min hit count
      accDf$hitsPass[accDf$sub==subi] = all(hitC128$x > minHits)
    }
  }else{
    #Is a dataset where participants only respond to repeats
    #Gets all the hit trials
    MM1HIT = MM1data[MM1data$isResponse==1 & MM1data$isRepeat==1,]
    #Gets all the miss trials
    MM1Miss = MM1data[MM1data$isResponse==0 & MM1data$isRepeat==1,]
    #Gets all the false alarm trials
    MM1FA = MM1data[MM1data$isResponse==1 & MM1data$isRepeat==0,]
    #Gets all the correct rejection trials
    MM1CR = MM1data[MM1data$isResponse==0 & MM1data$isRepeat==0,]
    
    #Counts number of hits
    hitC <- aggregate(MM1HIT$RT, by=list(MM1HIT$sub), FUN=count)
    #Counts number of misses
    missC <- aggregate(MM1Miss$RT, by=list(MM1Miss$sub), FUN=count)
    #Counts number of false alarms
    faC <- aggregate(MM1FA$RT, by=list(MM1FA$sub), FUN=count)
    #Counts number of correct rejections
    crC <- aggregate(MM1CR$RT, by=list(MM1CR$sub), FUN=count)
    
    #Names columns of missC
    names(missC) = c("sub","miss")
    #Will hold accuracy data
    accDf = hitC
    #Names columns of accDF
    names(accDf) = c("sub","hit")
    #Adds columns for misses
    accDf$miss = 0
    
    #Stores miss counts for each subject
    for(subi in missC$sub ){
      accDf$miss[accDf$sub == subi] = missC$miss[missC$sub == subi]
    }
    
    #Adds false alarm counts
    accDf$fa = faC$x
    #Adds correct rejection counts
    accDf$cr = crC$x
    #Calcualtes and stores total trial counts
    accDf$tot = accDf$hit + accDf$miss + accDf$fa + accDf$cr
    #Calculates and stores hit rate
    accDf$hr = accDf$hit / (accDf$hit+accDf$miss)
    #Calculates and stores false alarm rate
    accDf$far = accDf$fa / (accDf$fa+accDf$cr)
    #z-scores and stores hit rate
    accDf$zhr = qnorm(accDf$hr)
    #z-scores and stores false alarm rate
    accDf$zfar = qnorm(accDf$far)
    #Calculates and stores d-prime
    accDf$dprime = accDf$zhr - accDf$zfar
    
    #Gets list of all lags
    lagList = sort(unique(MM1data$lag))
    #Removes lag 0 (new item trials)
    lagList = lagList[lagList!=0]
    #Holds names for each lag's hit rate
    namevector <- paste("hr",lagList,sep="")
    #Adds columns to accDF for hit rate by lag
    accDf[,namevector] <- NA
    #Holds names for each lag's hit count
    namevectorHits <- paste("hits",lagList,sep="")
    #Adds columns for hit count by lag
    accDf[,namevectorHits] <- NA
    #Holds names for each lag's d-prime
    namevectorDp <- paste("dp",lagList,sep="")
    #Adds columns for d-prime by lag
    accDf[,namevectorDp] <- NA
    
    #Calculates accuracy info for each subject
    for(subi in subs){
      #Gets all hit trials
      MM1HIT128 = MM1data[MM1data$isResponse==1 & MM1data$isRepeat==1 & MM1data$sub==subi ,]
      #Gets all repeat trials
      MM1all128 = MM1data[MM1data$isRepeat==1 & MM1data$sub==subi ,]
      #Counts how many hits there were per lag
      hitC128 <- aggregate(MM1HIT128$RT, by=list(MM1HIT128$lag), FUN=count)
      #Counts how many repeats there were per lag
      allC128 <- aggregate(MM1all128$RT, by=list(MM1all128$lag), FUN=count)
      #Stores the hit rate for each lag
      accDf[accDf$sub==subi,namevector] = hitC128$x/allC128$x
      #Stores the hit count for each lag
      accDf[accDf$sub==subi,namevectorHits] = hitC128$x
      #Stores d-prime for each lag
      accDf[accDf$sub==subi,namevectorDp] = qnorm(hitC128$x/allC128$x) - accDf$zfar[accDf$sub==subi]
      #Stores if subject's hit rate is above minimum for each lag
      accDf$hrPass[accDf$sub==subi] = all(hitC128$x/allC128$x > hrcutoff)
      #Stores if subject's hit count is above minimum for each lag
      accDf$hitsPass[accDf$sub==subi] = all(hitC128$x > minHits)
    }
  }
  
  #Removes subjects that did not meet minimum hit counts for all lags
  MM1data = MM1data[MM1data$sub %in% accDf$sub[accDf$hitsPass] ,]
  #Saves accuracy data for all subjects including those removed
  write.table(accDf,paste("accDf_",datFile,".csv",sep=""), sep=",",row.names = FALSE)
  #Returns working directory to where it was
  setwd(mainDir)
  
  
  return(MM1data)
  
}

#Processes and saves the data for the six experiments
lts0Dat <- preProcess("LTS0")
lts1Dat <- preProcess("LTS1")
lts2Dat <- preProcess("LTS2")
lts3Dat <- preProcess("LTS3")
lts4Dat <- preProcess("LTS4")
lts5Dat <- preProcess("LTS5")

#Sets directory
setwd(mainDir)
setwd(subDir)

#This function will calculate and save all the stats
#   as well as save plots for each individual experiment
# MM1data: The processed data you want to analyse
# prefix: Name of the experiment, used for saving plots and determing experiment
# mulrep: If true, will perform analyses on multiple repetitions.
calculateEmpirical <- function(MM1data, prefix = "LTS1", mulrep=F){ 
  
  ##### ECDF Plot across subjects
  #Sets x-min for ecdf plots
  xl1 = 0.26
  #Sets x-max for ecdf plots
  xl2 = 2
  #Gets all lags
  lags = unique(MM1data$lag)
  #Gets all subjects
  subs = unique(MM1data$sub)
  #Removes lag 0
  lags = sort(lags[lags!=0])
  #Gets number of lags
  nl = len(lags)
  
  #Checks which experiment is being considered
  if(as.numeric(substring(prefix,4,4)) < 4)
  {
    #Participants only respond to repeats
    #Gets all hit trials
    MM1sel = MM1data[MM1data$isResponse==1 & MM1data$isRepeat==1 ,]
  }
  else{
    #Participants respond to all trials
    #Gets all the hit trials
    MM1sel = MM1data[MM1data$isRepeat==1 & MM1data$keypress == "LEFT", ]  
  }
  
  #Creates .pdf that will contain ecdf plot
  pdf(file = paste(prefix,"AllSubs.pdf",sep=""), width = 8, height = 6)
  #Sets some plot parameters
  par(new=F)
  par(mar=c(5,6.8,3.5,2)+0.1)  
  #Initializes iterator for changing line color
  i=0
  
  #Loop through the lags
  for ( lagi in lags){
    #Gets all RT's for lagi
    lagRT =  MM1sel$RT[MM1sel$lag==lagi]
    #Gets number of RT's
    n = len(lagRT)
    
    #Checks which experiment
    if (grepl("0",prefix) == TRUE) { 
      #Plots ecdf for current lag
      plot(sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(0,0,0,(nl-i)/nl), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 1"
    }else if (grepl("1",prefix) == TRUE) { 
      #Plots ecdf for current lag
      plot( sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(0,0,0,(nl-i)/nl), lwd = 2, lty=2,xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 2"
    }else if (grepl("2",prefix) == TRUE){ 
      #Plots ecdf for current lag
      plot( sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(0,0,1,(nl-i)/nl), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 3"
    } else if (grepl("3",prefix) == TRUE){ 
      #Plots ecdf for current lag
      plot( sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(0,0,1,(nl-i)/nl), lwd = 2, lty=2,xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 4"
    }
    else if (grepl("4",prefix) == TRUE){ 
      #Plots ecdf for current lag
      plot( sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(1,0,0,(nl-i)/nl), lwd = 2,xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 5"
    } 
    else{ 
      #Plots ecdf for current lag
      plot( sort(lagRT), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(1,0,0,(nl-i)/nl), lty =2, lwd = 2,xlab="",ylab="", xaxt='n',yaxt='n')
      #Sets title
      titlex = "Experiment 6"
    } 
    
    #Keeps plot on
    par(new=T)
    #Updates iterator
    i=i+1
  }
  #Sets plot title
  title( main = titlex ,cex.main = 2.5)
  #Sets x label
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  #Sets y label
  title( ylab = "Cumulative probability"  ,cex.lab = 2.5, line = 4.5)
  #Sets x-axis
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2, at = seq(0.3,1.8,0.3) ) # Below
  #Sets y-axis
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  #Closes plot 
  dev.off()
  
  ###### Accuracy Plots and Stats
  
  #Checks which experiment it is.
  if(as.numeric(substring(prefix,4,4)) < 4)
  {
    #Exp where participants only respond to repeats
    #Adds column to indicate if trial is correct
    MM1data$corr = 0
    #Stores if correct on repeat trials (only done on repeats)
    MM1data$corr[MM1data$isResponse==1 & MM1data$isRepeat==1 & MM1data$lag!=0] = 1
    #Holds the hit information
    MM1Hit = MM1data[MM1data$isResponse==1 & MM1data$isRepeat==1 & MM1data$lag!=0,]
    
  }
  else{
    #Exp where participants respond on all trials
    #Adds column to indicate if correct trial
    MM1data$corr = 0
    #Stores if correct on each trial (only done for repeats)
    MM1data$corr[MM1data$keypress=="LEFT" & MM1data$isRepeat==1 & MM1data$lag!=0] = 1
    #Stores the hits on each trial
    MM1Hit = MM1data[MM1data$keypress=="LEFT" & MM1data$isRepeat==1 & MM1data$lag!=0,]
  }
  
  #Gets all repetitions
  MM1Rep = MM1data[MM1data$isRepeat==1 & MM1data$lag!=0,]
  
  #Gets all false alarms
  #Checks which experiment
  if(as.numeric(substring(prefix,4,4)) == 0)
  {
    #MIT
    MM1FA <- sum(MM1data$Responded[MM1data$isRepeat == 0])/nrow(MM1data[MM1data$isRepeat == 0,])
  }else if(as.numeric(substring(prefix,4,4)) < 4)
  {
    #BU, subs only repsond to repeats
    MM1FA <- sum(MM1data$isResponse[MM1data$isRepeat == 0])/nrow(MM1data[MM1data$isRepeat == 0,])
    
  }else{
    #BU, subs respond to all trials
    MM1FA <- nrow(MM1data[MM1data$isRepeat == 0 & MM1data$keypress == "LEFT", ])/nrow(MM1data[MM1data$isRepeat == 0,])
  }
  #Writes out the false alarm rate and saves it in text file
  cat(paste("\n\n",prefix, "False Alarm Rate"), MM1FA, file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  
  #Counts up the hits for each lag and subject
  hitC <- aggregate(MM1Hit$RT, by=list(MM1Hit$sub,MM1Hit$lag), FUN=count)
  #Counts up the repetitions for each lag and subject
  repC <- aggregate(MM1Rep$RT, by=list(MM1Rep$sub,MM1Rep$lag), FUN=count)
  #Used to calculate error bars
  accLag = hitC
  #Adds column names
  names(accLag) = c("sub","lag","hit")
  #Stores total number of repeats
  accLag$tot = repC$x 
  #Calculates hit rates
  accLag$acc = accLag$hit/accLag$tot
  #Creates .pdf for boxplot of accuracy 
  #pdf(file = paste(prefix,"AccBoxplot.pdf", sep=""), width = 8, height = 6)
  #boxplot(accLag$acc~accLag$lag,ylim = c(0,1), main = "Accuracy by lag")
  #dev.off()
  
  #Calculates and stores log_2 of the lags
  accLag$llag = log2(as.numeric(as.character(accLag$lag)))
  #Calculates the within subject error bars
  accLagS <- summarySEwithin(accLag, measurevar="acc", withinvars="llag",
                             idvar="sub", na.rm=FALSE, conf.interval=.95)
  #Reformats log lag
  accLagS$llag = as.numeric(as.character(accLagS$llag))
  #Reformats lag
  accLagS$lag = 2^(accLagS$llag)
  #Duplicates accuracy info
  accLagS$parV = accLagS$acc
  #holds the within subject error bar info
  retHR = accLagS
  
  #Gets lags corresponding to highest and lowest hit rates
  HR_laghigh <- which.max(retHR[,3])
  HR_laglow <- which.min(retHR[,3])
  #Gets highest and lowest hit rates
  HR_high <- retHR[HR_laghigh,3]
  HR_low <- retHR[HR_laglow,3]
  #Calculates highest and lowest d-primes for lags
  dprime_high <- qnorm(HR_high) - qnorm(MM1FA)
  dprime_low <- qnorm(HR_low) - qnorm(MM1FA)
  
  #Saves HR and d-prime info in .txt file
  cat(paste("\n\n","High d-prime at lag",2^(HR_laghigh-1), "- HR =", HR_high,"and d' =", dprime_high), file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  cat(paste("\n\n","Low d-prime at lag",2^(HR_laglow-1), "- HR =",HR_low,"and d' =", dprime_low), file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  
  #Gets all trials that are repeats to be fed into logistic regression
  MM1dataLR <- MM1data[MM1data$isRepeat==1 & MM1data$lag!=0, ]
  #Gets the log_2(lag)
  MM1dataLR$llag <- log2(as.numeric(as.character(MM1dataLR$lag)))
  #Calculates logistic regression
  lmeHR <- glmer( corr~llag +(1|sub), data = MM1dataLR, family = "binomial")
  #Stores the summary of the logistic regression
  out = capture.output(summary(lmeHR))
  #Saves the summary of the logistic regression to the .txt file
  cat(paste("\n\n",prefix, "Hit Rate by lag", " - All Lags"), out, file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  
  #Removes immediate repetitions
  MM1dataLR = MM1dataLR[MM1dataLR$lag != 1,]
  #Recalculates the logistic regression
  lmeHR <- glmer( corr~llag +(1|sub), data = MM1dataLR, family = "binomial")
  #Grabs the output of the logistic regression
  out = capture.output(summary(lmeHR))
  #Plots HR by lag > 1 and the logistic regression
  #plotAcc(fname = paste(prefix,"Acc.pdf", sep=""), titlex = "", dfs= accLagS,lrmodel=lmeHR, axLine=4.5,xl2 = 7.10)
  
  #Saves the results of the logistic regression
  if (grepl("0",prefix) == TRUE) { 
    LTS0LogReg <<- lmeHR
  }else if (grepl("1",prefix) == TRUE) { 
    LTS1LogReg <<- lmeHR
  }else if (grepl("2",prefix) == TRUE){ 
    LTS2LogReg <<- lmeHR
  } else if (grepl("3",prefix) == TRUE){ 
    LTS3LogReg <<- lmeHR
  }
  else if (grepl("4",prefix) == TRUE){ 
    LTS4LogReg <<- lmeHR
  } 
  else{ 
    LTS5LogReg <<- lmeHR
  } 
  
  #Saves the results of the logistic regression to .txt files (one with everything 
  # one just with the regression stats)
  cat(paste("\n\n",prefix, "Hit Rate by lag", " - Exclude lag 1"), out, file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  cat(paste("\n\n",prefix, "Hit Rate by lag", " - Exclude lag 1"), out, file=paste(prefix,"tab.txt", sep=""), sep="\n", append=TRUE)
  
  ########## Format the medRT data 
  
  #Calculate the median RT by lag and subject
  rtMd <- aggregate(MM1Hit$RT, by=list(MM1Hit$sub,MM1Hit$lag), FUN=median)
  #Gets within subject error bars
  nCs = wrapperWithinSub(rtMd)
  retMed = nCs
  
  #Plot median RT with lag 1 included in line
  #plotRT(fname = paste(prefix,"medRT.pdf",sep=""), titlex = "Median by Lag", dfs= nCs, yl1=0.4,yl2=1.1, axLine=5, xl2 = nl-1)
  #Plot median RT without lag 1 included in line
  #plotRTclip(fname = paste(prefix,"medRTclip.pdf"), titlex = "", dfs= nCs, yl1=0.4,yl2=1.1, axLine=5, exclude = c(1), xl2 = nl-1 )
  
  #Used to calculate mixed effects regression
  names(rtMd) = c("sub","lag","rt")
  #Logs the lag
  rtMd$llag = log2(rtMd$lag)
  #Calculates the mixed effects regression
  lmeMd <- lme( rt~llag, random= ~1|sub, data = rtMd)
  #Stores the output of the regression
  out = capture.output(summary(lmeMd))
  #Saves the output of the regression
  cat(paste("\n\n",prefix, "Median RT by lag", " - All Lags"), out, file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  
  #Removes lag 1
  rtMd = rtMd[rtMd$lag != 1,]
  #Calculates the mixed effects regression
  lmeMd <- lme( rt~llag, random= ~1|sub, data = rtMd)
  #Gets the summary of the regression
  out = capture.output(summary(lmeMd))
  
  #Saves the results of the regression
  cat(paste("\n\n",prefix, "Median RT by lag", " - Exclude lag 1 "), out, file=paste(prefix,"empirical.txt", sep=""), sep="\n", append=TRUE)
  out = proctTable(summary(lmeMd))
  cat(paste("\n\n",prefix, "Median Rate by lag", " - Exclude lag 1"), out, file=paste(prefix,"tab.txt", sep=""), sep="\n", append=TRUE)
  
  
  
  ####### Get first decile information
  
  #Initializes dfT
  dfT<-NULL
  #Onlt want first decile
  qtick = c(0.1)
  
  #Loop through the lags and subjects
  #Need to get only correct responses
  for(lagi in lags){
    for(j in 1:len(subs)){
      #Checks which experiment we're dealing with
      if(as.numeric(substring(prefix,4,4)) < 4)
      {
        #Only responds to repeats
        #Gets all correct repeat data for a given lag
        datsel = MM1data$RT[MM1data$isResponse==1 & MM1data$isRepeat==1 & MM1data$lag!=0 & MM1data$sub==subs[j] & MM1data$lag==lagi]
        
      }
      else{
        #Responds to all trials
        #Gets all correct repeat data for a given lag
        datsel = MM1data$RT[MM1data$keypress=="LEFT" & MM1data$isRepeat==1 & MM1data$lag!=0 & MM1data$sub==subs[j] & MM1data$lag==lagi]
      }
      
      #Calculates the quantile for that lag and subject
      qd = quantile(datsel,qtick)
      #Stores the 1st decile info
      dfT = rbind(dfT, data.frame(  lag = rep(lagi,len(qtick)),sub=rep(subs[j],len(qtick)) , q = qtick, qd= qd ) )
    }
  }
  
  #Takes the log of lag
  dfT$llag = log2(dfT$lag)
  #Turns dfT into a data frame
  dfT = as.data.frame(dfT)
  #USed to generate within subject error bars
  dfQ01 = dfT[c(2,1,4)] ## Sub, lag, metric format
  #Generates within subject error bars
  nCs = wrapperWithinSub(dfQ01)
  #Plots first decile RT's by lag
  #plotRT(fname = paste(prefix,"quantile01.pdf",sep=""), titlex = "1st Decile by Lag", dfs= nCs, yl1=0.4,yl2=1.1, axLine=5, xl2 = nl-1)
  
  #Will be used to do logistic regression
  retQ01 = nCs
  #logs lag
  dfQ01$llag = log2(dfQ01$lag)
  #Calculates regression of 1st decile RT's on log lag
  lmeQ <- lme( qd~llag, random= ~1|sub, data = dfQ01)
  #Grabs the summary of the regression
  out = capture.output(summary(lmeQ))
  #Saves the results of the regression to .txt file
  cat(paste("\n\n",prefix, "1st Decile by lag", " - All Lags"), out, file=paste(prefix,"empirical.txt",sep=""), sep="\n", append=TRUE)
  
  #Removes lag 1
  dfQ01 = dfQ01[dfQ01$lag != 1,]
  #Does regression
  lmeQ <- lme( qd~llag, random= ~1|sub, data = dfQ01)
  #Grabs output of mixed effects regression
  out = capture.output(summary(lmeQ))
  #Saves output of the regression
  cat(paste("\n\n",prefix, "1st Decile RT by lag", " - Exclude lag 1 "), out, file=paste(prefix,"empirical.txt",sep=""), sep="\n", append=TRUE)
  out = proctTable(summary(lmeQ))
  cat(paste("\n\n",prefix, "1st Decile by lag", " - Exclude lag 1"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
  
  
  ##### Lag Modulation Factor Analysis
  
  
  #Will hold all of the relavant data for the analysis
  dfT<-NULL
  #Will hold each subject's intercept and slope for LMF
  dfSub<- NULL
  #List of the quantiles being considered
  qtick = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
  

  #Removes lag 1
  LMFLags <- lags[-1] 
  
  #Loop through subjects, quantiles, and lags
  for(j in 1:len(subs)){
    for(quanti in qtick){
      quanthold <- NULL
      for(lagi in LMFLags){
        #Checks which experiment
        if(as.numeric(substring(prefix,4,4)) < 4)
        {
          #Only respond to repeats
          #Gets all the correct responses
          datsel = MM1data$RT[MM1data$isResponse==1 & MM1data$isRepeat==1 & MM1data$sub==subs[j] & MM1data$lag==lagi]
          
        }
        else{
          #Respond to all
          #Gets all the correct responses
          datsel = MM1data$RT[MM1data$keypress=="LEFT" & MM1data$isRepeat==1 & MM1data$sub==subs[j] & MM1data$lag==lagi]
        }
        
        #Calculates the quantile
        qd = as.numeric(quantile(datsel,quanti))
        #Converts the RT's to ms
        quanthold <- rbind(quanthold, data.frame(lag = lagi, RT = 1000*qd))
      }
      #regress ___ quantile onto lag
      linmod <- lm(RT~log2(lag), data=quanthold)
      #Store slope of regression
      dfT <- rbind(dfT, data.frame(sub = subs[j],q = quanti,lmf = as.numeric(linmod$coefficients[2])))
      
    }
    #Stores all the info for subject
    subLMF <- dfT[dfT$sub == subs[j],]
    #Regress lag modulation factor onto quantile
    LMFlinmod <- lm(lmf~q,data=subLMF)
    #Store results of regression
    dfSub <- rbind(dfSub,data.frame(sub = subs[j], intercept = as.numeric(LMFlinmod$coefficients[1]), slope = as.numeric(LMFlinmod$coefficients[2])))
  }

  #Makes dfT a data frame
  dfT = as.data.frame(dfT)
  #Used to generate within subject error bars
  dfLMF = dfT
  #Generates within subject error bars
  nCs = wrapperWithinSub(dfLMF)
  retLMF = nCs
  
  #Calculates mixed effects regression of LMF on quantile for each subject
  lmeLMF <- lme(lmf~q, random= ~1|sub, data = dfLMF)
  #Captures output of regression and saves it
  out = capture.output(summary(lmeLMF))
  cat(paste("\n\n",prefix, "Lag Modulation Factor", " - Exclude lag 1 "), out, file=paste(prefix,"empirical.txt",sep=""), sep="\n", append=TRUE)
  out = proctTable(summary(lmeLMF))
  cat(paste("\n\n",prefix, "Lag Modulation Factor", " - Exclude lag 1"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
  
  #Creates pdf of lag modulation factor histogram at intercept for each subject
  pdf(file = paste(prefix,"AllSubsLMF.pdf",sep=""), width = 8, height = 6)
  #Sets plot aspects
  par(new=F)
  par(mar=c(5,6.8,3.5,2)+0.1)  
  xrange <- max(dfSub$intercept)+20
  axesTicks <- seq(-20,xrange,20)

  #Checks which experiment is being used
  if (grepl("0",prefix) == TRUE) { 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(10,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(0,0,0), lwd = 3, xaxt='n', yaxt='n', xlab="",ylab="", main="",cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd = 3)
      titlex = "Experiment 1"
    }else if (grepl("1",prefix) == TRUE) { 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(-5,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(0,0,0),lwd = 3, xlab="",ylab="",xaxt='n',yaxt='n', main="", cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd=3)
      titlex = "Experiment 2"
    }else if (grepl("2",prefix) == TRUE){ 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(-5,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(0,0,1), lwd = 3, xlab="",ylab="",xaxt='n',yaxt='n', main="", cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd=3)
      titlex = "Experiment 3"
    } else if (grepl("3",prefix) == TRUE){ 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(0,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(0,0,1), lwd = 3, xlab="",ylab="",xaxt='n',yaxt='n', main="", cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd=3)
      titlex = "Experiment 4"
    }
    else if (grepl("4",prefix) == TRUE){ 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(0,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(1,0,0), lwd = 3, xlab="",ylab="",xaxt='n',yaxt='n', main="", cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd=3)
      titlex = "Experiment 5"
    } 
    else{ 
      #Plots histogram of each subject's LMF at intercept
      hist(dfSub$intercept, breaks = seq(0,max(dfSub$intercept)+5,5), xlim = c(-5,xrange-15), col=rgb(1,0,0), lwd = 3, xlab="",ylab="",xaxt='n',yaxt='n', main="", cex=2)
      #Adds vertical line to emphasize zero
      abline(v=0, lwd=3)
      titlex = "Experiment 6"
    } 

  #Sets the title
  title( main = titlex ,cex.main = 3.5)
  #Sets the x-axis label
  title( xlab = "Slope Per Lag (ms)",cex.lab = 3, line = 3.5)
  #Sets the y-axis label
  title( ylab = "Frequency"  ,cex.lab = 3, line = 3.5)
  #Sets the axes and locations
  axis(1, labels=T,col.axis="black", at=axesTicks, cex.axis = 2, lwd=3) # Below
  axis(2, labels=T, col.axis="black", cex.axis = 2, lwd=3) # Left
  #Tuuns off plot
  dev.off()
  
  #Outputs the regression and error bar information for plots
  #that will use all six exp's
  return(list(retHR, retMed, retQ01, retLMF))
  
}

#Calculates the stats, saves them and plots for individual exps, and outputs info used in
# plots that contain data from all experiments.
lts0AMQ = calculateEmpirical(lts0Dat, prefix = "LTS0")
lts1AMQ = calculateEmpirical(lts1Dat, prefix = "LTS1")
lts2AMQ = calculateEmpirical(lts2Dat, prefix = "LTS2")
lts3AMQ = calculateEmpirical(lts3Dat, prefix = "LTS3")
lts4AMQ = calculateEmpirical(lts4Dat, prefix = "LTS4")
lts5AMQ = calculateEmpirical(lts5Dat, prefix = "LTS5")

#Counts how many lags there are
nl = len(unique(lts2AMQ[[1]]$llag))

#Plots hit rate as a function of lag for all six experiments
plotMulRT6(fname = "ltsAll_hr.pdf", titlex = "Hit rate", ylb = "Hit Rate",
           dfs0 = lts0AMQ[[1]], dfs1 = lts1AMQ[[1]], dfs2 = lts2AMQ[[1]], dfs3 = lts3AMQ[[1]], dfs4 = lts4AMQ[[1]], dfs5 = lts5AMQ[[1]],   
           yl1=0,yl2=1, axLine=5, xl2 = nl-1, legendPos = "bottomleft", HR=T) #splitLag = 6 ## does lag 32
#Plots median RT's as a function of lag for all six experiments
plotMulRT6(fname = "ltsAll_median.pdf", titlex = "Median RT by Lag", 
           dfs0 = lts0AMQ[[2]], dfs1 = lts1AMQ[[2]], dfs2 = lts2AMQ[[2]], dfs3 = lts3AMQ[[2]], dfs4 = lts4AMQ[[2]], dfs5 = lts5AMQ[[2]],
           yl1=0,yl2=1.1, axLine=5, xl2 = nl-1, legendPos = "bottomright")
#Plots 1st decile RT's as a function of lag for all six experiments
plotMulRT6(fname = "ltsAll_01quantile.pdf", titlex = "1st Decile by Lag", 
           dfs0 = lts0AMQ[[3]], dfs1 = lts1AMQ[[3]], dfs2 = lts2AMQ[[3]], dfs3 = lts3AMQ[[3]], dfs4 = lts4AMQ[[3]], dfs5 = lts5AMQ[[3]], 
           yl1=0,yl2=1.1, axLine=5, xl2 = nl-1, legendPos = "bottomright")
#Plots the lag modulation factor for all six experiments
#plotMulLMF6(dfs0 = lts0AMQ[[4]], dfs1 = lts1AMQ[[4]], dfs2 = lts2AMQ[[4]], dfs3 = lts3AMQ[[4]], dfs4 = lts4AMQ[[4]], dfs5 = lts5AMQ[[4]])

#############################################
############################################# Analysis of Multiple Repeats
#############################################

#This will generate the plots and calculate the stats
# for the multiple repetition analyses
calcMulRep <- function(MM1data, prefix = "LTS"){ 
  
  ####### Analysis of Multiple Repeats
  ######################################################
  
  #Will hold multiple repetition data
  MM1data$rseq = 0
  #Will be used for finding multiple reps
  MM1data$logicStr <- MM1data$ImgFile
  #Takes the log of the lag values
  MM1data$llag <- log2(MM1data$lag)
  
  #Checks which exp we're dealing with.
  if(as.numeric(substring(prefix,nchar(prefix),nchar(prefix))) < 5)
  {
    #Experiment where items could be presented up to 3 times (2 reps)
    
    #Labels all the second repetition trials
    MM1data$rseq[grepl("-mul",MM1data$logicStr) == TRUE ] = 2
    #Labels all the first repetition trials
    MM1data$rseq[grepl("-first",MM1data$logicStr) == TRUE ] = 1
    
    #Grabs all first repetition trials
    mulRep1 <- MM1data[grepl("-first",MM1data$logicStr) == TRUE & MM1data$isResponse == 1,]
    #Grabs all second repetition trials
    mulRep2 <- MM1data[grepl("-mul",MM1data$logicStr) == TRUE & MM1data$isResponse == 1,]
    #Grabs all repetition trials 
    mulRepA <- MM1data[(grepl("-mul",MM1data$logicStr) == TRUE | grepl("-first",MM1data$logicStr) == TRUE ) & MM1data$isResponse == 1,]
    #Gets all first and second repetition items (for parity with Exp 6)
    mulRep2A <- merge(mulRep2,mulRep1[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
    
  } else
  {
    #Experiment where items could be presented up to 6 times (5 reps)
    
    #Labels all the 5th repetition trials
    MM1data$rseq[grepl("-fifth",MM1data$logicStr) == TRUE ] = 5
    #Labels all the 4th repetition trials
    MM1data$rseq[grepl("-fourth",MM1data$logicStr) == TRUE ] = 4
    #Labels all the 3rd repetition trials
    MM1data$rseq[grepl("-third",MM1data$logicStr) == TRUE ] = 3
    #Labels all the 2nd repetition trials
    MM1data$rseq[grepl("-second",MM1data$logicStr) == TRUE ] = 2
    #Labels all the 1st repetition trials
    MM1data$rseq[grepl("-first",MM1data$logicStr) == TRUE ] = 1
    
    #Gets all trials that were first reptitions
    mulRep1 <- MM1data[MM1data$rseq == 1 & MM1data$keypress=="LEFT",]
    #Gets all trials that were multiple repetitions
    mulRep2 <- MM1data[MM1data$rseq > 1 & MM1data$keypress=="LEFT",]
    #Gets all 2nd repetition trials 
    mulRep2only <- MM1data[MM1data$rseq == 2 & MM1data$keypress=="LEFT",]
    #Gets all 3rd repetition trials 
    mulRep3only <- MM1data[MM1data$rseq == 3 & MM1data$keypress=="LEFT",]
    #Gets all 4th repetition trials 
    mulRep4only <- MM1data[MM1data$rseq == 4 & MM1data$keypress=="LEFT",]
    #Gets all 5th repetition trials 
    mulRep5only <- MM1data[MM1data$rseq == 5 & MM1data$keypress=="LEFT",]
    
    #Gets all repetition trials
    mulRepA = MM1data[MM1data$rseq > 0 & MM1data$keypress=="LEFT",]
    #Pulls together information for 2nd rep trials to calculate lag 1 & lag 2
    mulRep2Aonly <- merge(mulRep2only,mulRep1[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
    #Pulls together information for 3rd rep trials to calculate lag 1 & lag 2
    mulRep3Aonly <- merge(mulRep3only,mulRep2only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
    #Pulls together information for 4th rep trials to calculate lag 1 & lag 2
    mulRep4Aonly <- merge(mulRep4only,mulRep3only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
    #Pulls together information for 5th rep trials to calculate lag 1 & lag 2
    mulRep5Aonly <- merge(mulRep5only,mulRep4only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
    
    #Combines all the multiple repetition info with both lags calculated
    mulRep2A <- rbind(mulRep2Aonly,mulRep3Aonly,mulRep4Aonly,mulRep5Aonly)
    #Combines all the 3+ repetition info with both lags calculated (For follow up analysis)
    mulRep3A <- rbind(mulRep3Aonly,mulRep4Aonly,mulRep5Aonly)
    
    }
  
  #Log lag 1 and 2
  mulRep2A$lag1 = log2(mulRep2A$lag.y)
  mulRep2A$lag2 = log2(mulRep2A$lag.x)
  
  #Calculates median RT for each subject at each lag 1 X lag 2 value pairing
  mulRepMeds <- aggregate(mulRep2A$RT, by=list(mulRep2A$sub,mulRep2A$lag1,mulRep2A$lag2), FUN=median)
  #Names columns of mulRepMeds
  colnames(mulRepMeds) <- c("sub","lag1","lag2","RT")
  #Does linear mixed effects regression of lag 1 and lag 2.
  l1 = lme( RT~lag1+lag2, random= ~1|sub, data = mulRepMeds)
  
  #Grabs results of regression and saves them to .txt file
  out = capture.output(summary(l1))
  cat(paste("\n\n",prefix, "Regression of RT for Repetitions on lag1 and lag2"), out, file=paste(prefix,"rep.txt",sep=""), sep="\n", append=TRUE)
  out = proctTable(summary(l1))
  cat(paste("\n\n",prefix, "Regression of RT for rep 2 on lag1 and lag2"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
  
  #Checks if we're dealing with experiment 6 (follow up analyses)
  if(as.numeric(substring(prefix,nchar(prefix),nchar(prefix))) == 5)
  {
    #regression with trials where item has been repeated 3+ times
    
    #Takes the log of the lags
    mulRep3A$lag1 = log2(mulRep3A$lag.y)
    mulRep3A$lag2 = log2(mulRep3A$lag.x)
    #Calculates median RT for each subject at each lag 1 X lag 2 value pairing
    mulRepMeds <- aggregate(mulRep3A$RT, by=list(mulRep3A$sub,mulRep3A$lag1,mulRep3A$lag2), FUN=median)
    #Names columns of mulRepMeds
    colnames(mulRepMeds) <- c("sub","lag1","lag2","RT")
    #Does linear mixed effects regression of lag 1 and lag 2 on median response times
    l1 = lme( RT~lag1+lag2, random= ~1|sub, data = mulRepMeds)
    
    #Grabs result of regression and saves them to .txt file
    out = capture.output(summary(l1))
    cat(paste("\n\n",prefix, "Regression of RT for Repetitions on lag1 and lag2 (First Reps Excluded)"), out, file=paste(prefix,"rep.txt",sep=""), sep="\n", append=TRUE)
    out = proctTable(summary(l1))
    cat(paste("\n\n",prefix, "Regression of RT for rep 2 on lag1 and lag2 (First Reps Excluded)"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
    
    #Regression where second repetition trials are flagged
    
    #Variable to hold if trial is a second reptition
    mulRep2A$initial <- 0
    #Marks second repetiton trial
    mulRep2A$initial[grepl("-second",mulRep2A$logicStr.x) == TRUE ] <- 1
    
    #Gets all median response times for subjects at each lag 1 X lag 2 X (2nd Rep vs 3+ Rep)
    mulRepMeds <- aggregate(mulRep2A$RT, by=list(mulRep2A$sub,mulRep2A$lag1,mulRep2A$lag2,mulRep2A$initial), FUN=median)
    #Names columns of mulRepMeds
    colnames(mulRepMeds) <- c("sub","lag1","lag2","initial","RT")
    #Does linear mixed effects regression of lag 1, lag2, and repetition type on median RT's
    l1 = lme( RT~lag1+lag2+initial, random= ~1|sub, data = mulRepMeds)
    
    #Grabs result of regression and saves it to .txt file
    out = capture.output(summary(l1))
    cat(paste("\n\n",prefix, "Regression of RT for Repetitions on lag1 and lag2"), out, file=paste(prefix,"rep.txt",sep=""), sep="\n", append=TRUE)
    out = proctTable(summary(l1))
    cat(paste("\n\n",prefix, "Regression of RT for rep 2 on lag1 and lag2"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
     
  }
  
  
  #Analysis of effect of most recent lag and # of repetitions on median response times
  
  #Get median RT's for all multiple repetition items as a function of lag
  rtMd <- aggregate(mulRep2$RT, by=list(mulRep2$sub,mulRep2$llag), FUN=median)
  #Names the columns
  names(rtMd) = c("sub","llag","rt")
  #Gets all the unique lags
  repLags = unique(rtMd$llag) 
  
  #Checks which exp is being processed
  if(as.numeric(substring(prefix,nchar(prefix),nchar(prefix))) < 4)
  {
    #2nd repetition max
    
    #Gets median RT's for each lag for all multiple repetitions
    repMdraw <- aggregate(mulRep2$RT, by=list(mulRep2$sub,mulRep2$lag), FUN=median)
    #Gets all the trials without repetitions but with lags that correspond to second repetitions
    nonRep = MM1data[grepl("-mul",MM1data$logicStr) == FALSE & (MM1data$llag %in% repLags) & (MM1data$isResponse == 1),]
    #Gets the median RT's for each lag for first repetitions with valid lags
    rtMdNR <- aggregate(nonRep$RT, by=list(nonRep$sub,nonRep$llag), FUN=median)
    #Names columns
    names(rtMdNR) = c("sub","llag","rt")
    
    #Labels 2nd repetition trials
    rtMd$cond = 1
    #Labels 1st repetition trials
    rtMdNR$cond = 0
    #Combines the median RT's for first and second repetition trials
    rtAll = rbind(rtMd,rtMdNR)
    #Performs regression of lag and repetition on median RT
    lmeMd <- lme( rt~llag + cond + llag*cond, random= ~1|sub, data = rtAll) 
    #Grabs output of regression and saves it to 
    out = capture.output(summary(lmeMd))
    cat(paste("\n\n",prefix, "Multiple regression with lag and repetition"), out, file=paste(prefix,"rep.txt",sep=""), sep="\n", append=TRUE)
    cat(paste("\n\n",prefix, "Multiple regression with lag and repetition"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
    #Calculates within subject error bars
    dfs <- wrapperWithinSub2(rtAll)
    
    #Iterator to deal with line color
    temppp <- 0
    
    #If statements to get title, line color, and line type right for each plot
    if(prefix == "LTS0")
    {
      pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 4, height = 6)
      par(new=F)
      par(mar=c(5,8,3.5,2)+0.1)  
      
      
      par(new=F)
      
      #Loop over the repetitions
      for(i in sort(unique(dfs$rep)))
      {
        #Get the values for repetition i
        currentvalues <- dfs[dfs$rep == i,]
        #Plot the median RT's for lags on repetition i
        if(temppp == 0)
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,7),ylim = c(0.7,1.1),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2)
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=1, lwd = 2)
          
          temppp <- temppp+1
        }
        else
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,7), ylim = c(0.7,1.1), type = 'p', pch=2,col=rgb(.5,.5,.5),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2, col=rgb(.5,.5,.5))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(.5,.5,.5), lwd = 2)
          
        }
        #Keeps plot open
        par(new = T)
      }
      
      #Changes the lag markings on the xaxis
      axis(1,at=c(1,2,3,4,5,6,7),labels = c(2, 4, 8, 16, 32, 64, 128),cex.lab=2,cex.axis=2,cex.main=2)
      
      legend("bottomleft",legend = c('First Repetition','Second Repetition'), pch=1:2,col=c('black',rgb(.5,.5,.5)),cex = 1, lty=1)
      dev.off()
      
    }
    else if(prefix == "LTS1"){
      
      pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 8, height = 6)
      par(new=F)
      # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
      par(mar=c(5,8,3.5,2)+0.1)  
      
      
      par(new=F)
      
      #Loop over the repetitions
      for(i in sort(unique(dfs$rep)))
      {
        #Get the values for repetition i
        currentvalues <- dfs[dfs$rep == i,]
        #Plot the median RT's for lags on repetition i
        if(temppp == 0)
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,4),ylim = c(0.4,0.8),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)),lty=2,lwd = 2)
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=1, lwd = 2)
          
          temppp <- temppp+1
        }
        else
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,4), ylim = c(0.4,0.8), type = 'p', pch=2,col=rgb(.5,.5,.5),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 2,lwd = 2, col=rgb(.5,.5,.5))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(.5,.5,.5), lwd = 2)
          
        }
        #Keeps plot open
        par(new = T)
      }
      
      #Changes the lag markings on the xaxis
      axis(1,at=c(1,2,4),labels = c(2, 4, 16),cex.lab=2,cex.axis=2,cex.main=2)
      
      legend("bottomleft",legend = c('First Repetition','Second Repetition'), pch=1:2,col=c('black',rgb(.5,.5,.5)),cex = 1, lty=2)
      dev.off()
    }
    else if(prefix == "LTS2"){
      
      pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 8, height = 6)
      par(new=F)
      # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
      par(mar=c(5,8,3.5,2)+0.1)  
      
      
      par(new=F)
      
      #Loop over the repetitions
      for(i in sort(unique(dfs$rep)))
      {
        #Get the values for repetition i
        currentvalues <- dfs[dfs$rep == i,]
        #Plot the median RT's for lags on repetition i
        if(temppp == 0)
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,4),ylim = c(0.4,0.8),col=rgb(0,0,1),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2,col=rgb(0,0,1))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(0,0,1), lwd = 2)
          
          temppp <- temppp+1
        }
        else
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,4), ylim = c(0.4,0.8), type = 'p', pch=2,col=rgb(0,0,0.5),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2, col=rgb(0,0,1,0.5))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(0,0,1,0.5), lwd = 2)
          
        }
        #Keeps plot open
        par(new = T)
      }
      
      #Changes the lag markings on the xaxis
      axis(1,at=c(1,2,4),labels = c(2, 4, 16),cex.lab=2,cex.axis=2,cex.main=2)
      
      legend("bottomleft",legend = c('First Repetition','Second Repetition'), pch=1:2,col=c(rgb(0,0,1),rgb(0,0,1,.5)),cex = 1, lty=1)
      dev.off()
    }
    else if(prefix == "LTS3"){
      
      pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 8, height = 6)
      par(new=F)
      # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
      par(mar=c(5,8,3.5,2)+0.1)  
      
      
      par(new=F)
      
      #Loop over the repetitions
      for(i in sort(unique(dfs$rep)))
      {
        #Get the values for repetition i
        currentvalues <- dfs[dfs$rep == i,]
        #Plot the median RT's for lags on repetition i
        if(temppp == 0)
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,4),ylim = c(0.4,0.8),col=rgb(0,0,1),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 2,lwd = 2,col=rgb(0,0,1))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(0,0,1), lwd = 2)
          
          temppp <- temppp+1
        }
        else
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,4), ylim = c(0.4,0.8), type = 'p', pch=2,col=rgb(0,0,1,0.5),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 2,lwd = 2, col=rgb(0,0,1,0.5))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(0,0,1,0.5), lwd = 2)
          
        }
        #Keeps plot open
        par(new = T)
      }
      #Changes the lag markings on the xaxis
      axis(1,at=c(1,2,4),labels = c(2, 4, 16),cex.lab=2,cex.axis=2,cex.main=2)
      
      legend("bottomleft",legend = c('First Repetition','Second Repetition'), pch=1:2,col=c(rgb(0,0,1),rgb(0,0,1,.5)),cex = 1, lty=2)
      dev.off()
    } 
    else if(prefix == "LTS4"){
      
      pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 8, height = 6)
      par(new=F)
      # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
      par(mar=c(5,8,3.5,2)+0.1)  
      
      
      par(new=F)
      
      #Loop over the repetitions
      for(i in sort(unique(dfs$rep)))
      {
        #Get the values for repetition i
        currentvalues <- dfs[dfs$rep == i,]
        #Plot the median RT's for lags on repetition i
        if(temppp == 0)
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,4),ylim = c(0.4,0.8),col=rgb(1,0,0),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2,col=rgb(1,0,0))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(1,0,0), lwd = 2)
          
          temppp <- temppp+1
        }
        else
        {
          plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,4), ylim = c(0.4,0.8), type = 'p', pch=2,col=rgb(1,0,0,.5),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
          #Plots the line of best fit through the points for repetition i
          abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 1,lwd = 2, col=rgb(1,0,0,.5))
          #Add the errorbars for repetition i
          add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(1,0,0,.5), lwd = 2)
          
        }
        #Keeps plot open
        par(new = T)
      }
      
      #Changes the lag markings on the xaxis
      axis(1,at=c(1,2,4),labels = c(2, 4, 16),cex.lab=2,cex.axis=2,cex.main=2)
      
      legend("bottomleft",legend = c('First Repetition','Second Repetition'), pch=1:2,col=c(rgb(1,0,0),rgb(1,0,0,.5)),cex = 1, lty=1)
      dev.off()
    }
    
    
    
    
  }else
  {
    #5th repetition max
    
    #Gets median RT's for each subject X lag X repetition combination
    repMdraw <- aggregate(mulRep2$RT, by=list(mulRep2$sub,mulRep2$lag,mulRep2$rseq), FUN=median)
    #Gets all first repetition trials with lags that match those for multiple repetitions
    nonRep = MM1data[grepl("-second",MM1data$logicStr) == FALSE & grepl("-third",MM1data$logicStr) == FALSE & grepl("-fourth",MM1data$logicStr) == FALSE & grepl("-fifth",MM1data$logicStr) == FALSE & (MM1data$llag %in% repLags) & (MM1data$keypress == "LEFT"),]
    #Gets the median RT's for each subject X lag for first repetitions
    rtMdNR <- aggregate(nonRep$RT, by=list(nonRep$sub,nonRep$llag), FUN=median)
    #Names the columns of first rep trials
    names(rtMdNR) = c("sub","llag","rt")
    #Adds indicator for first repetitions
    rtMdNR$cond <- 0
    #Rearranges columns
    rtMdNR <- rtMdNR[,c(1,2,4,3)]
    #Add column names to mul rep trials
    colnames(repMdraw) <- colnames(rtMdNR)
    #Takes the log of the lags for the mul rep trials
    repMdraw$llag <- log2(as.numeric(repMdraw$llag))
    #Decreases the value of the condition column so that it goes 0,1,2,3,4 instead of 0,2,3,4,5
    repMdraw$cond <- as.numeric(repMdraw$cond - 1)
    #Combines all the repetitions
    rtAll = rbind(repMdraw,rtMdNR)
    
    #Performs regression of lag, condition, and interaction on RT
    lmeMd <- lme( rt~llag + cond + llag*cond, random= ~1|sub, data = rtAll) 
    #Grabs results of regression and writes them to .txt file
    out = capture.output(summary(lmeMd))
    cat(paste("\n\n",prefix, "Multiple regression with lag and repetition"), out, file=paste(prefix,"rep.txt",sep=""), sep="\n", append=TRUE)
    cat(paste("\n\n",prefix, "Multiple regression with lag and repetition"), out, file=paste(prefix,"tab.txt",sep=""), sep="\n", append=TRUE)
    
    #Generates error bars
    dfs <- wrapperWithinSub3(rtAll)
    
    #Creates .pdf for plot
    pdf(file = paste(prefix,"MulMed.pdf",sep=""), width = 4, height = 6)
    #Plot settings
    par(new=F)
    par(mar=c(5,5,3.5,1)+0.1)  
    #Iterator for line color
    coli <- 0
    pchi <- 1
    
    #Loop over the repetitions
    for(i in sort(unique(dfs$rep)))
    {
      #Get the values for repetition i
      currentvalues <- dfs[dfs$rep == i,]
      #Plot the median RT's for lags on repetition i
      if(coli == 0)
      {
        plot(as.numeric(currentvalues$llag),currentvalues$rt, xlim = c(1,4),ylim = c(0.4,0.8),col=rgb(1,0,0),type = 'p', xaxt = 'n',  xlab="Lag", ylab = "Response Time", main = 'Median RT by Lag',cex.lab=2,cex.axis=2,cex.main=2)
        #Plots the line of best fit through the points for repetition i
        abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 2,lwd = 2,col=rgb(1,0,0))
        #Add the errorbars for repetition i
        add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(1,0,0), lwd = 2)
      }
      else
      {
        plot(as.numeric(currentvalues$llag),currentvalues$rt,xlim = c(1,4), ylim = c(0.4,0.8), type = 'p', pch=pchi,col=rgb(1,0,0,1-(.15*coli)),yaxt ='n', xaxt = 'n',  xlab="", ylab = "", main = "",cex.lab=2,cex.axis=2,cex.main=2)
        #Plots the line of best fit through the points for repetition i
        abline(lm(currentvalues$rt~as.numeric(currentvalues$llag)), lty= 2,lwd = 2, col=rgb(1,0,0,1-(.15*coli)))
        #Add the errorbars for repetition i
        add.error.bars(as.numeric(currentvalues$llag), currentvalues$rt,currentvalues$ci,0.1,col=rgb(1,0,0,1-(.15*coli)), lwd = 2)
        
      }
      #Keeps plot open
      coli <- coli + 1
      pchi <- pchi + 1
      if (pchi == 3)
      {
        pchi = pchi+1
      }
      par(new = T)
    }
    
    #Changes the lag markings on the xaxis
    axis(1,at=c(1,2,4),labels = c(2, 4, 16), cex.lab=2,cex.axis=2,cex.main=2)
    
    legend("bottomleft",legend = c('First Repetition','Second Repetition','Third Repetition','Forth Repetition','Fifth Repetition'), pch=c(1,2,4,5,6),col=c(rgb(1,0,0),rgb(1,0,0,.85),rgb(1,0,0,.70),rgb(1,0,0,.55),rgb(1,0,0,.4)),cex = 1, lty=2)
    par(new=F)
    #Closes figure
    dev.off()
       
  }
}

#This will generate the plots and calculate the stats
# for the lag modulation factor analysis in experiment 6
calcMulRepLMF <- function(MM1data, prefix = "LTS5"){ 
  
  #Adds column for holding # of repetition for trial
  MM1data$rseq = 0
  #"Changes" column name
  MM1data$logicStr <- MM1data$ImgFile
  #Gets the log of the lag
  MM1data$llag = log2(MM1data$lag)
  
  #Labels how many times an item was presented
  MM1data$rseq[grepl("-fifth",MM1data$logicStr) == TRUE ] = 5
  MM1data$rseq[grepl("-fourth",MM1data$logicStr) == TRUE ] = 4
  MM1data$rseq[grepl("-third",MM1data$logicStr) == TRUE ] = 3
  MM1data$rseq[grepl("-second",MM1data$logicStr) == TRUE ] = 2
  MM1data$rseq[grepl("-first",MM1data$logicStr) == TRUE ] = 1
  
  #Gets all the first repetition trials
  mulRep1 = MM1data[MM1data$rseq == 1 & MM1data$keypress=="LEFT",]
  #Gets all the multiple repetition trials
  mulRep2 = MM1data[MM1data$rseq > 1 & MM1data$keypress=="LEFT",]
  #Gets all the 2nd repetition trials
  mulRep2only = MM1data[MM1data$rseq == 2 & MM1data$keypress=="LEFT",]
  #Gets all the 3rd repetition trials
  mulRep3only = MM1data[MM1data$rseq == 3 & MM1data$keypress=="LEFT",]
  #Gets all the 4th repetition trials
  mulRep4only = MM1data[MM1data$rseq == 4 & MM1data$keypress=="LEFT",]
  #Gets all the 5th repetition trials
  mulRep5only = MM1data[MM1data$rseq == 5 & MM1data$keypress=="LEFT",]
  
  #Gets all repetition trials
  mulRepA = MM1data[MM1data$rseq > 0 & MM1data$keypress=="LEFT",]
  #Calculates lag 1 and lag 2 for 2nd repetition trials
  mulRep2Aonly = merge(mulRep2only,mulRep1[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
  #Calculates lag 1 and lag 2 for 3rd repetition trials
  mulRep3Aonly = merge(mulRep3only,mulRep2only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
  #Calculates lag 1 and lag 2 for 4th repetition trials
  mulRep4Aonly = merge(mulRep4only,mulRep3only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
  #Calculates lag 1 and lag 2 for 5th repetition trials
  mulRep5Aonly = merge(mulRep5only,mulRep4only[,c("sub", "file", "logicStr","lag")], by=c("sub", "file") )
  #Puts all the mul rep trials together with lag 1 and lag 2
  mulRep2Plus = rbind(mulRep2Aonly,mulRep3Aonly,mulRep4Aonly,mulRep5Aonly)
  
  #Duplicates the lag info
  mulRep2Plus$lag <- mulRep2Plus$lag.x
  #Gets a list of the lags considered
  lags = sort(unique(mulRep2Plus$lag))
  #Gets a list of the subjects
  subs = unique(mulRep2Plus$sub)
  #Counts how many lags there are
  nl = len(lags)
  #Shortens variable name
  MM1sel = mulRep2Plus
  
  ############################################################
  ##### Lag Modulation Factor Analysis
  
  
  #Will hold all of the relavant data for the analysis
  dfT<-NULL
  #Will hold each subject's intercept and slope for LMF
  dfSub<- NULL
  #List of the quantiles being considered
  qtick = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
  
  #Stores the lags
  LMFLags <- lags
  
  #Loop through each lag, quantile, and subject
  for(j in 1:len(subs)){
    for(quanti in qtick){
      quanthold <- NULL
      for(lagi in LMFLags){
        
        #Gets the data we want      
        datsel = MM1sel$RT[MM1sel$keypress=="LEFT" & MM1sel$isRepeat==1 & MM1sel$sub==subs[j] & MM1sel$lag==lagi]
        #Calculates the quantile
        qd = as.numeric(quantile(datsel,quanti))
        #Stores the quantile in ms
        quanthold <- rbind(quanthold, data.frame(lag = lagi, RT = 1000*qd))
      }
      
      #calculates effect of lag on median RT for current quantile
      linmod <- lm(RT~log2(lag), data=quanthold)
      #Adds info to full data frame
      dfT <- rbind(dfT, data.frame(sub = subs[j],q = quanti,lmf = as.numeric(linmod$coefficients[2])))
    }
    
    #Gets the data for just the current subject
    subLMF <- dfT[dfT$sub == subs[j],]
    #Regresses lag modulation factor as a function of quantile 
    LMFlinmod <- lm(lmf~q,data=subLMF)
    #Stores the subject's intercept and slope of LMF
    dfSub <- rbind(dfSub,data.frame(sub = subs[j], intercept = as.numeric(LMFlinmod$coefficients[1]), slope = as.numeric(LMFlinmod$coefficients[2])))
  }
  
  #Turns dfT into a dataframe
  dfT = as.data.frame(dfT)
  #Holds onto 
  dfLMF = dfT ## Sub, lag, metric format
  #Calculates error bars
  nCs = wrapperWithinSub(dfT)
  #
  retLMF = nCs
  
  #lmeLMF <- lme(lmf~q, random= ~1|sub, data = dfLMF)
  lmeLMF <- lme(lmf~q, random= ~1|sub, data = dfT)
  out = capture.output(summary(lmeLMF))
  cat(paste("\n\n",prefix, "Lag Modulation Factor", " - Exclude lag 1 "), out, file=paste(prefix,"LMF.txt",sep=""), sep="\n", append=TRUE)
  
  out = proctTable(summary(lmeLMF))
  cat(paste("\n\n",prefix, "Lag Modulation Factor", " - Exclude lag 1"), out, file=paste(prefix,"LMF.txt",sep=""), sep="\n", append=TRUE)

}

#Reload the data and process it, but don't remove multiple repetitions
lts0DatM = preProcess("LTS0", removeMul=0)
lts1DatM = preProcess("LTS1", removeMul=0)
lts2DatM = preProcess("LTS2", removeMul=0)
lts3DatM = preProcess("LTS3", removeMul=0)
lts4DatM = preProcess("LTS4", removeMul=0)
lts5DatM = preProcess("LTS5", removeMul=0)

#Makes sure we're in the right directory
setwd(mainDir)
#Creates a folder name for multiple repetition plots
subDir = paste("plotsMulRep", Sys.Date(),sep = "")
#Creates the folder
dir.create(file.path(mainDir, subDir ))
#Sets the working directory to that folder
setwd(file.path(mainDir, subDir))

#Runs the stats and makes plots concerning multiple repetitions
calcMulRep(lts0DatM, prefix = "LTS0")
calcMulRep(lts1DatM, prefix = "LTS1")
calcMulRep(lts2DatM, prefix = "LTS2")
calcMulRep(lts3DatM, prefix = "LTS3")
calcMulRep(lts4DatM, prefix = "LTS4")
calcMulRep(lts5DatM, prefix = "LTS5")

#Calculates stats for Lag modulation factor
calcMulRepLMF(lts5DatM, prefix = "LTS5")

