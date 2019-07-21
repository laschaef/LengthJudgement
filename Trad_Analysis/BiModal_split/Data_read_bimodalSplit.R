library(data.table)
library(caTools)

# read in all datasets
dat1 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_1.txt', header= TRUE)
dat2 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_2.txt', header= TRUE)
dat3 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_3.txt', header= TRUE)
dat4 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_4.txt', header= TRUE)
dat5 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_5.txt', header= TRUE)
dat6 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_6.txt', header= TRUE)
dat7 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_7.txt', header= TRUE)
dat8 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_8.txt', header= TRUE)
dat9 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_9.txt', header= TRUE)
dat10 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_10.txt', header= TRUE)
dat11 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_11.txt', header= TRUE)
dat12 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_12.txt', header= TRUE)
dat13 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_13.txt', header= TRUE)
dat14 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_14.txt', header= TRUE)
dat15 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_15.txt', header= TRUE)
dat16 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_16.txt', header= TRUE)
dat17 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_17.txt', header= TRUE)
dat18 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_18.txt', header= TRUE)
dat19 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_19.txt', header= TRUE)
dat20 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_20.txt', header= TRUE)
dat21 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_21.txt', header= TRUE)
dat22 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_22.txt', header= TRUE)
dat23 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_23.txt', header= TRUE)
dat24 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_24.txt', header= TRUE)
dat25 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_25.txt', header= TRUE)
dat26 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_26.txt', header= TRUE)
dat27 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_27.txt', header= TRUE)
dat28 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_28.txt', header= TRUE)
dat29 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_29.txt', header= TRUE)
dat30 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_30.txt', header= TRUE)
dat31 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_31.txt', header= TRUE)
dat32 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_32.txt', header= TRUE)
dat33 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_33.txt', header= TRUE)
dat34 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_34.txt', header= TRUE)
dat35 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_35.txt', header= TRUE)
dat36 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_36.txt', header= TRUE)
dat37 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_37.txt', header= TRUE)
dat38 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_38.txt', header= TRUE)
dat39 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_39.txt', header= TRUE)
dat40 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_40.txt', header= TRUE)
dat41 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_41.txt', header= TRUE)
dat42 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_42.txt', header= TRUE)
dat43 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_43.txt', header= TRUE)
dat44 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_44.txt', header= TRUE)
dat45 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_45.txt', header= TRUE)
dat46 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_46.txt', header= TRUE)
dat47 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_47.txt', header= TRUE)
dat48 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_48.txt', header= TRUE)
dat49 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_49.txt', header= TRUE)
dat50 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_50.txt', header= TRUE)
dat52 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_52.txt', header= TRUE)
dat54 = read.delim('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_54.txt', header= TRUE)



# ******* combine short RT participants data into 1 file **********
#______________________________________________________________________________________________________________________________________
dat = rbind(dat5, dat7, dat11, dat15, dat17, dat21, dat23, dat27, dat33, dat39, dat41, dat46, dat47,
            dat54)
dat = na.omit(dat)  # 16800 obs
View(dat)
names(dat)
names(dat)[8]<-'decide'

# ******* Add in column to specify which condition *********
dat$condition = 1
dat[dat$decide=='NO AID' & dat$difficulty=='hard' & dat$falseAlarm=='n/a','condition'] = 2
dat[dat$decide=='DECIDE' & dat$difficulty=='easy' & dat$falseAlarm=='low','condition'] = 3
dat[dat$decide=='DECIDE' & dat$difficulty=='hard' & dat$falseAlarm=='low','condition'] = 4
dat[dat$decide=='DECIDE' & dat$difficulty=='easy' & dat$falseAlarm=='high','condition'] = 5
dat[dat$decide=='DECIDE' & dat$difficulty=='hard' & dat$falseAlarm=='high','condition'] = 6
dat[dat$decide=='REJECT' & dat$difficulty=='easy' & dat$falseAlarm=='low','condition'] = 7
dat[dat$decide=='REJECT' & dat$difficulty=='hard' & dat$falseAlarm=='low','condition'] = 8
dat[dat$decide=='REJECT' & dat$difficulty=='easy' & dat$falseAlarm=='high','condition'] = 9
dat[dat$decide=='REJECT' & dat$difficulty=='hard' & dat$falseAlarm=='high','condition'] = 10


# ****** only use participants with overall accuracy over 70% - newdat1 - used this
#______________________________________________________________________________________________________________________________________
ID_mean <- aggregate(. ~ ID, data = dat, FUN = mean)  # calculate overall accuracy for each participant

ID_bad_by_ID <- NULL
for(i in 1:max(ID_mean$ID)){
  if(ID_mean$ACC[i] < 0.70) ID_bad_by_ID <- c(ID_bad_by_ID, ID_mean$ID[i])
}

newdat1 <- dat[ !(dat$ID %in% ID_bad_by_ID), ]  # 16800 obs

# ****** write csv *********
write.csv(newdat1, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_overallACC_shortRT.csv')



# Reformat for 7020 analysis
newdat1$autoRT = NULL
newdat1$trial.start.time = NULL
newdat1$RTand = NULL
newdat1$RTor = NULL
# newdat1$order = NULL
newdat1$experiment = NULL

# ******* treatment A = decision type 
newdat1$A = 1
newdat1[newdat1$decide=='REJECT','A'] = 2
newdat1[newdat1$decide=='NO AID','A'] = 3

# ******* treatment B = aid accuracy 
newdat1$B = 1
newdat1[newdat1$falseAlarm=='low','B'] = 2
newdat1[newdat1$falseAlarm=='n/a','B'] = 3

# ******* treatment C = difficulty 
# a1 = easy
# a2 = hard
newdat1$C = 1
newdat1[newdat1$difficulty=='hard','C'] = 2

# ****** write csv for accuracy bar plot
ID_cond_mean <- aggregate(. ~ ID + condition, data = newdat1, FUN = mean)  # find mean for each participant by condition
View(ID_cond_mean)

## Insert means into dat 

Y_bar.jkl = tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$B,ID_cond_mean$C),mean)

n=14
ID_cond_mean$Y_bar.jkl = c(rep(Y_bar.jkl[3,3,1],n),rep(Y_bar.jkl[3,3,2],n),
                           rep(Y_bar.jkl[1,2,1],n),rep(Y_bar.jkl[1,2,2],n),rep(Y_bar.jkl[1,1,1],n),
                           rep(Y_bar.jkl[1,1,2],n),rep(Y_bar.jkl[2,2,1],n),rep(Y_bar.jkl[2,2,2],n),
                           rep(Y_bar.jkl[2,1,1],n),rep(Y_bar.jkl[2,1,2],n))  # need to match actual order of A, B, C

write.csv(ID_cond_mean, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/for_bar_plot_bySubject_shortRT.csv')


# remove condition 1 and 2 responses from the data
row_sub = apply(newdat1[14], 1, function(row) all(row >=3 ))  # find rows where condition >= 3
newdat1 = newdat1[row_sub,]  # only use rows where condition >= 3  # 13440 obs


# ****** write csv *********
write.csv(newdat1, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/2-Spring2019/PSY7020-ANOVA/Data_for_7020_analysis_shortRT.csv')


# ******* combine long RT participants data into 1 file **********
#______________________________________________________________________________________________________________________________________
dat = rbind(dat1, dat2, dat6, dat8, dat9, dat10, dat12, dat13, dat16, dat18, 
            dat19, dat22, dat26, dat28, dat29, dat30, dat32, dat34,
            dat35, dat36, dat37, dat38, dat40, dat42, dat43, dat44, dat45, dat48, 
            dat50, dat52)
dat = na.omit(dat)  # 36000 obs
View(dat)
names(dat)
names(dat)[8]<-'decide'

# ******* Add in column to specify which condition *********
dat$condition = 1
dat[dat$decide=='NO AID' & dat$difficulty=='hard' & dat$falseAlarm=='n/a','condition'] = 2
dat[dat$decide=='DECIDE' & dat$difficulty=='easy' & dat$falseAlarm=='low','condition'] = 3
dat[dat$decide=='DECIDE' & dat$difficulty=='hard' & dat$falseAlarm=='low','condition'] = 4
dat[dat$decide=='DECIDE' & dat$difficulty=='easy' & dat$falseAlarm=='high','condition'] = 5
dat[dat$decide=='DECIDE' & dat$difficulty=='hard' & dat$falseAlarm=='high','condition'] = 6
dat[dat$decide=='REJECT' & dat$difficulty=='easy' & dat$falseAlarm=='low','condition'] = 7
dat[dat$decide=='REJECT' & dat$difficulty=='hard' & dat$falseAlarm=='low','condition'] = 8
dat[dat$decide=='REJECT' & dat$difficulty=='easy' & dat$falseAlarm=='high','condition'] = 9
dat[dat$decide=='REJECT' & dat$difficulty=='hard' & dat$falseAlarm=='high','condition'] = 10


# ****** only use participants with overall accuracy over 70% - newdat1 - used this
#______________________________________________________________________________________________________________________________________
ID_mean <- aggregate(. ~ ID, data = dat, FUN = mean)  # calculate overall accuracy for each participant

ID_bad_by_ID <- NULL
for(i in 1:max(ID_mean$ID)){
  if(ID_mean$ACC[i] < 0.70) ID_bad_by_ID <- c(ID_bad_by_ID, ID_mean$ID[i])
}

newdat1 <- dat[ !(dat$ID %in% ID_bad_by_ID), ]  # 36000 obs

# ****** write csv *********
write.csv(newdat1, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_overallACC_longRT.csv')



# Reformat for 7020 analysis
newdat1$autoRT = NULL
newdat1$trial.start.time = NULL
newdat1$RTand = NULL
newdat1$RTor = NULL
# newdat1$order = NULL
newdat1$experiment = NULL

# ******* treatment A = decision type 
newdat1$A = 1
newdat1[newdat1$decide=='REJECT','A'] = 2
newdat1[newdat1$decide=='NO AID','A'] = 3

# ******* treatment B = aid accuracy 
newdat1$B = 1
newdat1[newdat1$falseAlarm=='low','B'] = 2
newdat1[newdat1$falseAlarm=='n/a','B'] = 3

# ******* treatment C = difficulty 
# a1 = easy
# a2 = hard
newdat1$C = 1
newdat1[newdat1$difficulty=='hard','C'] = 2

# ****** write csv for accuracy bar plot
ID_cond_mean <- aggregate(. ~ ID + condition, data = newdat1, FUN = mean)  # find mean for each participant by condition
View(ID_cond_mean)

## Insert means into dat 

Y_bar.jkl = tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$B,ID_cond_mean$C),mean)

n=30
ID_cond_mean$Y_bar.jkl = c(rep(Y_bar.jkl[3,3,1],n),rep(Y_bar.jkl[3,3,2],n),
                           rep(Y_bar.jkl[1,2,1],n),rep(Y_bar.jkl[1,2,2],n),rep(Y_bar.jkl[1,1,1],n),
                           rep(Y_bar.jkl[1,1,2],n),rep(Y_bar.jkl[2,2,1],n),rep(Y_bar.jkl[2,2,2],n),
                           rep(Y_bar.jkl[2,1,1],n),rep(Y_bar.jkl[2,1,2],n))  # need to match actual order of A, B, C

write.csv(ID_cond_mean, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/for_bar_plot_bySubject_longRT.csv')


# remove condition 1 and 2 responses from the data
row_sub = apply(newdat1[14], 1, function(row) all(row >=3 ))  # find rows where condition >= 3
newdat1 = newdat1[row_sub,]  # only use rows where condition >= 3  # 28800 obs


# ****** write csv *********
write.csv(newdat1, file = 'C:/Users/lasch/OneDrive/Documents/Wright State/2-Spring2019/PSY7020-ANOVA/Data_for_7020_analysis_longRT.csv')


