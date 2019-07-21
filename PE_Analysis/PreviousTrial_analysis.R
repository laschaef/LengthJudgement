library(UsingR)
library(ggplot2)
library(ez)
library("Hmisc")  # for error bars
library(psych)
library(lme4)

#______________________________________________________________________________________________________________________________________
## create standard error function-- 
se <- function(x) { 
  y <- x[!is.na(x)] 
  sqrt(var(as.vector(y))/length(y)) 
} 

#______________________________________________________________________________________________________________________________________
################### ******* read in data ******* ###################

# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_combined.csv')
dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_overallACC.csv')
# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_overallACC_shortRT.csv')
# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_overallACC_longRT.csv')
# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Mahoney_exp_decideACC.csv')

names(dat)
dat$X = NULL

#______________________________________________________________________________________________________________________________________
################### ******* Add columns to data ******** ############################

# add column for number value for decision type
dat$decide_lvl = 0
dat[dat$decide=='REJECT','decide_lvl'] = 1

# add column for number value for aid accuracy
dat$prob = 0.8
dat[dat$falseAlarm=='low','prob'] = 0.95

# add column that has true/false (1/0) of whether previous trial was incorrect (ACC.0==1 means previous trial incorrect)
dat$ACC.0 = 0
nmax = length(dat$ID)
for(n in 2:nmax){
  if(dat$ACC[n-1]==0) dat$ACC.0[n]=1
}

# # add column that has true/false of whether trial before incorrect trial was correct (ACC.0.1==1 means it was correct)
# dat$ACC.0.1 = 0
# for(n in 2:nmax){
#   if(dat$ACC.0[n]==1 & dat$ACC.0[n-1]==0) dat$ACC.0.1[n]=1
# }

# add column that has true/false of pre-error, post-correct trial for the incorrect trial (ACC.0.1==1 means it was pre-error, post-correct trial)
dat$ACC.0.1 = 0
for(n in 4:nmax){
  if(dat$ACC.0[n]==1 & dat$ACC[n-3]==1) dat$ACC.0.1[n-2]=1
}

# take RT diff for each PE and preE,postC pair 
dat$RT_adj = NaN
for(n in 4:nmax){
  if(dat$ACC.0[n]==1 & dat$ACC[n-3]==1) dat$RT_adj[n]=dat$humanRT[n] - dat$humanRT[n-2]
}

# take ACC diff for each PE and preE,postC pair
dat$ACC_adj = NaN
for(n in 4:nmax){
  if(dat$ACC.0[n]==1 & dat$ACC[n-3]==1) dat$ACC_adj[n]=dat$ACC[n] - dat$ACC[n-2]
}

################### ******* Remove zero RT ******** ############################
# ******* Go through each row and determine if human RT value is zero *********
row_sub = apply(dat[12], 1, function(row) all(row !=0 ))  # find rows where RT != 0
dat = dat[row_sub,]  # only use rows where RT != 0

# use 4.167783 sec as cutoff to remove (based on 44 participants)

# remove RT over 4.16 sec
row_sub = apply(dat[12], 1, function(row) all(row < 4.16))  # find rows where RT <4.16
dat = dat[row_sub,]  # only use rows where RT < 4.16

boxplot(dat$humanRT ~ dat$condition)


################### ******* Create add'l data frames ******** ############################
# separate data by NO AID trials
row_sub = apply(dat[8], 1, function(row) all(row != 'NO AID' ))  # find rows without NO AID response
dat.aid = dat[row_sub,]  # only use rows without NO AID response
dat.noaid = dat[!row_sub,]  # only use rows with NO AID response

# dataset with only correct responses
row_sub = apply(dat[15], 1, function(row) all(row ==1 ))  # find rows with correct responses
dat.correct = dat[row_sub,]  # only use rows with correct responses

# correct data with and without aid trials
row_sub = apply(dat.correct[8], 1, function(row) all(row != 'NO AID' ))  # find rows without NO AID response
dat.correct_aid = dat.correct[row_sub,]  # only use rows without NO AID response
dat.correct_noaid = dat.correct[!row_sub,]  # only use rows with NO AID response

#______________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________
# previous incorrect trial as predictor of ACC and RT
# ACC.0 = 1 means previous trial was incorrect

# dat$autoRT = NULL

#______________________________________________________________________________________________________________________________________
#### Accuracy

# regACC.order = glmer(ACC ~ ACC.0 + (1|ID), dat.decision, family = binomial())
# summary(regACC.order)  # previous trial incorrect is a predictor of accuracy

regACC.int_order = glmer(ACC ~ ACC.0 * prob + (1|ID), dat.aid, family = binomial())
summary(regACC.int_order)   # there is an interaction between aid accuracy and previous trial incorrect

# regACC.int_order2 = glmer(ACC ~ ACC.0 * prob + decide + (1|ID), dat.decision, family = binomial())
# summary(regACC.int_order2)  # decision type not significant

# regACC.int_order3 = glmer(ACC ~ ACC.0 * decide + (1|ID), dat.decision, family = binomial())
# summary(regACC.int_order3)  # interaction with decision type also not significant

regACC.int_order4 = glmer(ACC ~ ACC.0 * difficulty + ACC.0 * prob + (1|ID), dat.aid, family = binomial())
summary(regACC.int_order4)  # model doesn't converge, but shows difficulty, aid accuracy and previous 
                            # trial incorrect significant, plus interactions are significant

regACC.int_order5 = glmer(ACC ~ ACC.0 * prob + difficulty + (1|ID), dat.aid, family = binomial() )
summary(regACC.int_order5)  # model doesn't converge, but shows difficulty, aid accuracy and previous 
                            # trial incorrect significant, plus interaction (still) significant

regACC.int_order6 = glmer(ACC ~ ACC.0 * difficulty + (1|ID), dat.aid, family = binomial())
summary(regACC.int_order6)  # model converges, difficulty and previous trial incorrect significant predictor 
                            # of accuraccy, and interaction is significant

regACC.int_order7 = glmer(ACC ~ ACC.0 * difficulty + ACC.0 * prob + ACC.0 * decide + (1|ID), dat.aid, family = binomial())
summary(regACC.int_order7)   # model fails to converge

# __________________________________________________________________________________________________________
# __________________________________________________________________________________________________________
# ****** make some plots ******

# **** interaction - regACC.int_order ****
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts ACC
# write out equation
# ACC = -2.5426 + 1.4217(ACC.0) + 5.1119(prob) + -2.2058(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = -2.5426 + 1.4217(0) + 5.1119(prob) + -2.2058(0 x prob) = -2.5426 + 5.1119(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = -2.5426 + 1.4217(1) + 5.1119(prob) + -2.2058(1 x prob) = -1.1209 + 2.9061(prob)

# prob = c(0.8,.95)  # create vector of min and max of aid accuracy 
prob = c(0.60,.95) 
correct = exp(-2.5426 + 5.1119*prob)/(1+exp(-2.5426 + 5.1119*prob))  # line for previous trial correct
incorrect = exp(-1.1209 + 2.9061*prob)/(1+exp(-1.1209 + 2.9061*prob)) 
plot(prob, incorrect, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Accuracy"), 
     ylim=c(.6,1), 
     xlab = 'Aid Accuracy', ylab = 'Human Accuracy')
lines(prob, correct, type='l', lty = 2, col = 'black')  # adds another line to the first plot
legend(.6,.98,c('Previous Trial Incorrect','Previous Trial Correct'), lty = c(1,2), col = (c('black','black')),bty='n')

regACC.inter6 = glmer(ACC ~ (decide * falseAlarm) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter6) #--> used in paper

# __________________________________________________________________________________________________________
# separate decision data by DECIDE and REJECT trials
row_sub = apply(dat.aid[8], 1, function(row) all(row != 'REJECT' ))  # find rows with DECIDE response
dat.lvl4 = dat.aid[row_sub,]  # only use rows with DECIDE response
dat.lvl5 = dat.aid[!row_sub,]  # only use rows with DECIDE response

regACC.int_order_lvl4 = glmer(ACC ~ ACC.0 * prob + (1|ID), dat.lvl4, family = binomial())
summary(regACC.int_order_lvl4)  # only aid accuracy a significant (<.05) predictor

regACC.int_order_lvl5 = glmer(ACC ~ ACC.0 * prob + (1|ID), dat.lvl5, family = binomial())
summary(regACC.int_order_lvl5)  # interaction between aid accuracy and previous trial incorrect

# interactions 
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts ACC
# write out equation - DECIDE - regACC.int_order_decide
# ACC = -1.4947 + 0.8857(ACC.0) + 3.8877(prob) + -1.5054(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = -1.4947 + 0.8857(0) + 3.8877(prob) + -1.5054(0 x prob) = -1.4947 + 3.8877(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = -1.4947 + 0.8857(1) + 3.8877(prob) + -1.5054(1 x prob) = -0.609 + 2.3823(prob)

# write out equation - REJECT - regACC.int_order_reject
# ACC = -3.8046 + 1.9563(ACC.0) + 6.5982(prob) + -2.7198(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = -3.8046 + 1.9563(0) + 6.5982(prob) + -2.7198(0 x prob) = -3.8046 + 6.5982(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = -3.8046 + 1.9563(1) + 6.5982(prob) + -2.7198(1 x prob) = -1.8483 + 3.8784(prob)

prob = c(0.6,.7,.8,.95)
correct_lvl4 = (exp(-1.4947 + 3.8877*prob)/(1+exp(-1.4947 + 3.8877*prob)))*100  # line for previous trial correct
incorrect_lvl4 = (exp(-0.609 + 2.3823*prob))/((1+exp(-0.609 + 2.3823*prob)))*100
correct_lvl5 = exp(-3.8046 + 6.5982*prob)/(1+exp(-3.8046 + 6.5982*prob))*100  # line for previous trial correct
incorrect_lvl5 = exp(-1.8483 + 3.8784*prob)/(1+exp(-1.8483 + 3.8784*prob))*100

# AIDED data (split by difficulty and aid)
plot_data = with(dat.aid,tapply(ACC,list(ID,decide,prob,ACC.0),mean))

c4_error = c(se(plot_data[,1,1,1]),se(plot_data[,1,2,1]))*100  # se across participants for easy, correct, low acc aid (first)/high acc aid (second)
i4_error = c(se(plot_data[,1,1,2]),se(plot_data[,1,2,2]))*100
c5_error = c(se(plot_data[,3,1,1]),se(plot_data[,3,2,1]))*100
i5_error = c(se(plot_data[,3,1,2]),se(plot_data[,3,2,2]))*100

plot(prob, incorrect_lvl4, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Accuracy"), 
     ylim=c(50,100), 
     xlab = 'Aid Accuracy', ylab = 'Human Accuracy (%)')
segments(prob[3:4], incorrect_lvl4[3:4]-i4_error/2, prob[3:4], incorrect_lvl4[3:4]+i4_error/2, lty = 1, lwd = 1.5, col = "black")
segments(prob[3:4] - 0.005, incorrect_lvl4[3:4]-i4_error/2, prob[3:4] + 0.005, incorrect_lvl4[3:4]-i4_error/2, lty = "solid", lwd =1.5,col = "black")
segments(prob[3:4] - 0.005, incorrect_lvl4[3:4]+i4_error/2, prob[3:4] + 0.005, incorrect_lvl4[3:4]+i4_error/2, lty = "solid", lwd =1.5,col = "black")
lines(prob, correct_lvl4, type='l', lty = 2, lwd = 2, col = 'black')  # 
segments(prob[3:4], correct_lvl4[3:4]-c4_error/2, prob[3:4], correct_lvl4[3:4]+c4_error/2, lty = 2, lwd = 2, col = "black")
segments(prob[3:4] - 0.005, correct_lvl4[3:4]-c4_error/2, prob[3:4] + 0.005, correct_lvl4[3:4]-c4_error/2, lty = "solid", lwd =2 ,col = "black")
segments(prob[3:4] - 0.005, correct_lvl4[3:4]+c4_error/2, prob[3:4] + 0.005, correct_lvl4[3:4]+c4_error/2, lty = "solid", lwd =2,col = "black")
lines(prob, incorrect_lvl5, type='l', lty = 1, col = 'red')  # 
segments(prob[3:4], incorrect_lvl5[3:4]-i5_error/2, prob[3:4], incorrect_lvl5[3:4]+i5_error/2, lty = 1, lwd = 1.5, col = "red")
segments(prob[3:4] - 0.005, incorrect_lvl5[3:4]-i5_error/2, prob[3:4] + 0.005, incorrect_lvl5[3:4]-i5_error/2, lty = "solid", lwd =1.5,col = "red")
segments(prob[3:4] - 0.005, incorrect_lvl5[3:4]+i5_error/2, prob[3:4] + 0.005, incorrect_lvl5[3:4]+i5_error/2, lty = "solid", lwd =1.5,col = "red")
lines(prob, correct_lvl5, type='l', lty = 2, lwd = 2, col = 'red')  # 
segments(prob[3:4], correct_lvl5[3:4]-c5_error/2, prob[3:4], correct_lvl5[3:4]+c5_error/2, lty = 2, lwd = 2, col = "red")
segments(prob[3:4] - 0.005, correct_lvl5[3:4]-c5_error/2, prob[3:4] + 0.005, correct_lvl5[3:4]-c5_error/2, lty = "solid", lwd =2,col = "red")
segments(prob[3:4] - 0.005, correct_lvl5[3:4]+c5_error/2, prob[3:4] + 0.005, correct_lvl5[3:4]+c5_error/2, lty = "solid", lwd =2,col = "red")

legend(.59,102,c('Level 4, Previous Trial Incorrect','Level 4, Previous Trial Correct',
                 'Level 5, Previous Trial Incorrect','Level 5, Previous Trial Correct'), 
       lty = c(1,2), lwd=c(1.5,2,1.5,2),col = (c('black','black','red','red')),bty='n')

# plot(prob, incorrect_lvl4, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Accuracy for Level 4 Decisions"), 
#      ylim=c(.6,1), 
#      xlab = 'Aid Accuracy', ylab = 'Human Accuracy')
# lines(prob, correct, type='l', lty = 2, col = 'black')  # adds another line to the first plot
# legend(.6,.98,c('Previous Trial Incorrect','Previous Trial Correct'), lty = c(1,2), col = (c('black','black')),bty='n')
# 
# plot(prob, incorrect, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Accuracy for Level 5 Decisions"), 
#      ylim=c(.6,1), 
#      xlab = 'Aid Accuracy', ylab = 'Human Accuracy')
# lines(prob, correct, type='l', lty = 2, col = 'black')  # adds another line to the first plot
# legend(.6,.98,c('Previous Trial Incorrect','Previous Trial Correct'), lty = c(1,2), col = (c('black','black')),bty='n')

# __________________________________________________________________________________________________________
# separate decision data by HARD and EASY trials - used this 
row_sub = apply(dat.aid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.hard_aid = dat.aid[row_sub,]  # only use rows with hard response
dat.easy_aid = dat.aid[!row_sub,]  # only use rows with easy response

regACC.int_order_easy = glmer(ACC ~ ACC.0 * prob + (1|ID), dat.easy_aid, family = binomial())
summary(regACC.int_order_easy)  # interaction between aid accuracy and previous trial incorrect

regACC.int_order_hard = glmer(ACC ~ ACC.0 * prob + (1|ID), dat.hard_aid, family = binomial())
summary(regACC.int_order_hard)  # interaction between aid accuracy and previous trial incorrect

# interactions 
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts ACC
# write out equation - EASY - regACC.int_order_easy
# ACC = -2.4972 + 0.1253(ACC.0) + 5.3471(prob) + -0.7348(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = -2.4972 + 0.1253(0) + 5.3471(prob) + -0.7348(0 x prob) = -2.4972 + 5.3471(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = -2.4972 + 0.1253(1) + 5.3471(prob) + -0.7348(1 x prob) = -2.3719 + 4.6123(prob)

# write out equation - HARD - regACC.int_order_hard
# ACC = -2.6572 + 2.1322(ACC.0) + 5.0005(prob) + -2.9132(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = -2.6572 + 2.1322(0) + 5.0005(prob) + -2.9132(0 x prob) = -2.6572 + 5.0005(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = -2.6572 + 2.1322(1) + 5.0005(prob) + -2.9132(1 x prob) = -0.525 + 2.0873(prob)

prob = c(0.7,.8,.95) 
correct_easy = (exp(-2.4972 + 5.3471*prob)/(1+exp(-2.4972 + 5.3471*prob)))*100
incorrect_easy = (exp(-2.3719 + 4.6123*prob)/(1+exp(-2.3719 + 4.6123*prob)))*(100)
correct_hard = (exp(-2.6572 + 5.0005*prob)/(1+exp(-2.6572 + 5.0005*prob)))*100  # line for previous trial correct
incorrect_hard = (exp(-0.525 + 2.0873*prob)/(1+exp(-0.525 + 2.0873*prob)))*100

# AIDED data (split by difficulty and aid)
plot_data = with(dat.aid,tapply(ACC,list(ID,difficulty,prob,ACC.0),mean))

ce_error = c(se(plot_data[,1,1,1]),se(plot_data[,1,2,1]))*100  # se across participants for easy, correct, low acc aid (first)/high acc aid (second)
ie_error = c(se(plot_data[,1,1,2]),se(plot_data[,1,2,2]))*100
ch_error = c(se(plot_data[,2,1,1]),se(plot_data[,2,2,1]))*100
ih_error = c(se(plot_data[,2,1,2]),se(plot_data[,2,2,2]))*100

# ce_error = exp(0.2747 + 0.3121*prob)/(1+exp(0.2747 + 0.3121*prob))
# ie_error = exp(0.9101 + 1.0663*prob)/(1+exp(0.9101 + 1.0663*prob))
# ch_error = exp(0.2387 + 0.2726*prob)/(1+exp(0.2387 + 0.2726*prob)) 
# ih_error = exp(0.7504 + 0.8717*prob)/(1+exp(0.7504 + 0.8717*prob)) 


plot(prob, incorrect_easy, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Accuracy"), 
     ylim=c(65,100), 
     xlab = 'Aid Accuracy', ylab = 'Human Accuracy (%)')
segments(prob[2:3], incorrect_easy[2:3]-ie_error/2, prob[2:3], incorrect_easy[2:3]+ie_error/2, lty = 1, lwd = 1.5, col = "black")
segments(prob[2:3] - 0.005, incorrect_easy[2:3]-ie_error/2, prob[2:3] + 0.005, incorrect_easy[2:3]-ie_error/2, lty = "solid", lwd =1.5,col = "black")
segments(prob[2:3] - 0.005, incorrect_easy[2:3]+ie_error/2, prob[2:3] + 0.005, incorrect_easy[2:3]+ie_error/2, lty = "solid", lwd =1.5,col = "black")
lines(prob, correct_easy, type='l', lty = 2, lwd = 2, col = 'black')  # 
segments(prob[2:3], correct_easy[2:3]-ce_error/2, prob[2:3], correct_easy[2:3]+ce_error/2, lty = 2, lwd = 2, col = "black")
segments(prob[2:3] - 0.005, correct_easy[2:3]-ce_error/2, prob[2:3] + 0.005, correct_easy[2:3]-ce_error/2, lty = "solid", lwd =2 ,col = "black")
segments(prob[2:3] - 0.005, correct_easy[2:3]+ce_error/2, prob[2:3] + 0.005, correct_easy[2:3]+ce_error/2, lty = "solid", lwd =2,col = "black")
lines(prob, incorrect_hard, type='l', lty = 1, col = 'red')  # 
segments(prob[2:3], incorrect_hard[2:3]-ih_error/2, prob[2:3], incorrect_hard[2:3]+ih_error/2, lty = 1, lwd = 1.5, col = "red")
segments(prob[2:3] - 0.005, incorrect_hard[2:3]-ih_error/2, prob[2:3] + 0.005, incorrect_hard[2:3]-ih_error/2, lty = "solid", lwd =1.5,col = "red")
segments(prob[2:3] - 0.005, incorrect_hard[2:3]+ih_error/2, prob[2:3] + 0.005, incorrect_hard[2:3]+ih_error/2, lty = "solid", lwd =1.5,col = "red")
lines(prob, correct_hard, type='l', lty = 2, lwd = 2, col = 'red')  # 
segments(prob[2:3], correct_hard[2:3]-ch_error/2, prob[2:3], correct_hard[2:3]+ch_error/2, lty = 2, lwd = 2, col = "red")
segments(prob[2:3] - 0.005, correct_hard[2:3]-ch_error/2, prob[2:3] + 0.005, correct_hard[2:3]-ch_error/2, lty = "solid", lwd =2,col = "red")
segments(prob[2:3] - 0.005, correct_hard[2:3]+ch_error/2, prob[2:3] + 0.005, correct_hard[2:3]+ch_error/2, lty = "solid", lwd =2,col = "red")

# lines(prob, correct_easy, type='l', lty = 2, col = 'black')  # 
# lines(prob, incorrect_hard, type='l', lty = 1, col = 'red')  # 
# lines(prob, correct_hard, type='l', lty = 2, col = 'red')  # 
legend(.69,100,c('Easy, Previous Trial Incorrect','Easy, Previous Trial Correct',
                'Hard, Previous Trial Incorrect','Hard, Previous Trial Correct'), 
       lty = c(1,2), lwd=c(1.5,2,1.5,2),col = (c('black','black','red','red')),bty='n')

# ________________________________________________________________________________________________________
# plot NO AID data - ACC
plot_data = with(dat.noaid,tapply(ACC,list(ID,difficulty,ACC.0),mean))*100
plot_data_easy_no = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_hard_no = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_easy_no_error = c(se(plot_data[,1,1]),se(plot_data[,1,2]))
pd_hard_no_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

plot_data = with(dat.noaid,tapply(ACC,list(ID,ACC.0),mean))*100
plot_data_no = c(mean(plot_data[,1]),mean(plot_data[,2]))

pd_no_error = c(se(plot_data[,1]),se(plot_data[,2]))

x = c(0,1)

par(mfrow=c(1,1))

## combined easy/hard
plot(x,plot_data_no,lwd=2,ylim=c(75,95),col="black",type="b",ylab="Human Accuracy (%)",xlab="Previous Trial Correctness",xaxt="n",main="Average Accuracy
     when Previous Response Correct or Incorrect") # a1
segments(x, plot_data_no-pd_no_error/2, x, plot_data_no+pd_no_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_no-pd_no_error/2, x + 0.02, plot_data_no-pd_no_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_no+pd_no_error/2, x + 0.02, plot_data_no+pd_no_error/2, lty = "solid", lwd =1.5,col = "black")
axis(1,at=c(0,1),labels=c("Correct","Incorrect"))

# add AIDED data (easy/hard combined, split by aid accuracy)
plot_data = with(dat.aid,tapply(ACC,list(ID,prob,ACC.0),mean))*100
plot_data_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

lines(x,plot_data_low,lwd=2,col="black",type="b",lty=2,pch=2)
segments(x, plot_data_low-pd_low_error/2, x, plot_data_low+pd_low_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_low-pd_low_error/2, x + 0.02, plot_data_low-pd_low_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_low+pd_low_error/2, x + 0.02, plot_data_low+pd_low_error/2, lty = "solid", lwd =1.5,col = "black")
# axis(1,at=c(0,1),labels=c("Correct","Incorrect")) 
lines(x,plot_data_high,type="b",lty=3,pch=4,col="black",lwd=2)   #a2
segments(x, plot_data_high-pd_high_error/2, x, plot_data_high+pd_high_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_high-pd_high_error/2, x + 0.02, plot_data_high-pd_high_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_high+pd_high_error/2, x + 0.02, plot_data_high+pd_high_error/2, lty = "solid", lwd =1.5,col = "black")
legend(.55,95,legend=c("No Aid", "Low Accuracy Aid","High Accuracy Aid"),lty=1:3,pch=c(1:2,4),col="black",lwd=2) 

## only hard
plot(x,plot_data_hard_no,lwd=2,ylim=c(70,92),col="black",type="b",ylab="Human Accuracy (%)",xlab="Previous Trial Correctness",xaxt="n",main="Average Accuracy when Previous Response 
     Correct or Incorrect for Hard Trials") # a1
segments(x, plot_data_hard_no-pd_hard_no_error/2, x, plot_data_hard_no+pd_hard_no_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_no-pd_hard_no_error/2, x + 0.02, plot_data_hard_no-pd_hard_no_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_no+pd_hard_no_error/2, x + 0.02, plot_data_hard_no+pd_hard_no_error/2, lty = "solid", lwd =1.5,col = "black")
axis(1,at=c(0,1),labels=c("Correct","Incorrect"))

# add AIDED data (only hard, split by accuracy)
plot_data = with(dat.hard_aid,tapply(ACC,list(ID,prob,ACC.0),mean))*100
plot_data_hard_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_hard_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_hard_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_hard_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

lines(x,plot_data_hard_low,lwd=2,col="black",type="b",lty=2,pch=2)
segments(x, plot_data_hard_low-pd_hard_low_error/2, x, plot_data_hard_low+pd_hard_low_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_low-pd_hard_low_error/2, x + 0.02, plot_data_hard_low-pd_hard_low_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_low+pd_hard_low_error/2, x + 0.02, plot_data_hard_low+pd_hard_low_error/2, lty = "solid", lwd =1.5,col = "black")
# axis(1,at=c(0,1),labels=c("Correct","Incorrect")) 
lines(x,plot_data_hard_high,type="b",lty=3,pch=4,col="black",lwd=2)   #a2
segments(x, plot_data_hard_high-pd_hard_high_error/2, x, plot_data_hard_high+pd_hard_high_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_high-pd_hard_high_error/2, x + 0.02, plot_data_hard_high-pd_hard_high_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_high+pd_hard_high_error/2, x + 0.02, plot_data_hard_high+pd_hard_high_error/2, lty = "solid", lwd =1.5,col = "black")
legend(.55,92,legend=c("No Aid", "Low Accuracy Aid","High Accuracy Aid"),lty=1:3,pch=c(1:2,4),col="black",lwd=2) 

# plot(x,plot_data_easy,lwd=2,ylim=c(600,800),col="black",type="b",ylab="Median Response Time (sec)",xlab="Previous Trial Correctness",xaxt="n",main="Average Median Response Time without Aid
#      when Previous Response Correct or Incorrect
#      for Easy and Hard Trials") # a1
# segments(x, plot_data_easy-pd_easy_error/2, x, plot_data_easy+pd_easy_error/2, lty = 2, lwd = 1.5, col = "black")
# segments(x - 0.02, plot_data_easy-pd_easy_error/2, x + 0.02, plot_data_easy-pd_easy_error/2, lty = "solid", lwd =1.5,col = "black")
# segments(x - 0.02, plot_data_easy+pd_easy_error/2, x + 0.02, plot_data_easy+pd_easy_error/2, lty = "solid", lwd =1.5,col = "black")
# axis(1,at=c(0,1),labels=c("Correct","Incorrect"))
# lines(x,plot_data_hard,type="b",lty=2,pch=1,col="black",lwd=2)   #a2
# segments(x, plot_data_hard-pd_hard_error/2, x, plot_data_hard+pd_hard_error/2, lty = 2, lwd = 1.5, col = "black")
# segments(x - 0.02, plot_data_hard-pd_hard_error/2, x + 0.02, plot_data_hard-pd_hard_error/2, lty = "solid", lwd =1.5,col = "black")
# segments(x - 0.02, plot_data_hard+pd_hard_error/2, x + 0.02, plot_data_hard+pd_hard_error/2, lty = "solid", lwd =1.5,col = "black")
# legend(0,800,legend=c("Easy","Hard"),lty=1:2,pch=1,col="black",lwd=2)


#______________________________________________________________________________________________________________________________________
###### RT

regRT.order = lmer(humanRT ~ ACC.0 + (1|ID), dat.correct_aid, REML = F)
summary(regRT.order)   # previous trial correct is a significant predictor

regRT.int_order = lmer(humanRT ~ ACC.0 * prob + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order)    # there is an interaction between aid accuracy and previous trial incorrect

regRT.int_order1 = lmer(humanRT ~ ACC.0 * falseAlarm + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order1)   # same as above, but using a factor variable. there is an interaction between aid accuracy and previous trial incorrect

regRT.int_order2 = lmer(humanRT ~ ACC.0 * prob + decide + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order2)  # previous trial incorrect, aid accuracy and decision type are significant
                          # interaction of previous trial incorrect and aid accuracy significant

regRT.int_order3 = lmer(humanRT ~ ACC.0 * decide + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order3)  # previous trial incorrect and decision type are significant
                            # interaction of previous trial incorrect and decision type significant

regRT.int_order4 = lmer(humanRT ~ ACC.0 * difficulty + ACC.0 * prob + (1|ID), dat.correct_aid, REML = F) # used on poster
summary(regRT.int_order4)  # previous trial incorrect, difficulty, and aid accuracy are significant
                            # interaction of previous trial incorrect and aid accuracy significant
                            # interaction of previous trial incorrect and difficulty NOT significant

regRT.int_order5 = lmer(humanRT ~ ACC.0 * prob + difficulty + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order5)  # 

regRT.int_order6 = lmer(humanRT ~ ACC.0 * difficulty + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order6)  # same as int_order4

regRT.int_order7 = lmer(humanRT ~ ACC.0 * difficulty + ACC.0 * prob + ACC.0 * decide + (1|ID), dat.correct_aid, REML = F)
summary(regRT.int_order7)  # model failed to converge - used on poster

# ________________________________________________________________________________________________________
# ________________________________________________________________________________________________________
# | NOT REVISED YET ################################
# | interaction
# | hypothesis is that previous trial (moderator) has an effect on how aid accuracy (prob) predicts humanRT
# | humanRT = 1.38405 + -0.31888(ACC.0) + -0.68282(prob) + 0.46062(ACC.0 x prob)
# | set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# | humanRT = 1.38405 + -0.31888(0) + -0.68282(prob) + 0.46062(0 x prob) = 1.38405 + -0.68282(prob)
# | set ACC.0 to one and get equation for previous trial incorrect
# | humanRT = 1.38405 + -0.31888(1) + -0.68282(prob) + 0.46062(1 x prob) = 1.06517 + -0.2222(prob)
# | 
# | alternate interaction
# | hypothesis is that aid accuracy (moderator) has an effect on how previous trial predicts humanRT
# | humanRT = 0.837938 + 0.048662(ACC.0) + -0.102646(FA) + 0.070611(ACC.0 x FA)
# | set FA to zero and get equation for low accuracy aid
# | humanRT = 0.837938 + 0.048662(ACC.0) + -0.102646(0) + 0.070611(ACC.0 x 0) = 0.837938 + 0.048662(ACC.0)
# | set FA to one and get equation for high accuracy aid
# | humanRT = 0.837938 + 0.048662(ACC.0) + -0.102646(1) + 0.070611(ACC.0 x 1) = 0.735292 + 0.119273(ACC.0)
# | 
# | prob = c(min(dat.correct_decision$prob),max(dat.correct_decision$prob))  # create vector of min and max of aid accuracy
# | prob = c(0.6,0.95)
# | acc0 = c(0,1)
# | 
# | correct = (1.38405 - 0.68282*prob)*1000  ## line for previous trial correct
# | incorrect = (1.06517 - 0.2222*prob)*1000
# | 
# | low = (0.837938 + 0.048662*acc0)*1000  
# | high = (0.735292 + 0.119273*acc0)*1000
# | 
# | plot(prob, incorrect, type='l', col = 'black', main = c("Interaction of Previous Trial Correct and","Aid Accuracy on Human Response Time"), 
# |      ylim=c(600,1000), 
# |      xlab = 'Aid Accuracy', ylab = 'Human Response Time (ms)')
# | lines(prob, correct, type='l', lty = 2, col = 'black')  # adds another line to the first plot
# | legend(.6,700,c('Previous Trial Incorrect','Previous Trial Correct'), lty = c(1,2), col = (c('black','black')),bty='n')
# | 
# | plot(acc0, low, type='l', col = 'black', main = c("Interaction of Previous Trial Correct and","Aid Accuracy on Human Response Time"), 
# |      ylim=c(600,1000), 
# |      xlab = 'Previous Trial Correctness', ylab = 'Human Response Time (ms)')
# | lines(acc0, high, type='l', lty = 2, col = 'black')  # adds another line to the first plot
# | legend(.3,700,c('Low Accuracy Aid','High Accuracy Aid'), lty = c(1,2), col = (c('black','black')),bty='n')
# | 
# | # decision and false alarm interaction
# | regRT.inter1 = lmer(humanRT ~ (decide * falseAlarm) + difficulty + (1|ID), dat.correct_decision, REML = F)
# | summary(regRT.inter1)
# | 
# | regRT.inter1n = lmer(humanRT ~ (decide * prob) + difficulty + (1|ID), dat.correct_decision, REML = F)
# | summary(regRT.inter1n)  # --> used in paper
# | 
# | 
# | 
# | ________________________________________________________________________________________________________
# separate decision data by DECIDE and REJECT trials
row_sub = apply(dat.correct_aid[8], 1, function(row) all(row != 'REJECT' ))  # find rows with DECIDE response
dat.correct_lvl4 = dat.correct_aid[row_sub,]  # only use rows with DECIDE response
dat.correct_lvl5 = dat.correct_aid[!row_sub,]  # only use rows with DECIDE response

regRT.int_order_lvl4 = lmer(humanRT ~ ACC.0 * prob + (1|ID), dat.correct_lvl4, REML = F)
summary(regRT.int_order_lvl4)  #

qt(.05,48,lower.tail = FALSE)  # 1.677224
qt(.01,48,lower.tail = FALSE)  # 2.406581
qt(.001,48,lower.tail = FALSE) # 3.26891

regRT.int_order_lvl5 = lmer(humanRT ~ ACC.0 * prob + (1|ID), dat.correct_lvl5, REML = F)
summary(regRT.int_order_lvl5)  # 

# interactions
# write out equations - level 4 (DECIDE)
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts humanRT
# humanRT = 0.77850 + -0.13620(ACC.0) + -0.03007(prob) + 0.24301(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# humanRT = 0.77850 + -0.13620(0) + -0.03007(prob) + 0.24301(0 x prob) = 0.77850 + -0.03007(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# humanRT = 0.77850 + -0.13620(1) + -0.03007(prob) + 0.24301(1 x prob) = 0.6423 + 0.21294(prob)

# write out equations - level 5 (REJECT)
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts humanRT
# humanRT = 1.95959 + -0.49274(ACC.0) + -1.30960(prob) + 0.69286(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# humanRT = 1.95959 + -0.49274(0) + -1.30960(prob) + 0.69286(0 x prob) = 1.95959 + -1.30960(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# humanRT = 1.95959 + -0.49274(1) + -1.30960(prob) + 0.69286(1 x prob) = 1.46685 + -0.61674(prob)

prob = c(0.6,.8,0.95)
correct_lvl4 = (0.77850 + -0.03007*prob)*1000  ## line for previous trial correct
incorrect_lvl4 = (0.6423 + 0.21294*prob)*1000
correct_lvl5 = (1.95959 + -1.30960*prob)*1000  ## line for previous trial correct
incorrect_lvl5 = (1.46685 + -0.61674*prob)*1000

# AIDED data (split by decision type and aid)
plot_data = with(dat.correct_aid,tapply(humanRT,list(ID,decide,prob,ACC.0),median))*1000

c4_error = c(se(plot_data[,1,1,1]),se(plot_data[,1,2,1]))  # se across participants for level4, correct, low acc aid (first)/high acc aid (second)
i4_error = c(se(plot_data[,1,1,2]),se(plot_data[,1,2,2]))
c5_error = c(se(plot_data[,3,1,1]),se(plot_data[,3,2,1]))
i5_error = c(se(plot_data[,3,1,2]),se(plot_data[,3,2,2]))

plot(prob, incorrect_lvl4, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Median Response Time"), 
     ylim=c(600,1200), 
     xlab = 'Aid Accuracy', ylab = 'Median Response Time (ms)')
segments(prob[2:3], incorrect_lvl4[2:3]-i4_error/2, prob[2:3], incorrect_lvl4[2:3]+i4_error/2, lty = 1, lwd = 1.5, col = "black")
segments(prob[2:3] - 0.005, incorrect_lvl4[2:3]-i4_error/2, prob[2:3] + 0.005, incorrect_lvl4[2:3]-i4_error/2, lty = "solid", lwd =1.5,col = "black")
segments(prob[2:3] - 0.005, incorrect_lvl4[2:3]+i4_error/2, prob[2:3] + 0.005, incorrect_lvl4[2:3]+i4_error/2, lty = "solid", lwd =1.5,col = "black")
lines(prob, correct_lvl4, type='l', lty = 2, lwd = 2, col = 'black')  # 
segments(prob[2:3], correct_lvl4[2:3]-c4_error/2, prob[2:3], correct_lvl4[2:3]+c4_error/2, lty = 2, lwd = 2, col = "black")
segments(prob[2:3] - 0.005, correct_lvl4[2:3]-c4_error/2, prob[2:3] + 0.005, correct_lvl4[2:3]-c4_error/2, lty = "solid", lwd =2 ,col = "black")
segments(prob[2:3] - 0.005, correct_lvl4[2:3]+c4_error/2, prob[2:3] + 0.005, correct_lvl4[2:3]+c4_error/2, lty = "solid", lwd =2,col = "black")
lines(prob, incorrect_lvl5, type='l', lty = 1, col = 'red')  # 
segments(prob[2:3], incorrect_lvl5[2:3]-i5_error/2, prob[2:3], incorrect_lvl5[2:3]+i5_error/2, lty = 1, lwd = 1.5, col = "red")
segments(prob[2:3] - 0.005, incorrect_lvl5[2:3]-i5_error/2, prob[2:3] + 0.005, incorrect_lvl5[2:3]-i5_error/2, lty = "solid", lwd =1.5,col = "red")
segments(prob[2:3] - 0.005, incorrect_lvl5[2:3]+i5_error/2, prob[2:3] + 0.005, incorrect_lvl5[2:3]+i5_error/2, lty = "solid", lwd =1.5,col = "red")
lines(prob, correct_lvl5, type='l', lty = 2, lwd = 2, col = 'red')  # 
segments(prob[2:3], correct_lvl5[2:3]-c5_error/2, prob[2:3], correct_lvl5[2:3]+c5_error/2, lty = 2, lwd = 2, col = "red")
segments(prob[2:3] - 0.005, correct_lvl5[2:3]-c5_error/2, prob[2:3] + 0.005, correct_lvl5[2:3]-c5_error/2, lty = "solid", lwd =2,col = "red")
segments(prob[2:3] - 0.005, correct_lvl5[2:3]+c5_error/2, prob[2:3] + 0.005, correct_lvl5[2:3]+c5_error/2, lty = "solid", lwd =2,col = "red")
legend(.7,1225,c('Level 4, Previous Trial Incorrect','Level 4, Previous Trial Correct',
                 'Level 5, Previous Trial Incorrect','Level 5, Previous Trial Correct'),
       lty = c(1,2), lwd = c(1.5,2,1.5,2), col = (c('black','black','red','red')),bty='n')


# ________________________________________________________________________________________________________
# separate decision data by EASY and HARD trials -> used this on poster
row_sub = apply(dat.correct_aid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.correct_hard_aid = dat.correct_aid[row_sub,]  # only use rows with hard response
dat.correct_easy_aid = dat.correct_aid[!row_sub,]  # only use rows with easy response

regRT.int_order_easy = lmer(humanRT ~ ACC.0 * prob + (1|ID), dat.correct_easy_aid, REML = F)
summary(regRT.int_order_easy)  # interaction between aid accuracy and previous trial incorrect

regRT.int_order_hard = lmer(humanRT ~ ACC.0 * prob + (1|ID), dat.correct_hard_aid, REML = F)
summary(regRT.int_order_hard)  # interaction between aid accuracy and previous trial incorrect

# interactions 
# hypothesis is that previous trial (moderator) has an effect on how aid accuracy (FA) predicts humanRT
# write out equation - EASY - regRT.int_order_easy
# RT = 1.37980 + -0.47932(ACC.0) + -0.69050(prob) + 0.63018(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# RT = 1.37980 + -0.47932(0) + -0.69050(prob) + 0.63018(0 x prob) = 1.37980 - 0.69050(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# RT = 1.37980 + -0.47932(1) + -0.69050(prob) + 0.63018(1 x prob) = 0.90048 - 0.06032(prob)

# write out equation - HARD - regACC.int_order_hard
# ACC = 1.39636 + -0.19668(ACC.0) + -0.68145(prob) + 0.32238(ACC.0 x prob)
# set ACC.0 to zero and get equation for previous trial not incorrect (i.e. correct)
# ACC = 1.39636 + -0.19668(0) + -0.68145(prob) + 0.32238(0 x prob) = 1.39636 - 0.68145(prob)
# set ACC.0 to one and get equation for previous trial incorrect
# ACC = 1.39636 + -0.19668(1) + -0.68145(prob) + 0.32238(1 x prob) = 1.19968 - 0.35907(prob)

prob = c(0.7,.8,.95) 
correct_easy = (1.37980 - 0.69050*prob)*1000
incorrect_easy = (0.90048 - 0.06032*prob)*1000
correct_hard = (1.39636 - 0.68145*prob)*1000  # line for previous trial correct
incorrect_hard = (1.19968 - 0.35907*prob)*1000

# AIDED data (split by difficulty and aid)
plot_data = with(dat.correct_aid,tapply(humanRT,list(ID,difficulty,prob,ACC.0),median))*1000

ce_error = c(se(plot_data[,1,1,1]),se(plot_data[,1,2,1]))  # se across participants for easy, correct, low acc aid (first)/high acc aid (second)
ie_error = c(se(plot_data[,1,1,2]),se(plot_data[,1,2,2]))
ch_error = c(se(plot_data[,2,1,1]),se(plot_data[,2,2,1]))
ih_error = c(se(plot_data[,2,1,2]),se(plot_data[,2,2,2]))

# ce_error = (0.04373 + 0.04219*prob)*1000
# ie_error = (0.14105 + 0.1554*prob)*1000
# ch_error = (0.04604 + 0.04384*prob)*1000
# ih_error = (0.14956 + 0.16454*prob)*1000

plot(prob, incorrect_easy, type='l', col = 'black', main = c("Interaction of Previous Trial and","Aid Accuracy on Median Response Time"), 
     ylim=c(600,1000), 
     xlab = 'Aid Accuracy', ylab = 'Median Response Time (ms)')
segments(prob[2:3], incorrect_easy[2:3]-ie_error/2, prob[2:3], incorrect_easy[2:3]+ie_error/2, lty = 1, lwd = 1.5, col = "black")
segments(prob[2:3] - 0.005, incorrect_easy[2:3]-ie_error/2, prob[2:3] + 0.005, incorrect_easy[2:3]-ie_error/2, lty = "solid", lwd =1.5,col = "black")
segments(prob[2:3] - 0.005, incorrect_easy[2:3]+ie_error/2, prob[2:3] + 0.005, incorrect_easy[2:3]+ie_error/2, lty = "solid", lwd =1.5,col = "black")
lines(prob, correct_easy, type='l', lty = 2, lwd = 2, col = 'black')  # 
segments(prob[2:3], correct_easy[2:3]-ce_error/2, prob[2:3], correct_easy[2:3]+ce_error/2, lty = 2, lwd = 2, col = "black")
segments(prob[2:3] - 0.005, correct_easy[2:3]-ce_error/2, prob[2:3] + 0.005, correct_easy[2:3]-ce_error/2, lty = "solid", lwd =2 ,col = "black")
segments(prob[2:3] - 0.005, correct_easy[2:3]+ce_error/2, prob[2:3] + 0.005, correct_easy[2:3]+ce_error/2, lty = "solid", lwd =2,col = "black")
lines(prob, incorrect_hard, type='l', lty = 1, col = 'red')  # 
segments(prob[2:3], incorrect_hard[2:3]-ih_error/2, prob[2:3], incorrect_hard[2:3]+ih_error/2, lty = 1, lwd = 1.5, col = "red")
segments(prob[2:3] - 0.005, incorrect_hard[2:3]-ih_error/2, prob[2:3] + 0.005, incorrect_hard[2:3]-ih_error/2, lty = "solid", lwd =1.5,col = "red")
segments(prob[2:3] - 0.005, incorrect_hard[2:3]+ih_error/2, prob[2:3] + 0.005, incorrect_hard[2:3]+ih_error/2, lty = "solid", lwd =1.5,col = "red")
lines(prob, correct_hard, type='l', lty = 2, lwd = 2, col = 'red')  # 
segments(prob[2:3], correct_hard[2:3]-ch_error/2, prob[2:3], correct_hard[2:3]+ch_error/2, lty = 2, lwd = 2, col = "red")
segments(prob[2:3] - 0.005, correct_hard[2:3]-ch_error/2, prob[2:3] + 0.005, correct_hard[2:3]-ch_error/2, lty = "solid", lwd =2,col = "red")
segments(prob[2:3] - 0.005, correct_hard[2:3]+ch_error/2, prob[2:3] + 0.005, correct_hard[2:3]+ch_error/2, lty = "solid", lwd =2,col = "red")

# legend(.74,675,c('Easy, Previous Trial Incorrect','Easy, Previous Trial Correct'),
#        lty = c(1,2), lwd = c(1.5,2), col = "black" ,bty='n')

legend(.69,720,c('Easy, Previous Trial Incorrect','Easy, Previous Trial Correct',
                 'Hard, Previous Trial Incorrect','Hard, Previous Trial Correct'),
       lty = c(1,2), lwd = c(1.5,2,1.5,2), col = (c('black','black','red','red')),bty='n')


# ________________________________________________________________________________________________________
# plot NO AID data - RT - difficulty
plot_data = with(dat.correct_noaid,tapply(humanRT,list(ID,difficulty,ACC.0),median))*1000
plot_data_easy_correct_no = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_hard_correct_no = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_easy_correct_no_error = c(se(plot_data[,1,1]),se(plot_data[,1,2]))
pd_hard_correct_no_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

plot_data = with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0),median))*1000
plot_data_correct_no = c(mean(plot_data[,1]),mean(plot_data[,2]))

pd_correct_no_error = c(se(plot_data[,1]),se(plot_data[,2]))

x = c(0,1)

## combined easy/hard
plot(x,plot_data_correct_no,lwd=2,ylim=c(600,800),col="black",type="b",ylab="Median Response Time (ms)",xlab="Previous Trial Correctness",xaxt="n",main="Average Median Response Time
     when Previous Response Correct or Incorrect") # a1
segments(x, plot_data_correct_no-pd_correct_no_error/2, x, plot_data_correct_no+pd_correct_no_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_correct_no-pd_correct_no_error/2, x + 0.02, plot_data_correct_no-pd_correct_no_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_correct_no+pd_correct_no_error/2, x + 0.02, plot_data_correct_no+pd_correct_no_error/2, lty = "solid", lwd =1.5,col = "black")
axis(1,at=c(0,1),labels=c("Correct","Incorrect"))

# plot AIDED data (easy/hard combined, split by aid accuracy)
plot_data = with(dat.correct_aid,tapply(humanRT,list(ID,prob,ACC.0),median))*1000
plot_data_correct_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_correct_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_correct_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_correct_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

lines(x,plot_data_correct_low,lwd=2,col="black",type="b",lty=2,pch=2)
segments(x, plot_data_correct_low-pd_correct_low_error/2, x, plot_data_correct_low+pd_correct_low_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_correct_low-pd_correct_low_error/2, x + 0.02, plot_data_correct_low-pd_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_correct_low+pd_correct_low_error/2, x + 0.02, plot_data_correct_low+pd_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
lines(x,plot_data_correct_high,type="b",lty=3,pch=4,col="black",lwd=2)   #a2
segments(x, plot_data_correct_high-pd_correct_high_error/2, x, plot_data_correct_high+pd_correct_high_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_correct_high-pd_correct_high_error/2, x + 0.02, plot_data_correct_high-pd_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_correct_high+pd_correct_high_error/2, x + 0.02, plot_data_correct_high+pd_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
legend(0,800,legend=c("No Aid", "Low Accuracy Aid","High Accuracy Aid"),lty=1:3,pch=c(1:2,4),col="black",lwd=2) 

## hard only
plot(x,plot_data_hard_correct_no,lwd=2,ylim=c(625,825),col="black",type="b",ylab="Median Response Time (ms)",xlab="Previous Trial Correctness",xaxt="n",main="Average Median Response Time
     when Previous Response Correct or Incorrect 
     for Hard Trials") # a1
segments(x, plot_data_hard_correct_no-pd_hard_correct_no_error/2, x, plot_data_hard_correct_no+pd_hard_correct_no_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_correct_no-pd_hard_correct_no_error/2, x + 0.02, plot_data_hard_correct_no-pd_hard_correct_no_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_correct_no+pd_hard_correct_no_error/2, x + 0.02, plot_data_hard_correct_no+pd_hard_correct_no_error/2, lty = "solid", lwd =1.5,col = "black")
axis(1,at=c(0,1),labels=c("Correct","Incorrect"))

# add AIDED data (split by aid accuracy)
plot_data = with(dat.correct_hard_aid,tapply(humanRT,list(ID,prob,ACC.0),median))*1000
plot_data_hard_correct_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_hard_correct_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_hard_correct_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_hard_correct_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

lines(x,plot_data_hard_correct_low,lwd=2,col="black",type="b",lty=2,pch=2)
segments(x, plot_data_hard_correct_low-pd_hard_correct_low_error/2, x, plot_data_hard_correct_low+pd_hard_correct_low_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_correct_low-pd_hard_correct_low_error/2, x + 0.02, plot_data_hard_correct_low-pd_hard_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_correct_low+pd_hard_correct_low_error/2, x + 0.02, plot_data_hard_correct_low+pd_hard_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
lines(x,plot_data_hard_correct_high,type="b",lty=3,pch=4,col="black",lwd=2)   #a2
segments(x, plot_data_hard_correct_high-pd_hard_correct_high_error/2, x, plot_data_hard_correct_high+pd_hard_correct_high_error/2, lty = 2, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_hard_correct_high-pd_hard_correct_high_error/2, x + 0.02, plot_data_hard_correct_high-pd_hard_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_hard_correct_high+pd_hard_correct_high_error/2, x + 0.02, plot_data_hard_correct_high+pd_hard_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
legend(0,825,legend=c("No Aid", "Low Accuracy Aid","High Accuracy Aid"),lty=1:3,pch=c(1:2,4),col="black",lwd=2) 

# ________________________________________________________________________________________________________
# plot NO AID data - RT - decision type
plot_data = with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0),median))*1000
plot_data_correct_no = c(mean(plot_data[,1]),mean(plot_data[,2]))

pd_correct_no_error = c(se(plot_data[,1]),se(plot_data[,2]))  # same as difficulty

x = c(0,1)

## lvl4 and lvl5 split
plot(x,plot_data_correct_no,lwd=2,lty=2,ylim=c(600,950),col="blue",type="b",ylab="Median Response Time (ms)",xlab="Previous Trial Correctness",xaxt="n",main="Average Median Response Time
     when Previous Response Correct or Incorrect Trials") # a1
segments(x, plot_data_correct_no-pd_correct_no_error/2, x, plot_data_correct_no+pd_correct_no_error/2, lty = 2, lwd = 1.5, col = "blue")
segments(x - 0.02, plot_data_correct_no-pd_correct_no_error/2, x + 0.02, plot_data_correct_no-pd_correct_no_error/2, lty = "solid", lwd =1.5,col = "blue")
segments(x - 0.02, plot_data_correct_no+pd_correct_no_error/2, x + 0.02, plot_data_correct_no+pd_correct_no_error/2, lty = "solid", lwd =1.5,col = "blue")
axis(1,at=c(0,1),labels=c("Correct","Incorrect"))

# add AIDED data (split by aid accuracy)
plot_data = with(dat.correct_lvl4,tapply(humanRT,list(ID,prob,ACC.0),median))*1000
plot_data_lvl4_correct_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_lvl4_correct_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_lvl4_correct_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_lvl4_correct_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

plot_data = with(dat.correct_lvl5,tapply(humanRT,list(ID,prob,ACC.0),median))*1000
plot_data_lvl5_correct_low = c(mean(plot_data[,1,1]),mean(plot_data[,1,2]))  # incorrect = 0 first, incorrect = 1 second)
plot_data_lvl5_correct_high = c(mean(plot_data[,2,1]),mean(plot_data[,2,2]))

pd_lvl5_correct_low_error = c(se(plot_data[,1,1]),se(plot_data[,1,2])) 
pd_lvl5_correct_high_error = c(se(plot_data[,2,1]),se(plot_data[,2,2]))

lines(x,plot_data_lvl4_correct_low,lwd=2,col="black",type="b",lty=1,pch=2)
segments(x, plot_data_lvl4_correct_low-pd_lvl4_correct_low_error/2, x, plot_data_lvl4_correct_low+pd_lvl4_correct_low_error/2, lty = 1, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_lvl4_correct_low-pd_lvl4_correct_low_error/2, x + 0.02, plot_data_lvl4_correct_low-pd_lvl4_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_lvl4_correct_low+pd_lvl4_correct_low_error/2, x + 0.02, plot_data_lvl4_correct_low+pd_lvl4_correct_low_error/2, lty = "solid", lwd =1.5,col = "black")
lines(x,plot_data_lvl4_correct_high,type="b",lty=3,pch=4,col="black",lwd=2)   #a2
segments(x, plot_data_lvl4_correct_high-pd_lvl4_correct_high_error/2, x, plot_data_lvl4_correct_high+pd_lvl4_correct_high_error/2, lty = 3, lwd = 1.5, col = "black")
segments(x - 0.02, plot_data_lvl4_correct_high-pd_lvl4_correct_high_error/2, x + 0.02, plot_data_lvl4_correct_high-pd_lvl4_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
segments(x - 0.02, plot_data_lvl4_correct_high+pd_lvl4_correct_high_error/2, x + 0.02, plot_data_lvl4_correct_high+pd_lvl4_correct_high_error/2, lty = "solid", lwd =1.5,col = "black")
lines(x,plot_data_lvl5_correct_low,lwd=2,col="red",type="b",lty=1,pch=2)
segments(x, plot_data_lvl5_correct_low-pd_lvl5_correct_low_error/2, x, plot_data_lvl5_correct_low+pd_lvl5_correct_low_error/2, lty = 2, lwd = 1.5, col = "red")
segments(x - 0.02, plot_data_lvl5_correct_low-pd_lvl5_correct_low_error/2, x + 0.02, plot_data_lvl5_correct_low-pd_lvl5_correct_low_error/2, lty = "solid", lwd =1.5,col = "red")
segments(x - 0.02, plot_data_lvl5_correct_low+pd_lvl5_correct_low_error/2, x + 0.02, plot_data_lvl5_correct_low+pd_lvl5_correct_low_error/2, lty = "solid", lwd =1.5,col = "red")
lines(x,plot_data_lvl5_correct_high,type="b",lty=3,pch=4,col="red",lwd=2)   #a2
segments(x, plot_data_lvl5_correct_high-pd_lvl5_correct_high_error/2, x, plot_data_lvl5_correct_high+pd_lvl5_correct_high_error/2, lty = 2, lwd = 1.5, col = "red")
segments(x - 0.02, plot_data_lvl5_correct_high-pd_lvl5_correct_high_error/2, x + 0.02, plot_data_lvl5_correct_high-pd_lvl5_correct_high_error/2, lty = "solid", lwd =1.5,col = "red")
segments(x - 0.02, plot_data_lvl5_correct_high+pd_lvl5_correct_high_error/2, x + 0.02, plot_data_lvl5_correct_high+pd_lvl5_correct_high_error/2, lty = "solid", lwd =1.5,col = "red")

legend(0,950,legend=c("No Aid", "Level 4, Low Accuracy Aid","Level 4, High Accuracy Aid", "Level 5, Low Accuracy Aid", "Level 5, High Accuracy Aid")
       ,lty=c(2,1,3,1,3),pch=c(1,2,4,2,4),col=c("blue","black","black","red","red"),lwd=2) 


# par(mfrow=c(1,1))
# plot(x,plot_data_easy,lwd=2,ylim=c(600,800),col="black",type="b",ylab="Median Response Time (sec)",xlab="Previous Trial Correctness",xaxt="n",main="Average Median Response Time without Aid
#      when Previous Response Correct or Incorrect
#      for Easy and Hard Trials") # a1
# segments(x, plot_data_easy-pd_easy_error/2, x, plot_data_easy+pd_easy_error/2, lty = 2, lwd = 1.5, col = "black")
# segments(x - 0.02, plot_data_easy-pd_easy_error/2, x + 0.02, plot_data_easy-pd_easy_error/2, lty = "solid", lwd =1.5,col = "black")
# segments(x - 0.02, plot_data_easy+pd_easy_error/2, x + 0.02, plot_data_easy+pd_easy_error/2, lty = "solid", lwd =1.5,col = "black")
# axis(1,at=c(0,1),labels=c("Correct","Incorrect"))
# lines(x,plot_data_hard,type="b",lty=2,pch=1,col="black",lwd=2)   #a2
# segments(x, plot_data_hard-pd_hard_error/2, x, plot_data_hard+pd_hard_error/2, lty = 2, lwd = 1.5, col = "black")
# segments(x - 0.02, plot_data_hard-pd_hard_error/2, x + 0.02, plot_data_hard-pd_hard_error/2, lty = "solid", lwd =1.5,col = "black")
# segments(x - 0.02, plot_data_hard+pd_hard_error/2, x + 0.02, plot_data_hard+pd_hard_error/2, lty = "solid", lwd =1.5,col = "black")
# legend(0,800,legend=c("Easy","Hard"),lty=1:2,pch=1,col="black",lwd=2)

#_________________________________________________________________________________________________________
######## ******* Calculate Traditionl post-error adjustments ******** #######
### RT ####
meanRT_prev_no <- with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0),mean))*1000
adjustRT_no = meanRT_prev_no[,2]-meanRT_prev_no[,1]      
mean(adjustRT_no)
se(adjustRT_no)

meanRT_prev_aid <- with(dat.correct_aid,tapply(humanRT,list(ID,prob,ACC.0),mean))*1000
adjustRT_low = meanRT_prev_aid[,1,2]-meanRT_prev_aid[,1,1]  
mean(adjustRT_low)
se(adjustRT_low)

adjustRT_high = meanRT_prev_aid[,2,2]-meanRT_prev_aid[,2,1]  
mean(adjustRT_high)
se(adjustRT_high)

### ACC ###
meanACC_prev_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0),mean))*100
adjustACC_no = meanACC_prev_no[,2]-meanACC_prev_no[,1]
mean(adjustACC_no)
se(adjustACC_no)

meanACC_prev_aid <- with(dat.aid,tapply(ACC,list(ID,prob,ACC.0),mean))*100
adjustACC_low = meanACC_prev_aid[,1,2]-meanACC_prev_aid[,1,1]  
mean(adjustACC_low)
se(adjustACC_low)

adjustACC_high = meanACC_prev_aid[,2,2]-meanACC_prev_aid[,2,1]  
mean(adjustACC_high)
se(adjustACC_high)

######## ******* Calculate Robust post-error adjustments ******** #######
### RT ####
## this doesn't include the pairing of PE and preE,postC trials - takes diff of all PE trials 
# and all preE,postC trials for a subj - this is wrong
# meanRT_prev2_no <- with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0.1),mean))*1000
# meanRT_prev3_no <- with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0.1.1),mean))*1000
# adjustRT_no = meanRT_prev2_no[,2]-meanRT_prev3_no[,2]
# mean(adjustRT_no)
# se(adjustRT_no)

## takes diff for each PE and preE,postC pair (for all participants)
mean(dat.correct_noaid$RT_adj,na.rm=TRUE)*1000
se(dat.correct_noaid$RT_adj)*1000

## by participant - used this ******** pretty sure this is right
# remove NA trials
row_sub = !is.nan(dat.correct_noaid$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_noaid_values = dat.correct_noaid[row_sub,]  # only use rows with values

adjustRT_no2 = with(dat.correct_noaid_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_no2)
se(adjustRT_no2)

boxplot(adjustRT_no2)

## takes diff for each PE and preE,postC pair (for all participants combined)
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.correct_aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.correct_high = dat.correct_aid[row_sub,]  # only use rows with high accuracy aid
dat.correct_low = dat.correct_aid[!row_sub,]  # only use rows with low accuracy aid

mean(dat.correct_low$RT_adj,na.rm=TRUE)*1000
se(dat.correct_low$RT_adj)*1000

mean(dat.correct_high$RT_adj,na.rm=TRUE)*1000
se(dat.correct_high$RT_adj)*1000

## by participant - used this ******** pretty sure this is right
# remove NA trials
# high accuracy
row_sub = !is.nan(dat.correct_high$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_high_values = dat.correct_high[row_sub,]  # only use rows with values

adjustRT_high2 = with(dat.correct_high_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_high2)
se(adjustRT_high2)

# low accuracy
row_sub = !is.nan(dat.correct_low$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_low_values = dat.correct_low[row_sub,]  # only use rows with values

adjustRT_low2 = with(dat.correct_low_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_low2)
se(adjustRT_low2)

### ACC #### 
## no aid trials by participant - no pairwise differences
meanACC_prev2_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0.1),mean))*100   # pre-error, post-correct trials
adjustACC_no2 = meanACC_prev_no[,2]-meanACC_prev2_no[,2]
mean(adjustACC_no2)
se(adjustACC_no2)

# pairwise differences by participant - used this ******** I think this is right 
# remove NA trials
row_sub = !is.nan(dat.noaid$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.noaid_values = dat.noaid[row_sub,]  # only use rows with values

adjustACC_no3 = with(dat.noaid_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_no3)
se(adjustACC_no3)

## with aid
meanACC_prev2_aid <- with(dat.aid,tapply(ACC,list(ID,prob,ACC.0.1),mean))*100
adjustACC_low2 = meanACC_prev_aid[,1,2]-meanACC_prev2_aid[,1,2]
mean(adjustACC_low2)
se(adjustACC_low2)

adjustACC_high2 = meanACC_prev_aid[,2,2]-meanACC_prev2_aid[,2,2]
mean(adjustACC_high2)
se(adjustACC_high2)
sd(adjustACC_high2)


## pairwise differences by participant - used this ******** I think this is right 
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.high = dat.aid[row_sub,]  # only use rows with high accuracy aid
dat.low = dat.aid[!row_sub,]  # only use rows with low accuracy aid

# remove NA trials
# high accuracy
row_sub = !is.nan(dat.high$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.high_values = dat.high[row_sub,]  # only use rows with values

adjustACC_high3 = with(dat.high_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_high3)
se(adjustACC_high3)

# low accuracy
row_sub = !is.nan(dat.low$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.low_values = dat.low[row_sub,]  # only use rows with values

adjustACC_low3 = with(dat.low_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_low3)
se(adjustACC_low3)

#_________________________________________________________________________________________________________
######## ******* HARD TRIALS ONLY - Calculate Traditionl post-error adjustments ******** #######
### RT ####
row_sub = apply(dat.correct_noaid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.correct_hard_noaid = dat.correct_noaid[row_sub,]  # only use rows with hard response
dat.correct_easy_noaid = dat.correct_noaid[!row_sub,]  # only use rows with easy response

meanRT_hard_prev_no <- with(dat.correct_hard_noaid,tapply(humanRT,list(ID,ACC.0),mean))*1000
adjustRT_hard_no = meanRT_hard_prev_no[,2]-meanRT_hard_prev_no[,1]      
mean(adjustRT_hard_no)
se(adjustRT_hard_no)

meanRT_hard_prev_aid <- with(dat.correct_hard_aid,tapply(humanRT,list(ID,prob,ACC.0),mean))*1000
adjustRT_hard_low = meanRT_hard_prev_aid[,1,2]-meanRT_hard_prev_aid[,1,1]  
mean(adjustRT_hard_low)
se(adjustRT_hard_low)

adjustRT_hard_high = meanRT_hard_prev_aid[,2,2]-meanRT_hard_prev_aid[,2,1]  
mean(adjustRT_hard_high)
se(adjustRT_hard_high)

### ACC ###
row_sub = apply(dat.noaid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.hard_noaid = dat.noaid[row_sub,]  # only use rows with hard response
dat.easy_noaid = dat.noaid[!row_sub,]  # only use rows with easy response

meanACC_hard_prev_no <- with(dat.hard_noaid,tapply(ACC,list(ID,ACC.0),mean))*100
adjustACC_hard_no = meanACC_hard_prev_no[,2]-meanACC_hard_prev_no[,1]
mean(adjustACC_hard_no)
se(adjustACC_hard_no)

meanACC_hard_prev_aid <- with(dat.hard_aid,tapply(ACC,list(ID,prob,ACC.0),mean))*100
adjustACC_hard_low = meanACC_hard_prev_aid[,1,2]-meanACC_hard_prev_aid[,1,1]  
mean(adjustACC_hard_low)
se(adjustACC_hard_low)

adjustACC_hard_high = meanACC_hard_prev_aid[,2,2]-meanACC_hard_prev_aid[,2,1]  
mean(adjustACC_hard_high)
se(adjustACC_hard_high)

######## ******* Calculate Robust post-error adjustments ******** #######
### RT ####
## takes diff for each PE and preE,postC pair (for all participants)
mean(dat.correct_hard_noaid$RT_adj,na.rm=TRUE)*1000
se(dat.correct_hard_noaid$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
row_sub = !is.nan(dat.correct_hard_noaid$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_hard_noaid_values = dat.correct_hard_noaid[row_sub,]  # only use rows with values

adjustRT_hard_no2 = with(dat.correct_hard_noaid_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_hard_no2)
se(adjustRT_hard_no2)

## takes diff for each PE and preE,postC pair (for all participants combined)
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.correct_hard_aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.correct_hard_high = dat.correct_hard_aid[row_sub,]  # only use rows with high accuracy aid
dat.correct_hard_low = dat.correct_hard_aid[!row_sub,]  # only use rows with low accuracy aid

mean(dat.correct_hard_low$RT_adj,na.rm=TRUE)*1000
se(dat.correct_hard_low$RT_adj)*1000

mean(dat.correct_hard_high$RT_adj,na.rm=TRUE)*1000
se(dat.correct_hard_high$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
# high accuracy
row_sub = !is.nan(dat.correct_hard_high$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_hard_high_values = dat.correct_hard_high[row_sub,]  # only use rows with values

adjustRT_hard_high2 = with(dat.correct_hard_high_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_hard_high2)
se(adjustRT_hard_high2)

# low accuracy
row_sub = !is.nan(dat.correct_hard_low$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_hard_low_values = dat.correct_hard_low[row_sub,]  # only use rows with values

adjustRT_hard_low2 = with(dat.correct_hard_low_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_hard_low2)
se(adjustRT_hard_low2)

### ACC #### 
## no aid trials by participant - no pairwise differences
meanACC_hard_prev2_no <- with(dat.hard_noaid,tapply(ACC,list(ID,ACC.0.1),mean))*100   # pre-error, post-correct trials
adjustACC_hard_no2 = meanACC_hard_prev_no[,2]-meanACC_hard_prev2_no[,2]
mean(adjustACC_hard_no2)
se(adjustACC_hard_no2)

# pairwise differences by participant ******** I think this is right 
# remove NA trials
row_sub = !is.nan(dat.hard_noaid$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.hard_noaid_values = dat.hard_noaid[row_sub,]  # only use rows with values

adjustACC_hard_no3 = with(dat.hard_noaid_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_hard_no3)
se(adjustACC_hard_no3)

## with aid
meanACC_hard_prev2_aid <- with(dat.hard_aid,tapply(ACC,list(ID,prob,ACC.0.1),mean))*100
adjustACC_hard_low2 = meanACC_hard_prev_aid[,1,2]-meanACC_hard_prev2_aid[,1,2]
mean(adjustACC_hard_low2)
se(adjustACC_hard_low2)

adjustACC_hard_high2 = meanACC_hard_prev_aid[,2,2]-meanACC_hard_prev2_aid[,2,2]
mean(adjustACC_hard_high2)
se(adjustACC_hard_high2)
sd(adjustACC_hard_high2)


## pairwise differences by participant ******** I think this is right 
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.hard_aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.hard_high = dat.hard_aid[row_sub,]  # only use rows with high accuracy aid
dat.hard_low = dat.hard_aid[!row_sub,]  # only use rows with low accuracy aid

# remove NA trials
# high accuracy
row_sub = !is.nan(dat.hard_high$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.hard_high_values = dat.hard_high[row_sub,]  # only use rows with values

adjustACC_hard_high3 = with(dat.hard_high_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_hard_high3)
se(adjustACC_hard_high3)

# low accuracy
row_sub = !is.nan(dat.hard_low$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.hard_low_values = dat.hard_low[row_sub,]  # only use rows with values

adjustACC_hard_low3 = with(dat.hard_low_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_hard_low3)
se(adjustACC_hard_low3)

#_________________________________________________________________________________________________________
######## ******* EASY TRIALS ONLY - Calculate Traditionl post-error adjustments ******** #######
### RT ####
row_sub = apply(dat.correct_noaid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.correct_hard_noaid = dat.correct_noaid[row_sub,]  # only use rows with hard response
dat.correct_easy_noaid = dat.correct_noaid[!row_sub,]  # only use rows with easy response

meanRT_easy_prev_no <- with(dat.correct_easy_noaid,tapply(humanRT,list(ID,ACC.0),mean))*1000
adjustRT_easy_no = meanRT_easy_prev_no[,2]-meanRT_easy_prev_no[,1]      
mean(adjustRT_easy_no)
se(adjustRT_easy_no)

meanRT_easy_prev_aid <- with(dat.correct_easy_aid,tapply(humanRT,list(ID,prob,ACC.0),mean))*1000
adjustRT_easy_low = meanRT_easy_prev_aid[,1,2]-meanRT_easy_prev_aid[,1,1]  
mean(adjustRT_easy_low)
se(adjustRT_easy_low)

adjustRT_easy_high = meanRT_easy_prev_aid[,2,2]-meanRT_easy_prev_aid[,2,1]  
mean(adjustRT_easy_high)
se(adjustRT_easy_high)

### ACC ###
row_sub = apply(dat.noaid[5], 1, function(row) all(row != 'easy' ))  # find rows with hard response
dat.hard_noaid = dat.noaid[row_sub,]  # only use rows with hard response
dat.easy_noaid = dat.noaid[!row_sub,]  # only use rows with easy response

meanACC_easy_prev_no <- with(dat.easy_noaid,tapply(ACC,list(ID,ACC.0),mean))*100
adjustACC_easy_no = meanACC_easy_prev_no[,2]-meanACC_easy_prev_no[,1]
mean(adjustACC_easy_no)
se(adjustACC_easy_no)

meanACC_easy_prev_aid <- with(dat.easy_aid,tapply(ACC,list(ID,prob,ACC.0),mean))*100
adjustACC_easy_low = meanACC_easy_prev_aid[,1,2]-meanACC_easy_prev_aid[,1,1]  
mean(adjustACC_easy_low)
se(adjustACC_easy_low)

adjustACC_easy_high = meanACC_easy_prev_aid[,2,2]-meanACC_easy_prev_aid[,2,1]  
mean(adjustACC_easy_high)
se(adjustACC_easy_high)

######## ******* Calculate Robust post-error adjustments ******** #######
### RT ####
## takes diff for each PE and preE,postC pair (for all participants)
mean(dat.correct_easy_noaid$RT_adj,na.rm=TRUE)*1000
se(dat.correct_easy_noaid$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
row_sub = !is.nan(dat.correct_easy_noaid$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_easy_noaid_values = dat.correct_easy_noaid[row_sub,]  # only use rows with values

adjustRT_easy_no2 = with(dat.correct_easy_noaid_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_easy_no2)
se(adjustRT_easy_no2)

## takes diff for each PE and preE,postC pair (for all participants combined)
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.correct_easy_aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.correct_easy_high = dat.correct_easy_aid[row_sub,]  # only use rows with high accuracy aid
dat.correct_easy_low = dat.correct_easy_aid[!row_sub,]  # only use rows with low accuracy aid

mean(dat.correct_easy_low$RT_adj,na.rm=TRUE)*1000
se(dat.correct_easy_low$RT_adj)*1000

mean(dat.correct_easy_high$RT_adj,na.rm=TRUE)*1000
se(dat.correct_easy_high$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
# high accuracy
row_sub = !is.nan(dat.correct_easy_high$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_easy_high_values = dat.correct_easy_high[row_sub,]  # only use rows with values

adjustRT_easy_high2 = with(dat.correct_easy_high_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_easy_high2)
se(adjustRT_easy_high2)

# low accuracy
row_sub = !is.nan(dat.correct_easy_low$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_easy_low_values = dat.correct_easy_low[row_sub,]  # only use rows with values

adjustRT_easy_low2 = with(dat.correct_easy_low_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_easy_low2)
se(adjustRT_easy_low2)

### ACC #### 
## no aid trials by participant - no pairwise differences
meanACC_easy_prev2_no <- with(dat.easy_noaid,tapply(ACC,list(ID,ACC.0.1),mean))*100   # pre-error, post-correct trials
adjustACC_easy_no2 = meanACC_easy_prev_no[,2]-meanACC_easy_prev2_no[,2]
mean(adjustACC_easy_no2)
se(adjustACC_easy_no2)

# pairwise differences by participant ******** I think this is right 
# remove NA trials
row_sub = !is.nan(dat.easy_noaid$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.easy_noaid_values = dat.easy_noaid[row_sub,]  # only use rows with values

adjustACC_easy_no3 = with(dat.easy_noaid_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_easy_no3)
se(adjustACC_easy_no3)

## with aid
meanACC_easy_prev2_aid <- with(dat.easy_aid,tapply(ACC,list(ID,prob,ACC.0.1),mean))*100
adjustACC_easy_low2 = meanACC_easy_prev_aid[,1,2]-meanACC_easy_prev2_aid[,1,2]
mean(adjustACC_easy_low2)
se(adjustACC_easy_low2)

adjustACC_easy_high2 = meanACC_easy_prev_aid[,2,2]-meanACC_easy_prev2_aid[,2,2]
mean(adjustACC_easy_high2)
se(adjustACC_easy_high2)
sd(adjustACC_easy_high2)


## pairwise differences by participant ******** I think this is right 
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.easy_aid[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.easy_high = dat.easy_aid[row_sub,]  # only use rows with high accuracy aid
dat.easy_low = dat.easy_aid[!row_sub,]  # only use rows with low accuracy aid

# remove NA trials
# high accuracy
row_sub = !is.nan(dat.easy_high$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.easy_high_values = dat.easy_high[row_sub,]  # only use rows with values

adjustACC_easy_high3 = with(dat.easy_high_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_easy_high3)
se(adjustACC_easy_high3)

# low accuracy
row_sub = !is.nan(dat.easy_low$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.easy_low_values = dat.easy_low[row_sub,]  # only use rows with values

adjustACC_easy_low3 = with(dat.easy_low_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_easy_low3)
se(adjustACC_easy_low3)

#_________________________________________________________________________________________________________
######## ******* LEVEL 4 TRIALS ONLY - Calculate Traditionl post-error adjustments ******** #######
### RT ####
meanRT_prev_no <- with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0),mean))*1000
adjustRT_no = meanRT_prev_no[,2]-meanRT_prev_no[,1]      
mean(adjustRT_no)
se(adjustRT_no)

meanRT_lvl4_prev_aid <- with(dat.correct_lvl4,tapply(humanRT,list(ID,prob,ACC.0),mean))*1000
adjustRT_lvl4_low = meanRT_lvl4_prev_aid[,1,2]-meanRT_lvl4_prev_aid[,1,1]  
mean(adjustRT_lvl4_low)
se(adjustRT_lvl4_low)

adjustRT_lvl4_high = meanRT_lvl4_prev_aid[,2,2]-meanRT_lvl4_prev_aid[,2,1]  
mean(adjustRT_lvl4_high)
se(adjustRT_lvl4_high)

### ACC ###
meanACC_prev_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0),mean))*100
adjustACC_no = meanACC_prev_no[,2]-meanACC_prev_no[,1]
mean(adjustACC_no)
se(adjustACC_no)

meanACC_lvl4_prev_aid <- with(dat.lvl4,tapply(ACC,list(ID,prob,ACC.0),mean))*100
adjustACC_lvl4_low = meanACC_lvl4_prev_aid[,1,2]-meanACC_lvl4_prev_aid[,1,1]  
mean(adjustACC_lvl4_low)
se(adjustACC_lvl4_low)

adjustACC_lvl4_high = meanACC_lvl4_prev_aid[,2,2]-meanACC_lvl4_prev_aid[,2,1]  
mean(adjustACC_lvl4_high)
se(adjustACC_lvl4_high)

######## ******* Calculate Robust post-error adjustments ******** #######
### RT ####
## takes diff for each PE and preE,postC pair (for all participants)
mean(dat.correct_noaid$RT_adj,na.rm=TRUE)*1000
se(dat.correct_noaid$RT_adj)*1000

## by participant - used this ******** pretty sure this is right
# remove NA trials
row_sub = !is.nan(dat.correct_noaid$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_noaid_values = dat.correct_noaid[row_sub,]  # only use rows with values

adjustRT_no2 = with(dat.correct_noaid_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_no2)
se(adjustRT_no2)

## takes diff for each PE and preE,postC pair (for all participants combined)
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.correct_lvl4[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.correct_lvl4_high = dat.correct_lvl4[row_sub,]  # only use rows with high accuracy aid
dat.correct_lvl4_low = dat.correct_lvl4[!row_sub,]  # only use rows with low accuracy aid

mean(dat.correct_lvl4_low$RT_adj,na.rm=TRUE)*1000
se(dat.correct_lvl4_low$RT_adj)*1000

mean(dat.correct_lvl4_high$RT_adj,na.rm=TRUE)*1000
se(dat.correct_lvl4_high$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
# high accuracy
row_sub = !is.nan(dat.correct_lvl4_high$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_lvl4_high_values = dat.correct_lvl4_high[row_sub,]  # only use rows with values

adjustRT_lvl4_high2 = with(dat.correct_lvl4_high_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_lvl4_high2)
se(adjustRT_lvl4_high2)

# low accuracy
row_sub = !is.nan(dat.correct_lvl4_low$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_lvl4_low_values = dat.correct_lvl4_low[row_sub,]  # only use rows with values

adjustRT_lvl4_low2 = with(dat.correct_lvl4_low_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_lvl4_low2)
se(adjustRT_lvl4_low2)

### ACC #### 
## no aid trials by participant - no pairwise differences
meanACC_prev2_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0.1),mean))*100   # pre-error, post-correct trials
adjustACC_no2 = meanACC_prev_no[,2]-meanACC_prev2_no[,2]
mean(adjustACC_no2)
se(adjustACC_no2)  # same as above

# pairwise differences by participant ******** I think this is right 
# remove NA trials
row_sub = !is.nan(dat.noaid$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.noaid_values = dat.noaid[row_sub,]  # only use rows with values

adjustACC_no3 = with(dat.noaid_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_no3)
se(adjustACC_no3)

## with aid
meanACC_lvl4_prev2_aid <- with(dat.lvl4,tapply(ACC,list(ID,prob,ACC.0.1),mean))*100
adjustACC_lvl4_low2 = meanACC_lvl4_prev_aid[,1,2]-meanACC_lvl4_prev2_aid[,1,2]
mean(adjustACC_lvl4_low2)
se(adjustACC_lvl4_low2)

adjustACC_lvl4_high2 = meanACC_lvl4_prev_aid[,2,2]-meanACC_lvl4_prev2_aid[,2,2]
mean(adjustACC_lvl4_high2)
se(adjustACC_lvl4_high2)
sd(adjustACC_lvl4_high2)


## pairwise differences by participant ******** I think this is right 
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.lvl4[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.lvl4_high = dat.lvl4[row_sub,]  # only use rows with high accuracy aid
dat.lvl4_low = dat.lvl4[!row_sub,]  # only use rows with low accuracy aid

# remove NA trials
# high accuracy
row_sub = !is.nan(dat.lvl4_high$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.lvl4_high_values = dat.lvl4_high[row_sub,]  # only use rows with values

adjustACC_lvl4_high3 = with(dat.lvl4_high_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_lvl4_high3)
se(adjustACC_lvl4_high3)

# low accuracy
row_sub = !is.nan(dat.lvl4_low$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.lvl4_low_values = dat.lvl4_low[row_sub,]  # only use rows with values

adjustACC_lvl4_low3 = with(dat.lvl4_low_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_lvl4_low3)
se(adjustACC_lvl4_low3)

#_________________________________________________________________________________________________________
######## ******* LEVEL 5 TRIALS ONLY - Calculate Traditionl post-error adjustments ******** #######
### RT ####
meanRT_prev_no <- with(dat.correct_noaid,tapply(humanRT,list(ID,ACC.0),mean))*1000
adjustRT_no = meanRT_prev_no[,2]-meanRT_prev_no[,1]      
mean(adjustRT_no)
se(adjustRT_no)

meanRT_lvl5_prev_aid <- with(dat.correct_lvl5,tapply(humanRT,list(ID,prob,ACC.0),mean))*1000
adjustRT_lvl5_low = meanRT_lvl5_prev_aid[,1,2]-meanRT_lvl5_prev_aid[,1,1]  
mean(adjustRT_lvl5_low)
se(adjustRT_lvl5_low)

adjustRT_lvl5_high = meanRT_lvl5_prev_aid[,2,2]-meanRT_lvl5_prev_aid[,2,1]  
mean(adjustRT_lvl5_high)
se(adjustRT_lvl5_high)

### ACC ###
meanACC_prev_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0),mean))*100
adjustACC_no = meanACC_prev_no[,2]-meanACC_prev_no[,1]
mean(adjustACC_no)
se(adjustACC_no)

meanACC_lvl5_prev_aid <- with(dat.lvl5,tapply(ACC,list(ID,prob,ACC.0),mean))*100
adjustACC_lvl5_low = meanACC_lvl5_prev_aid[,1,2]-meanACC_lvl5_prev_aid[,1,1]  
mean(adjustACC_lvl5_low)
se(adjustACC_lvl5_low)

adjustACC_lvl5_high = meanACC_lvl5_prev_aid[,2,2]-meanACC_lvl5_prev_aid[,2,1]  
mean(adjustACC_lvl5_high)
se(adjustACC_lvl5_high)

######## ******* Calculate Robust post-error adjustments ******** #######
### RT ####
## takes diff for each PE and preE,postC pair (for all participants)
mean(dat.correct_noaid$RT_adj,na.rm=TRUE)*1000
se(dat.correct_noaid$RT_adj)*1000

## by participant - used this ******** pretty sure this is right
# remove NA trials
row_sub = !is.nan(dat.correct_noaid$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_noaid_values = dat.correct_noaid[row_sub,]  # only use rows with values

adjustRT_no2 = with(dat.correct_noaid_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_no2)
se(adjustRT_no2)

## takes diff for each PE and preE,postC pair (for all participants combined)
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.correct_lvl5[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.correct_lvl5_high = dat.correct_lvl5[row_sub,]  # only use rows with high accuracy aid
dat.correct_lvl5_low = dat.correct_lvl5[!row_sub,]  # only use rows with low accuracy aid

mean(dat.correct_lvl5_low$RT_adj,na.rm=TRUE)*1000
se(dat.correct_lvl5_low$RT_adj)*1000

mean(dat.correct_lvl5_high$RT_adj,na.rm=TRUE)*1000
se(dat.correct_lvl5_high$RT_adj)*1000

## by participant ******** pretty sure this is right
# remove NA trials
# high accuracy
row_sub = !is.nan(dat.correct_lvl5_high$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_lvl5_high_values = dat.correct_lvl5_high[row_sub,]  # only use rows with values

adjustRT_lvl5_high2 = with(dat.correct_lvl5_high_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_lvl5_high2)
se(adjustRT_lvl5_high2)

# low accuracy
row_sub = !is.nan(dat.correct_lvl5_low$RT_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.correct_lvl5_low_values = dat.correct_lvl5_low[row_sub,]  # only use rows with values

adjustRT_lvl5_low2 = with(dat.correct_lvl5_low_values,tapply(RT_adj,ID,mean))*1000
mean(adjustRT_lvl5_low2)
se(adjustRT_lvl5_low2)

### ACC #### 
## no aid trials by participant - no pairwise differences
meanACC_prev2_no <- with(dat.noaid,tapply(ACC,list(ID,ACC.0.1),mean))*100   # pre-error, post-correct trials
adjustACC_no2 = meanACC_prev_no[,2]-meanACC_prev2_no[,2]
mean(adjustACC_no2)
se(adjustACC_no2)  # same as above

# pairwise differences by participant ******** I think this is right 
# remove NA trials
row_sub = !is.nan(dat.noaid$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.noaid_values = dat.noaid[row_sub,]  # only use rows with values

adjustACC_no3 = with(dat.noaid_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_no3)
se(adjustACC_no3)

## with aid
meanACC_lvl5_prev2_aid <- with(dat.lvl5,tapply(ACC,list(ID,prob,ACC.0.1),mean))*100
adjustACC_lvl5_low2 = meanACC_lvl5_prev_aid[,1,2]-meanACC_lvl5_prev2_aid[,1,2]
mean(adjustACC_lvl5_low2)
se(adjustACC_lvl4_low2)

adjustACC_lvl5_high2 = meanACC_lvl5_prev_aid[,2,2]-meanACC_lvl5_prev2_aid[,2,2]
mean(adjustACC_lvl5_high2)
se(adjustACC_lvl5_high2)
sd(adjustACC_lvl5_high2)


## pairwise differences by participant ******** I think this is right 
# separate decision data by LOW and HIGH accuracy aid
row_sub = apply(dat.lvl5[21], 1, function(row) all(row != .8 ))  # find rows with high accuracy
dat.lvl5_high = dat.lvl5[row_sub,]  # only use rows with high accuracy aid
dat.lvl5_low = dat.lvl5[!row_sub,]  # only use rows with low accuracy aid

# remove NA trials
# high accuracy
row_sub = !is.nan(dat.lvl5_high$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.lvl5_high_values = dat.lvl5_high[row_sub,]  # only use rows with values

adjustACC_lvl5_high3 = with(dat.lvl5_high_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_lvl5_high3)
se(adjustACC_lvl5_high3)

# low accuracy
row_sub = !is.nan(dat.lvl5_low$ACC_adj)  # find if rows aren't NaN (these are the rows to keep)
dat.lvl5_low_values = dat.lvl5_low[row_sub,]  # only use rows with values

adjustACC_lvl5_low3 = with(dat.lvl5_low_values,tapply(ACC_adj,ID,mean))*100
mean(adjustACC_lvl5_low3)
se(adjustACC_lvl5_low3)
