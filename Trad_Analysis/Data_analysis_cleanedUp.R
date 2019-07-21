library(UsingR)
library(ggplot2)
library(ez)
library("Hmisc")  # for error bars
library(psych)
library(lme4)

# ******* read in data ******* 
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

# add column that has true/false (1/0) of whether previous trial was incorrect
dat$ACC.0 = 0
nmax = length(dat$ID)
for(n in 2:nmax){
  if(dat$ACC[n-1]==0) dat$ACC.0[n]=1
}

################### ******* Remove zero RT ******** ############################
# ******* Go through each row and determine if human RT value is zero *********
row_sub = apply(dat[12], 1, function(row) all(row !=0 ))  # find rows where RT != 0
dat = dat[row_sub,]  # only use rows where RT != 0


################### ******* Look for outliers and remove ******** ############################
boxplot(dat$humanRT ~ dat$condition)

q1 = quantile(dat$humanRT[dat$condition==1], probs = seq(0,1,.01))
q2 = quantile(dat$humanRT[dat$condition==2], probs = seq(0,1,.01))
q3 = quantile(dat$humanRT[dat$condition==3], probs = seq(0,1,.01))
q4 = quantile(dat$humanRT[dat$condition==4], probs = seq(0,1,.01))
q5 = quantile(dat$humanRT[dat$condition==5], probs = seq(0,1,.01))
q6 = quantile(dat$humanRT[dat$condition==6], probs = seq(0,1,.01))
q7 = quantile(dat$humanRT[dat$condition==7], probs = seq(0,1,.01))
q8 = quantile(dat$humanRT[dat$condition==8], probs = seq(0,1,.01))
q9 = quantile(dat$humanRT[dat$condition==9], probs = seq(0,1,.01))
q10 = quantile(dat$humanRT[dat$condition==10], probs = seq(0,1,.01))

p97=c(q1[98],q2[98],q3[98],q4[98],q5[98],q6[98],q7[98],q8[98],q9[98],q10[98])
p98=c(q1[99],q2[99],q3[99],q4[99],q5[99],q6[99],q7[99],q8[99],q9[99],q10[99])
p99=c(q1[100],q2[100],q3[100],q4[100],q5[100],q6[100],q7[100],q8[100],q9[100],q10[100])

# use 4.167783 sec as cutoff to remove (based on 44 participants)

# remove RT over 4.16 sec
row_sub = apply(dat[12], 1, function(row) all(row < 4.16))  # find rows where RT <4.16
dat = dat[row_sub,]  # only use rows where RT < 4.16

boxplot(dat$humanRT ~ dat$condition)


################### ******* Create add'l data frames ******** ############################
# separate data by NO AID trials
row_sub = apply(dat[8], 1, function(row) all(row != 'NO AID' ))  # find rows without NO AID response
dat.decision = dat[row_sub,]  # only use rows without NO AID response
dat.noaid = dat[!row_sub,]  # only use rows with NO AID response

# dataset with only correct responses
row_sub = apply(dat[15], 1, function(row) all(row ==1 ))  # find rows with correct responses
dat.correct = dat[row_sub,]  # only use rows with correct responses

# correct data without NO AID trials
row_sub = apply(dat.correct[8], 1, function(row) all(row != 'NO AID' ))  # find rows without NO AID response
dat.correct_decision = dat.correct[row_sub,]  # only use rows without NO AID response
dat.correct_noaid = dat.correct[!row_sub,]  # only use rows with NO AID response


#______________________________________________________________________________________________________________________________________
# ********* some descriptive statistics - for all trials ********

sum_stats <- data.frame(c(aggregate(humanRT ~ condition, data = dat, FUN = mean),aggregate(humanRT ~ condition, data = dat, FUN = sd),
        aggregate(humanRT ~ condition, data = dat, FUN = min), aggregate(humanRT ~ condition, data = dat, FUN = max),
        aggregate(humanRT ~ condition, data = dat, FUN = median),
        aggregate(humanResp ~ condition, data = dat, FUN = mean),aggregate(humanResp ~ condition, data = dat, FUN = sd),
        aggregate(ACC ~ condition, data = dat, FUN = mean), aggregate(ACC ~ condition, data = dat, FUN = sd)))

sum_stats$condition.1=NULL
sum_stats$condition.2=NULL
sum_stats$condition.3=NULL
sum_stats$condition.4=NULL
sum_stats$condition.5=NULL
sum_stats$condition.6=NULL
sum_stats$condition.7=NULL
sum_stats$condition.8=NULL

names(sum_stats)[3]<-'humanRT sd'
names(sum_stats)[4]<-'humanRT min'
names(sum_stats)[5]<-'humanRT max'
names(sum_stats)[6]<-'humanRT median'
names(sum_stats)[8]<-'humanResp sd'
names(sum_stats)[10]<-'ACC sd'

n = c(sum(dat$condition == 1), sum(dat$condition == 2), sum(dat$condition == 3), sum(dat$condition == 4), sum(dat$condition == 5), 
      sum(dat$condition == 6), sum(dat$condition == 7), sum(dat$condition == 8), sum(dat$condition == 9), sum(dat$condition == 10))

sum_stats = cbind(n,sum_stats)

# des.mat <- describeBy(dat,list(dat$ID,dat$condition),mat=TRUE) # find mean/median/sd/se for each subject and condition

#______________________________________________________________________________________________________________________________________
# reduce data by participant (over different conditions)

ID_cond_mean <- aggregate(. ~ ID + condition, data = dat, FUN = mean)  # find means for each participant by condition
ID_cond_median <- aggregate(. ~ ID + condition, data = dat, FUN = median)  # find median for each participant by condition

cond_median <- aggregate(humanRT ~ condition, data = dat, FUN = median) # find median RT by condition --> used this in paper

cond_mean_mean <- aggregate(. ~ condition, data = ID_cond_mean, FUN = mean)  # find means for condition
cond_median_mean <- aggregate(. ~ condition, data = ID_cond_median, FUN = mean)  # find means of medians for condition

cond_mean_sd <- aggregate(. ~ condition, data = ID_cond_mean, FUN = sd)  # find sds for condition
cond_median_sd <- aggregate(. ~ condition, data = ID_cond_median, FUN = sd)  # find sd of medians for condition

# cond_median_median <- aggregate(humanRT ~ condition, data = ID_cond_median, FUN = median)  # find median of medians of RT by condition

#______________________________________________________________________________________________________________________________________
# Plot of data (not used)

## Run boxplot to find statistics, but don't draw the boxplots 
S <- boxplot(ID_cond_mean$ACC ~ ID_cond_mean$condition, plot=FALSE)

sum_stats$ACC_q1 = S$stats[2,]
sum_stats$ACC_q3 = S$stats[4,]

means = cond_mean$ACC
condition = cond_mean$condition

#boxplot(ID_cond_mean$ACC ~ ID_cond_mean$condition)
plot(condition, means, type ='p', pch = 18, cex = 1.2, main = "User accuracy by test condition", 
     xlab = "Condition", ylab = "User Accuracy", yaxs = "i", ylim = c(.4,1), xaxp  = c(1, 10, 9))

barplot(means[1:2], names = condition[1:2], main = "User accuracy by test condition", 
     xlab = "Condition", ylab = "User Accuracy", ylim = c(0,1))
barplot(means[3:6], names.arg = condition[3:6], type = 'b')

at <- c(1:length(S$names)) 

##-  Get CIs -## 
## create standard error function-- 
se <- function(x) { 
  y <- x[!is.na(x)] 
  sqrt(var(as.vector(y))/length(y)) 
} 

## create length function for non-missing values 
lngth <- function(x){ 
  y <- x[!is.na(x)] 
  length(y) 
} 

## Compute vectors of standard error and n - for accuracy
Hse_acc <- by(ID_cond_mean$ACC,ID_cond_mean$condition,se) 
Hn_acc  <- by(ID_cond_mean$ACC,ID_cond_mean$condition,lngth) 

cond_mean$se_ACC = c(Hse_acc[1],Hse_acc[2],Hse_acc[3],Hse_acc[4],Hse_acc[5],Hse_acc[6],Hse_acc[7],Hse_acc[8],Hse_acc[9],Hse_acc[10])

## Compute vectors of standard error and n - for RT
Hse_rt <- by(ID_cond_median$humanRT,ID_cond_median$condition,se) 
Hn_rt  <- by(ID_cond_median$humanRT,ID_cond_median$condition,lngth) 

## compute 95% CIs and store in vectors 
civ.u_acc <- cond_mean$ACC + qt(.975, df=Hn_acc-1) * Hse_acc # Upper bound CI 
civ.l_acc <- cond_mean$ACC + qt(.025, df=Hn_acc-1) * Hse_acc # Lower bound CI 

cond_mean$u_acc = c(civ.u_acc[1],civ.u_acc[2],civ.u_acc[3],civ.u_acc[4],civ.u_acc[5],civ.u_acc[6],civ.u_acc[7],
                    civ.u_acc[8],civ.u_acc[9],civ.u_acc[10])
cond_mean$l_acc = c(civ.l_acc[1],civ.l_acc[2],civ.l_acc[3],civ.l_acc[4],civ.l_acc[5],civ.l_acc[6],civ.l_acc[7],
                    civ.l_acc[8],civ.l_acc[9],civ.l_acc[10])

## compute 95% CIs and store in vectors 
civ.u_rt <- cond_median$humanRT + qt(.975, df=Hn_rt-1) * Hse_rt # Upper bound CI 
civ.l_rt <- cond_median$humanRT + qt(.025, df=Hn_rt-1) * Hse_rt # Lower bound CI 

cond_mean$u_rt = c(civ.u_rt[1],civ.u_rt[2],civ.u_rt[3],civ.u_rt[4],civ.u_rt[5],civ.u_rt[6],civ.u_rt[7],
                    civ.u_rt[8],civ.u_rt[9],civ.u_rt[10])
cond_mean$l_rt = c(civ.l_rt[1],civ.l_rt[2],civ.l_rt[3],civ.l_rt[4],civ.l_rt[5],civ.l_rt[6],civ.l_rt[7],
                    civ.l_rt[8],civ.l_rt[9],civ.l_rt[10])

## Draw CI, first vertical line, then upper and lower horizontal 
segments(at, civ.u_acc, at, civ.l_acc, lty = "solid", lwd = 1.5, col = "black") 
segments(at - 0.15, civ.u_acc, at + 0.15, civ.u_acc, lty = "solid", lwd =2,col = "black") 
segments(at - 0.15, civ.l_acc, at + 0.15, civ.l_acc, lty = "solid", lwd =2,col = "black") 

## Draw quantiles, first vertical line, then upper and lower horizontal 
segments(at, sum_stats$ACC_q3, at, sum_stats$ACC_q1, lty = 2, lwd = 1.5, col = "black") 
segments(at - 0.15, sum_stats$ACC_q3, at + 0.15, sum_stats$ACC_q3, lty = "solid", lwd =1.5,col = "black") 
segments(at - 0.15, sum_stats$ACC_q1, at + 0.15, sum_stats$ACC_q1, lty = "solid", lwd =1.5,col = "black") 
#segments(at - .15, S$stats[3,], at + 0.15, S$stats[3,], lty = 'solid', lwd = 3, col = 'black')
points(at,S$stats[3,], pch = 1, col = 'black', cex = 1.5)
legend(1,.6,c('Mean', 'CI', 'Quartile', 'Median'), lty = c(0,1,2,0), pch = c(18,NA_integer_,NA_integer_,1), lwd = c(0,2,1.5,0), bty='n')

## Draw Mean values to the left edge of each violinplot 
text(at - 0.1, means, labels = formatC(means, format = "f", digits = 2), 
     pos = 2, cex = 1, col = "black") 

## Draw Median values to the right edge of each violinplot 
text(at + 0.1, S$stats[3, ], labels = formatC(S$stats[3, ], 
                                              format = "f", digits = 2), pos = 4, cex = 1, col = "black") 

## Print "n" under the name of measure 
mtext(S$n, side = 1, at = at, cex=.75, line = 2.5) 

# ggplot(data = cond_mean) + geom_col(aes(x = condition, y = ACC, fill=factor(decide)))+theme_bw()+geom_errorbar(aes(condition,ACC,ymin = l_acc, ymax = u_acc))


#______________________________________________________________________________________________________________________________________
# ********* some descriptive statistics - for correct trials ********

sum_stats_correct <- data.frame(c(aggregate(humanRT ~ condition, data = dat.correct, FUN = mean),aggregate(humanRT ~ condition, data = dat.correct, FUN = sd),
               aggregate(humanRT ~ condition, data = dat.correct, FUN = min), aggregate(humanRT ~ condition, data = dat.correct, FUN = max),
               aggregate(humanRT ~ condition, data = dat.correct, FUN = median),
               aggregate(humanResp ~ condition, data = dat.correct, FUN = mean),aggregate(humanResp ~ condition, data = dat.correct, FUN = sd),
               aggregate(ACC ~ condition, data = dat.correct, FUN = mean), aggregate(ACC ~ condition, data = dat.correct, FUN = sd)))

sum_stats_correct$condition.1=NULL
sum_stats_correct$condition.2=NULL
sum_stats_correct$condition.3=NULL
sum_stats_correct$condition.4=NULL
sum_stats_correct$condition.5=NULL
sum_stats_correct$condition.6=NULL
sum_stats_correct$condition.7=NULL
sum_stats_correct$condition.8=NULL

names(sum_stats_correct)[3]<-'humanRT sd'
names(sum_stats_correct)[4]<-'humanRT min'
names(sum_stats_correct)[5]<-'humanRT max'
names(sum_stats_correct)[6]<-'humanRT median'
names(sum_stats_correct)[8]<-'humanResp sd'
names(sum_stats_correct)[10]<-'ACC sd'

n_correct = c(sum(dat.correct$condition == 1), sum(dat.correct$condition == 2), sum(dat.correct$condition == 3), 
      sum(dat.correct$condition == 4), sum(dat.correct$condition == 5), sum(dat.correct$condition == 6), 
      sum(dat.correct$condition == 7), sum(dat.correct$condition == 8), sum(dat.correct$condition == 9), 
      sum(dat.correct$condition == 10))

sum_stats_correct = cbind(n_correct,sum_stats_correct)



#______________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________
################# ********** RT analysis ************ ############################

# ******* linear mixed-effects regression ******** 
# just use decision data (decide/reject) - leave out baseline (no aid)

# run linear mixed-effects regression (lmer) over reduced # trials with RT as outcome 
# data contains only correct responses from aided trials

# no predictors - not sure what value this adds
regRT.onlyaided = lmer(humanRT ~ (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided)

# decision type as predictor
regRT.onlyaided1 = lmer(humanRT ~ decide + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided1)

regRT.onlyaided1n = lmer(humanRT ~ decide_lvl + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided1n)

# decision type, aid accuracy (False Alarm) and trial difficuly as predictors 
regRT.onlyaided2 = lmer(humanRT ~ decide + falseAlarm + difficulty + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided2)

regRT.onlyaided2n = lmer(humanRT ~ decide + prob + difficulty + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided2n)  # did this to help me figure out direction of relationship


# use anova to compare the 2 models
anova(regRT.onlyaided1,regRT.onlyaided2, test="Chisq")

# I think this tells me that the 2nd model better describes the RT data
# decision type and false alarm have large t-values (likely significant), difficulty likely isn't significant


# condition as predictor
regRT.onlyaided3 = lmer(humanRT ~ condition + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided3)

# use anova to compare these 2 models
anova(regRT.onlyaided3,regRT.onlyaided2, test="Chisq")

# model onlyaided2 is still the better model


# decision type and aid accuracy (False Alarm) as predictors 
regRT.onlyaided8 = lmer(humanRT ~ decide + falseAlarm + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided8)

# use anova to compare these 2 models
anova(regRT.onlyaided8,regRT.onlyaided2, test="Chisq")

# model onlyaided2 is still the better model


# decision type, aid accuracy (False Alarm), stimulus length as predictors 
regRT.onlyaided9 = lmer(humanRT ~ decide + falseAlarm + stimlength + (1|ID), dat.correct_decision, REML = F)
summary(regRT.onlyaided9)

# use anova to compare these 2 models
anova(regRT.onlyaided9,regRT.onlyaided2, test="Chisq")

# no difference in the models ?? 


# run multiple regression over reduced # trials with RT as outcome and decide (3 levels), 
# difficulty and falseAlarm as predictors
regRT.onlyaided6 = lm(humanRT ~ decide + falseAlarm + difficulty, dat.correct_decision)
summary(regRT.onlyaided6) 

anova(regRT.onlyaided4,regRT.onlyaided6, test="Chisq")

# model onlyaided6 is the better model


regRT.onlyaided7 = lm(humanRT ~ decide + falseAlarm, dat.correct_decision)
summary(regRT.onlyaided7)

anova(regRT.onlyaided6,regRT.onlyaided7, test = "Chisq")

# onlyaided7 is the better model



#______________________________________________________________________________________________________________________________________
# ********** more RT analysis ************ 
# ****** look for interactions *******
# decision and false alarm interaction
regRT.inter1 = lmer(humanRT ~ (decide * falseAlarm) + difficulty + (1|ID), dat.correct_decision, REML = F)
summary(regRT.inter1)

regRT.inter1n = lmer(humanRT ~ (decide * prob) + difficulty + (1|ID), dat.correct_decision, REML = F)
summary(regRT.inter1n)  # --> used in paper

confint(regRT.inter1n)

# use anova to compare this to onlyaided2 
anova(regRT.inter1,regRT.onlyaided2, test="Chisq")
anova(regRT.inter1n,regRT.onlyaided2n, test="Chisq")

# inter1 is the better model

regRT.inter2n = lmer(humanRT ~ (decide * prob * difficulty) + (1|ID), dat.correct_decision, REML = F)
summary(regRT.inter2n)

# use anova to compare this to inter1 
anova(regRT.inter1n,regRT.inter2n, test="Chisq")

# inter1 is the better model

# ********* plot interaction  exclding RT < 4.16 sec - used to find 92% intersect mentioned in conclusion
# hypothesis is that decision condition (moderator) has an effect on how aid accuracy (prob) predicts humanRT
# write out equation
# humanRT = 0.761594 + 1.235451(decide) + -0.015948(prob) + -1.331415(decide x prob)
# set decide to zero and get equation for direct decision 
# humanRT = 0.761594 + 1.235451(0) + -0.015948(prob) + -1.331415(0 x prob) = 0.761594 - 0.015948(prob)
# set decide to one and get equation for accept/reject decision
# humanRT = 0.761594 + 1.235451(1) + -0.015948(prob) + -1.331415(1 x prob) = 1.997045 - 1.347363(prob)


# plot - using decide_lvl and prob --> updated w/ N = 48 values
prob = c(min(dat.correct_decision$prob),max(dat.correct_decision$prob))  # create vector of min and max of aid accuracy
# all subject interaction
# decide = (0.761594 - 0.015948*prob)*1000  # line for decide condition
# reject = (1.997045 - 1.347363*prob)*1000

# fast subject interaction
# decide = (0.506504 + 0.237608*prob)*1000  # line for decide condition
# reject = (1.86156 - 1.563472*prob)*1000

# slow subject interaction
decide = (0.87421 - 0.12604*prob)*1000   # line for decide condition
reject = (1.98801 - 1.16766*prob)*1000

plot(prob, reject, type='l', col = 'black', main = c("Interaction of Decision Type and","Aid Accuracy on Human Response Time"), 
     ylim=c(300,1200), 
     xlab = 'Aid Accuracy', ylab = 'Human Response Time (ms)')
lines(prob, decide, type='l', lty = 2, col = 'black')  # adds another line to the first plot
legend(.88,600,c('Agree/Disagree','Select'), lty = c(1,2), col = (c('black','black')),bty='n')


### plot for poster with error bars

pd_RT = as.data.frame(with(ID_cond_median,tapply(humanRT*1000,list(ID,falseAlarm,decide),mean)))  #fa: 3=no aid, 2=high acc aid, 1=low acc aid
names(pd_RT)[1]<-'low_rt_select'  #was 1.1        #decide: 3=reject, 2=no aid, 1=select
names(pd_RT)[2]<-'high_rt_select'  #was 2.1
names(pd_RT)[6]<-'no_aid'         #was 3.2
names(pd_RT)[7]<-'low_rt_reject'   #was 1.3
names(pd_RT)[8]<-'high_rt_reject'  #was 2.3


plot_data_decide = c(mean(pd_RT$low_rt_select),mean(pd_RT$high_rt_select))  # low acc first, high acc second
plot_data_reject = c(mean(pd_RT$low_rt_reject),mean(pd_RT$high_rt_reject))

pd_decide_error = c(sd(pd_RT$low_rt_select),sd(pd_RT$high_rt_select)) 
pd_reject_error = c(sd(pd_RT$low_rt_reject),sd(pd_RT$high_rt_reject))

# pd_decide_se = c(se(pd_RT$low_rt_select),se(pd_RT$high_rt_select)) 
# pd_reject_se = c(se(pd_RT$low_rt_reject),se(pd_RT$high_rt_reject))
# 
# # ## Compute standard error and n - for accuracy
# Hse_rt <- as.data.frame(with(ID_cond_median,tapply(humanRT*1000,list(falseAlarm,decide),se)))
# Hn_rt  <- with(ID_cond_median,tapply(humanRT*1000,list(falseAlarm,decide),lngth))
# 
# 
# # ## compute 95% CIs 
# civ.u_rt <- plot_data_AB + qt(.975, df=Hn_rt-1) * Hse_rt # Upper bound CI
# civ.l_rt <- plot_data_AB + qt(.025, df=Hn_rt-1) * Hse_rt # Lower bound CI
# 
# # d1 = data.frame(
# #   x  = c(1:2)
# #   , y  = plot_data_decide
# #   , up = c(civ.u_rt[1,1], civ.u_rt[3])
# #   , down = c(civ.l_rt[1], civ.l_rt[3])
# # )
# # 
# # d2 = data.frame(
# #   x  = c(1:2)
# #   , y  = c(plot_data_AB[2], plot_data_AB[4])
# #   , up = c(civ.u_rt[2], civ.u_rt[4])
# #   , down = c(civ.l_rt[2], civ.l_rt[4])
# # )

x = c(0,1)
plot(x,plot_data_decide,lwd=2,ylim=c(300,1000),col="orangered",type="b",ylab="Median Response Time (sec)",xlab="Aid Accuracy",xaxt="n",main=c("Interaction of Decision Type and","Aid Accuracy on Reponse Time")) # a1 
segments(x, plot_data_decide-(pd_decide_error/2), x, plot_data_decide+pd_decide_error/2, lty = 2, lwd = 1.5, col = "orangered")
segments(x - 0.02, plot_data_decide-pd_decide_error/2, x + 0.02, plot_data_decide-pd_decide_error/2, lty = "solid", lwd =1.5,col = "orangered")
segments(x - 0.02, plot_data_decide+pd_decide_error/2, x + 0.02, plot_data_decide+pd_decide_error/2, lty = "solid", lwd =1.5,col = "orangered")
axis(1,at=c(0,1),labels=c("low (80%)","high (95%)"))  
lines(x,plot_data_reject,type="b",lty=1,pch=1,col="grey40",lwd=2)   #a2
segments(x, plot_data_reject-pd_reject_error/2, x, plot_data_reject+pd_reject_error/2, lty = 2, lwd = 1.5, col = "grey40")
segments(x - 0.02, plot_data_reject-pd_reject_error/2, x + 0.02, plot_data_reject-pd_reject_error/2, lty = "solid", lwd =1.5,col = "grey40")
segments(x - 0.02, plot_data_reject+pd_reject_error/2, x + 0.02, plot_data_reject+pd_reject_error/2, lty = "solid", lwd =1.5,col = "grey40")
legend(0.6,600,legend=c("Select","Agree/Disagree"),lty=1,pch=1,col=c("orangered","grey40"),lwd=2)






#______________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________
############################ ********** Accuracy analysis ************ #############################

# ******* general linear mixed-effects regression ******** 
# just use decision data (decide/reject) - leave out baseline (no aid)

# decision type as predictor
regACC.onlyaided1 = glmer(ACC ~ decide + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided1)

# decision type, aid accuracy (False Alarm) and trial difficuly as predictors 
regACC.onlyaided2 = glmer(ACC ~ decide + falseAlarm + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided2)

regACC.onlyaided2n = glmer(ACC ~ decide_lvl + prob + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided2n)

# use anova to compare the 2 models
anova(regACC.onlyaided1,regACC.onlyaided2n, test="Chisq")

# I think this tells me that the 2nd model better describes the ACC data
# false alarm and difficulty are significant, decision is also significant, but with higher p (p = 0.04)

# add RT as a predictor - don't do this, use LBA
regACC.onlyaided3 = glmer(ACC ~ humanRT + decide + falseAlarm + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided3)

# use anova to compare the 2 models
anova(regACC.onlyaided3,regACC.onlyaided2n, test="Chisq")

# onlyaided3 (with RT) is the better model

# remove decision type
regACC.onlyaided4 = glmer(ACC ~ humanRT + falseAlarm + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided4)

# use anova to compare the 2 models
anova(regACC.onlyaided3,regACC.onlyaided4, test="Chisq")

# onlyaided3 (with decision type) is better model


# condition as predictor
regACC.onlyaided4 = glmer(ACC ~ condition + (1|ID), dat.decision, family = binomial())
summary(regACC.onlyaided4)  # condition is significant - makes sense since it's some combination of decision type, FA and difficulty

# use anova to compare these 2 models
anova(regACC.onlyaided4,regACC.onlyaided2n, test="Chisq")

# model onlyaided2n is still the better model


# ****** look for interactions *******
# decision and false alarm interaction
regACC.inter1 = glmer(ACC ~ humanRT + (decide * falseAlarm) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter1)

regACC.inter1n = glmer(ACC ~ humanRT + (decide_lvl * prob) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter1n)

# use anova to compare this to onlyaided2 
anova(regACC.inter1,regACC.onlyaided3, test="Chisq")

# inter1 is the better model

regACC.inter2 = glmer(ACC ~ humanRT + (humanRT * falseAlarm) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter2)

# use anova to compare this to inter1
anova(regACC.inter1,regACC.inter2, test="Chisq")

# inter1 is the better model

regACC.inter3 = glmer(ACC ~ humanRT + (falseAlarm * decide) + (falseAlarm * humanRT) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter3)

# use anova to compare this to inter1
anova(regACC.inter1,regACC.inter3, test="Chisq")

# inter3 is the better model

regACC.inter4 = glmer(ACC ~ (falseAlarm * decide) + (falseAlarm * humanRT) + (difficulty * humanRT) + (1|ID), dat.decision, family = binomial())
summary(regACC.inter4)

anova(regACC.inter4, regACC.inter3)

# inter4 is the better model

##########################################
#################### no RT in interactions --> values used in paper
##########################################
regACC.inter6 = glmer(ACC ~ (decide * falseAlarm) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter6) # --> values used in paper

# estimated probabilities for decicion type
exp(1.78192)/(1+exp(1.78192))   # select
exp(1.78192-.12822)/(1+exp(1.78192-.12822))  # 

regACC.inter6n = glmer(ACC ~ (decide * prob) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter6n)


#______________________________________________________________________________________________________________________________________
# Accuracy glmer

# no RT in interactions --> values used in paper
regACC.inter6 = glmer(ACC ~ (decide * falseAlarm) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter6) # --> values used in paper

# estimated probabilities for decicion type
exp(1.78192)/(1+exp(1.78192))   # select -> 0.8559338
exp(1.78192-.12822)/(1+exp(1.78192-.12822))  # agree/disagree -> 0.8393905

# estimated probabilities for aid accuracy
exp(1.78192)/(1+exp(1.78192))   # low accuracy (or high FA) -> 0.8559338
exp(1.78192+0.58412)/(1+exp(1.78192+0.58412))  # high accuracy (or high FA) -> 0.9142008

# estimated probabilities for task difficulty
exp(1.78192)/(1+exp(1.78192))   # easy -> 0.8559338
exp(1.78192-0.43215)/(1+exp(1.78192-0.43215))  # difficult -> 0.794092

# estimated proabilities for combined conditions - this doens't really tell me anything
exp(1.78192)/(1+exp(1.78192))   # select/low accuracy/easy -> 0.8559338
exp(1.78192-0.12822+0.58412-0.43215)/(1+exp(1.78192-0.12822+0.58412-0.43215))  # agree-disagree/high accuracy/hard 
# -> 0.8588377

# interaction
# hypothesis is that decision condition (moderator) has an effect on how aid accuracy (FA) predicts ACC
# write out equation
# ACC = 1.78192 + -0.12822(decide) + 0.58412(prob) + 0.37739(decide x prob)
# set decide to zero and get equation for direct decision 
# ACC = 1.78192 + -0.12822(0) + 0.58412(prob) + 0.37739(0 x prob) = 1.78192 + 0.58412(prob)
# set decide to one and get equation for accept/reject decision
# ACC = 1.78192 + -0.12822(1) + 0.58412(prob) + 0.37739(1 x prob) = 1.6537 + 0.96151(prob)

# plot - using decide and falseAlarm - didn't update with N=48 values
prob = c(0,1)  # create vector of min and max of aid accuracy - 0 = high FA, 1 = low FA
decide = exp(1.78192 + 0.58412*prob)/(1+exp(1.78192 + 0.58412*prob))  # line for decide condition
reject = exp(1.6537 + 0.96151*prob)/(1+exp(1.6537 + 0.96151*prob))

plot(prob, reject, type='l', col = 'black', main = c("Interaction of Decision Type and","Aid Accuracy on Accuracy"), 
     ylim=c(.8,1), 
     xlab = 'Aid Accuracy', ylab = 'Human Accuracy')
lines(prob, decide, type='l', lty = 2, col = 'black')  # adds another line to the first plot
legend(.6,.98,c('Accept/Reject','Select'), lty = c(1,2), col = (c('black','black')),bty='n')

#### look at interaction using prob instead of FA - gives exact same lines - yay!!! --> updated with N=48 values
regACC.inter6n = glmer(ACC ~ (decide * prob) + difficulty + (1|ID), dat.decision, family = binomial())
summary(regACC.inter6n)

prob = c(0.8,.95)  # create vector of min and max of aid accuracy 
# all subjects N=48
# decide = exp(-1.2935 + 3.8363*prob)/(1+exp(-1.2935 + 3.8363*prob))  # line for decide condition
# reject = exp(-3.3952 + 6.2637*prob)/(1+exp(-3.3952 + 6.2637*prob)) 

# fast subjects N = 14
# decide = exp(-2.62779 + 5.16881*prob)/(1+exp(-2.62779 + 5.16881*prob))
# reject = exp(-6.96951 + 9.98042*prob)/(1+exp(-6.96951 + 9.98042*prob))

# slow subjects N = 30
decide = exp(-0.64110 + 3.20783*prob)/(1+exp(-0.64110 + 3.20783*prob))
reject = exp(-1.41638 + 4.16993*prob)/(1+exp(-1.41638 + 4.16993*prob))

plot(prob, reject, type='l', col = 'black', main = c("Interaction of Decision Type and","Aid Accuracy on Accuracy"), 
     ylim=c(.8,1), 
     xlab = 'Aid Accuracy', ylab = 'Human Accuracy')
lines(prob, decide, type='l', lty = 2, col = 'black')  # adds another line to the first plot
legend(.88,.85,c('Agree/Disagree','Select'), lty = c(1,2), col = (c('black','black')),bty='n')

### plot for poster with error bars
pd_ACC = as.data.frame(with(ID_cond_mean,tapply(ACC,list(ID,falseAlarm,decide),mean)))  #fa: 3=no aid, 2=high acc aid, 1=low acc aid
names(pd_ACC)[1]<-'low_acc_select'          #decide: 3=reject, 2=no aid, 1=select
names(pd_ACC)[2]<-'high_acc_select' 
names(pd_ACC)[6]<-'no_aid'
names(pd_ACC)[7]<-'low_acc_reject' 
names(pd_ACC)[8]<-'high_acc_reject' 


plot_data_decide = c(mean(pd_ACC$low_acc_select),mean(pd_ACC$high_acc_select))  # low acc first, high acc second
plot_data_reject = c(mean(pd_ACC$low_acc_reject),mean(pd_ACC$high_acc_reject))

pd_decide_error = c(sd(pd_ACC$low_acc_select),sd(pd_ACC$high_acc_select)) 
pd_reject_error = c(sd(pd_ACC$low_acc_reject),sd(pd_ACC$high_acc_reject))

x = c(0,1)
plot(x,plot_data_decide,lwd=2,ylim=c(0.75,1),col="orangered",type="b",ylab="Accuracy",xlab="Aid Accuracy",xaxt="n",main=c("Interaction of Decision Type and","Aid Accuracy on Accuracy")) # a1 
segments(x, plot_data_decide-(pd_decide_error/2), x, plot_data_decide+pd_decide_error/2, lty = 2, lwd = 1.5, col = "orangered")
segments(x - 0.02, plot_data_decide-pd_decide_error/2, x + 0.02, plot_data_decide-pd_decide_error/2, lty = "solid", lwd =1.5,col = "orangered")
segments(x - 0.02, plot_data_decide+pd_decide_error/2, x + 0.02, plot_data_decide+pd_decide_error/2, lty = "solid", lwd =1.5,col = "orangered")
axis(1,at=c(0,1),labels=c("low (80%)","high (95%)"))  
lines(x,plot_data_reject,type="b",lty=1,pch=1,col="grey40",lwd=2)   #a2
segments(x, plot_data_reject-pd_reject_error/2, x, plot_data_reject+pd_reject_error/2, lty = 2, lwd = 1.5, col = "grey40")
segments(x - 0.02, plot_data_reject-pd_reject_error/2, x + 0.02, plot_data_reject-pd_reject_error/2, lty = "solid", lwd =1.5,col = "grey40")
segments(x - 0.02, plot_data_reject+pd_reject_error/2, x + 0.02, plot_data_reject+pd_reject_error/2, lty = "solid", lwd =1.5,col = "grey40")
legend(0,1,legend=c("Select","Agree/Disagree"),lty=1,pch=1,col=c("orangered","grey40"),lwd=2)










