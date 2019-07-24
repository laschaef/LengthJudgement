library(lme4)

# ******** read in all 3 participants' data and view **********
pilot1 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_101.txt', header= TRUE)
pilot2 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_103.txt', header= TRUE)
pilot3 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_105.txt', header= TRUE)
View(pilot1)
View(pilot2)
View(pilot3)
pilot1$gender = 'MALE'
pilot2$gender = 'FEMALE'
pilot3$gender = 'FEMALE'
summary(pilot1)  #mean ACC = .8817, mean humanRT = .7681,  mean autoRT = .009147
summary(pilot2)  #mean ACC = .8692, mean humanRT = 1.1741, mean autoRT = .5804
summary(pilot3)  #mean ACC = .885,  mean humanRT = 1.390,  mean autoRT = 1.253

# ******* combine all participants data into 1 file **********
pilot = rbind(pilot1, pilot2, pilot3)
pilot = na.omit(pilot)
View(pilot)

pilot$decide = 0
pilot[pilot$condition=='REJECT','decide'] = 1

pilot$prob = 0.8
pilot[pilot$falseAlarm=='low','prob'] = 0.95

# ******* run linear regression on all data with RT as outcome **********
# run multiple linear regression over all trials with RT as outcome and all factors as predictors
RTreg.all = lm(humanRT ~ ., pilot)
summary(RTreg.all)

# run multiple linear regression over all trials with RT as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
RTreg = lm(humanRT ~ condition + difficulty + falseAlarm, pilot)
summary(RTreg)  # condition and FA are significant predictors, R^2 = .08

# add automation RT as a predictor
RTreg.autoRT = lm(humanRT ~ condition + difficulty + falseAlarm + autoRT, pilot)
summary(RTreg.autoRT)  # condition, FA and autoRT are significant predictors, R^2 = .27

# add stimlength as predictor and remove automation RT
RTreg.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, pilot)
summary(RTreg.stimlength)  # condition, FA aad stimlength are significant predictors, R^2 = .09

# look for interactions
RTreg_X1 = lm(humanRT ~ difficulty * falseAlarm, pilot)
summary(RTreg_X1) # interaction not significant, only FA is significant, R^2 = .02
RTreg_X2 = lm(humanRT ~ falseAlarm * condition, pilot)
summary(RTreg_X2) # condition and interaction are significant, R^2 = .09
RTreg_X3 = lm(humanRT ~ difficulty * condition, pilot)
summary(RTreg_X3) # difficulty, condition and interaction are significant, R^2 = .06
RTreg_X4 = lm(humanRT ~ falseAlarm * condition + difficulty * condition, pilot)
summary(RTreg_X4)  # condtion, difficulty, FA/condition interaction and difficulty/condition interaction are significant, R^2 = .1
RTreg_X5 = lm(humanRT ~ condition + difficulty * autoRT, pilot)
summary(RTreg_X5)

# run with linear mixed-effects model
# need some dummy variables
#condition_num = 
#RT_reg = lmer(humanRT ~ condition + difficulty + falseAlarm + (falseAlarm|ID), pilot)
#summary(logRT_reg)

# run RT for each participant
RTreg1 = lm(humanRT ~ condition + difficulty + falseAlarm, pilot1)
summary(RTreg1) # condition and FA are significant predictors, R^2 = .07
RTreg1.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, pilot1)
summary(RTreg1.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .08

RTreg2 = lm(humanRT ~ condition + difficulty + falseAlarm, pilot2)
summary(RTreg2) # condition, difficulty, and FA are significant predictors, R^2 = .08
RTreg2.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, pilot2)
summary(RTreg2.stimlength) # condition, FA, difficulty and stimlength are significant predictors, R^2 = .13

RTreg3 = lm(humanRT ~ condition + difficulty + falseAlarm, pilot3)
summary(RTreg3) # condition and difficulty are significant predictors, R^2 = .35
RTreg3.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, pilot3)
summary(RTreg3.stimlength) # condition, difficulty and stimlength are significant predictors, R^2 = .36

# ******* run linear regression on all data with ACC as outcome **********
# run multiple linear regression over all trials with RT as outcome and all factors as predictors
ACCreg.all = lm(ACC ~ ., pilot)
summary(ACCreg.all)

# run multiple regression over all trials with accuracy (ACC) as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
ACCreg = lm(ACC ~ condition + difficulty + falseAlarm, pilot)
summary(ACCreg) # condition and FA are significant predictors, R^2 = .04

# add automation RT as a predictor
ACCreg.autoRT = lm(ACC ~ condition + difficulty + falseAlarm + autoRT, pilot)
summary(ACCreg.autoRT) # condition and FA are significant predictors, R^2 = .04

# add stimlength as predictor and remove automation RT
ACCreg.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, pilot)
summary(ACCreg.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .04

# look for interactions
ACCreg_X1 = lm(ACC ~ difficulty * falseAlarm, pilot)
summary(ACCreg_X1) # interaction not significant, only FA is significant, R^2 = .04
ACCreg_X2 = lm(ACC ~ falseAlarm * condition, pilot)
summary(ACCreg_X2) # interaction not significant, only FA is significant, R^2 = .04
ACCreg_X3 = lm(ACC ~ difficulty * condition, pilot)
summary(ACCreg_X3) # difficulty, condition and interaction are significant, R^2 = .004
ACCreg_X4 = lm(ACC ~ falseAlarm + difficulty * condition, pilot)
summary(ACCreg_X4)
ACCreg_X5 = lm(ACC ~ falseAlarm + stimlength + difficulty * condition, pilot)
summary(ACCreg_X5) # FA, stimlength, difficulty, condition and interaction are significant, R^2 = .04

# run mixed effects logistic regression
logACC_reg = glm(ACC ~.,family=binomial(link='logit'),data=pilot)
summary(logACC_reg)
logACC_reg_a = anova(logACC_reg, test="Chisq")

logACC_reg.diff = glm(ACC ~ experiment + decide + prob,family=binomial(link='logit'),data=pilot)
summary(logACC_reg.diff)
anova_logACC_reg.diff = anova(logACC_reg.diff, test="Chisq")

# run ACC for each participant
ACCreg1 = lm(ACC ~ condition + difficulty + falseAlarm, pilot1)
summary(ACCreg1) # FA is significant predictor, R^2 = .03
ACCreg1.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, pilot1)
summary(ACCreg1.stimlength) # FA is significant predictor, R^2 = .03

ACCreg2 = lm(ACC ~ condition + difficulty + falseAlarm, pilot2)
summary(ACCreg2) # condition and FA are significant predictors, R^2 = .05
ACCreg2.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, pilot2)
summary(ACCreg2.stimlength) # condition, FA, and stimlength are significant predictors, R^2 = .06

ACCreg3 = lm(ACC ~ condition + difficulty + falseAlarm, pilot3)
summary(ACCreg3) # FA and difficulty are significant predictors, R^2 = .04
ACCreg3.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, pilot3)
summary(ACCreg3.stimlength) # difficulty and FA are significant predictors, R^2 = .04

# ******* I want to be able to use less trials - do results change if 100 trials ********
# take only first 100 trials for each  
reduced_pilot1 = subset(pilot1, pilot1$trial < 101)
reduced_pilot2 = subset(pilot2, pilot2$trial < 101)
reduced_pilot3 = subset(pilot3, pilot3$trial < 101)

reduced_pilot = subset(pilot, pilot$trial < 101)

reduced_pilot = rbind(reduced_pilot1, reduced_pilot2, reduced_pilot3)
reduced_pilot = na.omit(reduced_pilot)
View(reduced_pilot)

# run multiple linear regression over all trials with RT as outcome and all factors as predictors
RTregR.all = lm(humanRT ~ ., reduced_pilot)
summary(RTregR.all)

# find difference in coefficients found with 150 trials and 100 trials
RT_coef_diff = coef(RTreg.all) - coef(RTregR.all)

# run multiple regression over reduced # trials with RT as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
RTregR = lm(humanRT ~ condition + difficulty + falseAlarm, reduced_pilot)
summary(RTregR)  # condition and FA are significant predictors, R^2 = .07
# look at interactions
RTregR_X1 = lm(humanRT ~ difficulty * falseAlarm, reduced_pilot)
summary(RTregR_X1) # interaction not significant, only FA is significant, R^2 = .02
RTregR_X2 = lm(humanRT ~ falseAlarm * condition, reduced_pilot)
summary(RTregR_X2) # condition and interaction are significant, R^2 = .08
RTregR_X3 = lm(humanRT ~ difficulty * condition, reduced_pilot)
summary(RTregR_X3) # difficulty, condition and interaction are significant, R^2 = .06
RTregR_X4 = lm(humanRT ~ falseAlarm * condition + difficulty * condition, reduced_pilot)
summary(RTregR_X4)  # condtion, difficulty, FA/condition interaction and difficulty/condition interaction are significant, R^2 = .08

# add automation RT as a predictor
RTregR.autoRT = lm(humanRT ~ condition + difficulty + falseAlarm + autoRT, reduced_pilot)
summary(RTregR.autoRT)  # condition, FA and autoRT are significant predictors, R^2 = .27

# add stimlength as predictor and remove automation RT
RTregR.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot)
summary(RTregR.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .08

# run for each participant
RTregR1 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced_pilot1)
summary(RTregR1) # condition and FA are significant predictors, R^2 = .06
RTregR1.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot1)
summary(RTregR1.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .07

RTregR2 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced_pilot2)
summary(RTregR2) # **** //different// **** condition and FA are significant predictors, R^2 = .06
RTregR2.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot2)
summary(RTregR2.stimlength) # **** //different// **** condition, FA, and stimlength are significant predictors, R^2 = .13

RTregR3 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced_pilot3)
summary(RTregR3) # condition and difficulty are significant predictors, R^2 = .32
RTregR3.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot3)
summary(RTregR3.stimlength) # **** //different// **** condition and difficulty are significant predictors, R^2=.32

# run multiple regression over reduced # trials with accuracy (ACC) as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
ACCregR = lm(ACC ~ condition + difficulty + falseAlarm, reduced_pilot)
summary(ACCregR) # **** //different// **** FA is significant predictor, R^2 = .04

# add automation RT as a predictor
ACCregR.autoRT = lm(ACC ~ condition + difficulty + falseAlarm + autoRT, reduced_pilot)
summary(ACCregR.autoRT) # **** //different// **** FA is significant predictor, R^2 = .04
# add stimlength as predictor and remove automation RT
ACCregR.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot)
summary(ACCregR.stimlength) # **** //different// **** FA is significant predictor, R^2 = .04

# look for interactions
ACCregR_X1 = lm(ACC ~ difficulty * falseAlarm, reduced_pilot)
summary(ACCregR_X1) # interaction not significant, only FA is significant, R^2 = .04
ACCregR_X2 = lm(ACC ~ falseAlarm * condition, reduced_pilot)
summary(ACCregR_X2) # interaction not significant, only FA is significant, R^2 = .04
ACCregR_X3 = lm(ACC ~ difficulty * condition, reduced_pilot)
summary(ACCregR_X3) # **** //different// **** condition and interaction are significant, R^2 = .002
ACCregR_X4 = lm(ACC ~ falseAlarm + difficulty * condition, reduced_pilot)
summary(ACCregR_X4) # **** //different// **** FA, condition and interaction are significant, R^2 = .04
ACCregR_X5 = lm(ACC ~ falseAlarm + stimlength + difficulty * condition, reduced_pilot)
summary(ACCregR_X5) # **** //different// **** FA, condition, and interaction are siginificant, R^2 = .04

#run mixed effects logistic regression
logACC_regR <- glm(ACC ~.,family=binomial(link='logit'),data=reduced_pilot)
summary(logACC_regR)
logACC_regR_a = anova(logACC_regR, test="Chisq")

logACC_regR.diff = glm(ACC ~ experiment + prob + decide,family=binomial(link='logit'),data=reduced_pilot)
summary(logACC_regR.diff)
anova_logACC_regR.diff = anova(logACC_regR.diff, test="Chisq")

logACC_regR.RT = glm(ACC ~ experiment + prob + decide + humanRT,family=binomial(link='logit'),data=reduced_pilot)
summary(logACC_regR.RT)
anova_logACC_regR.RT = anova(logACC_regR.RT, test="Chisq")

logACC_regR.int = glm(ACC ~ humanRT * falseAlarm ,family=binomial(link='logit'),data=reduced_pilot)
summary(logACC_regR.int)

# find difference in coefficients found with 150 trials and 100 trials
ACC_coef_diff = coef(logACC_reg) - coef(logACC_regR)

# run ACC for each participant
ACCregR1 = lm(ACC ~ condition + difficulty + falseAlarm, reduced_pilot1)
summary(ACCregR1) # FA is significant predictor, R^2 = .03
ACCregR1.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot1)
summary(ACCregR1.stimlength) # FA is significant predictor, R^2 = .03
reduced_pilot1$ID <- NULL
reduced_pilot1$gender <- NULL
reduced_pilot1$order <- NULL
View(reduced_pilot1)
logACC_regR1 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced_pilot1)
summary(logACC_regR1)
anova(logACC_regR1, test="Chisq")

ACCregR2 = lm(ACC ~ condition + difficulty + falseAlarm, reduced_pilot2)
summary(ACCregR2) # **** //different// **** FA is significant predictor, R^2 = .05
ACCregR2.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot2)
summary(ACCregR2.stimlength) # condition, FA, and stimlength are significant predictors, R^2 = .06
reduced_pilot2$ID <- NULL
reduced_pilot2$gender <- NULL
reduced_pilot2$order <- NULL
View(reduced_pilot2)
logACC_regR2 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced_pilot2)
summary(logACC_regR2)
anova(logACC_regR2, test="Chisq")

ACCregR3 = lm(ACC ~ condition + difficulty + falseAlarm, reduced_pilot3)
summary(ACCregR3) # **** //different// **** FA is significant predictor, R^2 = .04
ACCregR3.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, reduced_pilot3)
summary(ACCregR3.stimlength) # **** //different// **** FA is significant predictor, R^2 = .04
reduced_pilot3$ID <- NULL
reduced_pilot3$gender <- NULL
reduced_pilot3$order <- NULL
View(reduced_pilot3)
logACC_regR3 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced_pilot3)
summary(logACC_regR3)
anova(logACC_regR3, test="Chisq")

# take only last 100 trials for each  
reduced2_pilot1 = subset(pilot1, pilot1$trial > 50)
reduced2_pilot2 = subset(pilot2, pilot2$trial > 50)
reduced2_pilot3 = subset(pilot3, pilot3$trial > 50)

reduced2_pilot = rbind(reduced2_pilot1, reduced2_pilot2, reduced2_pilot3)
reduced2_pilot = na.omit(reduced2_pilot)
View(reduced2_pilot)

# run multiple regression over reduced # trials with RT as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
RTreg2R = lm(humanRT ~ condition + difficulty + falseAlarm, reduced2_pilot)
summary(RTreg2R)  # condition and FA are significant predictors, R^2 = .08
# add automation RT as a predictor
RTreg2R.autoRT = lm(humanRT ~ condition + difficulty + falseAlarm + autoRT, reduced2_pilot)
summary(RTreg2R.autoRT)  # condition, FA and autoRT are significant predictors, R^2 = .27
# add stimlength as predictor and remove automation RT
RTreg2R.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced2_pilot)
summary(RTreg2R.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .08

# run for each participant
RTreg2R1 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced2_pilot1)
summary(RTreg2R1) # condition and FA are significant predictors, R^2 = .07
RTreg2R1.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced2_pilot1)
summary(RTreg2R1.stimlength) # condition, FA and stimlength are significant predictors, R^2 = .08

RTreg2R2 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced2_pilot2)
summary(RTreg2R2) # **** //different// **** condition and FA are significant predictors, R^2 = .07
RTreg2R2.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced2_pilot2)
summary(RTreg2R2.stimlength) # **** //different// **** condition, FA, and stimlength are significant predictors, R^2 = .13

RTreg2R3 = lm(humanRT ~ condition + difficulty + falseAlarm, reduced2_pilot3)
summary(RTreg2R3) # condition and difficulty are significant predictors, R^2 = .34
RTreg2R3.stimlength = lm(humanRT ~ condition + difficulty + falseAlarm + stimlength, reduced2_pilot3)
summary(RTreg2R3.stimlength) # condition, difficulty and stimlength are significant predictors, R^2=.34

# run multiple regression over reduced # trials with accuracy (ACC) as outcome and condition (DECIDE/REJECT), 
# difficulty, (std=0.15 or .30) and false (.95 or .80) as predictors
ACCreg2R = lm(ACC ~ condition + difficulty + falseAlarm, reduced2_pilot)
summary(ACCreg2R) # **** //different// **** FA is significant predictor, R^2 = .03
ACC_X1_reg2R = lm(ACC ~ difficulty * falseAlarm, reduced2_pilot)
summary(ACC_X1_reg2R) # interaction not significant, only FA is significant, R^2 = .04
ACC_X2_reg2R = lm(ACC ~ falseAlarm * condition, reduced2_pilot)
summary(ACC_X2_reg2R) # interaction not significant, only FA is significant, R^2 = .04
ACC_X3_reg2R = lm(ACC ~ difficulty * condition, reduced2_pilot)
summary(ACC_X3_reg2R) # **** //different// **** condition and interaction are significant, R^2 = .002
ACC_X4_reg2R = lm(ACC ~ falseAlarm + difficulty * condition, reduced2_pilot)
summary(ACC_X4_reg2R) # **** //different// **** FA, condition and interaction are significant, R^2 = .04
# add stimlength as predictor 
ACCreg2R.stimlength = lm(ACC ~ condition + difficulty + falseAlarm + stimlength, reduced2_pilot)
summary(ACCreg2R.stimlength) # **** //different// **** FA is significant predictor, R^2 = .04

logACC_reg2R <- glm(ACC ~.,family=binomial(link='logit'),data=reduced2_pilot)
summary(logACC_reg2R)
anova(logACC_reg2R, test="Chisq")


# run ACC for each participant
reduced2_pilot1$ID <- NULL
reduced2_pilot1$gender <- NULL
reduced2_pilot1$order <- NULL
View(reduced2_pilot1)
logACC_reg2R1 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced2_pilot1)
summary(logACC_reg2R1)
anova(logACC_reg2R1, test="Chisq")

reduced2_pilot2$ID <- NULL
reduced2_pilot2$gender <- NULL
reduced2_pilot2$order <- NULL
View(reduced2_pilot2)
logACC_reg2R2 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced2_pilot2)
summary(logACC_reg2R2)
anova(logACC_reg2R2, test="Chisq")

reduced2_pilot3$ID <- NULL
reduced2_pilot3$gender <- NULL
reduced2_pilot3$order <- NULL
View(reduced2_pilot3)
logACC_reg2R3 <- glm(ACC ~.,family=binomial(link='logit'),data=reduced2_pilot3)
summary(logACC_reg2R3)
anova(logACC_reg2R3, test="Chisq")

# something fishy...the last 10 trials seem to be the difference between condition being 
# significant or not
last10_pilot1 = subset(pilot1, pilot1$trial > 140)
last10_pilot2 = subset(pilot2, pilot2$trial > 140)
last10_pilot3 = subset(pilot3, pilot3$trial > 140)

last10_pilot = rbind(last10_pilot1, last10_pilot2, last10_pilot3)
View(last10_pilot)

logACC_regL10R3 <- glm(ACC ~.,family=binomial(link='logit'),data=last10_pilot)
summary(logACC_regL10R3)
anova(logACC_regL10R3, test="Chisq")


#*********for David's class****************************************************************

# use DECIDE subset, easy and low FA
decide_pilot = subset(pilot, pilot$condition == 'DECIDE')
decide_pilot1 = subset(pilot1, pilot1$condition == 'DECIDE')
decide_pilot2 = subset(pilot2, pilot2$condition == 'DECIDE')
decide_pilot3 = subset(pilot3, pilot3$condition == 'DECIDE')

# decide_pilot = subset(decide_pilot, decide_pilot$difficulty == 'easy')
# decide_pilot = subset(decide_pilot, decide_pilot$falseAlarm == 'low')


decide_pilot = na.omit(decide_pilot)
View(decide_pilot)
ACCreg.decide.RT = lm(ACC ~ humanRT * stimlength, decide_pilot)
summary(ACCreg.decide.RT)

RTreg.decide.ACC = lm(ACC ~ humanRT * FA, decide_pilot)
summary(RTreg.decide.ACC)

reject_pilot = subset(pilot, pilot$condition == 'REJECT')
reject_pilot1 = subset(pilot1, pilot1$condition == 'REJECT')
reject_pilot2 = subset(pilot2, pilot2$condition == 'REJECT')
reject_pilot3 = subset(pilot3, pilot3$condition == 'REJECT')

reject_pilot = na.omit(reject_pilot)
View(reject_pilot)

# check for mediation
#1 establish FA->ACC relationship
decide_pilot$FA = .8
decide_pilot[decide_pilot$falseAlarm == 'low', 'FA'] = .95
reject_pilot$FA = .8
reject_pilot[reject_pilot$falseAlarm == 'low', 'FA'] = .95
pilot$FA = .8
pilot[pilot$falseAlarm == 'low', 'FA'] = .95
reg1 = lm(ACC ~ FA, pilot)
summary(reg1)



#2 establish path a FA->humanRT
reg2 = lm(humanRT ~ FA, pilot)
summary(reg2)


#establish path b and c' (need to control for other variable)
reg3 = lm(ACC ~ humanRT + FA, pilot)
summary(reg3)


# *** moderation ***  The slope of lines are opposite of what I'd expected (neg instead of pos) and the 
# intercept is also the opposite (high instead of low)
pilot23 = rbind(pilot2, pilot3)
reg10 = lm(ACC ~ humanRT + autoRT, pilot23)
summary(reg10)

reg11 = lm(ACC ~ humanRT * autoRT, pilot23)
summary(reg11)  # product term explains an additional 5% of the variance and is statistically significant

anova(reg10,reg11)

# write out the equation
# ACC = 1.21 + -.30(humanRT) + -.23(autoRT) + .22(humanRT x autoRT)
# ACC = 1.21 + -.23(autoRT) + (-.30 + .22(autoRT))(humanRT)

# ////// lower autoRT (autoRT = -0.336471) -> ACC = 1.29 - .38(humanRT)
# ////// higher autoRT (autoRT = 0.336471) -> ACC = 1.13 - .23(humanRT)

humanRT23 = c(min(pilot23$humanRT),max(pilot23$humanRT))  # create vector of min and max of human RT
low_autoRT23 = .29 + .38*humanRT23 # line for low autoRT
high_autoRT23 = .13 + .23*humanRT23
plot(humanRT23, low_autoRT23, type='l', col = 'orange', ylim=c(min(pilot23$ACC),max(pilot23$ACC)),
     xlab = 'Human Response Time', ylab = 'Accuracy')
lines(humanRT23, high_autoRT23, type='l', col = 'blue') 
legend(3,.4,c('Low Auto RT','High Auto RT'), lty = c(1,1), col = (c('orange','blue')),bty='n')




#for all 3 participants
# write out the equation
# ACC = .99 + -.14(humanRT) + -.03(autoRT) + .08(humanRT x autoRT)
# ACC = .99 + -.03(autoRT) + (-.14 + .08(autoRT))(humanRT)

# ////// lower autoRT (autoRT = -0.5085306) -> ACC = 1.005256 + .18(humanRT)
# ////// higher autoRT (autoRT = 0.5085306) -> ACC = 0.9747441 - .10(humanRT)

humanRT = c(min(pilot$humanRT),max(pilot$humanRT))  # create vector of min and max of human RT
low_autoRT = 1.00 - .18*humanRT # line for low autoRT
high_autoRT = 0.97 - .10*humanRT
plot(humanRT, low_autoRT, type='l', col = 'orange', ylim=c(min(pilot$ACC),max(pilot$ACC)),
     xlab = 'Human Response Time', ylab = 'Accuracy')
lines(humanRT, high_autoRT, type='l', col = 'blue') 
legend(0,.4,c('Low Auto RT','High Auto RT'), lty = c(1,1), col = (c('orange','blue')),bty='n')



