library(lme4)

# ******** read in all 3 participants' data and view **********
pilot1 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_101.txt', header= TRUE)
pilot2 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_103.txt', header= TRUE)
pilot3 = read.table('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/Mahoney_exp_105.txt', header= TRUE)
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

# ****** use first 100 trials for each condition ************
pilot1 = subset(pilot1, pilot1$trial < 101)
pilot2 = subset(pilot2, pilot2$trial < 101)
pilot3 = subset(pilot3, pilot3$trial < 101)

pilot = subset(pilot, pilot$trial < 101)

View(pilot)

# ********** RT analysis ************
# ****** multiple regression ******** 
# run multiple linear regression over reduced # trials with RT as outcome and all factors as predictors
regRT.all = lm(humanRT ~ ., pilot)
summary(regRT.all)

# run multiple regression over reduced # trials with RT as outcome and decide (decide=0, reject = 1), 
# experiment difficulty (easy=0, hard=1) and prob (.95 or .80) as predictors
regRT.big3 = lm(humanRT ~ decide + difficulty + prob, pilot)
summary(regRT.big3) #R^2 = .07

# remove difficulty
regRT.big2 = lm(humanRT ~ decide + prob, pilot)
summary(regRT.big2) #R^2 = .07

# add stimlength as predictor 
regRT.stimlength = lm(humanRT ~ decide + prob + stimlength, pilot)
summary(regRT.stimlength) #R^2 = .08

# run multiple regression with RT as outcome and interaction between decide and prob
regRT.inter = lm(humanRT ~ decide * prob, pilot)
summary(regRT.inter) #R^2 = .08

anova(regRT.big2,regRT.inter)

# hypothesis is that decision condition (moderator) has an effect on how aid accuracy (prob) predicts humanRT
# write out equation
# humanRT = 1.2592 + 1.4648(decide) + -.3017(prob) + -1.3792(decide x prob)
# set decide to zero and get equation for direct decision 
# humanRT = 1.2592 + 1.4648(0) + -.3017(prob) + -1.3792(0 x prob) = 1.2592 - .3017(prob)
# set employ to one and get equation for accept/reject decision
# humanRT = 1.2592 + 1.4648(1) + -.3017(prob) + -1.3792(1 x prob) = 2.724 - 1.6809(prob)

# plot
prob = c(min(pilot$prob),max(pilot$prob))  # create vector of min and max of aid accuracy
decide = 1.2592 - .3017*prob  # line for decide condition
reject = 2.724 - 1.6809*prob
plot(prob, reject, type='l', col = 'purple', ylim=c(.75,1.5), 
     xlab = 'Aid Accuracy', ylab = 'Human Response Time')
lines(prob, decide, type='l', col = 'blue')  # adds another line to the first plot
legend(.9,1.45,c('Accept/Reject','Select'), lty = c(1,1), col = (c('purple','blue')),bty='n')

# run multiple regression with RT as outcome and interaction between decide and stimlength
regRT.inter2 = lm(humanRT ~ decide * stimlength, pilot)
summary(regRT.inter2) #p = .46 --> no interaction

# run multiple regression with RT as outcome and interaction between prob and stimlength
regRT.inter3 = lm(humanRT ~ prob * stimlength, pilot)
summary(regRT.inter3) #p = .19 --> no interaction

# ****** linear mixed-effects *******
# run linear mixed-effects model with RT as outcome and decide (decide=0, reject = 1), 
# experiment difficulty (easy=0, hard=1) and prob (.95 or .80) as predictors
lmerRT.big3 = lmer(humanRT ~ decide + experiment + prob + (1|ID), pilot, REML = FALSE)
summary(lmerRT.big3)

# ********** accuracy analysis ************
# ************* gmler ***********
regACC.all <- glm(ACC ~.,family=binomial(link='logit'),data=pilot)
summary(regACC.all)
regACC.all_a = anova(regACC.all, test="Chisq")

# of possible interest: autoResp, 002223 

regACC.prob = glmer(ACC ~ prob + decide + (1 + prob|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.prob)
anova(regACC.prob, test="Chisq")

regACC.RT = glmer(ACC ~ difficulty + prob + decide + humanRT + (1 + prob + humanRT|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.RT)
anova(regACC.RT, test="Chisq")

regACC.RT2 = glmer(ACC ~ prob + humanRT + (1 + prob + humanRT|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.RT2)
anova(regACC.RT2, test="Chisq")

regACC.inter = glmer(ACC ~ prob * humanRT + (1 + prob + humanRT|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.inter)  #p=.003 --> significant interaction

anova(regACC.RT2, regACC.inter)

regACC.inter2 = glmer(ACC ~ decide * prob + (1 + decide + prob|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.inter2) #p = .02 
anova(regACC.inter, regACC.inter2) #hmmm, what to make of this?

regACC.inter3 = glmer(ACC ~ decide * humanRT + (1 + decide + humanRT|ID),family=binomial(link='logit'),data=pilot)
summary(regACC.inter3) #p = .89 --> no interaction

# ********* signal detection model **************
# use fitglme, with probit link function (matches signal detection Gaussian model) to find c and dprime.
sig.detection = glmer(ACC ~ signal + (1 + signal | ID), family=binomial(link='probit'), data=pilot)
# both intercept and slope vary with subject. each subject has their own intercept and slope
summary(sig.detection)
