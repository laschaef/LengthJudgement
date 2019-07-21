# RBF-222 (repeated measures) --> nned to replace calculated value - ones in here are from modified dataset

####################### IVs

### A - decision type 
### a1 = direct decision (level 4 on Parasuraman, Sheridan, and Wickens model)
### a2 = agree/disagree (level 5)

### B - aid accuracy
### b1 = low (0.80) 
### b2 = high (0.95)

### C - difficulty
### c1 = easy (SD of signal = 0.15)
### c2 = hard (SD of signal = 0.3)


### Mixed-effects model (model III)

####################### DV

### median response time (correct responses only)
### mean accuracy (all responses)

####################### Hypotheses 

### sigma_pi_square = 0 
### mu.1.. = mu.2..
### mu..1. = mu..2. 
### mu...1 = mu...2
### alpha*beta_jkl = 0  

######################## exp units: subjects or blocks (each block is composed of one subject)  

# N = 48*8 # --> this isn't actually used

### n = number of subjects - repeated measures

# n = 48  # both
# n = 14  # short RT
n = 30   # long RT

### p = number of levels of treatment A
### q = number of levels of treatment B
### r = number of levels of treatment C

p = 2
q = 2
r = 2

#########################################################
### read  in data (has both correct and incorrect responses, conditions 3-10, removed 0 RT and RT>4.16sec)
#########################################################
# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/2-Spring2019/PSY7020-ANOVA/Data_for_7020_analysis.csv')
# dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/2-Spring2019/PSY7020-ANOVA/Data_for_7020_analysis_shortRT.csv')
dat = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/2-Spring2019/PSY7020-ANOVA/Data_for_7020_analysis_longRT.csv')


dat$X = NULL

#########################################################
### data - humanRT
#########################################################
# only use correct responses (ACC = 1) for RT
# dataset with only correct responses 
row_sub = apply(dat[12], 1, function(row) all(row ==1 ))  # find rows with correct responses
correctdat = dat[row_sub,]  # only use rows with correct responses
# incorrectdat = dat[!row_sub,]  # 6717

### descriptive stats - across all correct trials

                                                      # short RT subjects
with(correctdat,tapply(humanRT,A,median))             # 0.6458721 0.4570148  
with(correctdat,tapply(humanRT,B,median))             # 0.6129315 0.5543301 
with(correctdat,tapply(humanRT,C,median))             # 0.5865741 0.5865281
with(correctdat,tapply(humanRT,condition,median))     #         3         4         5         6         7         8         9        10 
                                                      # 0.6555685 0.6865424 0.6248266 0.6222974 0.3163201 0.3166401 0.5984640 0.5752790 

###### median response time by subject and condition
ID_cond_median <- aggregate(. ~ ID + condition, data = correctdat, FUN = median)  # find median for each participant by condition
View(ID_cond_median)

###### median response time by subject (for all conditions)
ID_median <- aggregate(. ~ ID, data = correctdat, FUN = median)  # find median for each participant by condition
View(ID_median)  # missing(even-1) extra(even-2)

### summary tables 
# with(correctdat,tapply(humanRT,list(ID,A,B,C),mean))
with(correctdat,tapply(humanRT,list(ID,A,B,C),median))


## Insert means into data frame

Y_bar.jkl = tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$B,ID_cond_median$C),mean) 

ID_cond_median$Y_bar.jkl = c(rep(Y_bar.jkl[1,2,1],n),rep(Y_bar.jkl[1,2,2],n),rep(Y_bar.jkl[1,1,1],n),
                             rep(Y_bar.jkl[1,1,2],n),rep(Y_bar.jkl[2,2,1],n),rep(Y_bar.jkl[2,2,2],n),
                             rep(Y_bar.jkl[2,1,1],n),rep(Y_bar.jkl[2,1,2],n))  # need to match actual order of A, B, C


########### test for sphericity - only 2 levels of p so not applicable
# Sphericity refers to the condition where the variances of the differences between all 
# possible pairs of within-subject conditions (i.e., levels of the independent variable) are equal. 
# The violation of sphericity occurs when it is not the case that the variances of the differences 
# between all combinations of the conditions are equal. 

########### compute sum of squares 

### computational symbols 

Y = sum(ID_cond_median$humanRT)^2/(n*p*q*r)
ABCS = sum(ID_cond_median$humanRT^2)
S = sum(with(ID_cond_median,tapply(humanRT,list(ID),sum))^2/(p*q*r)) 
A = sum(tapply(ID_cond_median$humanRT,ID_cond_median$A,sum)^2/(n*q*r))
B = sum(tapply(ID_cond_median$humanRT,ID_cond_median$B,sum)^2/(n*p*r))
C = sum(tapply(ID_cond_median$humanRT,ID_cond_median$C,sum)^2/(n*p*q))
AB = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$B),sum)^2/(n*r))
AC = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$C),sum)^2/(n*q))
BC = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$B,ID_cond_median$C),sum)^2/(n*p))
# AS = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$ID),sum)^2/(q*r))
# BS = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$B,ID_cond_median$ID),sum)^2/(p*r))
ABC = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$B,ID_cond_median$C),sum)^2/n)
# ABS = sum(tapply(ID_cond_median$humanRT,list(ID_cond_median$A,ID_cond_median$B,ID_cond_median$ID),sum)^2/r)


### sum of squares 

SSTO = ABCS - Y
SSBL = S - Y
SSTREAT = ABC - Y
SSA = A - Y
SSB = B - Y
SSC = C - Y
SSAXB = AB - A - B + Y
SSAXC = AC - A - C + Y
SSBXC = BC - B - C + Y
SSAXBXC = ABC - AB - AC - BC + A + B + C - Y
SSRES = ABCS - ABC - S + Y  
# SSAXBL = AS - A - S + Y
# SSAXBXBL = ABS - AB - AS - BS + A + B + S - Y 

## residuals

ID_cond_median$z_ijk = (ID_cond_median$humanRT - ID_cond_median$Y_bar.jkl) / sqrt(SSRES /(n*p*q*r-1))

############## histograms to inspect normality, outliers, and homogeneity of variance  

par(mfrow=c(2,4)) 
with(ID_cond_median,tapply(z_ijk,list(A,B,C),hist,xlim=c(-6,6),ylim=c(0,20)))  # most look normal, a couple look skewed
with(ID_cond_median,tapply(z_ijk,list(A,B,C),boxplot))  # a couple outliers
with(ID_cond_median,tapply(humanRT,list(A,B,C),boxplot))  # a couple outliers

############# plot against order of observation to inspect independence 

par(mfrow=c(2,4)) 
with(ID_cond_median,tapply(z_ijk,list(A,B,C),plot,ylim=c(-5,5)))   # one maybe has a trend


########### non-additivity (between treatment and subject) test (Tukey) --> Pg 299 --> not applicable b/c no error term
# --> even if I could, the treatments are a combined 

### treatment A
Y_bar = mean(with(ID_cond_median,tapply(humanRT,list(A),mean)))  # mean for all levels of A for all subjects
Y_ij = with(ID_cond_median,tapply(humanRT,list(ID,A),mean)) # mean by subject for each A (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of A
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_median,tapply(humanRT,list(A),mean))  # mean by level of A for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 1.87
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 - treatment A additive (no interaction with blocks)
pf(F_NONADD,nu1,nu2,lower.tail = FALSE) # 0.18

# plot epsilon_hat_ij and Y_hat_ij for treatment A
Y_bar_j_array = rep(Y_bar_j,each=n)
dim(Y_bar_j_array)=c(n,p)

Y_bar_i_array = cbind(Y_bar_i,Y_bar_i)

epsilon_hat_ij = Y_ij - Y_bar_j_array - Y_bar_i_array + Y_bar  

alpha_j = Y_bar_j - Y_bar
pi_i = Y_bar_i - Y_bar

Y_hat_ij = Y_bar + (Y_bar_j_array - Y_bar) + (Y_bar_i_array - Y_bar)  

Y_ij == Y_hat_ij + epsilon_hat_ij  # double check

par(mfrow=c(1,1))
plot(c(Y_hat_ij[,1],Y_hat_ij[,2]),c(epsilon_hat_ij[,1],epsilon_hat_ij[,2]))  # not an obvious visual trend


### treatment B
Y_bar = mean(with(ID_cond_median,tapply(humanRT,list(B),mean)))  # mean for all levels of B for all subjects
Y_ij = with(ID_cond_median,tapply(humanRT,list(ID,B),mean)) # mean by subject for each B (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of B
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_median,tapply(humanRT,list(B),mean))  # mean by level of B for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 4e-5
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 of non-additivity for treatment B


### treatment C
Y_bar = mean(with(ID_cond_median,tapply(humanRT,list(C),mean)))  # mean for all levels of C for all subjects
Y_ij = with(ID_cond_median,tapply(humanRT,list(ID,C),mean)) # mean by subject for each C (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of C
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_median,tapply(humanRT,list(C),mean))  # mean by level of C for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 0.25
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 of non-additivity for treatment C




#########################################################
### Confirmatory data analysis 
#########################################################

MSBL = SSBL / (n-1)
MSTREAT = SSTREAT / (p*q*r - 1)
MSA = SSA / (p-1)
MSB = SSB / (q-1)
MSC = SSC / (r-1)
MSAXB = SSAXB / ((p-1)*(q-1))
MSAXC = SSAXC / ((p-1)*(r-1))
MSBXC = SSBXC / ((q-1)*(r-1))
MSAXBXC = SSAXBXC / ((p-1)*(q-1)*(r-1))
MSRES = SSRES / ((n-1)*(p*q*r-1))


### F tests 

F_BL = MSBL / MSRES     # 4.31, df = 27,189   --> sig.
                        # 4.69, df = 13, 91   --> sig.
F_A = MSA / MSRES       # 1.35, df = 1,189    --> not sig. (different than with entire dataset)
                        # 54.6, df = 1, 91    --> sig.
F_B = MSB / MSRES       # 17.7, df = 1,189    --> sig. 
                        # 15.9, df = 1, 91    --> sig.
F_C = MSC / MSRES       # 1,62, df = 1,189    --> not sig.
                        # 0.26, df = 1, 91    --> not sig.
F_AXB = MSAXB / MSRES   # 17.3, df = 1,189    --> sig.
                        # 20.7, df = 1, 91    --> sig.
F_AXC = MSAXC / MSRES   # 0.02, df = 1,189  --> not sig.
                        # 0.58, df = 1, 91   --> not sig.
F_BXC = MSBXC / MSRES   # 0.02, df = 1,189   --> not sig.
                        # 0.03, df = 1, 91   --> not sig.
F_AXBXC = MSAXBXC / MSRES # 0.05, df = 1,189 --> not sig.
                          # 0.003, df = 1, 91  --> not sig.

qf(.05,1,91,lower.tail = FALSE)   # 3.95
qf(.05,13,91,lower.tail = FALSE)  # 1.82

# qf(0.05,1,189,lower.tail = FALSE)   # 3.89
# qf(0.05,27,329,lower.tail = FALSE)  # 1.52
# qf(0.05,1,47,lower.tail = FALSE)    # 4.05

pf(F_A,1,329,lower.tail = FALSE)  # 
pf(F_B,1,329,lower.tail = FALSE)  # <0.001
pf(F_AXB,1,329,lower.tail = FALSE)  # <0.001

### effect size - eta_squared

eta_square_A = SSA / SSTO    # 0.018
                             # 0.22
eta_sqaure_B = SSB / SSTO    # 0.05
                             # 0.07
eta_square_AB = SSAXB / SSTO # 0.05
                             # 0.08

### double check 
with(ID_cond_median, summary(aov(humanRT ~ factor(A) * factor(B) * factor(C) + factor(ID))))


#########################################################
### graph significant interaction (AxB) 
#########################################################

# A x B
# plot_data_AB = with(correctdat,tapply(humanRT,list(A,B),median))*1000
plot_data_AB = with(ID_cond_median,tapply(humanRT,list(A,B),mean))*1000

par(mfrow=c(1,1))
plot(plot_data_AB[1,],lwd=2,ylim=c(600,900),col="black",type="b",ylab="Median Response Time (sec)",xlab="Aid Accuracy",xaxt="n",main="Interaction of Decision Type and 
Aid Accuracy on Human Response Time") # a1 
#Median response time as a function of decision type and aid accuracy")  
axis(1,at=c(1,2),labels=c("low (80%)","high (95%)")) 
lines(plot_data_AB[2,],type="b",lty=2,pch=2,col=1,lwd=2)   #a2
legend(1.75,900,legend=c("Level 4","Level 5"),lty=1:2,pch=1:2,col=1,lwd=2) 

plot(plot_data_AB[,1],lwd=3,ylim=c(400,1000),type="b",ylab="Median Response Time (sec)",xlab="Decision Type",xaxt="n",main="Median response time as a function
     of decision type and aid accuracy")  # b1
axis(1,at=c(1,2),labels=c("level 4","level 5"))
lines(plot_data_AB[,2],type="b",lty=2,pch=2,col=1,lwd=3)   #b2
legend(1,500,legend=c("low (80%)","high (95%)"),lty=1:2,pch=1:2,col=1,lwd=3)


#########################################################
### compute omega_hat_squared, effect size and power for significant factors (A, B, AXB)
#########################################################

################### measures of effect magnitude: strength of association and effect size 

########## for a mixed model in which the levels of treatment are fixed and those of the blocks are random  
############ Partial Omega square and partial intraclass correlation - for treatments with significant effects

# Omega_hat_square_A_vs_B_C_inter_BL = (p-1)/(n*p*q*r) * (MSA - MSRES) / (MSRES + (p-1)/(n*p*q*r) * (MSA - MSRES))  # 0.024 --> small
Omega_hat_square_B_vs_A_C_inter_BL = (q-1)/(n*p*q*r) * (MSB - MSRES) / (MSRES + (q-1)/(n*p*q*r) * (MSB - MSRES))  # 0.07 --> medium
Omega_hat_square_AxB_vs_A_B_C_inter_BL = ((p-1)*(q-1)/(n*p*q*r) * (MSAXB - MSRES)) / (MSRES + (p-1)*(q-1)/(n*p*q*r) * (MSAXB - MSRES))  # 0.07 --> medium

######### Compute roe_hat_Y|BL.A,B,C,AxB,AxC,BxC,AxBxC (intraclass correlation)

sigma_hat_squared_epsilon = MSRES

sigma_hat_squared_pi = (1/(p*q*r))*(MSBL - MSRES)

ro_I_hat = sigma_hat_squared_pi / (sigma_hat_squared_epsilon + sigma_hat_squared_pi)  # 0.29

ro_I_hat = 1/(p*q*r) * (MSBL - MSRES) / (MSRES + 1/(p*q*r) * (MSBL - MSRES))  # 0.29

# This is a medium correlation so the blocking variable was (somewhat) effective.

########## Cohen's f

# f_hat_A = sqrt(Omega_hat_square_A_vs_B_C_inter_BL / (1 - Omega_hat_square_A_vs_B_C_inter_BL))  # 0.158 --> small
f_hat_B = sqrt(Omega_hat_square_B_vs_A_C_inter_BL / (1 - Omega_hat_square_B_vs_A_C_inter_BL))  # 0.27 --> medium
f_hat_AxB = sqrt(Omega_hat_square_AxB_vs_A_B_C_inter_BL / (1 - Omega_hat_square_AxB_vs_A_B_C_inter_BL))  # 0.27 --> medium

################################ Power

nu1_A = p-1            # 1
nu1_B = q-1            # 1
nu1_AXB = (p-1)*(q-1)  # 1
nu2 = (n-1)*(p*q*r-1)  # 189

# Phi_hat_A = sqrt(((p-1) / (n*p*q*r) * (MSA - MSRES)) / (MSRES/(n*q*r)))   # 1.68
Phi_hat_B = sqrt(((q-1) / (n*p*q*r) * (MSB - MSRES)) / (MSRES/(n*p*r)))   # 2.89
Phi_hat_AB = sqrt(((p-1)*(q-1) / (n*((p-1)*(q-1)*(r-1)+1)) * (MSAXB - MSRES)) / (MSRES/n))  # 2.85

### use Tang's chart (Table E.12) to determine power  
# Power_A = .67 --> old/wrong
# Power_B = .982
# Power_AB = .98

# double check
# lambda_hat_A = ((p-1) / (n*p*q*r) * (MSA - MSRES)) / (MSRES/(n*p*q*r))  # 5.63 -> does not match value calculated by G*Power
lambda_hat_B = ((q-1) / (n*p*q*r) * (MSB - MSRES)) / (MSRES/(n*p*q*r))  # 16.7 -> does not match value calculated by G*Power

# Phi_hat_A = sqrt(lambda_hat_A / p)   # 1.68
Phi_hat_B = sqrt(lambda_hat_B / q)   # 2.89

### from G*Power --> not right
### Power_A = 0.99  (384 samples, 48 groups, 2 measurements)
### Power_B = 1.0  (384 samples, 48 groups, 2 measurements)
### Power_AB = 1.0  (384 samples, 48 groups, 2 measurements)

################################ Estimate no of blocks to achive desired power (0.8)

# for B
n = 28  # from study
n_prime = 14

Phi_hat_B = sqrt(n_prime) * sqrt((q-1)/(n*q) * (MSB - MSRES) / MSRES)  # 2.04

# for AB --> need to do this
n = 28  # from study
n_prime = 14

Phi_hat_AB = sqrt(n_prime) * sqrt(((p-1)*(q-1))/(n*q*p) * (MSAXB - MSRES) / MSRES)  # 



















#########################################################
### data - accuracy
#########################################################

### summary tables 

with(dat,tapply(ACC,list(ID,A,B,C),mean)) 

### descriptive stats 

with(dat,tapply(ACC,list(A),mean))             # 0.8783997 0.8293210
with(dat,tapply(ACC,list(A),sd))               # 0.3268307 0.3762365
with(dat,tapply(ACC,list(B),mean))             # 0.8090821 0.8988032
with(dat,tapply(ACC,list(B),sd))               # 0.3930331 0.3015957
with(dat,tapply(ACC,list(C),mean))             # 0.8552930 0.8525288 
with(dat,tapply(ACC,list(C),sd))               # 0.3518129 0.3545828


########### compute sum of squares 
###### mean accuracy time by subject and condition
ID_cond_mean <- aggregate(. ~ ID + condition, data = dat, FUN = mean)  # find mean for each participant by condition
View(ID_cond_mean)

###### mean accuracy by subject (for all conditions)
ID_mean <- aggregate(. ~ ID, data = dat, FUN = mean)  # find mean for each participant by condition
View(ID_mean)  # missing(even-1) extra(even-2)

### summary tables 
with(dat,tapply(ACC,list(ID,A,B,C),mean))

## Insert means into data frame

Y_bar.jkl = tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$B,ID_cond_mean$C),mean)

ID_cond_mean$Y_bar.jkl = c(rep(Y_bar.jkl[1,2,1],n),rep(Y_bar.jkl[1,2,2],n),rep(Y_bar.jkl[1,1,1],n),
                           rep(Y_bar.jkl[1,1,2],n),rep(Y_bar.jkl[2,2,1],n),rep(Y_bar.jkl[2,2,2],n),
                           rep(Y_bar.jkl[2,1,1],n),rep(Y_bar.jkl[2,1,2],n))  # need to match actual order of A, B, C


### computational symbols 

Y = sum(ID_cond_mean$ACC)^2/(n*p*q*r)
ABCS = sum(ID_cond_mean$ACC^2)
S = sum(with(ID_cond_mean,tapply(ACC,list(ID),sum))^2/(p*q*r)) 
A = sum(tapply(ID_cond_mean$ACC,ID_cond_mean$A,sum)^2/(n*q*r))
B = sum(tapply(ID_cond_mean$ACC,ID_cond_mean$B,sum)^2/(n*p*r))
C = sum(tapply(ID_cond_mean$ACC,ID_cond_mean$C,sum)^2/(n*p*q))
AB = sum(tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$B),sum)^2/(n*r))
AC = sum(tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$C),sum)^2/(n*q))
BC = sum(tapply(ID_cond_mean$ACC,list(ID_cond_mean$B,ID_cond_mean$C),sum)^2/(n*p))
ABC = sum(tapply(ID_cond_mean$ACC,list(ID_cond_mean$A,ID_cond_mean$B,ID_cond_mean$C),sum)^2/n)

### sum of squares 

SSTO = ABCS - Y
SSBL = S - Y
SSTREAT = ABC - Y
SSA = A - Y
SSB = B - Y
SSC = C - Y
SSAXB = AB - A - B + Y
SSAXC = AC - A - C + Y
SSBXC = BC - B - C + Y
SSAXBXC = ABC - AB - AC - BC + A + B + C - Y
SSRES = ABCS - ABC - S + Y  

## residuals

ID_cond_mean$z_ijk = (ID_cond_mean$ACC - ID_cond_mean$Y_bar.jkl) / sqrt(SSRES /(n*p*q*r-1))

############## histograms to inspect normality, outliers, and homogenerity of variance  

par(mfrow=c(2,4)) 
with(ID_cond_mean,tapply(z_ijk,list(A,B,C),hist,xlim=c(-6,6),ylim=c(0,20)))  # half look normal, half look skewed
with(ID_cond_mean,tapply(z_ijk,list(A,B,C),boxplot))  # a couple outliers
with(ID_cond_mean,tapply(ACC,list(A,B,C),boxplot))  # a couple outliers

############# plot against order of observation to inspect independence 

par(mfrow=c(2,4)) 
with(ID_cond_mean,tapply(z_ijk,list(A,B,C),plot,ylim=c(-5,5)))   # one maybe has a trend

########### non-additivity (between treatment and subject) test (Tukey)

### treatment A
Y_bar = mean(with(ID_cond_mean,tapply(ACC,list(A),mean)))  # mean for all levels of A for all subjects
Y_ij = with(ID_cond_mean,tapply(ACC,list(ID,A),mean)) # mean by subject for each A (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of A
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_mean,tapply(ACC,list(A),mean))  # mean by level of A for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 0.17
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 of non-additivity for treatment A


### treatment B
Y_bar = mean(with(ID_cond_mean,tapply(ACC,list(B),mean)))  # mean for all levels of B for all subjects
Y_ij = with(ID_cond_mean,tapply(ACC,list(ID,B),mean)) # mean by subject for each B (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of B
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_mean,tapply(ACC,list(B),mean))  # mean by level of B for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 0.005
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 of non-additivity for treatment B


### treatment C
Y_bar = mean(with(ID_cond_mean,tapply(ACC,list(C),mean)))  # mean for all levels of C for all subjects
Y_ij = with(ID_cond_mean,tapply(ACC,list(ID,C),mean)) # mean by subject for each C (across other treatments) 
Y_bar_i = (Y_ij[,1] + Y_ij[,2]) / 2  # mean by subject for all levels of C
d_i = Y_bar_i - Y_bar
Y_bar_j = with(ID_cond_mean,tapply(ACC,list(C),mean))  # mean by level of C for all subjects
d_j = Y_bar_j - Y_bar
d_ij = d_i %*% t(d_j)

SSNONADD = sum(d_ij*Y_ij)^2 / (sum(d_i^2)*sum(d_j^2))
SSREM = SSRES - SSNONADD

nu1 = 1
nu2 = (n-1)*(p-1) - 1 

F_NONADD = (SSNONADD/nu1) / (SSREM/nu2)  # 2e-6
alpha = 0.10
qf(alpha,nu1,nu2,lower.tail = FALSE) # 2.91 --> accept H0 of non-additivity for treatment C

#########################################################
### Confirmatory data analysis 
#########################################################

MSBL = SSBL / (n-1)
MSTREAT = SSTREAT / (p*q*r - 1)
MSA = SSA / (p-1)
MSB = SSB / (q-1)
MSC = SSC / (r-1)
MSAXB = SSAXB / ((p-1)*(q-1))
MSAXC = SSAXC / ((p-1)*(r-1))
MSBXC = SSBXC / ((q-1)*(r-1))
MSAXBXC = SSAXBXC / ((p-1)*(q-1)*(r-1))
MSRES = SSRES / ((n-1)*(p*q*r-1))  

### F tests 

F_BL = MSBL / MSRES     # 3.29, df = 27,189   --> sig.
F_A = MSA / MSRES       # 1.80, df = 1,189   --> not sig.
F_B = MSB / MSRES       # 76.7, df = 1,189    --> sig. 
F_C = MSC / MSRES       # 18.3, df = 1,189    --> sig.
F_AXB = MSAXB / MSRES   # 5.93, df = 1,189    --> sig.
F_AXC = MSAXC / MSRES   # .67, df = 1,189  --> not sig.
F_BXC = MSBXC / MSRES   # .10, df = 1,189   --> not sig.
F_AXBXC = MSAXBXC / MSRES # .116, df = 1,189 --> not sig.

qf(0.05,1,189,lower.tail = FALSE)   # 3.89
qf(0.05,27,189,lower.tail = FALSE)  # 1.54

### double check 
with(ID_cond_mean, summary(aov(ACC ~ factor(A) * factor(B) * factor(C) + factor(ID))))

### effect size - eta_squared

eta_square_A = SSA / SSTO
eta_sqaure_B = SSB / SSTO     # .20
eta_sqaure_C = SSC / SSTO     # .05
eta_square_AB = SSAXB / SSTO  # .016

######################################################### 
### graph significant interaction (AxB) 
#########################################################

# A x B
# plot_data_AB = with(dat,tapply(ACC,list(A,B),mean))
plot_data_AB = with(ID_cond_mean,tapply(ACC,list(A,B),mean))


par(mfrow=c(1,1))
plot(plot_data_AB[1,],lwd=2,ylim=c(0.75,.95),col=1,type="b",ylab="Accuracy",xlab="Aid Accuracy",xaxt="n",main="Interaction of Decision Type and
     Aid Accuracy on Accuracy")  # a1 
axis(1,at=c(1,2),labels=c("low (80%)","high (95%)")) 
lines(plot_data_AB[2,],type="b",lty=2,pch=2,lwd=2,col=1)   #a2
legend(1,.95,legend=c("Level 4","Level 5"),lty=1:2,pch=1:2,col=1,lwd=2) 

plot(plot_data_AB[,1],lwd=2,ylim=c(.7,1),type="b",ylab="Accuracy",xlab="Decision Type",xaxt="n",main="Accuracy as a function
     of decision type and aid accuracy")  # b1
axis(1,at=c(1,2),labels=c("level 4","level 5"))
lines(plot_data_AB[,2],type="b",lty=2,pch=2,col=1,lwd=2)   #b2
legend(1,1,legend=c("low (80%)","high (95%)"),lty=1:2,pch=1:2,col=1,lwd=2)

######################################################### 
### compute omega_hat_squared, effect size and power - for significant factors
#########################################################

################### measures of effect magnitude: strength of association and effect size 

########## for a mixed model in which the levels of treatment are fixed and those of the blocks are random  
############ Partial Omega square and partial intraclass correlation - for treatments with significant effects

Omega_hat_square_B_vs_A_C_inter_BL = (q-1)/(n*p*q*r) * (MSB - MSRES) / (MSRES + (q-1)/(n*p*q*r) * (MSB - MSRES))  # .25 --> large
Omega_hat_square_C_vs_A_B_inter_BL = (r-1)/(n*p*q*r) * (MSC - MSRES) / (MSRES + (r-1)/(n*p*q*r) * (MSC - MSRES))  # .07 --> medium
Omega_hat_square_AxB_vs_A_B_C_inter_BL = ((p-1)*(q-1)/(n*p*q*r) * (MSAXB - MSRES)) / (MSRES + (p-1)*(q-1)/(n*p*q*r) * (MSAXB - MSRES))  # .02 --> medium

######### Compute roe_hat_Y|BL.A,B,C,AxB,AxC,BxC,AxBxC (intraclass correlation)

sigma_hat_squared_epsilon = MSRES

sigma_hat_squared_pi = (1/(p*q*r))*(MSBL - MSRES)

ro_I_hat = sigma_hat_squared_pi / (sigma_hat_squared_epsilon + sigma_hat_squared_pi)  # 0.22

# This is a medium correlation so the blocking variable was effective (maybe?).

########## Cohen's f

f_hat_B = sqrt(Omega_hat_square_B_vs_A_C_inter_BL / (1 - Omega_hat_square_B_vs_A_C_inter_BL))  # 0.58 --> large
f_hat_C = sqrt(Omega_hat_square_C_vs_A_B_inter_BL / (1 - Omega_hat_square_C_vs_A_B_inter_BL))  # 0.28 --> medium
f_hat_AxB = sqrt(Omega_hat_square_AxB_vs_A_B_C_inter_BL / (1 - Omega_hat_square_AxB_vs_A_B_C_inter_BL))  # 0.15 --> medium

################################ Power 

nu1_A = p-1            # 1
nu1_B = q-1            # 1
nu1_AXB = (p-1)*(q-1)  # 1
nu2 = (n-1)*(p*q-1)  # 81?

Phi_hat_B = sqrt(((q-1) / (n*p*q*r) * (MSB - MSRES)) / (MSRES/(n*p*r)))   # 6.15
Phi_hat_C = sqrt(((r-1) / (n*p*q*r) * (MSC - MSRES)) / (MSRES/(n*p*q)))   # 2.94
Phi_hat_AB = sqrt(((p-1)*(q-1) / (n*((p-1)*(q-1)+1)) * (MSAXB - MSRES)) / (MSRES/n))  # 1.57 --> think this is wrong

### use Tang's chart (Table E.12) to determine power  
# Power_B = 
# Power_C = 
# Power_AB = 

# double check
lambda_hat_B = ((q-1) / (n*p*q*r) * (MSB - MSRES)) / (MSRES/(n*p*q*r))  # 75.7 -> does not match value calculated by G*Power
lambda_hat_C = ((r-1) / (n*p*q*r) * (MSC - MSRES)) / (MSRES/(n*p*q*r))  # 17.3 -> does not match value calculated by G*Power

Phi_hat_B = sqrt(lambda_hat_B / q)   # 6.15 
Phi_hat_C = sqrt(lambda_hat_C / r)   # 2.94 


### from G*Power --> not right
### Power_A = 0.99  (384 samples, 48 groups, 2 measurements)
### Power_B = 1.0  (384 samples, 48 groups, 2 measurements)
### Power_AB = 1.0  (384 samples, 48 groups, 2 measurements)

################################ Estimate no of blocks to achive desired power (0.8)

n = 28  # from study
n_prime = 14

Phi_hat_B = sqrt(n_prime) * sqrt((q-1)/(n*q) * (MSB - MSRES) / MSRES)  # 2.04





nu1_A = p-1
nu1_B = q-1
nu1_AXB = (p-1)*(q-1)
nu2 = (n-1)*(p*q*r-1)  

### from G*Power 
### Power_A = 0.99  (384 samples, 48 groups, 2 measurements)
### Power_B = 1.0  (384 samples, 48 groups, 2 measurements)
### Power_AB = 1.0  (384 samples, 48 groups, 2 measurements)

lambda_hat_A = ((p-1) / (n*p*q*r) * (MSA - MSRES)) / (MSRES/(n*p*q*r))  # 5.44 -> does not match value calculated by G*Power
lambda_hat_B = ((q-1) / (n*p*q*r) * (MSB - MSRES)) / (MSRES/(n*p*q*r))  # 32.99 -> does not match value calculated by G*Power


################################ Estimate no of blocks to achive desired power (0.8)

# n = 8  # from pilot study 
# n_prime = 5
# 
# Phi_hat_A = sqrt(n_prime) * sqrt((p-1)/(n*p) * (MSA - MSRES) / MSRES) 

