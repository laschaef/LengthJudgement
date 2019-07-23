# ******* read in data ******* 

trust = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/Trust_Survey_forAnalysis.csv')

names(trust)
names(trust)[1]='ID'

reg1 = lm(Q4_1 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg1)

reg2 = lm(Q4_2 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg2)

reg3 = lm(Q4_3 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg3)

reg4 = lm(Q4_4 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg4)

reg5 = lm(Q4_5 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg5)

reg6 = lm(Q4_6 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg6)

reg7 = lm(Q4_7 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg7)

reg8 = lm(Q4_8 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg8)

reg9 = lm(Q4_9 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg9)

reg10 = lm(Q4_10 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg10)

reg11 = lm(Q4_11 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg11)

reg12 = lm(Q4_12 ~ ID + Q2 +Q3 +A3 + A10, trust)
summary(reg12)

