library(ggplot2)

# RBF-222 (repeated measures)

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

### mean accuracy 

####################### Hypotheses 

### sigma_pi_square = 0 
### mu.1.. = mu.2..
### mu..1. = mu..2. 
### mu...1 = mu...2
### alpha*beta_jkl = 0  

######################## exp units: subjects or blocks (each block is composed of one subject)  

N = 48*8 # --> this isn't actually used

### n = number of subjects - repeated measures

n = 48

### p = number of levels of treatment A
### q = number of levels of treatment B
### r = number of levels of treatment C

p = 2
q = 2
r = 2

#########################################################
### read  in data (has both correct and incorrect responses, conditions 3-10, removed 0 RT and RT>4.16sec)
#########################################################
# ID_cond_mean = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/for_bar_plot_bySubject_longRT.csv')
ID_cond_mean = read.csv('C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/for_bar_plot_bySubject_shortRT.csv')

ID_cond_mean$X = NULL


#########################################################
### data - mean accuracy time by subject and condition
#########################################################

### summary tables 

with(ID_cond_mean,tapply(ACC,list(ID,A,B,C),mean)) 

### descriptive stats 

with(ID_cond_mean,tapply(ACC,A,mean))           
with(ID_cond_mean,tapply(ACC,A,sd))               
with(ID_cond_mean,tapply(ACC,B,mean))            
with(ID_cond_mean,tapply(ACC,B,sd))              
with(ID_cond_mean,tapply(ACC,C,mean))            
with(ID_cond_mean,tapply(ACC,C,sd))               

with(ID_cond_mean,tapply(ACC,condition,mean))     
with(ID_cond_mean,tapply(ACC,condition,sd))       

########### compute sum of squares 

### downsample - equal male/female, 3 of each order (24 subjects/8 conditions)
ID_cond_mean_mod = rbind(ID_cond_mean[ID_cond_mean$ID==1,],ID_cond_mean[ID_cond_mean$ID==3,],
                         ID_cond_mean[ID_cond_mean$ID==5,],ID_cond_mean[ID_cond_mean$ID==6,],
                         ID_cond_mean[ID_cond_mean$ID==7,],ID_cond_mean[ID_cond_mean$ID==9,],
                         ID_cond_mean[ID_cond_mean$ID==12,],ID_cond_mean[ID_cond_mean$ID==13,],
                         ID_cond_mean[ID_cond_mean$ID==15,],ID_cond_mean[ID_cond_mean$ID==19,],
                         ID_cond_mean[ID_cond_mean$ID==21,],ID_cond_mean[ID_cond_mean$ID==22,],
                         ID_cond_mean[ID_cond_mean$ID==25,],ID_cond_mean[ID_cond_mean$ID==26,],
                         ID_cond_mean[ID_cond_mean$ID==28,],ID_cond_mean[ID_cond_mean$ID==30,],
                         ID_cond_mean[ID_cond_mean$ID==32,],ID_cond_mean[ID_cond_mean$ID==34,],
                         ID_cond_mean[ID_cond_mean$ID==36,],ID_cond_mean[ID_cond_mean$ID==40,],
                         ID_cond_mean[ID_cond_mean$ID==42,],ID_cond_mean[ID_cond_mean$ID==43,],
                         ID_cond_mean[ID_cond_mean$ID==47,],ID_cond_mean[ID_cond_mean$ID==48,])
ID_cond_mean_mod = ID_cond_mean_mod[order(ID_cond_mean_mod$condition),]
View(ID_cond_mean_mod)

n = 24

### summary tables 

with(ID_cond_mean_mod,tapply(ACC,list(ID,A,B,C),mean)) 

### descriptive stats 

with(ID_cond_mean_mod,tapply(ACC,A,mean))             
with(ID_cond_mean_mod,tapply(ACC,A,sd))               
with(ID_cond_mean_mod,tapply(ACC,B,mean))              
with(ID_cond_mean_mod,tapply(ACC,B,sd))              
with(ID_cond_mean_mod,tapply(ACC,C,mean))             
with(ID_cond_mean_mod,tapply(ACC,C,sd))                

cond_mean = with(ID_cond_mean,tapply(ACC,condition,mean))    
cond_sd = with(ID_cond_mean,tapply(ACC,condition,sd))       

# ID_cond_mean_mod$cond_mean = rep(cond_mean,each = n)
ID_cond_mean$cond_sd = rep(cond_sd, each = n)

cond_mean = aggregate(. ~ condition, data = ID_cond_mean, FUN = mean) 
cond_mean$label = round(cond_mean$Y_bar.jkl,2)


# bar plot - this creates plot for all participants (need to change if want plot for 7020 paper)
ggplot(data=cond_mean, aes(x=factor(condition), y=Y_bar.jkl, fill=factor(A), label = label)) +
  geom_text(nudge_x = .04, nudge_y = .03, fontface="bold", size = 5) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", width=.8) + theme_bw() + 
  geom_errorbar(aes(ymin=Y_bar.jkl-cond_sd, ymax=Y_bar.jkl+cond_sd),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
  scale_fill_manual(values=c("orangered","grey50","goldenrod"),
                    name="Decision\nType",
                    breaks=c(3, 1, 2),
                    labels=c("No Aid", "Select", "Agree/Disagree")) +
  scale_y_continuous(breaks=0:5*.2) +         # Set tick every .2
  xlab("") +
  ylab("Mean Accuracy") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                   labels=c("Easy / No", "Hard / No", "Easy / High", 
                            "Hard / High", "Easy / Low", "Hard / Low",
                            "Easy / High", "Hard / High", "Easy / Low", "Hard / Low")) + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=18)) +
  theme(axis.title.y = element_text(face="bold", size=22),axis.text.y  = element_text( vjust=0.5, size=18)) +
  theme(legend.title = element_text(size=22, face="bold"),legend.text = element_text(size = 18)) + 
  ggtitle("Mean Accuracy by Condition") + 
  theme(plot.title = element_text(size=28, face="bold")) + 
  
  # ggsave("C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/accuracy_bar_plot_longRT.png")
  ggsave("C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/accuracy_bar_plot_shortRT.png")
