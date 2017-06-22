sampling()
sampling2()

#READ IN DATA SAVED FROM VARIANCE IN CAPTURE PROBABILITY CODE BELOW
probs<-read.table(file="C:/Users/sreynolds/Documents/GitHub/Capture-Probabilities/CPtrials.csv")
dat<-read.table(file="C:/Users/sreynolds/Documents/GitHub/Capture-Probabilities/df.csv")
dat2<-read.table(file="C:/Users/sreynolds/Documents/GitHub/Capture-Probabilities/df_hat.csv")

#p != TP p1+p2 NOR 1-(1-p1)(1-p2)
boxplot(prob~type, dat)

#p1_hat!=p1 AND p2_hat!=p2  (p1_hat+p2_hat=p3)
boxplot(prob1~type, dat2, main="Trap 1")
boxplot(prob2~type, dat2, main="Trap 2")



##################################################################################
#######################
# CAPTURE PROBABILITY #
#######################

catch1<-replicate(50000, sampling())
p1<-sum(catch1)/length(catch1)
p1

catch2<-replicate(50000, sampling(trap=c(5,1)))
p2<-sum(catch2)/length(catch2)
p2


catch3<-replicate(50000, sampling2())
p3<-sum(catch3[1,])/ncol(catch3)
p3
p1_hat<-sum(catch3[2,])/ncol(catch3)
p1_hat 
p2_hat<-sum(catch3[3,])/ncol(catch3)
p2_hat

#p1+p2!=p3
#p1_hat+p2_hat=p3, BUT p1_hat!=p1 and p2_hat!=p2


##############################################################################
###################################
# VARIANCE IN CAPTURE PROBABILITY #
###################################

p1=NULL
for(i in 1:100)
{
  catch1<-replicate(10000, sampling())
  p1<-c(p1, sum(catch1)/length(catch1))
}

p2=NULL
for(i in 1:100)
{
  catch2<-replicate(10000, sampling(trap=c(5,1)))
  p2<-c(p2, sum(catch2)/length(catch2))
}

p3=NULL
p1_hat=NULL
p2_hat=NULL
for(i in 1:100)
{
  catch3<-replicate(10000, sampling2())
  p3<-c(p3, sum(catch3[1,])/ncol(catch3))
  p1_hat<-c(p1_hat,sum(catch3[2,])/ncol(catch3))
  p2_hat<-c(p2_hat,sum(catch3[3,])/ncol(catch3))
}

combine<-as.data.frame(t(rbind(p1,p2,p3,p1_hat,p2_hat)))
write.table(combine,file="C:/Users/sreynolds/Documents/R Files/CPtrials.csv")

dat<-data.frame(prob=c(p3, p1+p2, 1-(1-p1)*(1-p2)), type=c(rep("p",100),rep("p1+p2",100),rep("1-(1-p1)(1-p2)",100)))
boxplot(prob~type, dat)
write.table(dat,file="C:/Users/sreynolds/Documents/R Files/df.csv")

dat2<-data.frame(prob1=c(p1,p1_hat), prob2=c(p2,p2_hat), type=c(rep("alone",100), rep("together",100)))
boxplot(prob1~type, dat2)
boxplot(prob2~type, dat2)
write.table(dat2,file="C:/Users/sreynolds/Documents/R Files/dfhat.csv")

