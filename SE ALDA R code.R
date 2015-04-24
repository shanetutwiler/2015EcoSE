#Author:  M. Shane Tutwiler
# Harvard Graduate School of Education

# Individual Growth Curve models (Singer & Willett, 2003) predicting student self-efficacy

nlc <- read.delim("C:/Users/mst216.AD/Documents/EcoMUVE analyses/nlc.txt")

nlc$pr <- nlc$pi
nlc$pi <- NULL

attach(nlc)
nlc$time.post.ecomuve[time == 0] <- 0
nlc$time.post.ecomuve[time == 1] <- 0
nlc$time.post.ecomuve[time == 2] <- 1
detach(nlc)

View(nlc)

#create a pre-intervention science identity variable
nlc.t0 <- subset(nlc, time == 0, select = c(id, ident))
View(nlc.t0)

nlc.new <- merge(nlc, nlc.t0, by = "id")
View(nlc.new)

library(nlme)

#Fit individual growth models

se.m1 <- lme(se~time, data=nlc, random=~time|id, method="ML") #base longitudinal model

se.m2 <- lme(se~time + teach.1, data=nlc, random=~time|id, method="ML") #M1 + fixed effects of teacher on intercept

anova(se.m1, se.m2)

se.m3 <- lme(se~time + female, data=nlc, random=~time|id, method="ML") #M1 + fixed effects of female on intercept

anova(se.m1, se.m3)

se.m4 <- lme(se~time  + female + time:teach.1, data=nlc, random=~time|id, method="ML") #M3 + effects of teacher on slope

anova(se.m3, se.m4)

se.m5 <- lme(se~time + teach.1 + female + time:teach.1 + time:female, data=nlc, random=~time|id, method="ML") #M4 + effects of female on slope

anova(se.m4, se.m5)

se.m6 <- lme(se~time + female + time:teach.1, data=nlc, random=~time|id, method="ML") #Reduced model, constant slope before and after completing EcoMUVE

#Level 1: SEij = B0 + B1*TIMEij + eij
#Level 2: B0 = g00 + g01*FEMALEi + u0i
#Level 2: B1 = g10 + g11*TEACH.1i + u1i

se.m7 <- lme(se~time + female + time:teach.1 + ms + n + pr, data=nlc, random=~time|id, method="ML") #M6 + TV effect of EcoMUVE roles on intercept

anova(se.m6, se.m7)

se.m8 <- lme(se~time + female + time:teach.1 + time.post.ecomuve, data=nlc, random=~time + time.post.ecomuve|id, method="ML") #final model + differential slope post-EcoMUVE

anova(se.m6, se.m8)

se.m9 <- lme(se~time + female + time:teach.1 + time.post.ecomuve + time.post.ecomuve:teach.1, data=nlc, random=~time + time.post.ecomuve|id, method="ML") #M8 + TV effect of teacher on post-EcoMUVE slope

anova(se.m8, se.m9)

#Time vectors for plots
visit <- c(0,1,2)
post.visit <- c(0,0,1)

#Growth model of prototypical student, controling for gender and teacher effects.
#fixef.8 <- fixef(se.m8)
#fit.8 <- fixef.8[[1]] + visit[1:3]*fixef.8[[2]] + 0.5113*fixef.8[[3]] + post.visit[1:3]*fixef.8[[4]] +visit[1:3]*0.466*fixef.8[[5]]

#Growth model of prototypical student, by gender, controlling for teacher effects.
#fit.8.f <- fixef.8[[1]] + visit[1:3]*fixef.8[[2]] + 1*fixef.8[[3]] + post.visit[1:3]*fixef.8[[4]] +visit[1:3]*0.466*fixef.8[[5]]
#fit.8.m <- fixef.8[[1]] + visit[1:3]*fixef.8[[2]] + 0*fixef.8[[3]] + post.visit[1:3]*fixef.8[[4]] +visit[1:3]*0.466*fixef.8[[5]]

#PLOTS OF PROTOTYPICAL STUDENTS

#par(mfrow = c(1,3))
#plot(visit[1:3], fit.8, type="b", ylim =c(0,6), lty=2, xlab= "Pre EcoMUVE=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Model estimated self-efficacy")
#title("Individual Growth Model \n Student Self-Efficacy")


#plot(visit[1:3], fit.8.f, type="b", pch=0, ylim =c(0,6), xlab= "Pre EcoMUVE=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Model estimated self-efficacy")
#lines(visit[1:3], fit.8.m, type="b", pch=17)
#lines(visit[1:3], fit.8, type="b", lty=2)
#title("Individual Growth Model \n Student Self-Efficacy by Gender")
#legend(0, 6, c("Male", "Female"), pch=c(17,0))

#plot(visit[1:3], fit.8.f, type="b", pch=0, ylim =c(0,6), xlab= "Pre EcoMUVE=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Model estimated self-efficacy")
#lines(visit[1:3], fit.8.m, type="b", pch=17)
#title("Individual Growth Model \n Student Self-Efficacy by Gender")
#legend(0, 6, c("Male", "Female"), pch=c(17,0))

#Examining the effect of pre-intervention science identity on the final model (m8)
se.m8a <- lme(se~time + female + time:teach.1 + time.post.ecomuve, data=nlc.new, random=~time + time.post.ecomuve|id, method="ML") #refit final model in dataset with pre-identity variable for all students

se.m8b <- lme(se~time + ident.y + female + time:teach.1 + time.post.ecomuve, data=nlc.new, random=~time + time.post.ecomuve|id, method="ML") #effect of identity on intercept

anova(se.m8b, se.m8a)

se.m8c <- lme(se~time + ident.y + female + time:teach.1 + time:ident.y + time.post.ecomuve, data=nlc.new, random=~time + time.post.ecomuve|id, method="ML") #effect of identity on EcoMUVE slope

anova(se.m8c, se.m8b)

se.m8d <- lme(se~time + ident.y + female + time:teach.1 + time:ident.y + time.post.ecomuve + time.post.ecomuve:ident.y, data=nlc.new, random=~time + time.post.ecomuve|id, method="ML") #effect of identity on EcoMOBILE slope

anova(se.m8d, se.m8c)

#FINAL MODEL (WITH IDENTITY CONTROL)
#Level 1: SEij = B0 + B1*TIMEij + B2*TIME.POST.ECOMUVEij + eij
#Level 2: B0 = g00 + g01*FEMALEi + g02*IDENTi + u0i
#Level 2: B1 = g10 + g11*TEACH.1i + g12*IDENTi + u1i
#Level 2: B2  = g20 +  u2i

se.m8c <- lme(se~time + ident.y + female + time:teach.1 + time:ident.y + time.post.ecomuve, data=nlc.new, random=~time + time.post.ecomuve|id, method="ML") #effect of identity on EcoMUVE slope

# Descriptive stats for plots
summary(nlc.new$teach.1)
summary(nlc.new$female)
summary(nlc.new$ident.y)
sd(nlc.new$ident.y)

#Growth model of prototypical student, controling for gender, ident, and teacher effects.
fixef.8c <- fixef(se.m8c) #fetch fixed effects from final model
fixef.8c
fit.8c <- fixef.8c[[1]] + visit[1:3]*fixef.8c[[2]] + 2.971*fixef.8c[[3]] + 0.5304*fixef.8c[[4]] + post.visit[1:3]*fixef.8c[[5]] + visit[1:3]*0.4674*fixef.8c[[6]] + visit[1:3]*2.971*fixef.8c[[7]] #full model
fit.8c.hi <- fixef.8c[[1]] + visit[1:3]*fixef.8c[[2]] + 4.217*fixef.8c[[3]] + 0.5304*fixef.8c[[4]] + post.visit[1:3]*fixef.8c[[5]] + visit[1:3]*0.4674*fixef.8c[[6]] + visit[1:3]*4.217*fixef.8c[[7]] #high interest (+1sd)
fit.8c.lo <- fixef.8c[[1]] + visit[1:3]*fixef.8c[[2]] + 1.725*fixef.8c[[3]] + 0.5304*fixef.8c[[4]] + post.visit[1:3]*fixef.8c[[5]] + visit[1:3]*0.4674*fixef.8c[[6]] + visit[1:3]*1.725*fixef.8c[[7]] #low interest (-1sd)

par(mfrow = c(2,2))
boxplot(se~time, data=nlc.new, main="Student Self-Efficacy Over Time", xlab="Pre=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Self-Efficacy", ylim=c(0,6))

plot(visit[1:3], fit.8c, type="b", ylim =c(0,6), lty=2, xlab= "Pre=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Model estimated self-efficacy", xaxt="n")
title("Individual Growth Model \n Student Self-Efficacy")
axis(1, at=c(0,1,2))

plot(visit[1:3], fit.8c.hi, type="b", pch=0, ylim =c(0,6), xlab= "Pre=0, Post EcoMUVE=1, Post EcoMOBILE=2", ylab="Model estimated self-efficacy", xaxt="n")
lines(visit[1:3], fit.8c.lo, type="b", pch=17)
title("Individual Growth Model \n Student Self-Efficacy \n (by Pre Science Identity)")
legend(0, 3, c("High Ident", "Low Ident"), pch=c(0,17))
axis(1, at=c(0,1,2))

