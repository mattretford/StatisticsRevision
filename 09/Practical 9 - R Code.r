##################################################################################################

# Q1 

# Write down the definition of a likelihood in words.

# Write down the definition of a prior probability of an unknown disease prevalence 
# from a Bayesian perspective.

# Write down the definition of a prior probability of an unknown disease prevalence 
# from a frequentist perspective.

###########################################################################################

# Q2
# cancer example - 4 possible outcomes, cancer, food poisoning, ulcer, infection

# prior for each outcome based on past experience

prior=c(70, 800, 80, 50)/1000   ; prior

# sensitivity of biomarker test for each outcome

like=c(56/70, 400/800, 4/80, 15/50)   ;  like

# calculate the marginal probability of a positive biomarker test by hand
# interpret this in words

marginal<-sum(prior*like)     ;  marginal

# calculate the posterior probability of each outcome conditional on a positive biomarker test
# what is the most probable outcome for a patient with positive test

post<-prior*like/marginal     ; post

# repeat these calculations for a negative test - how does the posterior change in this case?
# probability of a negative test

likeneg<- 1-like              ; likeneg

marginalneg<-1-marginal  ; marginalneg

postneg<-prior*likeneg/marginalneg  ;postneg

# food poisoning still most likely, probability of ulcer increased to ~15%, cancer unlikely <3%


###########################################################################################

# Q3

# Use the following function to draw your own leaf plot

leafplot <- function(sensi, speci){
  
  pretest <- seq(0, 1, 0.01) #possible pre-test probabilities 
  
  #probability of having Covid-19 after a positive test result 
  pos.test <- sensi*pretest/(sensi*pretest+(1-speci)*(1-pretest))
  
  #probability of having Covid-19 after a negative test result 
  neg.test <- ((1-sensi)*(pretest))/((1-sensi)*pretest+speci*(1-pretest))
  
  #plot leaves
  plot(pretest, pos.test, type="l", col="darkgreen", 
       xlab="Pre-test Probability", ylab="Post-test Probability")
  points(pretest, neg.test, type="l", col="darkgreen")
  abline(a=0, b=1, col="darkgreen")
  
  
  
  arrows(pretest[51],0,pretest[51],pos.test[51],length=0.0,angle=30,lwd=2,col="purple")
  arrows(pretest[51],pos.test[51],0,pos.test[51],length=0.15,angle=30,lwd=2,col="purple")
  
  arrows(pretest[51],0,pretest[51],neg.test[51],length=0.0,angle=30,lwd=2,col="orange")
  arrows(pretest[51],neg.test[51],0,neg.test[51],length=0.15,angle=30,lwd=2,col="orange")
  
  legend(0.6,0.2,c("Positive test","Negative test"),lty=1, lwd=c(2:2), col=c("purple","orange"), cex=0.6)  
}


# If your prior (pre-test) probability is 0.5, 
# sensitivity is 0.8 and specificity is 0.98 
# read off the posterior (post-test) probability of disease if the test was postitive? 
# and if the test was negative?

par(mfrow=c(1,2))

leafplot(sensi=0.8, speci=0.98)

# Repeat this for sensitivity 0.8 and specificity 0.7

leafplot(0.8, 0.7)

# Explain in words why the posteriors differ (if they do).


#############################################################

# Q4

# 1. Plot a Beta(5,10).

options(repr.plot.width=7, rer.plot.height=3)
theta <- seq(0, 1, 0.01)
plot(theta, dbeta(theta, 5, 10), type="l", 
     ylim=c(0,4), xlab=expression(theta), ylab="density")

# 2. Observe 3 patients from 5 trials
# Beta(5, 10) x Binomial(3 | 5) -> Beta(8, 12)

lines(theta, dbeta(theta, 8, 12), type="l", 
      col="red", xlab=expression(theta), ylab="density")
legend(0.8,3,c("prior", "posterior"),col=1:2, lty=c(1,1))

# find 95% credible interval for the Beta(8, 12) posterior using qbeta 

qbeta(0.025, 8, 12)       # 0.2025214
qbeta(0.975, 8, 12)       # 0.6164221

# width of CrI
qbeta(0.975, 8, 12) - qbeta(0.025, 8, 12)


# 3. Find the 95% HPDi for the Beta(8, 12) posterior using HDInterval

install.packages("HDInterval")
library("HDInterval")
hdi(qbeta, 0.95, shape1=8, shape2=12)

# lower     upper 
# 0.1961515 0.6091670 

# width of HPDi
0.6091670 - 0.1961515

# The 95% CrI is slightly wider than the HPDi 0.4139006 vs 0.4130155
# The difference is small because the posterior distribution is close to symmetrical


# 4. The 95% credible interval is an interval within which theta lies with probability=0.95.
# The 95%HPDi is the narrowest credible interval for theta.
# 95%CI means if the study was completed many times 95% of the resulting confidence intervals contain theta.

###########################################################################################

# Q5
# 5.1
# In a different study 1 patient got pain relief of 5 who were given a drug 
# use this to construct a prior

# mean = 0.2 -> Beta(a,4a) - eg Beta(1,4) Beta(2,8), Beta(3,12)

# plot these

options(repr.plot.width=7, rer.plot.height=3)
theta <- seq(0, 1, 0.01)
plot(theta, dbeta(theta, 1, 4), type="l", col="green",
     ylim=c(0,6), xlab=expression(theta), ylab="density")

# Observe 3 patients from 5 trials
# Beta(5, 10) x Binomial(3 | 5) -> Beta(8, 12)

lines(theta, dbeta(theta, 2, 8), type="l", col="red")
lines(theta, dbeta(theta, 3, 12), type="l", col="blue")

# Note that a,b don't have to be integers
lines(theta, dbeta(theta, 1.5, 6), type="l", col="Orange")
legend(0.6, 5, c("Beta(1,4)", "Beta(2,8)", "Beta(3,12)", "Beta(1.5,6)"), col=c("green", "red", "blue", "orange"), lty=1)
# Let's use the Beta(1,4) prior

# 5.2. Beta(1,4) x Binomial(7 | 50) -> Beta(8, 47) 

# Prior
a.prior <- 1 ; b.prior <- 4
# Likelihood
y <- 7  ; n <- 50
# Posterior
a.post <- a.prior+y
b.post <- b.prior+n-y

a.post/(a.post+b.post)

a.post*b.post/((a.post+b.post)*(a.post+b.post)*(a.post+b.post+1))

# Let's plot prior, likelihood and posterior

options(repr.plot.width=7, rer.plot.height=3)
theta <- seq(0, 1, 0.01)
plot(theta, dbeta(theta, a.prior, b.prior), type="l", col="green",
     ylim=c(0,10), xlab=expression(theta), ylab="density")

lines(theta, dbeta(theta, 8, 44), type="l", col="red")
lines(theta, dbeta(theta, a.post, b.post), type="l", col="black")

#5.3. Copy the draft function for calculating the posterior

binbayes <- function(a.prior, b.prior, y, n){
  #Fill in the parameters of the posterior distribution
  a.post <- a.prior+y ##fill this in!!!!!!!
  b.post <- b.prior+n-y ##fill this in!!!!!!!
  #Plot the posterior distribution
  p = seq(0,1, length=100)
  plot(p, dbeta(p, a.post, b.post), ylab="density", type="l")
  return(c(a.post=a.post, b.post=b.post))
}


# Beta prior with mean ~0.15 from ~20 patients 
# For example, n = a.prior+b.prior = 20 
# a.prior/(a.prior+b.prior)=0.15  ->  a.prior=20*0.15=3

a.prior=3  ; b.prior=17  ; y=3  ; n=15

binbayes(a.prior, b.prior, y, n)

# a.post b.post 
# 6   29 

# 5. Posterior probability that theta lies in the interval (0.1, 0.25)

a.post=6   ; b.post=29

pbeta(0.1, a.post, b.post)        # 0.1185296
pbeta(0.25, a.post, b.post)       # 0.8862041

pbeta(0.25, a.post, b.post) - pbeta(0.1, a.post, b.post)   

# 0.7676746 - approx 75% probability that theta lies in the interval (0.1, 0.25)


####
# If we used a vague prior

binbayes(1,1,3,15)

a.post=4   ; b.post=13

pbeta(0.1, a.post, b.post)        # 0.06840617
pbeta(0.25, a.post, b.post)       # 0.5950129

pbeta(0.25, a.post, b.post) - pbeta(0.1, a.post, b.post)   

# 0.5266067