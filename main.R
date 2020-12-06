source("oop_env.R")
source("MAXQ0.R")

#----------------------------------------------------------
# example of running one episode 
e <- as.proto(MAXQ0)
e$init() # needs to be always called after the creation of the object
e$MAXQ0(7,e$get.rands())
#-----------------------------------------------------
# running n episodes and observing the resulting rewards and value estimates.
e$add.seed(12)
e$init()
system.time( rewards <- e$Estimate.V.C(2000, alpha=.01, DF=.9, epsilon=.001))
plot(1:length(rewards),rewards)
min(rewards); max(rewards) ;mean(rewards); sqrt(var(rewards))
plot(NA,NA, xlim = c(0,length(rewards)), ylim=c(min(rewards),max(rewards)),
     xlab = "Episodes",ylab='total rewards',main = 'Sum of rewards over time episodes \n
     alpha=.01, gamma=.9, epsilon=.001')
lines(1:length(rewards),rewards)

Vest <- e$V
Cestim <- e$C
#----------------------------------------------------------
rmax=6
IQR(rewards); sd(rewards)
min(rewards); median(rewards); max(rewards)
mean(rewards[1000:2000]); mean(rewards)