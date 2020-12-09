source("taxi_env.R")
source("MAXQ0.R")

#----------------------------------------------------------
# example of running one episode 
e <- MAXQ0$proto()
e$init() # needs to be always called after the creation of the object
e$MAXQ0(7,e$taxi$set.random.state(FALSE))
#-----------------------------------------------------
# running n episodes and observing the resulting rewards, steps and value estimates
e$init()

printstats <- function(X){
  print(paste(min(X), median(X), mean(X), max(X)))
  print(paste(IQR(X), var(X)))
}
nepis = 6000 # number of episodes to simulate
system.time( rewstp <- e$Estimate.V.C(nepis, alpha=.8, DF=.999, epsilon=.01,theseed=10,decay=FALSE,resetV=TRUE))
rewards <- rewstp[[1]]
steps <- rewstp[[2]]

printstats(rewards)
printstats(steps)
printstats(rewards[500:nepis])
printstats(steps[500:nepis])

plot(NA,NA, xlim = c(0,length(rewards)), ylim=c(min(rewards),max(steps)),
     xlab = "Episodes",ylab=NA, main = "sum of rewards and number of\nsteps to termination per episode")
lines(1:length(rewards),rewards, col="blue")
lines(1:length(rewards),steps, col="red")
legend('bottomright', legend = c('steps','rewards'), col = c('red', 'blue'),
       xpd = TRUE, inset=c(0,0), bty='n', lty=c(1,1), cex=.8, lwd=2)
