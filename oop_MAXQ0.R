library(nnet)
library(proto)

MAXQ0 <- proto(expr = {
  # Matrices (all are defined later below)
  V <- NA # holds the estimates for the value functions
  C <- NA 
  # Variables
  epsilon <- NA # for the epsilon-greedy policy
  DF <- NA # the discounted factor
  alpha <- NA # learning rate 
  gain <- 0 # sum of rewards for a single episode
  taxi <- as.proto(taxi.env)
  # available methods
  add.seed <- NA
  init <- NA
  is.terminal <- NA
  Action.space <- NA
  EvaluateMaxNode <- NA
  Policy <- NA
  MAXQ0 <- NA
  Estimate.V.C <- NA
  get.rands <- NA
})
#----------------------------------
MAXQ0$add.seed <- function(.,seed){set.seed(seed)}
MAXQ0$init <- function(.,reset.estimates=TRUE, alpha=.2,DF=1,epsilon=.01){
  if(reset.estimates == TRUE){
    .$V <- matrix(0,11,500)
    .$C <-list()
    for(i in 1:11){
      .$C[[i]] <- matrix(0,500,11)
    }
  }
  .$alpha <- alpha
  .$DF <- DF
  .$gain <- 0
  .$epsilon <- epsilon
  .$taxi <- as.proto(taxi.env)
}
#--------------------------------------
MAXQ0$is.terminal <- function(., i, s){
  # returns TRUE if s is an end state for the subtask i.
  # whenver in task 7(root), 9(dropoff), or 10(navig with a parent 9), terminate
  # only of the episode is done
  if(all(s==c(0,0,0,0))){return(TRUE)
  }else if(any(i==c(8,10)) && s[3]==5){return(TRUE) # successfully picked up the passenger
  }else if(any(i==c(9,11)) && s[3]!=5){return(TRUE) # attempting to drop off when passenger is not on board
  }else if(any(i==1:6)){return(TRUE) # any primitive action is excuted once
  }else if(i==10 && all(s[1:2]==.$taxi$loc.indx(s[3]))){return(TRUE)#taxi ready to pick up
  }else if(i==11 && all(s[1:2]==.$taxi$loc.indx(s[4]))){return(TRUE)
  }else {return(FALSE)}
}
#-------------------------------------------------------------------
MAXQ0$Action.space <- function(.,i){
  # expects only i from 7 to 13. ie, composite actions [subtasks]
  if(i==7){return(c(8,9))
  }else if(i==8){return(c(5,10))
  }else if(i==9){return(c(6,11))
  }else if(i==10 || i==11){return(1:4)}
}
#---------------------------------------------------
MAXQ0$EvaluateMaxNode <- function(., i, s){
  # if i is primitive then no update is required then it's updated in MAXQ0()
  if(any(i==1:6)){return()}
  # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
  actions <- .$Action.space(i)
  for(a in actions){.$EvaluateMaxNode(a, s)}
  .$V[i,.$taxi$encode(s)] <- max(.$V[actions,.$taxi$encode(s)]+.$C[[i]][.$taxi$encode(s),actions])
}
#---------------------------------------------------
MAXQ0$Policy <- function(.,i,s){
  # Epsilon-greedy policy
  actions <- .$Action.space(i)
  if(runif(1) < .$epsilon){return(sample(actions,1))} # epsilon greedy
  return(actions[which.max(.$V[actions,.$taxi$encode(s)]+
                                      .$C[[i]][.$taxi$encode(s),actions])])
}
#---------------------------------------------------
MAXQ0$MAXQ0 <- function(.,i, s){
  # if it's primitive
  .$taxi$s <- s
  .$taxi$a <- i
  
  if(i <=6){
    .$taxi$render()
    r <- .$taxi$step(i)
    .$gain <- .$gain + r
    .$V[i,.$taxi$encode(s)] <- (1-.$alpha) * .$V[i,.$taxi$encode(s)] + .$alpha * r
    return(1)
  }
# else if i is a composite action (subtask)
  count = 0
  while(!.$is.terminal(i,s)){
     a <- .$Policy(i,s) 
     N <- .$MAXQ0(a,s)
     if(all(.$taxi$ss==c(0,0,0,0))) {return(count)} # goal is achieved.
     .$EvaluateMaxNode(i,.$taxi$ss)
     .$C[[i]][.$taxi$encode(s),a] <- (1-.$alpha) * .$C[[i]][.$taxi$encode(s),a] + 
       .$alpha * (.$DF^N) * .$V[i,.$taxi$encode(.$taxi$ss)]
     count <- count + N
     s <- .$taxi$ss
     .$taxi$update()
  }
  return(count)

}
# #---------------------------------------------
MAXQ0$Estimate.V.C <- function(., n=100){
  # runs n episodes of MAXQ0() using random initial state at each episode
  # and updating the estimates for value functions and keeping track of the rewards
  rewards <- c()
  for(i in 1:n){
    .$init(FALSE,alpha=.2, DF=1, epsilon=1/i)
    # for each run generate a random state.
    s0 <- c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1))
    .$MAXQ0(7, s0) # always start from the root subtask
    rewards <- c(rewards, .$gain)
    if(i%%500==0){print(paste0(i," episodes are simulated"))}
  }
  return(rewards)
}
MAXQ0$get.rands <- function(.){ # returns a random state
  return(c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1)))
}
# 
#----------------------------------------------------------
# example of running one episode 
e <- as.proto(MAXQ0)
e$init() # needs to be always called after the creation of the object
e$MAXQ0(7,e$get.rands())
#-----------------------------------------------------
# running n episodes and observing the resulting rewards and value estimates.
e$add.seed(12)
e$init()
rewards <- e$Estimate.V.C(2000)
plot(1:length(rewards),rewards)
min(rewards); max(rewards) ;mean(rewards); sqrt(var(rewards))
plot(NA,NA, xlim = c(0,length(rewards)), ylim=c(min(rewards),max(rewards)),
     xlab = "Episodes",ylab='total rewards')
lines(1:length(rewards),rewards)

Vest <- e$V
Cestim <- e$C
#----------------------------------------------------------