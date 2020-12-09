library(proto)
source('taxi_env.R')

MAXQ0 <- proto(expr = {
  # Matrices (all are defined later below)
  V <- NA # holds the estimates for the state value functions
  C <- NA # are action-state value estimates.
  # Variables
  epsilon <- .01 # for the epsilon-greedy policy.
  DF <- 1 # the discounted factor
  alpha <- .2 # learning rate 
  gain <- 0 # sum of rewards for a single episode
  taxi <- taxi.env$proto()
  # available methods
  add.seed <- NA
  init <- 
  is.terminal <- NA
  Action.space <- NA
  EvaluateMaxNode <- NA
  Policy <- NA
  MAXQ0 <- NA
  Estimate.V.C <- NA
})
#----------------------------------
MAXQ0$add.seed <- function(.,seed){set.seed(seed)}
MAXQ0$init <- function(.,reset.estimates=TRUE, alpha=.2,DF=1,epsilon=.01){
  if(reset.estimates == TRUE){
    .$V <- array(0,c(11,500))
    .$C <- array(0, c(11,500,11))
  }
  .$alpha <- alpha
  .$DF <- DF
  .$gain <- 0
  .$epsilon <- epsilon
  .$taxi$init()
}

#--------------------------------------
MAXQ0$is.terminal <- function(., i, s){
  # returns TRUE if s is an end state for the subtask i.
  # whenver in task 7(root), 9(dropoff), or 10(navig with a parent 9), terminate
  # only if the episode is over.
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
  .$V[i,.$taxi$encode(s)] <- max(.$V[actions,.$taxi$encode(s)]+.$C[i,.$taxi$encode(s),actions])
}
#---------------------------------------------------
MAXQ0$Policy <- function(.,i,s){
  # Epsilon-greedy policy
  actions <- .$Action.space(i)
  if(runif(1) < .$epsilon){return(sample(actions,1))} # epsilon greedy
  return(actions[which.max(.$V[actions,.$taxi$encode(s)]+ .$C[i,.$taxi$encode(s),actions])])
}
#---------------------------------------------------
MAXQ0$MAXQ0 <- function(.,i, s){
  # if it's primitive
  .$taxi$s <- s
  
  if(i <=6){
    r <- .$taxi$step(i)
    .$gain <- .$gain + r
    .$V[i,.$taxi$encode(s)] <- (1-.$alpha) * .$V[i,.$taxi$encode(s)] + .$alpha * r
    return(1)
  }else{
# else if i is a composite action (subtask)
  count = 0
  while(!.$is.terminal(i,s)){
     a <- .$Policy(i,s) 
     N <- .$MAXQ0(a,s)
     if(all(.$taxi$ss==c(0,0,0,0))) {return(count)} # goal is achieved.
     .$EvaluateMaxNode(i,.$taxi$ss)
     .$C[i,.$taxi$encode(s),a] <- (1-.$alpha) * .$C[i,.$taxi$encode(s),a] + 
       .$alpha * (.$DF^N) * .$V[i,.$taxi$encode(.$taxi$ss)]
     count <- count + N
     s <- .$taxi$ss
     .$taxi$update()
  }
  if (all(.$taxi$ss == c(-1,-1,-1,-1))){ # this is to avoid the situation when we start 
    # at root then do navigate destination and hit a terminal state. 
    .$taxi$ss <- .$taxi$s}
  return(count)

}}
# #---------------------------------------------
MAXQ0$Estimate.V.C <- function(., n=100, alpha=.2, DF=1, epsilon=.01,theseed=12, decay=FALSE, resetV=TRUE){
  # runs n episodes of MAXQ0() using random initial state at each episode
  # and updating the estimates for value functions
  # return a list of returns and steps till termination
  .$init(resetV)
  rewards <- c()
  steps <- c()
  .$add.seed(theseed)
  .$taxi$add.seed(theseed)
  # decaying learning rate alpha. default is FALSE
  if(decay==TRUE){ lr <- alpha * sort(runif(n),decreasing = TRUE)
  }else { lr <- alpha * rep(1,n) }
  
  for(i in 1:n){
    .$init(FALSE,lr[[i]], DF, epsilon)
    # for each run generate a random state.
    .$MAXQ0(7, .$taxi$set.random.state(FALSE)) # always start from the root subtask
    rewards <- c(rewards, .$gain)
    steps <- c(steps, .$taxi$count)
    if(i%%400==0){print(paste0(i," episodes are simulated"))}
  }
  return(list(rewards,steps))
}
#--------------------------------------------------------------
