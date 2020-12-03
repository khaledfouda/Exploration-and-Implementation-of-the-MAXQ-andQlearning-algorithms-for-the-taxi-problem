library(nnet)
library(proto)

#V <- matrix(runif(13*500,-.01,.01),13,500)

alpha <- .1
DF <- .8
MAXQ0 <- proto(expr = {
  
  V <- NA
  C <- NA
  #i <- NA # current subtask in action
  #s <- NA # current state
  taxi <- taxi.env$proto()
  gain <- 0
  
  is.terminal <- NA
  Action.space <- NA
  EvaluateMaxNode <- NA
  Policy <- NA
  MAXQ0.learn <- NA
  Estimate.V.C <- NA
})
#----------------------------------
MAXQ0$V <- matrix(0,13,500)
MAXQ0$C <-list()
for(i in 1:13){
  MAXQ0$C[[i]] <- matrix(0,500,13)
  #C[[i]] <- matrix(runif(10*500,-.01,.01), 500, 10)
}
#--------------------------------------
MAXQ0$is.terminal <- function(., i, s){
  # returns TRUE if s is an end state for the subtask i.
  # whenver in task 7(root), 9(dropoff), or 10(navig with a parent 9), terminate
  # only of the episode is done
  if(all(s==c(0,0,0,0))){return(TRUE)
  }else if(i==8 && s[3]==5){return(TRUE) # successfully picked up the passenger
  }else if(i==9 && s[3]!=5){return(TRUE) # attempting to drop off when passenger is not on board
  }else if(any(i==1:6)){return(TRUE) # any primitive action is excuted once
  }else if(i==10 && s[1:2]==.$taxi$loc.indx(1)){return(TRUE)
  }else if(i==11 && s[1:2]==.$taxi$loc.indx(2)){return(TRUE)
  }else if(i==12 && s[1:2]==.$taxi$loc.indx(3)){return(TRUE)
  }else if(i==13 && s[1:2]==.$taxi$loc.indx(4)){return(TRUE)
  }else {return(FALSE)}
}
MAXQ0$Action.space <- function(.,i){
  # expects only i from 7 to 13. ie, composite actions [subtasks]
  if(i==7){return(c(8,9))
  }else if(i==8){return(c(5,10:13))
  }else if(i==9){return(c(6,10:13))
  }else {return(1:4)} # == else if(i is in 10:13)
}
#---------------------------------------------------
MAXQ0$EvaluateMaxNode <- function(., i, s){
  if(any(i==1:6)){ # if i is a primitive max node
    # no update required as it's already updated in the main function
    return()
  }else{ # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
    actions <- .$Action.space(i)
    #actions <- half.greedy.Action.space(i,s)
    for(a in actions){.$EvaluateMaxNode(a, s)}
    #print(c(encode(s),i,actions))
    .$V[i,.$taxi$encode(s)] <- max(.$V[actions,.$taxi$encode(s)]+
                                     .$C[[i]][.$taxi$encode(s),actions])
    #print(.$V[i,.$taxi$encode(s)])
    return()
  }
}
#---------------------------------------------------
MAXQ0$Policy <- function(.,i,s){
  # Epsilon-greedy policy
  actions <- .$Action.space(i)
  #actions <- half.greedy.Action.space(i,s)
  if(runif(1) < .001){return(sample(actions,1))} # epsilon greedy
  return(actions[nnet::which.is.max(.$V[actions,.$taxi$encode(s)]+
                                      .$C[[i]][.$taxi$encode(s),actions])])
}
#---------------------------------------------------
MAXQ0$MAXQ0.learn <- function(.,i, s){
  # if it's primitive
  
  if(i <=6){
    .$taxi$s <- s
    .$taxi$a <- i
    .$taxi$step()
    .$gain <- .$gain + .$taxi$r
    .$V[i,.$taxi$encode(s)] <- (1-alpha) * .$V[i,.$taxi$encode(s)] + alpha * .$taxi$r
    return(1)
  }else{
  count = 0
  while(!.$is.terminal(i,s) && .$gain > -1000){
     a <- .$Policy(i,s) 
     N <- .$MAXQ0.learn(a,s)
     if(all(.$taxi$ss==c(0,0,0,0))) {return(count)}
     .$EvaluateMaxNode(i,.$taxi$ss)
     .$C[[i]][.$taxi$encode(s),a] <- (1-alpha) * .$C[[i]][.$taxi$encode(s),a] + 
       alpha * (DF^N) * .$V[i,.$taxi$encode(.$taxi$ss)]
     count <- count + N
     s <- .$taxi$ss
     .$taxi$s <- .$taxi$ss
  }
  #if(.$gain<=-1000){.$gain=-}
  #.$taxi$render()
  return(count)
 }
}
# #---------------------------------------------
MAXQ0$Estimate.V.C <- function(., n=100){
  s0s <- list()
  set.seed(12)
  rewards <- c()
  for(i in 1:n){
    # for each run generate a random state.
    s0 <- c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1))
    #s0 <- c(3,2,4,1)
    # print(s0)
    .$MAXQ0.learn(7, s0)
    rewards <- c(rewards, .$gain)
    .$gain <- 0
  }
  return(rewards)
}
# 
#----------------------------------------------------------




learn <- as.proto(MAXQ0)
#learn <- MAXQ0$proto()
s0 <- c(3,2,1,4)
rrr <- learn$MAXQ0.learn(7,s0)
learn$Policy(7,s0)
learn$gain


rewards <- learn$Estimate.V.C(50)
plot(1:length(rewards),rewards)
max(rewards)
min(rewards)


# o <- Estimate.V.C(500, Vestim, Cestim)
# Vestim <- o[[1]]
# Cestim <- o[[2]]
# rewards <- o[[3]]
# 
# #debug(MAXQ0.learn)
# 
# #s0 = c(4,1,5,3)
# o <-MAXQ0.learn(7,s0,Vestim,Cestim,0)
# o[[5]]



# # half.greedy.Action.space <- function(i,s){
#   # This version is stricter where the action space depends on the state as well as the
#   # subtask.
#   # expects only i from 7 to 13. ie, composite actions [subtasks]
#   if(i==7){
#     if(s[3]!=5){return(c(8))
#     }else {return(c(9))}
#     
#   }else if(i==8){
#     if(all(loc.indx(s[3])==s[1:2])){return(c(5))
#     }else if(s[[3]]==1){return(c(10))
#     }else if(s[[3]]==2){return(c(11))
#     }else if(s[[3]]==3){return(c(12))
#     }else if(s[[3]]==4){return(c(13))}
# 
#   }else if(i==9){
#     if(all(loc.indx(s[4])==s[1:2])){return(c(6))
#     }else if(s[[4]]==1){return(c(10))
#     }else if(s[[4]]==2){return(c(11))
#     }else if(s[[4]]==3){return(c(12))
#     }else if(s[[4]]==4){return(c(13))}
#   
#   }else {return(1:4)} # == else if(i is in 10:13)
# }
# 