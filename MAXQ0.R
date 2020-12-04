library(nnet)

#V <- matrix(runif(13*500,-.01,.01),13,500)
V <- matrix(0,11,500)
C <-list()
for(i in 1:11){
  C[[i]] <- matrix(0,500,11)
  #C[[i]] <- matrix(runif(10*500,-.01,.01), 500, 10)
}
alpha <- .2
DF <- 1
epsilon <- .01
#--------------------------------------
is.terminal <- function(i,s){
  # returns TRUE if s is an end state for the subtask i.
  # whenver in task 7(root), 9(dropoff), or 10(navig with a parent 9), terminate
  # only of the episode is done
  if(all(s==c(0,0,0,0))){return(TRUE)
  }else if(any(i==c(8,10)) && s[3]==5){return(TRUE) # successfully picked up the passenger
  }else if(any(i==c(9,11)) && s[3]!=5){return(TRUE) # attempting to drop off when passenger is not on board
  }else if(any(i==1:6)){return(TRUE) # any primitive action is excuted once
  }else if(i==10 && all(s[1:2]==loc.indx(s[3]))){return(TRUE)#taxi ready to pick up
  }else if(i==11 && all(s[1:2]==loc.indx(s[4]))){return(TRUE)
  }else {return(FALSE)}
}
Action.space. <- function(i){
  # expects only i from 7 to 13. ie, composite actions [subtasks]
  if(i==7){return(c(8,9))
  }else if(i==8){return(c(5,10))
  }else if(i==9){return(c(6,11))
  }else if(i==10 || i==11){return(1:4)}
}
Action.space <- function(i,s){
  # expects only i from 7 to 13. ie, composite actions [subtasks]
  if(i==7){
    if(s[3]=='5'){return(c(9))}else{return(c(8))}
  }else if(i==8){
    if(all(loc.indx(s[3])==s[1:2]) ){return(c(5))
    }else if(s[3]==1){return(c(10))
    }else if(s[3]==2){return(c(11))
    }else if(s[3]==3){return(c(12))
    }else if(s[3]==4){return(c(13))}
  }else if(i==9){
    if(all(loc.indx(s[4])==s[1:2]) ){return(c(6))
    }else if(s[4]==1){return(c(10))
    }else if(s[4]==2){return(c(11))
    }else if(s[4]==3){return(c(12))
    }else if(s[4]==4){return(c(13))}}
  return(1:4) # == else if(i is in 10:13)
}

#---------------------------------------------------
EvaluateMaxNode <- function(i, s, V, C){
  if(any(i==1:6)){ # if i is a primitive max node
    # no update required as it's already updated in the main function
    return(list(V, C))
  }else{ # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
    actions <- Action.space(i,s)
    #actions <- half.greedy.Action.space(i,s)
    for(a in actions){
      out <- EvaluateMaxNode(a, s, V, C)
      V <- out[[1]]
      C <- out[[2]]
    }
    #print(c(encode(s),i,actions))
    V[i,encode(s)] <- max(V[actions,encode(s)]+C[[i]][encode(s),actions])
    return(list(V, C))
  }
}
#---------------------------------------------------
Policy <- function(i,s,V,C){
  # Epsilon-greedy policy
  actions <- Action.space(i,s)
  #actions <- half.greedy.Action.space(i,s)
  if(runif(1) < .001){return(sample(actions,1))} # epsilon greedy
  return(actions[nnet::which.is.max(V[actions,encode(s)]+C[[i]][encode(s),actions])])
}
#---------------------------------------------------
MAXQ0.learn <- function(i, s, V, C, gain){
  # if it's primitive
  if(i <=6){
    sr <- step(s,i)
    gain <- gain + sr[[2]]
    V[i,encode(s)] <- (1-alpha) * V[i,encode(s)] + alpha * sr[[2]]
    return(list(1,sr[[1]], V, C, gain))
  }else{
  count = 0
  while(!is.terminal(i,s) && gain > -1000){
     a <- Policy(i,s,V,C) 
    out <- MAXQ0.learn(a,s,V,C, gain)
     N <- out[[1]]
     ss <- out[[2]]
     V <- out[[3]]
     C <- out[[4]]
     gain <- out[[5]]
     if(all(ss==c(0,0,0,0))) {return(list(count,ss,V,C,gain))}
     out <- EvaluateMaxNode(i,ss,V,C)
     V <- out[[1]]
     C <- out[[2]]
     C[[i]][encode(s),a] <- (1-alpha) * C[[i]][encode(s),a] + alpha * (DF^N) * V[i,encode(ss)]
     count <- count + N
     s <- ss
  }
  if(gain<=-50){gain=-200}
  #render(s)
  return(list(count,s, V, C, gain))
 }
}
#---------------------------------------------
Estimate.V.C <- function(n=100, Vestim, Cestim){
  s0s <- list()
  #set.seed(12)
  rewards <- c()
  for(i in 1:n){
    # for each run generate a random state.
    s0 <- c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1))
    #s0 <- c(3,2,4,1)
    # print(s0)
    o <- MAXQ0.learn(7, s0, Vestim, Cestim,0)
    Vestim <- o[[3]]
    Cestim <- o[[4]]
    rewards <- c(rewards, o[[5]])
  }
  return(list(Vestim, Cestim, rewards,s0s))
}

o <- Estimate.V.C(10, V, C)
Vestim <- o[[1]]
Cestim <- o[[2]]
rewards <- o[[3]]

#debug(MAXQ0.learn)

plot(1:length(rewards),rewards)
max(rewards)
min(rewards)
#s0 = c(4,1,5,3)
o <-MAXQ0.learn(7,s0,V,C,0)
o[[5]]