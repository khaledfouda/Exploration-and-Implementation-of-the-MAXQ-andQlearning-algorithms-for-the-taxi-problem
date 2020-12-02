library(nnet)

#V <- matrix(runif(13*500,-.01,.01),13,500)
V <- matrix(0,13,500)
C <-list()
for(i in 1:13){
  C[[i]] <- matrix(0,500,13)
  #C[[i]] <- matrix(runif(10*500,-.01,.01), 500, 10)
}
alpha <- .1
DF <- .8
#--------------------------------------
is.terminal <- function(i,s){
  # returns TRUE if s is an end state for the subtask i.
  # whenver in task 7(root), 9(dropoff), or 10(navig with a parent 9), terminate
  # only of the episode is done
  if(all(s==c(0,0,0,0))){return(TRUE)
  }else if(i==8 && s[3]==5){return(TRUE) # successfully picked up the passenger
  }else if(i==9 && s[3]!=5){return(TRUE) # attempting to drop off when passenger is not on board
  }else if(any(i==1:6)){return(TRUE) # any primitive action is excuted once
  }else if(i==10 && s[1:2]==loc.indx(1)){return(TRUE)
  }else if(i==11 && s[1:2]==loc.indx(2)){return(TRUE)
  }else if(i==12 && s[1:2]==loc.indx(3)){return(TRUE)
  }else if(i==13 && s[1:2]==loc.indx(4)){return(TRUE)
  }else {return(FALSE)}
}
Action.space <- function(i){
  # expects only i from 7 to 13. ie, composite actions [subtasks]
  if(i==7){return(c(8,9))
  }else if(i==8){return(c(5,10:13))
  }else if(i==9){return(c(6,10:13))
  }else {return(1:4)} # == else if(i is in 10:13)
}
#---------------------------------------------------
EvaluateMaxNode <- function(i, s, V, C){
  if(any(i==1:6)){ # if i is a primitive max node
    # no update required as it's already updated in the main function
    return(list(V, C))
  }else{ # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
    actions <- Action.space(i)
    for(a in actions){
      out <- EvaluateMaxNode(a, s, V, C)
      V <- out[[1]]
      C <- out[[2]]
    }
    V[i,s] <- max(V[actions,encode(s)]+C[[i]][encode(s),actions])
    return(list(V, C))
  }
}
#---------------------------------------------------
Policy <- function(i,s,V,C){
  # Epsilon-greedy policy
  actions <- Action.space(i)
  if(runif(1) < .1){return(sample(actions,1))} # epsilon greedy
  return(actions[nnet::which.is.max(V[actions,encode(s)]+C[[i]][encode(s),actions])])
}
#---------------------------------------------------
MAXQ0.learn <- function(i, s, V, C){
  # if it's primitive
  if(i <=6){
    sr <- step(s,i)
    V[i,encode(s)] <- (1-alpha) * V[i,encode(s)] + alpha * sr[[2]]
    return(list(1,sr[[1]], V, C))
  }else{
  count = 0
  while(!is.terminal(i,s)){
     a <- Policy(i,s,V,C) 
    #a <- sample(Action.space(i),1)
     out <- MAXQ0.learn(a,s,V,C)
     N <- out[[1]]
     ss <- out[[2]]
     V <- out[[3]]
     C <- out[[4]]
     if(all(ss==c(0,0,0,0))) {return(list(count,ss,V,C))}
     out <- EvaluateMaxNode(i,ss,V,C)
     V <- out[[1]]
     C <- out[[2]]
     C[[i]][encode(s),a] <- (1-alpha) * C[[i]][encode(s),a] + alpha * (DF^N) * V[i,encode(ss)]
     count <- count + N
     s <- ss
  }
  render(s)
  return(list(count,s, V, C))
 }
}
#---------------------------------------------

s0 <- c(3,4,2,4)
render(s0)
#o <- MAXQ0.learn(7,s0, V, C)
#o <- EvaluateMaxNode(7,s0,V,C)
# debug(MAXQ0.learn)
#--------------------------------------------------

MAXQ0.test <- function(i, s0, V, C){
  s <- s0
  for(i in 1:100){
    render(s)
    Sys.sleep(0.1)
    actions <- Action.space(i)
    a <- actions[nnet::which.is.max(V[actions,encode(s)]+C[[i]][encode(s),actions])]
    o <- step(s,a)
    s <- o[[1]]
  }
}
#MAXQ0.test(7,s0,V,C)


MAXQ0.test <- function(i,s0){
  if(any(i==1:6)){ # if i is a primitive max node
    return(list(V[i,encode(s)], i, V, C))
  }else{ # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
    actions <- Action.space(i)
    for(j in actions){
      out <- EvaluateMaxNode(j, s, V, C)
      C <- out[[4]]
      V <- out[[3]]
      a <- out[[2]]
      V[j,encode(s)] <- out[[1]]
    }
    b <- actions[nnet::which.is.max(V[actions,encode(s)]+C[[i]][encode(s),actions])]
    return(list(V[b,encode(s)], b, V, C))
  }
}