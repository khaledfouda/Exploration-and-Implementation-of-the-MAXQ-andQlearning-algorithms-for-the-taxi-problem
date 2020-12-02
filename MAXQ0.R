V <- matrix(runif(10*500),10,500)
V <- matrix(0,10,500)

Q <-list()
for(i in 1:10){
  Q[[i]] <- matrix(0,500,10)
}
C <-list()
for(i in 1:10){
  C[[i]] <- matrix(0,500,10)
}
alpha <- .01
DF <- .9
#--------------------------------------
end.stateQ <- function(i,s){
  # returns TRUE if s is an end state for the subtask i.
  if((i==7||i==9)&&s==c(0,0,0,0)){return(TRUE)
  }else if(i==8 && s[3]==5){return(TRUE)
  }else if(i==9 && s[3]!=5){return(TRUE)
  }else if(any(i==1:6)){return(TRUE)
  }else if(i==10 && loc.indx(s[3])==s[1:2] ){return(TRUE)
  }else if(i==10 && s[3]==5 && loc.indx(s[4])==s[1:2]){return(TRUE)}
  return(FALSE)
}
A.space <- function(i){
  if(i==7){return(c(8,9))}
  if(i==8){return(c(5,10))}
  if(i==9){return(c(6,10))}
  if(i==10){return(1:4)}
}
#---------------------------------------------------
EvaluateMaxNode <- function(i, s, V, C){
  if(any(i==1:6)){ # if i is a primitive max node
    return(list(V[i,encode(s)], i, V, C))
  }else{ # if i is a composite max node: return max(in a) of Q[i,s,a]=V[a,s]+C[i,s,a]
    actions <- A.space(i)
    for(j in actions){
      out <- EvaluateMaxNode(j, s, V, C)
      C <- out[[4]]
      V <- out[[3]]
      a <- out[[2]]
      V[j,encode(s)] <- out[[1]]
    }
    b <- actions[which.max(V[actions,s]+C[[i]][encode(s),actions])]
    return(list(V[b,encode(s)], b, V, C))
  }
}
#---------------------------------------------------
Policy <- function(i,s,V,C){
  actions <- A.space(i)
  if(runif(1) < .1){return(sample(actions,1))} # epsilon greedy
  return(actions[which.max(V[actions,s]+C[[i]][encode(s),actions])])
}
#---------------------------------------------------
MAXQ.0.learn <- function(i, s, V, C){
  # if it's primitive
  if(i <=6){
    sr <- step(s,i)
    V[i,encode(s)] <- (1-alpha) * V[i,encode(s)] + alpha * sr[[2]]
    return(list(1,sr[[1]], V, C))
  }else{
  count = 0
  while(!end.stateQ(i,s)){
     a <- Policy(i,s,V,C) 
    #a <- sample(A.space(i),1)
     out <- MAXQ.0.learn(a,s,V,C)
     N <- out[[1]]
     ss <- out[[2]]
     V <- out[[3]]
     C <- out[[4]]
     #sr <- step(s,a)
     out <- EvaluateMaxNode(i,ss,V,C)
     estim <- out[[1]]
     V <- out[[3]]
     C <- out[[4]]
     C[[i]][encode(s),a] <- (1-alpha) * C[[i]][encode(s),a] + alpha * (DF^N) * estim
     count <- count + N
     s <- ss
     }
  return(list(count,s, V, C))
 }
}
#---------------------------------------------

s0 <- c(3,4,2,3)
render(s0)
#o <- MAXQ.0.learn(7,s0, V, C)
 o <- EvaluateMaxNode(7,s0,V,C)
# debug(MAXQ.0.learn)