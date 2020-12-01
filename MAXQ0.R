end.stateQ <- function(i,s){
  # returns TRUE if s is an end state for the subtask i.
  if((i==7||i==9)&&s==c(0,0,0,0)){return(TRUE)
  }else if(i==8 && s[3]==5){return(TRUE)}
  return(FALSE)
}
A.space <- function(i){
  if(i==7){return(c(8,9))}
  if(i==8){return(c(5,10))}
  if(i==9){return(c(6,10))}
  if(i==20){return(1:4)}
}

MAXQ.0.learn <- function(i, s){
  # if it's primitive
  if(i <=6){
    sr <- step(s,i)
    V[i,encode(s)] <- (1-alpha) * V[i,encode(s)] + alpha * sr[[2]]
    return(1)
  }else{
   count = 0
   while(!end.stateQ(i,s)){
     a <- sample(A.space(i),1)
     N <- MAXQ.0.learn(a,s)
     sr <- step(s,i)
     C[i,encode(s),a] <- (1-alpha) * C[i,encode(s),a] + alpha * (DF^N) * V[i,encode(sr[[1]])]
     count <- count + N
     s <- sr[[1]]
   }
   return(count)
 }
}
