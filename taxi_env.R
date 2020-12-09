library(proto)
library(crayon)

#     1 2 3 4 5
#    +---------+
#   1|R: | : :G|1
#   2| : | : : |2
#   3| : : : : |3
#   4| | :#| : |4
#   5|Y| : |B: |5
#    +---------+
#     1 2 3 4 5 



taxi.env <- proto(expr={
  # avaiable variables
  s = c(-1,-1,-1,-1) #current state, a vector of c(taxi_row, taxi_col, pass_loc, dest_loc)
  ss = c(-1,-1,-1,-1) # next state chosen by step()
  count = 0 # number of steps from the beginning until reaching the terminal state
  # available functions (defined later below)
  init = NA # resets the preceding variables
  render = NA 
  step = NA 
  hitting.wallQ = NA
  encode = NA
  decode = NA
  loc.indx = NA
  set.random.state = NA
  add.seed = NA
  
})
taxi.env$init <- function(.){
  .$s <- c(-1,-1,-1,-1)
  .$ss <- c(-1,-1,-1,-1)
  .$count <- 0
}
taxi.env$render <- function(.){ # prints out the current state .$s
  out <- c("|R: | : :G|",
           "| : | : : |",
           "| : : : : |",
           "| | : | : |",
           "|Y| : |B: |")
  substr(out[.$s[1]],2*.$s[2],2*.$s[2])='#'
  if(.$s[3]!=5){
    ploc <- .$loc.indx(.$s[3])
    dloc <- .$loc.indx(.$s[4])
    if(ploc[1]!= dloc[1]){
      out[ploc[1]] = paste0(substr(out[ploc[1]],1,2*ploc[2]-1),
                            green(substr(out[ploc[1]],2*ploc[2],2*ploc[2])),
                            substr(out[ploc[1]],2*ploc[2]+1,11))
      out[dloc[1]] = paste0(substr(out[dloc[1]],1,2*dloc[2]-1),
                            red(substr(out[dloc[1]],2*dloc[2],2*dloc[2])),
                            substr(out[dloc[1]],2*dloc[2]+1,11))
    }else if(ploc[2]<dloc[2]){
      out[ploc[1]] = paste0(substr(out[ploc[1]],1 ,2*ploc[2]-1),
                            green(substr(out[ploc[1]],2*ploc[2],2*ploc[2])),
                            substr(out[ploc[1]], 2*ploc[2]+1, 2*dloc[2]-1),
                            red(substr(out[dloc[1]],2*dloc[2],2*dloc[2])),
                            substr(out[dloc[1]],2*dloc[2]+1,11))
    }else if(ploc[2]>dloc[2]){
      out[ploc[1]] = paste0(substr(out[dloc[1]],1 ,2*dloc[2]-1),
                            red(substr(out[dloc[1]],2*dloc[2],2*dloc[2])),
                            substr(out[dloc[1]], 2*dloc[2]+1, 2*ploc[2]-1),
                            green(substr(out[ploc[1]],2*ploc[2],2*ploc[2])),
                            substr(out[ploc[1]],2*ploc[2]+1,11))
    }
  }else{
    loc <- .$loc.indx(.$s[4])
    out[loc[1]] = paste0(substr(out[loc[1]],1,2*loc[2]-1),
                         red(substr(out[loc[1]],2*loc[2],2*loc[2])),
                         substr(out[loc[1]],2*loc[2]+1,11))
  }
  write("+---------+",stdout())
  write(out,stdout())
  write("+---------+",stdout())
}

taxi.env$step <- function(.,a){
  # given an action a and using the current state, it maps them to the next state .$ss and a reward (returned)
  # a is an integer from 1 to 6
  .$ss <- .$s
  .$r <- -1
  .$count <- .$count + 1
  # next state of zeros means that he successfully dropped off the passenger and the episode is over.
  if(a==6){
    # if successfully dropping-off the passenger
    if(.$s[3]==5 && all(.$loc.indx(.$s[4])==.$s[1:2])){
      .$ss <- c(0,0,0,0)
      .$r <- 20
    }else {.$r <- -10}
    #..........
  }else if(a==5){
    # if successfully picking up the passenger
    if(.$s[3]!=5 && all(.$loc.indx(.$s[3])==.$s[1:2])){
      .$ss[3]=5
      .$r = 0
    }else {.$r <- -10}
    #..........#North
  }else if(a==1 && .$hitting.wallQ(a)==FALSE){.$ss[1]=.$ss[1]-1
    #..........#South
  }else if(a==2 && .$hitting.wallQ(a)==FALSE){.$ss[1]=.$ss[1]+1
  #................#East
  }else if(a==3 && .$hitting.wallQ(a)==FALSE){.$ss[2]=.$ss[2]+1
  #...............#west
  }else if(a==4 && .$hitting.wallQ(a)==FALSE){.$ss[2]=.$ss[2]-1}
  return(.$r)
}

taxi.env$hitting.wallQ <- function(.,a){
  # true if the action would result in hitting a wall
  if( (a==1 && .$s[1]==1) || (a==2 && .$s[1]==5)){return(TRUE)
  }else if (a==3){
    if(.$s[2]==5){return(TRUE)}
    if(((.$s[2]==1||.$s[2]==3)&&(.$s[1]==5||.$s[1]==4))||(.$s[2]==2&&(.$s[1]==1||.$s[1]==2))){return(TRUE)}
  }else if(a==4){
    if(.$s[2]==1){return(TRUE)}
    if(((.$s[2]==2||.$s[2]==4)&&(.$s[1]==5||.$s[1]==4))||(.$s[2]==3&&(.$s[1]==2||.$s[1]==1))){return(TRUE)}
  }
  return(FALSE)
}
taxi.env$encode <- function(.,s){
  # encode the state(a,b,c,d) where a,b,c can have values from 1 to 5 and d can have values
  # from 1 to 4. 
  # then the encoded variable can have values between 1 and 500
  s = s-1
  return(4*(5*(5*s[1]+s[2])+s[3])+s[4]+1)
}
taxi.env$decode <- function(.,i){
  # the inverse of encode()
  i <- i-1
  d <- i%%4
  i <- i %/% 4
  c <- i%%5
  i <- i%/%5
  b <- i%%5
  i <- i%/%5
  return(c(i+1,b+1,c+1,d+1))
}
taxi.env$loc.indx <- function(.,i){
  # takes a number between 1 and 4 representing (R,G,Y,B) and returns
  # the equivalent (row,col) location
  if(i==1){return(c(1,1))
  }else if(i==2){return(c(1,5))
  }else if(i==3){return(c(5,1))
  }else if(i==4){return(c(5,4))}
  return(c(0,0)) # index other than 1 to 4
}
taxi.env$set.random.state <- function(., set=TRUE){ # sets .$s to a random state or return it
  if (set==TRUE){
    .$s <- c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1))
  }else{
    return(c(sample(1:5,1),sample(1:5,1),sample(1:4,1),sample(1:4,1)))
  }
}
taxi.env$add.seed <- function(.,seed){set.seed(seed)}
taxi.env$update <- function(.){.$s <- .$ss}
#------------------------------------------------
# Usage :
#t <- taxi.env$proto(0)
#t$set.random.state(); t$render()
#t$step(a=1); t$update(); t$render()
# t$s <- t$decode(150); t$render()
#$add.seed(12)