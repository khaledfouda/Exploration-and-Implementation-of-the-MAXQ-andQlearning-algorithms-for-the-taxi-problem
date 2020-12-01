library(crayon)
##    1 2 3 4 5
#    +---------+
#   1|R: | : :G|1
#   2| : | : : |2
#   3| : : : : |3
#   4| | : | : |4
#   5|Y| : |B: |5
#    +---------+
#     1 2 3 4 5 
#
#--------------------------------
render <- function(state){
  out <- c("|R: | : :G|",
           "| : | : : |",
           "| : : : : |",
           "| | : | : |",
           "|Y| : |B: |")
  substr(out[state[1]],2*state[2],2*state[2])='#'
  if(state[3]!=5){
    loc <- loc.indx(state[3])
    out[loc[1]] = paste0(substr(out[loc[1]],1,2*loc[2]-1),
                         red(substr(out[loc[1]],2*loc[2],2*loc[2])),
                         substr(out[loc[1]],2*loc[2]+1,11))
  }
  write("+---------+",stdout())
  write(out,stdout())
  write("+---------+",stdout())
}
render(c(1,2,1,1))
#--------------------------------
encode <- function(s){
  # encode the state(a,b,c,d) where a,b,c can have values from 1 to 5 and d can have values
  # from 1 to 4. 
  # then the encoded variable can have values between 1 and 500
  s = s-1
  return(4*(5*(5*s[1]+s[2])+s[2])+s[4])
}
decode <- function(i){
  # the inverse of encode()
  d <- i%%4
  i <- i %/% 4
  c <- i%%5
  i <- i%/%5
  b <- i%%5
  i <- i%/%5
  return(c(i+1,b+1,c+1,d+1))
}
encode(c(3,3,3,3))
decode(250)

#-----------------------------
loc.indx <- function(i){
  # takes a number between 1 and 4 representing (R,G,Y,B) and returns
  # the equivalent (row,col) location
  if(i==1){return(c(1,1))
  }else if(i==2){return(c(1,5))
  }else if(i==3){return(c(5,1))
  }else if(i==4){return(c(5,4))}
  return(c(0,0)) # index other than 1 to 4
}
#------------------------------
hitting.wallQ <- function(r,c,a){
  # true if the action would result in hitting a wall
  if( (a==1 && r==5) || (a==2 && r==1)){return(TRUE)
  }else if (a==3){
    if(c==5){return(TRUE)}
    if(((c==1||c==3)&&(r==5||r==4))||(c==2&&(r==1||r==2))){return(TRUE)}
  }else if(a==4){
    if(c==1){return(TRUE)}
    if(((c==2||c==4)&&(r==5||r==4))||(c==3&&(r==2||r==1))){return(TRUE)}
  }
  return(FALSE)
}
#------------------------------
step(state,action){
  # state is a vector of c(taxi_row, taxi_col, pass_loc, dest_loc)
  # action is an integer from 1 to 10
  # return a list of the reward and next state
  # a next state of c(0,0,0,0) means that he successfully dropped off the passenger
  # and the episode is over.
  if(action==6){
    # if successfully dropping-off the passenger
    if(state[3]==5 && all(loc.indx(state[4])==state[1:2])){
      return(list(c(0,0,0,0), 20))
    }else {return(list(state, -10))}
  #..........
  }else if(action==5){
    # if successfully picking up the passenger
    if(state[3]!=5 && all(loc.indx(state[3])==state[1:2])){
      state[3]=5
      return(list(state,0))
    }else {return(list(state,-10))}
  #..........
  }else if(action==1){
    if(hitting.wallQ(state[1],state[2],action)==FALSE){state[1]=state[1]+1}
    return(list(state,-1))
  #..........
  }else if(action==2){
    if(hitting.wallQ(state[1],state[2],action)==FALSE){state[1]=state[1]-1}
    return(list(state,-1))
  #..............
  }else if(action==3){
    if(hitting.wallQ(state[1],state[2],action)==FALSE){state[2]=state[2]+1}
    return(list(state,-1))
  #.................
  }else if(action==4){
    if(hitting.wallQ(state[1],state[2],action)==FALSE){state[2]=state[2]-1}
    return(list(state,-1))
  }
}
#--------------------------------------------------------
