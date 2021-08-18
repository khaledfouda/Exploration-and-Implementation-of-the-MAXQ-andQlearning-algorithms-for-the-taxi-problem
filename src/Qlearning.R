source("taxi_env.R")


#updates q-values using the value function
update_q_table<-function(prev_state,action,reward,next_state,alpha,gamma,q_table){
  if(next_state==-124){ #qa=0 if state is terminal
    qa<-0
  }
  else{
    qa<-max(q_table[,next_state]) 
  }
  q_table[action,prev_state]<-q_table[action,prev_state] + alpha*(reward+gamma*qa-q_table[action,prev_state])
  return(q_table)
}
#picks actions according to an epsilon greedy policy
epsilon_greedy_policy<-function(state,epsilon,q_table){
  if (runif(1) <epsilon){
    action <-sample(1:6,size=1)
    return(action)
  }
  else{
    action<- which.max(q_table[,state])
    return(action)
  }
  
}

#training function
trainQlearning<-function(alpha,gamma,epsilon){
  #initialize environment and Q-table
  t <- as.proto(taxi.env)
  q_table <- matrix(0, nrow = 6, ncol = 500)
  reward_per_ep<-rep(NA,2000)
  
  #2000 episodes
  for (i in 1:2000){
    r<-0
    prev_state<-t$set.random.state() #start at a random state
    next_state<-0
    
    #Run until reaching terminal state
    while(TRUE){
      prev_state_enc<- t$encode(prev_state)
      action <- epsilon_greedy_policy(prev_state_enc,epsilon,q_table) #choose action
      
      reward<- t$step(a=action) #receive reward
      r<-r+reward
      t$update() 
      next_state<-t$s #retrieve future state
      next_state_enc<-t$encode(next_state)
     
      #update q_table
      q_table<-update_q_table(prev_state_enc, action, reward, next_state_enc, alpha, gamma,q_table)
      
      #break loop if terminal state is reached
      if(identical(-124,next_state_enc)){
        reward_per_ep[i]<-r
        break
      }
      prev_state <- next_state
    }
  }
  return(list(q_table,reward_per_ep))
}

#testing learned q_table with no exploration
testQlearning<-function(q_table){

  #initialize environment 
  t <- as.proto(taxi.env)
  # place agent in random state
  prev_state<-t$set.random.state()
  while(TRUE){
    
    
    prev_state_enc<- t$encode(prev_state)
    action_max<- which.max(q_table[,prev_state_enc]) #take the best action
    
    reward<- t$step(a=action_max)
    
    t$update()
    next_state<-t$s
    
    next_state_enc<-t$encode(next_state)
    
    t$render()
    
    if(identical(-124,next_state_enc)){ #break if terminal state is reached
      break
    }
    
    prev_state <- next_state
  }
}



eps<-trainQlearning(0.4,0.999,0.017)

plot(eps[[2]],type="l",main="Rewards earned per episodes trained",xlab="Episode",ylab="Rewards")

testQlearning(eps[[1]])
