source("taxi_env.R")


#initializing q_table

t <- as.proto(taxi.env)
q_table <- matrix(0, nrow = 6, ncol = 500)

update_q_table<-function(prev_state,action,reward,next_state,alpha,gamma,q_table){
  if(next_state==-124){
    qa<-0
  }
  else{
    qa<-max(q_table[,next_state])
  }
  q_table[action,prev_state]<-q_table[action,prev_state] + alpha*(reward+gamma*qa-q_table[action,prev_state])
  return(q_table)
}

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
trainQlearning<-function(alpha,gamma,epsilon){
  t <- as.proto(taxi.env)
  q_table <- matrix(0, nrow = 6, ncol = 500)
  reward_per_ep<-rep(NA,2000)
  for (i in 1:2000){
    r<-0
    prev_state<-t$set.random.state()
    next_state<-0
    while(TRUE){
      prev_state_enc<- t$encode(prev_state)
      
      action <- epsilon_greedy_policy(prev_state_enc,epsilon,q_table)
      
      reward<- t$step(a=action)
      r<-r+reward
      t$update()
      next_state<-t$s
      next_state_enc<-t$encode(next_state)
     
      
      q_table<-update_q_table(prev_state_enc, action, reward, next_state_enc, alpha, gamma,q_table)
      if(identical(-124,next_state_enc)){
        reward_per_ep[i]<-r
        break
      }
      
      prev_state <- next_state
      
      
    }
    
  }
  return(list(q_table,reward_per_ep))
}

#testing learned q_table
testQlearning<-function(q_table){
  t <- as.proto(taxi.env)
  prev_state<-t$set.random.state()
  while(TRUE){
    
    
    prev_state_enc<- t$encode(prev_state)
    action_max<- which.max(q_table[,prev_state_enc])
    
    reward<- t$step(a=action_max)
    
    t$update()
    next_state<-t$s
    
    next_state_enc<-t$encode(next_state)
    
    t$render()
    
    if(identical(-124,next_state_enc)){
      break
    }
    
    prev_state <- next_state
  }
}



eps<-trainQlearning(0.4,0.999,0.017)

plot(eps[[2]],type="l",main="Rewards earned per episodes trained",xlab="Episode",ylab="Rewards")
