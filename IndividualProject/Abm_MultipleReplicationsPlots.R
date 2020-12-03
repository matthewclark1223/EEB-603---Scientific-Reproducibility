abm<-function(#Specified parameters
  
  Runs=10,
  Individuals=100, #number of total resource users in a population
  
  TotalCarryingCapacity=10000, #total available resource units
  
  StartPercCarryingCapacity = 0.65, #amount of resources available in the landscape at the start in proportion to CC
  
  PercProtected=0.8, #percent of the total resource that's in a protected area
  
  CoopPercStart=0.9, #percent of individuals who start by following the rules at t0
  
  LearningStrategy = "Success Bias", #options are Success Bias & Conformist
  
  TimeSteps=30,
  ResourceRegenerationPerTimeStep=1.3,
  harvestMax=17,
  ProbOfMobility=0.6){
  
  ## Create output dfs
  
  outputmeanTimeProtect<-as.data.frame(matrix(NA,TimeSteps,Runs)) #output df for mean time harvesting from protected area
  names(outputmeanTimeProtect)<- paste("Run", 1:Runs, sep="")
  
  outputmeanTimeWorking<-as.data.frame(matrix(NA,TimeSteps,Runs))#output df for mean time harvesting from working area
  names(outputmeanTimeWorking)<- paste("Run", 1:Runs, sep="")
  
  outputNumResourcesProtect<-as.data.frame(matrix(NA,TimeSteps,Runs))#output df for resources in protected area
  names(outputNumResourcesProtect)<- paste("Run", 1:Runs, sep="")
  
  outputpercCCProtect<-as.data.frame(matrix(NA,TimeSteps,Runs))#output df for % of CC in protected area
  names(outputpercCCProtect)<- paste("Run", 1:Runs, sep="")
  
  outputNumResourcesWorking<-as.data.frame(matrix(NA,TimeSteps,Runs))#output df for resources in working area
  names(outputNumResourcesWorking)<- paste("Run", 1:Runs, sep="")
  
  outputpercCCWorking<-as.data.frame(matrix(NA,TimeSteps,Runs))#output df for % of CC in working area
  names(outputpercCCWorking)<- paste("Run", 1:Runs, sep="")
  
  outputmeanPayoff<-as.data.frame(matrix(NA,TimeSteps,Runs)) #output df for mean payoff
  names(outputmeanPayoff)<- paste("Run", 1:Runs, sep="")
  
  
  
  for (r in 1:Runs){
    PercWorking= 1-PercProtected #percent of resource in a working landscape
    TotalCCResourceProtected=PercProtected*TotalCarryingCapacity #total resource units in carrying capacity of protected area
    TotalCCResourceWorking=PercWorking*TotalCarryingCapacity #total resource units in carrying capacity of working area
    StartResourceWorking = TotalCCResourceWorking*StartPercCarryingCapacity #number of resources in the working landscape starting
    StartResourceProtected = TotalCCResourceProtected*StartPercCarryingCapacity #number of resources in the protected landscape starting
    CoopNumStart= as.integer(CoopPercStart*Individuals) #number of individuals cooperating fully at t0
    DefNumStart= Individuals - CoopNumStart#number of individuals defecting at t0
    PercTimeProtected = c(rep(0,CoopNumStart),rbeta(DefNumStart,1,2)) #percent of their foraging time each indv spends in the PA
    PercTimeWorking = (1-PercTimeProtected)#percent of their foraging time each indv spends in the working landscape
    
    agents<-data.frame(PercTimeProtected,
                       PercTimeWorking,
                       PayoffProtectedLastTime = rep(NA,Individuals),
                       PayoffWorkingLastTime= rep(NA,Individuals))   #dataframe to be filled with initial payoffs
    
    ProtectPerDefect<-ifelse(DefNumStart ==0,as.integer(StartResourceProtected),as.integer(StartResourceProtected/DefNumStart)) #protected area resources per each individual harvesting there
    WorkingPerTotal<-as.integer(StartResourceWorking/Individuals) #working landscape resources per individual
    
    for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
      agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
      agents[i,4]<-rbinom(1,as.integer(WorkingPerTotal),agent2$PercTimeWorking)
      agents[i,4]<-ifelse(agents[i,4]>=harvestMax,harvestMax,agents[i,4])
      agents[i,3]<- ifelse(agents[i,4]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
      agents[i,3]<-ifelse(agents[i,3]>harvestMax-agents[i,4],harvestMax-agents[i,4],agents[i,3])
    } #see what they get from the working landscape first
    
    
    
    agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
    
    
    
    
    outputmeanTimeProtect[1,r] <- mean(agents$PercTimeProtected)  #fill it with the outouts from time 1
    outputmeanTimeWorking[1,r]  <- mean(agents$PercTimeWorking) 
    outputNumResourcesProtect[1,r] <- StartResourceProtected-sum(agents$PayoffProtectedLastTime) 
    outputNumResourcesWorking[1,r] <- StartResourceWorking-sum(agents$PayoffWorkingLastTime) 
    outputpercCCProtect[1,r]  <- outputNumResourcesProtect[1,r]/TotalCCResourceProtected
    outputpercCCWorking[1,r]  <- outputNumResourcesWorking[1,r]/TotalCCResourceWorking
    outputmeanPayoff[1,r]  <- mean(agents$PayoffTotalLastTime) 
    
    
    
    for (t in 2:TimeSteps){  #run the abm for all o fthe time steps
      
      #choose new strategy  
      
      LastTimeAgents<-agents  #save the agents from the pervious time to a new df
      
      LastTimePercTimeProtected<-LastTimeAgents$PercTimeProtected  #pull out the previous pretected foraging times as vector
      LastTimePercWorking<-LastTimeAgents$PercTimeWorking  #pull out the previous working foraging times as vector
      
      PercTimeProtected<-rep(NA,length(PercTimeProtected))  #nullify the previous perc time protected vector so we can fill it later and be confident the values changed
      
      if(LearningStrategy == "Success Bias"){ 
        for (j in 1:nrow(LastTimeAgents)){  
          ThisAgent<-LastTimeAgents[j,]      #modify the foraging strategy of each agent individually
          OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
          ThisAgentPayoff<-ThisAgent$PayoffTotalLastTime   #payoff last time period for this agent
          OtherAgentSpecific<-OtherAgents[sample(nrow(OtherAgents),1),] #choose a specific agent for them to get paired up with
          OtherAgentPayoff<-OtherAgentSpecific$PayoffTotalLastTime  #Payoff the other agent recieved
          OtherAgentPercProtect<-OtherAgentSpecific$PercTimeProtected #strategy of other agent
          ThisAgentPercProtect<-ThisAgent$PercTimeProtected  #strategy of this agent
          
          PercTimeProtected[j]<-ifelse(ThisAgentPayoff >=OtherAgentPayoff,ThisAgent$PercTimeProtected,
                                       (ThisAgentPercProtect+((OtherAgentPercProtect-ThisAgentPercProtect)/2))) #vector of new strategies
          
          #if this agent did better than the randomly selected agent then they keep their strategy
          #if they did worse thnan they modify their strategy to be the difference between their previou sstrategy and the randomly selected agent
          
        }}
      #if(LearningStrategy == "Conformist"){
      # for (j in 1:nrow(LastTimeAgents)){  
      #  ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
      # OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
      #OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
      #OtherAgentsPercProtect<-mean(OtherAgentsSample$PercTimeProtected )# mean strategy of other agents
      #ThisAgentPercProtect<-ThisAgent$PercTimeProtected  #strategy of this agent
      
      #PercTimeProtected[j]<-(ThisAgentPercProtect+((OtherAgentsPercProtect-ThisAgentPercProtect)/2)) #vector of new strategies
      #}}
      
      #   if(LearningStrategy == "Conformist"){
      #    for (j in 1:nrow(LastTimeAgents)){  
      #     ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
      #    OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
      #   OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
      #  ThisAgentPercProtect<-ThisAgent$PercTimeProtected
      # if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 & ThisAgentPercProtect ==0.0){PercTimeProtected[j]<-rbeta(1,1,2)}
      #if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 & ThisAgentPercProtect >0.0){PercTimeProtected[j]<-ThisAgentPercProtect}
      #    if(length(which(OtherAgentsSample$PercTimeProtected==0.0))>2){PercTimeProtected[j]<-0.0}
      #  }}
      
      if(LearningStrategy == "Conformist"){
        for (j in 1:nrow(LastTimeAgents)){  
          ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
          OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
          OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
          ThisAgentPercProtect<-ThisAgent$PercTimeProtected
          if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 ){
            PercTimeProtected[j]<-mean(OtherAgentsSample[OtherAgentsSample$PercTimeProtected!=0.0,]$PercTimeProtected )# mean strategy of other agents
          }
          if(length(which(OtherAgentsSample$PercTimeProtected==0.0))>2){PercTimeProtected[j]<-0.0}
        }}
      
      PercTimeWorking<- 1-PercTimeProtected   #time foraging in the working landscape
      
      
      agents<-data.frame(PercTimeProtected,   #new agent dataframe to fill
                         PercTimeWorking,
                         PayoffProtectedLastTime = rep(NA,Individuals),
                         PayoffWorkingLastTime= rep(NA,Individuals)) 
      
      
      #New resource pools to pull from
      #make sure they dont regenerate past their carrying capacity
      NewProtectedResourcesTotal<-round(outputNumResourcesProtect[t-1,r] * ResourceRegenerationPerTimeStep,digits=0)
      ProtectedResourcesOverCC<-round(NewProtectedResourcesTotal-TotalCCResourceProtected,digits=0)
      ProtectedResourcesOverCC<- ifelse(ProtectedResourcesOverCC<=  0,0, ProtectedResourcesOverCC)
      
      NewWorkingResourcesTotal<-round(outputNumResourcesWorking[t-1,r] * ResourceRegenerationPerTimeStep,digits=0)
      WorkingResourcesOverCC<-round(NewWorkingResourcesTotal-TotalCCResourceWorking,digits=0)
      WorkingResourcesOverCC<- ifelse(WorkingResourcesOverCC<=  0,0, WorkingResourcesOverCC)
      
      #make it so only new resources can leave 
      LeaveWorking<-rbinom(1, WorkingResourcesOverCC,ProbOfMobility) #number of resources which leave the protected area
      LeaveProtected<-rbinom(1,ProtectedResourcesOverCC,ProbOfMobility)#number of resources which leave the working area
      
      
      #do the accounting on entering vs leaving individuals
      NewWorkingResourcesTotal<-NewWorkingResourcesTotal+LeaveProtected
      NewProtectedResourcesTotal<-NewProtectedResourcesTotal+LeaveWorking
      
      #Make sure they dont go over the CC again
      NewWorkingResourcesTotal<-ifelse(NewWorkingResourcesTotal<=TotalCCResourceWorking,NewWorkingResourcesTotal,TotalCCResourceWorking)
      NewProtectedResourcesTotal<-ifelse(NewProtectedResourcesTotal<=  TotalCCResourceProtected,NewProtectedResourcesTotal, TotalCCResourceProtected)
      
      ##
      
      
      ##calculate available resources per individual
      if(nrow(agents[agents$PercTimeProtected>0,])==0){ProtectPerDefect<-NewProtectedResourcesTotal}
      if(nrow(agents[agents$PercTimeProtected>0,])!=0){ProtectPerDefect<-as.integer(NewProtectedResourcesTotal/nrow(agents[agents$PercTimeProtected>0,]))}
      WorkingPerTotal<-as.integer(NewWorkingResourcesTotal/Individuals)
      
      
      for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
        agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
        agents[i,4]<-rbinom(1,as.integer(WorkingPerTotal),agent2$PercTimeWorking)
        agents[i,4]<-ifelse(agents[i,4]>=harvestMax,harvestMax,agents[i,4])
        agents[i,3]<- ifelse(agents[i,4]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
        agents[i,3]<-ifelse(agents[i,3]>harvestMax-agents[i,4],harvestMax-agents[i,4],agents[i,3])
      } #see what they get from the working landscape first
      
      
      
      agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
      
      outputmeanTimeProtect[t,r] <- mean(agents$PercTimeProtected)  #fill it with the outouts from time 1
      outputmeanTimeWorking[t,r]  <- mean(agents$PercTimeWorking) 
      outputNumResourcesProtect[t,r] <- NewProtectedResourcesTotal-sum(agents$PayoffProtectedLastTime) 
      outputNumResourcesWorking[t,r]  <- NewWorkingResourcesTotal-sum(agents$PayoffWorkingLastTime)
      outputpercCCProtect[t,r]  <- outputNumResourcesProtect[t,r]/TotalCCResourceProtected
      outputpercCCWorking[t,r]  <- outputNumResourcesWorking[t,r]/TotalCCResourceWorking
      outputmeanPayoff[t,r]  <- mean(agents$PayoffTotalLastTime)
      
    }
    par(mfrow=c(2,2))
    
    #mean payoff
    plot(rowMeans(outputmeanPayoff), type = 'l', ylab = "Harvest Payoff", xlab = "Timestep", ylim=c(0,harvestMax), lwd = 3, main = "Mean Payoff")
    
    for (j in 1:Runs) {  
      
      lines(outputmeanPayoff[,j], type = 'l')  # add lines for each run, up to r_max
      
    }
    
    #% CC time working
    plot(rowMeans(outputmeanTimeWorking), type = 'l', ylab = "Effort in Working Landscape", xlab = "Timestep", ylim=c(0,1), lwd = 3, main = "Average Effort in Working (coop)")
    
    for (j in 1:Runs) {  
      
      lines(outputmeanTimeWorking[,j], type = 'l')  # add lines for each run, up to r_max
      
    }
    
    #% CC protected
    plot(rowMeans(outputpercCCProtect), type = 'l', ylab = "Carrying Capacity", xlab = "Timestep", ylim=c(0,1), lwd = 3, main = "Percent CC Protected")
    
    for (j in 1:Runs) {  
      
      lines(outputpercCCProtect[,j], type = 'l')  # add lines for each run, up to r_max
      
    }
    
    #% CC working
    plot(rowMeans(outputpercCCWorking), type = 'l', ylab = "Carrying Capacity", xlab = "Timestep", ylim=c(0,1), lwd = 3, main = "Percent CC Working")
    
    for (j in 1:Runs) {  
      
      lines(outputpercCCWorking[,j], type = 'l')  # add lines for each run, up to r_max
      
    }

    
  }
  outputmeanPayoff$Timestep<-c(1:TimeSteps)
  outputmeanTimeProtect$Timestep<-c(1:TimeSteps)
  outputmeanTimeWorking$Timestep<-c(1:TimeSteps)
  outputNumResourcesProtect$Timestep<-c(1:TimeSteps)
  outputNumResourcesWorking$Timestep<-c(1:TimeSteps)
  outputpercCCProtect$Timestep<-c(1:TimeSteps)
  outputpercCCWorking$Timestep<-c(1:TimeSteps)
  outputmeanPayoff$Timestep<-c(1:TimeSteps)
  
  
  FullOutput<-list(meanPayoff=outputmeanPayoff,
                   meanTimeProtect=outputmeanTimeProtect,
                   meanTimeWorking=outputmeanTimeWorking,
                   NumResourcesProtect=outputNumResourcesProtect,
                   NumResourcesWorking=outputNumResourcesWorking,
                   percCCProtect=outputpercCCProtect,
                   percCCWorking=outputpercCCWorking,
                   meanPayoff=outputmeanPayoff)
  
  FullOutput<<-FullOutput
  
  
}









