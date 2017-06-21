#######################
#      FUNCTIONS      #
#######################

#FUNCTION OF ONE FISH SWIMMING IN A BOUNDED AREA WITH ONE "TRAP"
## OUTPUTS: 
### 0-FISH WAS NOT CAUGHT
### 1-FISH WAS CAUGHT
sampling<-function(xmax=8,
                   ymax=4,
                   steps=25,
                   trap=c(2,3))
{
  # INITIAL POSITION OF THE FISH (W/IN A CLOSED, BOUNDED 9 x 5 LATTICE)
  pos<-matrix(0,nrow=2,ncol=steps)
  pos[1,1]<-sample(0:xmax, 1)
  pos[2,1]<-sample(0:ymax, 1)
  
  # VECTOR OF MOVEMENT CHOICES 
  choice<-sample(1:4, steps-1, replace=T)
  
  # FISH MOVEMENT
  ##  FISH HAVE EQUAL PROBABILITY OF MOVING IN 1 of 4 DIRECTIONS
  ##  (UPSTREAM, DOWNSTREAM, LEFT, RIGHT) UNLESS:
  ##    1. FISH IN A CORNER -- EQUAL PROBABILITY OF MOVING ALONG THE BOUNDARY
  ##        UP/DOWNSTREAM (whichever appropriate) AS THEY DO MOVING ALONG
  ##        THE BOUNDARY CROSS-STREAM
  ##    2. FISH ON THE BOUNDARY -- HAVE EQUAL PROBABILITY TO MOVE AWAY FROM
  ##        THE BOUNDARY AS THEY DO ALONG THE BOUNDARY
  ##    3. FISH IN A "TRAP" -- STAY IN THE TRAP, DO NOT MOVE
  for(t in 1:(steps-1))
  {
    if(all(pos[,t]==trap)) pos[,t+1]<-pos[,t] #TRAP
    if(!all(pos[,t]==trap)) #NOT IN A TRAP
    {
      if(pos[2,t]!=0 & pos[2,t]!=ymax) #MIDDLE OF STREAM
      {
        if(choice[t]==3) pos[,t+1]<-(pos[,t]+c(0,1)) #LEFT
        if(choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,-1)) #RIGHT
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0)) #UPSTREAM
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0)) #DOWNSTREAM IF AT UPSTREAM BOUNDARY
        }
        if(choice[t]==2)
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0)) #DOWNSTREAM
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0)) #UPSTREAM IF AT DOWNSTREAM BOUNDARY
        }
      }
      if(pos[2,t]==0) #RIGHT BOUNDARY
      {
        if(choice[t]==3|choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,1)) #LEFT
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0)) #UPSTREAM
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0))#DOWNSTREAM IF AT UPSTREAM BOUNDARY
        }
        if(choice[t]==2) 
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0)) #DOWNSTREAM
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0))#UPSTREAM IF AT DOWNSTREAM BOUNDARY
        }
      }
      if(pos[2,t]==ymax) # LEFT BOUNDARY
      {
        if(choice[t]==3|choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,-1)) #RIGHT
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0)) #UPSTREAM
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0)) #DOWNSTREAM IF AT UPSTREAM BOUNDARY
        }
        if(choice[t]==2)
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0)) #DOWNSTREAM
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0)) #UPSTREAM IF AT DOWNSTREAM BOUNDARY
        }
      }
    }
  }
  catch<-ifelse(all(pos[,steps]==trap),1,0) #IF IN THE "TRAP" WE CAUGHT THE FISH!
  return(catch)
  #return(list(catch=catch, position=pos, move=choice))
}


#SAME FUNCTION AS ABOVE EXCEPT WITH 2 "TRAPS" AND DIFFERENT OUTPUTS:
## OUTPUTS: 
### A VECTOR OF LENGTH 3 OF CATCHES (0 NO CATCH; 1 CATCH)
#### OUT[1] OVERALL CATCH
#### OUT[2] "TRAP1" CATCH
#### OUT[3] "TRAP2" CATCH

sampling2<-function(xmax=8,
                   ymax=4,
                   steps=25,
                   trap1=c(2,3),
                   trap2=c(5,1))
{
  pos<-matrix(0,nrow=2,ncol=steps)
  pos[1,1]<-sample(0:xmax, 1)
  pos[2,1]<-sample(0:ymax, 1)
  
  choice<-sample(1:4, steps-1, replace=T)
  for(t in 1:(steps-1))
  {
    if(all(pos[,t]==trap1)|all(pos[,t]==trap2)) pos[,t+1]<-pos[,t]
    if(!(all(pos[,t]==trap1)|all(pos[,t]==trap2)))
    {
      if(pos[2,t]!=0 & pos[2,t]!=ymax)
      {
        if(choice[t]==3) pos[,t+1]<-(pos[,t]+c(0,1))
        if(choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,-1))
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0))
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0))
        }
        if(choice[t]==2)
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0))
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0))
        }
      }
      if(pos[2,t]==0)
      {
        if(choice[t]==3|choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,1))
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0))
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0))
        }
        if(choice[t]==2)
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0))
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0))
        }
      }
      if(pos[2,t]==ymax)
      {
        if(choice[t]==3|choice[t]==4) pos[,t+1]<-(pos[,t]+c(0,-1))
        if(choice[t]==1)
        {
          if(pos[1,t]!=xmax) pos[,t+1]<-(pos[,t]+c(1,0))
          if(pos[1,t]==xmax) pos[,t+1]<-(pos[,t]+c(-1,0))
        }
        if(choice[t]==2)
        {
          if(pos[1,t]!=0) pos[,t+1]<-(pos[,t]+c(-1,0))
          if(pos[1,t]==0) pos[,t+1]<-(pos[,t]+c(1,0))
        }
      }
    }
  }
  catch<-ifelse(all(pos[,steps]==trap1)|all(pos[,steps]==trap2),1,0)
  catch1<-ifelse(all(pos[,steps]==trap1),1,0)
  catch2<-ifelse(all(pos[,steps]==trap2),1,0)
  out<-c(catch, catch1, catch2)
  return(out)
  #return(list(catch=catch, position=pos, move=choice))
}
