best <- function(state, outcome) {
  States<-c("AL","AK","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  poss_outcome<-c("heart attack","heart failure","pneumonia")
  position<-match(state,States)
  if (is.na(position)){
    stop("invalid state")
  }
  outcomef<-match(outcome,poss_outcome)
  if (is.na(outcomef)){
    stop("invalid outcome")
  }
  switch(as.character(outcomef),"1"=corr_out<-11,"2"=corr_out<-17,"3"=corr_out<-23)
  load("Hospital.Name")
  outcome_file<-read.csv("outcome-of-care-measures.csv")## Read outcome data
  correlated<-split(outcome_file[[corr_out]],outcome_file[[7]])
  correlatedState<-split(outcome_file[[2]],outcome_file[[7]])
  position<-match(state,names(correlated))
  new_corr<-as.numeric(as.vector(correlated[[position]]))
  rangeval<-min(new_corr,na.rm = T)
  ##print(rangeval)
  hospital<-numeric(0)
  hospital2<-numeric(0)
  for(i in 1:100){
    hospital2[i]<-match(rangeval,correlatedState[state])
    ##print(outcome_file[[corr_out]])
    if(is.na(hospital2[i])){
      break
    }    
      else{
        hospital[i]<-hospital2[i]
        outcome_file[[corr_out]][hospital[i]]<-0
        i<i+1
      }
    }
  
  ## Check that state and outcome are valid
  HN<-sort(HN[hospital],decreasing = T)
  print(HN)
  HN[1]## Return hospital name in that state with lowest 30-day death
  ## rate
}