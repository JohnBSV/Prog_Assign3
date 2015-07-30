best <- function(state, outcome) {
  States<-c("AL","AK","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  poss_outcome<-c("heart attack","heart failure","pneumonia")
  outcome_file<-read.csv("outcome-of-care-measures.csv")
  
  ##Estableciendo variebles de trabajo
  Hospital.Names<-as.vector(outcome_file[[2]])
  STATES<-as.vector(outcome_file[[7]])
  HeartA<-outcome_file[[11]]
  HeartF<-outcome_file[[17]]
  pneumonia<-outcome_file[[23]]
  
  ##Creating outcome data frame
  HeartA<-as.numeric(as.vector(HeartA))
  HeartF<-as.numeric(as.vector(HeartF))
  pneumonia<-as.numeric(as.vector(pneumonia))
  outcome.frame<-data.frame(HeartA,HeartF,pneumonia)
  
  ##Establish working var
  switch (outcome,
    "heart attack" = work_outcome<-1,
    "heart failure"= work_outcome<-2,
    "pneumonia"=work_outcome<-3
  )
  ##Split working vars and hospital names
  workvar<-split(outcome.frame[[work_outcome]],STATES)
  Hospital.Names.States<-split(Hospital.Names,STATES)
  
  ## Generate Min
  out_min<-min(workvar[[state]],na.rm = T)
  
  
  ## Find ties
  tie<-sapply(workvar[[state]],match,out_min,nomatch=FALSE)
  tie<-sapply(tie,as.logical)
  
  
  ## Create a list of the best hospitals and sorted
  best_places<-Hospital.Names.States[[state]][tie]
  best_places<-sort(best_places)
  
  ##Print results
  best_places[1]
   
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate 
}