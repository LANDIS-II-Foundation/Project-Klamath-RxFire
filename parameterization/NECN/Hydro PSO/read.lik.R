

read.lik <- function(sim,obs) {

  matchup_df <- na.omit(data.frame(sim, obs))

  sim1 <- matchup_df[,1]
  obs1 <- matchup_df[,2]
  
  compare_all<-NSE(sim1,obs1, na.rm=TRUE)
  
  return(as.numeric(compare_all))

}


