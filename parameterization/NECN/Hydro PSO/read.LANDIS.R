
read.LANDIS <- function(file1="NECN-succession-log-short.csv", file2="NECN-calibrate-log.csv") {
  
  #first agb values
  data_bio <- read.table(file1, sep=",",header=T, fill=TRUE)
  
  data75_bio <- c(75, max(data_bio$AGB[51:75], na.rm = TRUE))
  data150_bio <- c(150, max(data_bio$AGB[126:150], na.rm = TRUE))
  
  data_results_bio <- rbind(data75_bio, data150_bio)
  
  #now lai values
  data_lai <- read.table(file2, sep=",",header=T, fill=TRUE) %>% 
    filter(Month==7)
  
  data75_lai <- c(75, max(data_lai$TreeLAI[51:75], na.rm = TRUE))
  data150_lai <- c(150, max(data_lai$TreeLAI[126:150], na.rm = TRUE))
  
  data_results_lai <- rbind(data75_lai, data150_lai)
  
  sim <- rbind(data_results_bio, data_results_lai)[,2] %>% as.vector()
  
  return(sim)
  
}
