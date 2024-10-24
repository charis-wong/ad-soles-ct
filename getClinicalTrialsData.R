getADClinicalTrialsData <- function(drug){
  drug <- str_replace(drug, "\\s", "+")
  
  url1<- paste0("https://clinicaltrials.gov/api/v2/studies?query.cond=%28alzheimer+dementia%29+OR+%28alzheimer+disease%29&query.intr=", 
                drug, 
                "&fields=NCTId%7CBriefTitle%7CCondition%7CInterventionName%7CInterventionDescription%7CStudyType%7CPhase%7COverallStatus",
                "&countTotal=true&pageSize=1000")
  
  xmldata<-GET(url1)
  data <- fromJSON(httr::content(xmldata, "text"))
  results <- data$studies%>%as.data.frame()
  try({
    results<-results%>%
      unnest(cols=c("protocolSection"))
    results<-results%>%
      unnest(cols=c(names(results)))
    
    results[,4] <- apply(results[,4], 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" | ")))
    
    results <- results%>%
      mutate(row = row_number())
    interventions <-results$interventions
    
    interventionDescription = data.frame(row = c(), description = c())
    for(i in 1:length(interventions)){
      dat <- interventions[[i]]
      if(any(names(dat)%in%'description')==TRUE){
        interventionDescription_i <- data.frame(row = i, description = paste(dat$description, collapse = "|"))
        interventionDescription = rbind(interventionDescription, interventionDescription_i)
      }
    }
    
    results <- left_join(results, interventionDescription, by = "row")%>%
      select(-row, -interventions)
  }, silent = TRUE)
  if(nrow(results)==0) results <- NULL 
  return(results)
} 



getOtherNeuroClinicalTrialsData <- function(drug){
  drug <- str_replace(drug, "\\s", "+")
  
  url1<- paste0("https://clinicaltrials.gov/api/v2/studies?query.cond=%28vascular+dementia%29+OR+%28cerebral+small+vessel+disease%29+OR+%28neurodegenerative+disease%29+OR+%28stroke%29&query.intr=", 
                drug, 
                "&fields=NCTId%7CBriefTitle%7CCondition%7CInterventionName%7CInterventionDescription%7CStudyType%7CPhase%7COverallStatus",
                "&countTotal=true&pageSize=1000") 
  
  
  xmldata<-GET(url1)
  data <- fromJSON(httr::content(xmldata, "text"))
  results <- data$studies%>%as.data.frame()
  
  try({
    results<-results%>%
      unnest(cols=c("protocolSection"))
    results<-results%>%
      unnest(cols=c(names(results)))
    
    results[,4] <- apply(results[,4], 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" | ")))
    
    results <- results%>%
      mutate(row = row_number())
    interventions <-results$interventions
    
    interventionDescription = data.frame(row = c(), description = c())
    for(i in 1:length(interventions)){
      dat <- interventions[[i]]
      if(any(names(dat)%in%'description')==TRUE){
        interventionDescription_i <- data.frame(row = i, description = paste(dat$description, collapse = "|"))
        interventionDescription = rbind(interventionDescription, interventionDescription_i)
      }
    }
    
    results <- left_join(results, interventionDescription, by = "row")%>%
      select(-row, -interventions)
  }, silent = TRUE)
  if(nrow(results)==0) results <- NULL 
  return(results)
} 

