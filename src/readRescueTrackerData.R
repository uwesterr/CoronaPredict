# reads data from rescuetracker api and stores data in data.frame
library(httr)
library(tidyverse)


rescuetrackData <- GET("https://apps.rescuetrack.com/rrb/api/v1/getCapacityDump", add_headers(Cookie = "rt-sso-sid=094eb893-5d4c-4640-9e92-5a795ede04d2"))
rescuetrackDataContent <- content(rescuetrackData)#  %>% as_tibble() %>% unnest()


foldersDf <- rescuetrackDataContent[["folders"]]  #%>% as_tibble(.name_repair = c("universal")) %>% transpose() %>% as_tibble() 


index <- 0
krankenhausData <- tibble(id = double(),  folderName = character(), resourceType = character(),
                          free = double(),  total = double(),  timestamp = date())
krankenhausDataLoop <- tibble(id = 1,  folderName = "character()", resourceType = "character()",
                          free = 1e7,  total = 1e7,  timestamp = as.Date('2020-01-01'))
for (folderIndex in seq(1,length(foldersDf))) {
  folderIndex
  if (length(foldersDf[[folderIndex]][["resources"]])>0) {
    for (resourceIndex in seq(1,length(foldersDf[[folderIndex]][["resources"]]))) {
      for (capacitiesIndex in seq(1,length(foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]]))) {
        index <- index + 1
        krankenhausDataLoop$folderName[1] <- foldersDf[[folderIndex]][["folderName"]]
        krankenhausDataLoop$resourceType <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["resourceTypeName"]]
        krankenhausDataLoop$free <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["free"]]
        krankenhausDataLoop$total <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["total"]]
        krankenhausDataLoop$timestamp <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["timestamp"]]
        krankenhausData <- bind_rows(krankenhausData,krankenhausDataLoop)
        index
      }
    }  
  }

}

# krankenhausWide <- krankenhausData %>% pivot_wider(names_from = resourceType, values_from = c(free, total, timestamp))
