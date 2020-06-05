# This script transform IDFG snorkel data from long to wide form

#The original data has all observations in a single row and thus multiple rows per survey
#The output from this file has a single row representing all data from a survey event
getwd()
Snorkel_long <- read.csv("Snorkel_2003to2019.csv")
#look at column names
colnames(Snorkel_long)
#lots of columns we don't need

#reduce dataset to important columns
library(dplyr)
Snorkel_long <- dplyr::select(Snorkel_long, FishSurveyID, SectionID, Year, Strata, Section, SurveyDate, FishPresDesc, TransectLength, 
              MeanWidth, SectionArea, Method, SName,SumNumberCounted,  LengthGroup, NewLong, NewLat, 
              Drainage,Huc4ID, Huc4Name,Huc5ID, Huc5Name,Huc6ID,Huc6Name, SiteDescription, SurveyNotes) 
colnames(Snorkel_long)


#first combine species and length to make a new ID column for species/length bin 
Snorkel_long$Species_LBin <- paste0(Snorkel_long$SName," ", Snorkel_long$LengthGroup)
Snorkel_long$Species_LBin 
Snorkel_long$Species_LBin  <- gsub(" ", "", Snorkel_long$Species_LBin)
#remove extra spaces at end of column

#prep condensed data
sort(unique(Snorkel_long$SName))
#create lists of Snorkel_long$SName values that allign with each condensed column
ChinookList <- c("Chinook Salmon (Hatchery)", "Chinook Salmon (Summer Run)", "Chinook Salmon (Spring Run)", "Chinook Salmon",
                 "Chinook Salmon (Spring/Summer Run)")
SteelheadList <- c("Steelhead (Hatchery)", "Steelhead (Snake River Basin)", "Steelhead (A Run)")

BrookBullCutRainbowList <- c("Brook Trout", "Brook Trout X Bull Trout", "Bull Trout","Cutthroat Trout", "Cutthroat Trout X Steelhead (Hybrid)",
                     "Rainbow / Redband Trout (Wild)", "Rainbow Trout", "Rainbow Trout (Hatchery)","Rainbow X Cutthroat Trout",
                     "Westslope Cutthroat Trout")
CohoList <- c("Coho Salmon")
SockeyeList <- c("Sockeye Salmon (Snake River Runs)", "Kokanee (Early Spawner)", "")
Salmonid_noIDList<- c("Salmonid (Var. Sp.)")
SalmonidFryList <- c("Fry (Oncorhynchus var. sp.)")
OtherFishList <- c("Bluegill", "Bridgelip Sucker", "Brown Trout", "Mottled Sculpin", "Pacific Lamprey", "Pumpkinseed", 
                   "Shorthead Sculpin","Speckled Dace","Largemouth Bass","Longnose Dace","Mountain Sucker","Northern Pikeminnow","Paiute Sculpin",
                   "Sucker (Var. Sp.)", "Whitefish (Var. Sp. Prosopium)", "Yellow Perch", "Common Carp","Dace (Var. Sp.)","Largescale Sucker",
                   "Longnose Sucker", "Mountain Whitefish", "Redside Shiner", "Peamouth","Sculpin (Var. Species)") 

#loop to summarize into total observed fish by categories
Snorkel_wide <- matrix(nrow=0, ncol=23);Snorkel_wide <- as.data.frame(Snorkel_wide)
colnames(Snorkel_wide) <- c("FishSurveyID","SectionID","Stream","Strata","NewLat","NewLong","Huc4Name","Huc5Name","Huc6Name","SurveyDate","Year","TransectLength","MeanWidth","SectionArea", "Chinook","Steelhead","Brook_Bull_Cut_Rainbow","Sockeye","Coho","Salmonid_noID", "SalmonidFry_noID",   "Other_fish", "SurveyNotes")
n <- 1
for(i in unique(Snorkel_long$FishSurveyID)){
    #for tracking progress  
  print(paste0("survey #",i, ":  ",round(n/length(unique(Snorkel_long$FishSurveyID))*100,2)," % Done"))
    n<-n+1
    #create empty dataframe to recieve binned survey data
  Snorkel_wide_sub <- matrix(nrow=1, ncol=23);Snorkel_wide_sub <- as.data.frame(Snorkel_wide_sub)
  colnames(Snorkel_wide_sub) <- c("FishSurveyID","SectionID","Stream","Strata", "NewLat","NewLong","Huc4Name","Huc5Name","Huc6Name","SurveyDate","Year","TransectLength","MeanWidth","SectionArea", "Chinook","Steelhead","Brook_Bull_Cut_Rainbow","Sockeye","Coho","Salmonid_noID", "SalmonidFry_noID",   "Other_fish", "SurveyNotes")
  #Subset to each survey in loop
  Snorkel_long_sub<-subset(Snorkel_long, FishSurveyID == i)
    #Filling in wide form survey information
  Snorkel_wide_sub$FishSurveyID<- unique(Snorkel_long_sub$FishSurveyID)
  Snorkel_wide_sub$SectionID<- unique(Snorkel_long_sub$SectionID)
  Snorkel_wide_sub$Year<- unique(Snorkel_long_sub$Year)
  Snorkel_wide_sub$Stream<- unique(Snorkel_long_sub$Stream)
  Snorkel_wide_sub$NewLat<- unique(Snorkel_long_sub$NewLat)
  Snorkel_wide_sub$NewLong<- unique(Snorkel_long_sub$NewLong)
  Snorkel_wide_sub$SurveyDate<- unique(Snorkel_long_sub$SurveyDate)
  Snorkel_wide_sub$MeanWidth<- unique(Snorkel_long_sub$MeanWidth)
  Snorkel_wide_sub$SectionLength<- unique(Snorkel_long_sub$SectionLength)
  Snorkel_wide_sub$SectionArea<- unique(Snorkel_long_sub$SectionArea)
  Snorkel_wide_sub$Strata<- unique(Snorkel_long_sub$Strata)
  Snorkel_wide_sub$Huc4Name<- unique(Snorkel_long_sub$Huc4Name)
  Snorkel_wide_sub$Huc5Name<- unique(Snorkel_long_sub$Huc5Name)
  Snorkel_wide_sub$Huc6Name<- Snorkel_long_sub$Huc6Name[1] #a few have two names here so can't use unique
  Snorkel_wide_sub$SurveyNotes<- unique(Snorkel_long_sub$SurveyNotes)
    #Filling in wide form binned survey counts
  Snorkel_wide_sub$Chinook <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% ChinookList,]$SumNumberCounted)
  Snorkel_wide_sub$Steelhead <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% SteelheadList,]$SumNumberCounted)
  Snorkel_wide_sub$Brook_Bull_Cut_Rainbow <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% BrookBullCutRainbowList,]$SumNumberCounted)
  Snorkel_wide_sub$Sockeye <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% SockeyeList,]$SumNumberCounted)
  Snorkel_wide_sub$Coho <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% CohoList,]$SumNumberCounted)
  Snorkel_wide_sub$Salmonid_noID<- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% Salmonid_noIDList,]$SumNumberCounted)
  Snorkel_wide_sub$Other_fish <- sum(Snorkel_long_sub[Snorkel_long_sub$SName %in% OtherFishList,]$SumNumberCounted)
    #combine all survey data to one datafame
  Snorkel_wide <- rbind(Snorkel_wide, Snorkel_wide_sub)
}

#write to csv
head(Snorkel_wide)
write.csv(Snorkel_wide, "Snorel_Data_Wide.csv")


subset(Snorkel_wide, SectionID == 13928)

subset(Snorkel_long, FishSurveyID == 67525)
subset(Snorkel_long, SectionID == 13928 & Year == 2008)
13928 
67525