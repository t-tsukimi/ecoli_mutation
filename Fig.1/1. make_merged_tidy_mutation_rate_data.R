rm(list = ls())
library(tidyverse)



# input mutation rate data ------------------------------------------------
dt <- read.csv("mutation_rate_data.csv", row.names = 1)
dt$identifer <- paste(dt$SampleName, dt$Ex, sep = "_")
unique(dt$identifer) %>% length()



# Add up mutation rates for the same gene ---------------------------------
convert1String <- function(vector, sep = " ") {
        String <- ""
        for (i in 1:length(vector)) {
                String <- paste(String, vector[i], sep = sep)
        }
        return(substr(String, start = 2, stop = nchar(String)))
        
}

NewImpactVec <- c()
for (i in 1:nrow(dt)) {
        if(dt$MutationImpact[i] %in% c("MODERATE", "HIGH")) {
                NewImpactVec <- c(NewImpactVec, "HIGHMODERATE")
        } else {
                NewImpactVec <- c(NewImpactVec, dt$MutationImpact[i])
        }
}
dt <- cbind(dt, NewImpactVec)

sum_df <- data.frame()
samples <- unique(dt$identifer)
days <- unique(dt$Day)
impact <- unique(dt$NewImpactVec)
for (i in samples) {
        targetSample <- filter(dt, identifer == i)
        for (j in days) {
                targetSampleDay <- filter(targetSample, Day == j)
                for (k in impact) {
                        targetSampleDayImpact <- filter(targetSampleDay, NewImpactVec == k)
                        targetGenes <- unique(targetSampleDayImpact$MutationGene)
                        for (l in targetGenes) {
                                targetDF <- filter(targetSampleDayImpact, MutationGene == l)
                                if(nrow(targetDF) == 1) {
                                        addedRow <- targetDF
                                        sum_df <-  rbind(sum_df, addedRow)
                                } else {
                                        addedRow <- targetDF[1, ]
                                        addedRow$MutationRate <- sum(targetDF$MutationRate)
                                        addedRow$MutationGenePos <- convert1String(targetDF$MutationGenePos, sep = " ")
                                        addedRow$MutationType <- convert1String(targetDF$MutationType, sep = " ")
                                        sum_df <- rbind(sum_df, addedRow)
                                }
                                
                        }
                }
        }
}

write.csv(sum_df, "merged_mutation_rate_tidy.csv")

