rm(list = ls())
library(tidyverse)

convert1String <- function(vector, sep = " ") {
        String <- ""
        for (i in 1:length(vector)) {
                String <- paste(String, vector[i], sep = sep)
        }
        return(substr(String, start = 2, stop = nchar(String)))
        
}


# input merged mutation rate data -----------------------------------------
dt <- read.csv("merged_mutation_rate_tidy.csv")
dt$identifer <- paste(dt$SampleName, dt$Ex, sep = "_")


# calc mutation prevalence ------------------------------------------------
dt4ext.Filter <- filter(dt, Day == "84", NewImpactVec == "HIGHMODERATE")

max_df <- data.frame()
samples <- unique(dt4ext.Filter$identifer)

for (i in samples) {
        targetSample <- filter(dt4ext.Filter, identifer == i)
        targetGenes <- unique(targetSample$MutationGene)
        for (j in targetGenes) {
                targetDF <- filter(targetSample, MutationGene == j)
                if(nrow(targetDF) == 1) {
                        addedRow <- targetDF
                        addedRow$count <- 1
                        max_df <-  rbind(max_df, addedRow)
                } else {
                        addedRow <- targetDF[1, ]
                        addedRow$MutationRate <- max(targetDF$MutationRate)
                        addedRow$MutationGenePos <- convert1String(targetDF$MutationGenePos, sep = " ")
                        addedRow$count <- 1
                        max_df <- rbind(max_df, addedRow)
                }
        }
}

max_df_countSum <- max_df %>% 
        group_by(Diet, Strain, MutationGene, ) %>% 
        summarise(GeneCountSum = sum(count))

write.csv(max_df_countSum, "mutation_prevalence.csv")
