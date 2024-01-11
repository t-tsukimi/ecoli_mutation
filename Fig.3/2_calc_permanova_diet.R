rm(list = ls())

library(tidyverse)
library(vegan)

# input sugar conc. data --------------------------------------------------
dt <- read.csv("sugar_conc.csv", 
               row.names = 1, check.names = F)
for (i in 1:nrow(dt)) {
        if(length(grep("AIN", dt$group[i])) == 1) {
                dt$CMF_AIN[i] <- "AIN"
        } else {
                dt$CMF_AIN[i] <- "CMF"
        }
}
dt$CMF_AIN <- factor(dt$CMF_AIN, levels = c("CMF", "AIN"))


# select target data -----------------------------------------------------
targetData <- dt %>% dplyr::filter(Day == 0, content == "feces")

metaboNum <- ncol(targetData) -5
detected_column <-  c(colSums(targetData[, 1:metaboNum]) != 0, rep(F, 5))


# performed permanova -----------------------------------------------------
scaledData <- scale(targetData[, detected_column], scale = T)
test_result <- adonis(scaledData ~ CMF_AIN, data = targetData, method='eu')
test_result
# Call:
# adonis(formula = scaledData ~ CMF_AIN, data = targetData, method = "eu") 
# 
# Permutation: free
# Number of permutations: 999
# 
# Terms added sequentially (first to last)
# 
# Df      SumsOfSqs  MeanSqs  R2        Pr(>F)    
# CMF_AIN     1      361.41   0.65472   0.001 ***
# Residuals  22      190.59   0.34528           
# Total      23      552.00   1.00000           
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1