rm(list = ls())
library(tidyverse)
library(dendextend)
library(ComplexHeatmap)

dt <- read.csv("merged_mutation_rate_tidy.csv")
dt$identifer <- paste(dt$Ex, dt$SampleName, sep = "_")

dt4heat <- dt[, c(2:16, 20)]
dt4heat$MutationGenePos <- paste(dt4heat$MutationGene, dt4heat$POS, sep = "_")
dt4heat.Filter <- filter(dt4heat, MutationImpact %in% c("HIGH", "MODERATE"), 
                         Diet != "AIN", Strain != "AID-KO", Day != 147) 

dt4heat.Filter.Simple <- dt4heat.Filter[, c(10, 16, 15)]
dt4heat.Filter.Simple.W <- tidyr::spread(dt4heat.Filter.Simple, 
                                         key = MutationGene, 
                                         value = MutationRate)
rownames(dt4heat.Filter.Simple.W) <- dt4heat.Filter.Simple.W$identifer
dt4heat.Filter.Simple.W <- dt4heat.Filter.Simple.W[, -1]
dt4heat.Filter.Simple.W.T <- t(dt4heat.Filter.Simple.W)

dt4heat.Filter.Simple.W.T[is.na(dt4heat.Filter.Simple.W.T)] <- 0
dt4heat.Filter.Simple.W.T[dt4heat.Filter.Simple.W.T > 100] <- 100


# Design the appearance of the heatmap ------------------------------------
colorFun <- circlize::colorRamp2(c(0, 80), c("white", "red"))
columnOrder <- colnames(dt4heat.Filter.Simple.W.T)

DayVec <- sapply(colnames(dt4heat.Filter.Simple.W.T), FUN = function(x) {
        substr(x, start = nchar(x) - 1, stop = nchar(x)) %>% return()
})
DayBoxColor <- c("14" = "#FAFAF9", "21" = "#E2E2E2", "28" = "#D6D3D1", 
                 "56" = "#A8A29E", "84" = "#78716C")
mouseVec <- c(rep("mutL1", 4), rep("mutL2", 1), rep("mutL3", 4), 
              rep("mutL4", 4), rep("mutL5", 4), rep("mutS1", 2), 
              rep("mutS2", 2), rep("mutS3", 2))
mouseColorVec <- c("mutL1" = "#BFDBFE", "mutL2" = "#93C5FD", "mutL3" = "#60A5FA", 
                   "mutL4" = "#3B82F6", "mutL5" = "#2563EB", "mutS1" = "#BBF7D0", 
                   "mutS2" = "#34D399", "mutS3" = "#059669")
ExVec <- c(rep(1, 9), rep(2, 8), rep(3, 6))
ExColor <- c("1" = "#FFF2BB", "2" = "#FFE67B", "3" = "#FFD215")

bottomBox <- HeatmapAnnotation(Day = DayVec, 
                               Mouse = mouseVec,
                               Experiment = ExVec,
                               col = list(Day = DayBoxColor, 
                                          Mouse = mouseColorVec, 
                                          Experiment = ExColor), 
                               simple_anno_size = unit(0.3, "cm"))


# Draw heatmap ------------------------------------------------------------
pdf("heatmap.pdf")
ComplexHeatmap::Heatmap(dt4heat.Filter.Simple.W.T, 
                        name = "Mutation Rate",
                        cluster_columns = FALSE, 
                        col = colorFun, 
                        row_names_side = "left", 
                        column_split = mouseVec,
                        bottom_annotation = bottomBox, 
                        border = T, 
                        column_order = columnOrder, 
                        row_names_gp = gpar(fontsize = 3), 
                        column_names_gp = gpar(fontsize = 7))
dev.off()
