rm(list = ls())
library(tidyverse)
library(dendextend)
library(ComplexHeatmap)


dt <- read.csv("merged_mutation_rate_tidy.csv")
dt$identifer <- paste(dt$Ex, dt$SampleName, sep = "_")

dt4heat <- dt[, c(2:16, 20)]
dt4heat$MutationGenePos <- paste(dt4heat$MutationGene, dt4heat$POS, sep = "_")
dt4heat.Filter <- filter(dt4heat, MutationImpact %in% c("HIGH", "MODERATE"), 
                         Strain != "AID-KO", Day == 84, MutationGene %in% prev_genes$MutationGene) 

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
colorFun <- circlize::colorRamp2(c(10, 100), c("white", "#ff5636")) 
orderVec <- c(2:4, 7:8, 12:14, 1, 5:6, 9:11)
columnOrder <- colnames(dt4heat.Filter.Simple.W.T)[orderVec]

DayVec <- sapply(colnames(dt4heat.Filter.Simple.W.T), FUN = function(x) {
        substr(x, start = nchar(x) - 1, stop = nchar(x)) %>% return()
})
DayBoxColor <- c("14" = "#FAFAF9", "21" = "#E2E2E2", "28" = "#D6D3D1", "56" = "#A8A29E", "84" = "#78716C")
mouseVec <- c(rep("AIN1", 1), rep("mutL1", 1), rep("mutL2", 1), rep("mutL3", 1), 
              rep("AIN2", 1), rep("AIN3", 1), rep("mutL4", 1), rep("mutL5", 1),
              rep("AIN4", 1), rep("AIN5", 1), rep("AIN6", 1), rep("mutS1", 1), 
              rep("mutS2", 1), rep("mutS3", 1))
mouseColorVec <- c("mutL1" = "#BFDBFE", "mutL2" = "#93C5FD", "mutL3" = "#60A5FA", 
                   "mutL4" = "#3B82F6", "mutL5" = "#2563EB", "mutS1" = "#BBF7D0", 
                   "mutS2" = "#34D399", "mutS3" = "#059669", 
                   "AIN1" = "#F2C7B6", "AIN2" = "#F4A173", "AIN3" = "#FB923C", 
                   "AIN4" = "#EF7625", "AIN5" = "#EA650A", "AIN6" = "#C9570A")
bottomBox <- HeatmapAnnotation(Day = DayVec, 
                               Mouse = mouseVec,
                               col = list(Day = DayBoxColor, 
                                          Mouse = mouseColorVec))


# Draw heatmap ------------------------------------------------------------
pdf("heatmap_diet.pdf", height = 4)
ComplexHeatmap::Heatmap(dt4heat.Filter.Simple.W.T, 
                        name = "Mutation Rate",
                        cluster_columns = F, 
                        cluster_rows = F,
                        col = colorFun, 
                        row_names_side = "left", 
                        column_split = mouseVec,
                        bottom_annotation = bottomBox, 
                        border = T, 
                        rect_gp = gpar(col = "black", lwd = .3),
                        row_order = rowOrder,
                        row_names_gp = gpar(fontsize = 4), 
                        column_names_gp = gpar(fontsize = 7))
dev.off()
