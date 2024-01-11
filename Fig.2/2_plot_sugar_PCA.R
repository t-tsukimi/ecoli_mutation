rm(list = ls())

library(tidyverse)
library(ggrepel)
library(gridExtra)
library(ggsci)


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


# calc score and loadings ------------------------------------------------
targetData <- dt %>% dplyr::filter(Day == 14, content == "feces", 
                                   group %in% c("vs_2_araC", "vs_3_araCmalI", "vs_2_malI"))

metaboNum <- ncol(targetData) -5
detected_column <-  c(colSums(targetData[, 1:metaboNum]) != 0, rep(F, 5))
result <- stats::prcomp(targetData[, detected_column], scale = T) 
summary(result)
# Importance of components:
#         PC1    PC2    PC3    PC4     PC5     PC6     PC7
# Standard deviation     2.5015 2.0616 1.6764 1.1564 1.12260 0.95369 0.66372
# Proportion of Variance 0.3476 0.2361 0.1561 0.0743 0.07001 0.05053 0.02447
# Cumulative Proportion  0.3476 0.5837 0.7399 0.8142 0.88418 0.93471 0.95919

scores <- as.data.frame(result$x)
scores$group <- targetData$group
scores$CMF_AIN <- targetData$CMF_AIN
fcl <- sweep(result$rotation, MARGIN = 2, result$sdev, FUN = "*") %>% as.data.frame()


# plot PCA and loadings ---------------------------------------------------
gscore <- ggplot(data = scores, aes(x = PC1, y = PC2, 
                                    colour = group, shape = CMF_AIN)) +
        geom_point(size = 3) + 
        theme_classic() + 
        scale_color_npg() +
        theme(axis.title = element_text(size = 15, colour = "black"), 
              axis.text = element_text(size = 12, colour = "black"), 
              legend.position = "top") + 
        xlab("PC1 (34.8%)") +
        ylab("PC2 (23.6%)")

gloadings <- ggplot(data = fcl, aes(x = PC1, y = PC2, label = rownames(fcl))) +
        geom_point(size = 3) + 
        geom_text_repel(max.overlaps = 15) +
        theme_classic() + 
        scale_color_npg() +
        theme(axis.title = element_text(size = 15, colour = "black"), 
              axis.text = element_text(size = 12, colour = "black")) + 
        xlab("Loadings1") +
        ylab("Loadings2") +
        labs()

pdf("PCA_loadings.pdf", width = 5)
grid.arrange(gscore, gloadings, ncol = 1, heights = unit(c(0.5, 0.4), "null"))
dev.off()
