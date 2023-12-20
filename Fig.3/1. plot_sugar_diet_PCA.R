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


# calc score and loadings -------------------------------------------------
targetData <- dt %>% dplyr::filter(Day == 0, content == "feces")

metaboNum <- ncol(targetData) -5
detected_column <-  c(colSums(targetData[, 1:metaboNum]) != 0, rep(F, 5))
result <- stats::prcomp(targetData[, detected_column], scale = T) #UV
summary(result) 
# Importance of components:
#         PC1    PC2     PC3     PC4     PC5    PC6     PC7
# Standard deviation     4.0523 1.5897 1.06677 1.00781 0.92630 0.7542 0.67514
# Proportion of Variance 0.6842 0.1053 0.04742 0.04232 0.03575 0.0237 0.01899
# Cumulative Proportion  0.6842 0.7895 0.83693 0.87925 0.91500 0.9387 0.95770

scores <- as.data.frame(result$x)
scores$group <- targetData$group
scores$CMF_AIN <- targetData$CMF_AIN
fcl <- sweep(result$rotation, MARGIN = 2, result$sdev, FUN = "*") %>% as.data.frame()


# plot PCA and loadings ---------------------------------------------------
gscore <- ggplot(data = scores, aes(x = PC1, y = PC2, 
                                    colour = CMF_AIN)) +
        geom_point(size = 3) + 
        theme_classic() + 
        scale_color_manual(values = c("#1EB5B8", "#EB746A")) +
        theme(axis.title = element_text(size = 15, colour = "black"), 
              axis.text = element_text(size = 12, colour = "black"), 
              legend.position = "top") + 
        xlab("PC1 (68.4%)") +
        ylab("PC2 (10.5%)")

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

pdf("PCA_loadings_diet.pdf", width = 5)
grid.arrange(gscore, gloadings, ncol = 1, heights = unit(c(0.5, 0.4), "null"))
dev.off()
