rm(list = ls())
library(tidyverse)
library(ggVennDiagram)

CMF <- read.csv("DESeq_CMF.csv")
AIN <- read.csv("DESeq_AIN.csv")


# extract genes by p value and fold change --------------------------------
CMF_p0.05 <- filter(CMF, p.val < 0.05)
CMF_FDR0.05 <- filter(CMF, p.FDR < 0.05)
CMF_FDR0.05FC1 <- filter(CMF, p.FDR < 0.05 & (log2FC > 1 | log2FC < -1))

AIN_p0.05 <- filter(AIN, p.val < 0.05)
AIN_FDR0.05 <- filter(AIN, p.FDR < 0.05)
AIN_FDR0.05FC1 <- filter(AIN, p.FDR < 0.05 & (log2FC > 1 | log2FC < -1))

CMF_FDR0.05FC1$gene_locus <- paste(CMF_FDR0.05FC1$gene, 
                                  CMF_FDR0.05FC1$locus_tag, 
                                  sep = "_")
AIN_FDR0.05FC1$gene_locus <- paste(AIN_FDR0.05FC1$gene, 
                                   AIN_FDR0.05FC1$locus_tag, 
                                   sep = "_")

common_CMF_AIN <- intersect(CMF_FDR0.05FC1$gene_locus, AIN_FDR0.05FC1$gene_locus)
only_CMF <- setdiff(CMF_FDR0.05FC1$gene_locus, AIN_FDR0.05FC1$gene_locus)


# Draw vennDiagram --------------------------------------------------------
pdf("DEG_vennDiagram.pdf")
ggVennDiagram(list(CMF = CMF_FDR0.05FC1$gene_locus, AIN = AIN_FDR0.05FC1$gene_locus), 
              label = c("count"), set_size = 10, label_size = 14) + 
        scale_fill_gradient(low = "white", high = "white") + 
        scale_color_manual(values = c("black", "black")) + 
        theme(legend.position = "none")
dev.off()


# Draw volcano plot -------------------------------------------------------
CMF$colVec <- "NotSig"
CMF$colVec[CMF$log2FC > 1 & CMF$p.FDR < 0.05] <- "Up"
CMF$colVec[CMF$log2FC < -1 & CMF$p.FDR < 0.05] <- "Down"

only_CMF_geneName <- setdiff(CMF_FDR0.05FC1$gene, AIN_FDR0.05FC1$gene)

CMF$colVec[CMF$log2FC > 1 & CMF$p.FDR < 0.05 & CMF$gene %in% only_CMF_geneName] <- "Up101"
CMF$colVec[CMF$log2FC < 1 & CMF$p.FDR < 0.05 & CMF$gene %in% only_CMF_geneName] <- "Down101"
CMF$labVec <- NA
CMF$labVec[CMF$colVec != "NotSig"] <- CMF$gene[CMF$colVec != "NotSig"]

library(ggrepel)
volcano_theme <- theme(axis.text = element_text(size = 12, colour = "black"),
                       legend.text = element_text(size = 12, colour = "black"),
                       strip.background = element_blank(), 
                       axis.title = element_text(size = 15),
                       axis.line = element_line(colour = "black", size = .4, lineend = "square"), 
                       axis.ticks = element_line(size = .4))

g <- ggplot(data = CMF, aes(x = log2FC, y = -log10(p.FDR), colour = colVec, label = labVec)) +
        geom_vline(xintercept = c(-1, 1), col = "gray", linetype = "dashed") +
        geom_hline(yintercept = -log10(0.05), col = "gray", linetype = "dashed") + 
        geom_point() + 
        theme_classic() +
        volcano_theme +
        geom_text_repel(colour = "black", max.overlaps = 70) +
        scale_color_manual(values = c("lightblue", "blue", "gray", "pink", "red")) +
        ylab("-log10(p.adj)")
g
ggsave("volcano_plot.pdf", plot = g, width = 7)
