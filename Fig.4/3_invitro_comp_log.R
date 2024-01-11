rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

dt <- read.csv("invitro_comp_input.csv", check.names = F)
dt$competition <- factor(dt$competition, 
                         levels = c("ΔgatCΔaraC", "ΔgatCΔmalI", "ΔgatCΔmelR", 
                                    "ΔgatCΔaraCΔmalI"))


# calc mean and se --------------------------------------------------------
dt$group <- paste(dt$competition, dt$Day, sep = "_")
dt <- dt %>% mutate(compIndex = CFUkm /CFUcm)
meanVec <- dt %>% group_by(group) %>% summarise(mean = mean(compIndex))
seVec <- dt %>% group_by(group) %>% summarise(se = sd(compIndex)/sqrt(3))

mean_se <- inner_join(meanVec, seVec)
mean_se$Day <- sapply(mean_se$group, function(x){
        strsplit(x, "_")[[1]][2]
})
mean_se$Day <- factor(mean_se$Day, levels = c("Day0", "Day7", "Day14"))
mean_se$competition <- sapply(mean_se$group, function(x){
        strsplit(x, "_")[[1]][1]
})
mean_se$competition <- factor(mean_se$competition, 
                              levels = c("ΔgatCΔaraC", "ΔgatCΔmalI", "ΔgatCΔmelR", 
                                         "ΔgatCΔaraCΔmalI"))


# Design the appearance of the figure -------------------------------------
grpColour <- c("white", "#bfbfbf", "#808080")
invivo_comp_theme <- theme(axis.text = element_text(colour = "black", size = 10),
                           axis.title.x = element_blank(),
                           axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                           panel.grid = element_blank(), 
                           panel.background = element_rect(fill = "transparent", colour = "black"), 
                           strip.background = element_blank(), 
                           axis.ticks = element_blank(), 
                           strip.text.x = element_text(size = 11))

scientific_notation <- function(x) {
        x <- format(x, scientific = TRUE)
        x <- gsub("^(.*)e", "'\\1'e", x)
        x <- gsub("e", "%*%10^", x)
        x <- gsub('\\+', '', x)
        parse(text = x)
}


# Draw graph --------------------------------------------------------------
pdf("invitro_comp_log.pdf", height = 3)
g <- ggplot(mean_se, aes(x = Day, y = mean, fill = Day)) +
        geom_bar(stat = "identity", position = "dodge", colour = "black") +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.25)) +
        geom_jitter(data = dt, aes(x = Day, y = compIndex), height = 0) +
        theme_classic() + 
        invivo_comp_theme + 
        scale_fill_manual(values = grpColour) + 
        ylab("Competition Index\n(multiple deficient / ΔgatC)") + 
        facet_wrap(~ competition, nrow = 1) + 
        guides(fill = guide_legend(title = NULL)) + 
        scale_y_log10(labels = scientific_notation) + 
        NULL
print(g)
dev.off()
