rm(list = ls())

library(dplyr)
library(tidyr)


# input CFU data ----------------------------------------------------------
dt <- read.csv("invivoCFU.csv", row.names = 1)
dt$strain_diet <- paste(dt$strain, dt$diet, sep = "_")
dt$identifer <- paste(dt$strain_diet, dt$replicate, sep = "_")
        
mean <- dt %>% group_by(strain_diet) %>% 
        summarise(across(innoculation:Day14, mean)) %>% 
        gather(key = Day, value = mean, -strain_diet) 
sd <- dt %>% group_by(strain_diet) %>% 
        summarise(across(innoculation:Day14, sd)) %>% 
        gather(key = Day, value = sd, -strain_diet) 
sd$sd[1:6] <- rep(0, 6) 
      

mean_sd <- full_join(mean, sd) 
mean_sd$Day <- factor(mean_sd$Day, levels = c("innoculation", "Day1", "Day3", 
                                              "Day5", "Day7", "Day10", "Day14"))
mean_sd$strain_diet <- factor(mean_sd$strain_diet, 
                              levels = c("gatCaraC_CMF", "gatCmalI_CMF", "gatCmelR_CMF", 
                                         "gatCaraCmalI_CMF", "delta4_CMF", "delta4_AIN"))



# plot competition index --------------------------------------------------
library(ggplot2)
library(ggsci)
pdf("competition_index.pdf", height = 4)
ggplot(dplyr::filter(mean_sd, !strain_diet %in% c("delta4_CMF", "delta4_AIN")), 
       aes(x = Day, y = mean, group = strain_diet, colour = strain_diet)) +
        geom_line() +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = mean - sd / sqrt(4), 
                          ymax = mean + sd / sqrt(4), width = 0.15)) +
        theme_classic() +
        scale_color_npg() +
        ylab("Competition Index") +
        theme(text = element_text(family = "Helvetica"), 
              axis.text = element_text(colour = "black", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1), 
              axis.title = element_text(colour = "black", size = 18)) + 
        scale_y_log10(breaks = c(10^0, 10^2, 10^4, 10^6), 
                      labels = c(0, 2, 4, 6))
dev.off()
