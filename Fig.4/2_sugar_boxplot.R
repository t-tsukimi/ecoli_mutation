rm(list = ls())
library(tidyverse)
library(ggsci)

# input sugar conc. data --------------------------------------------------
dt <- read.csv("sugar_conc.csv", 
               stringsAsFactors = F, row.names = 1, check.names = F)


# Data formatting for plotting --------------------------------------------
dt_tidy_long <- gather(dt, key = Sugar, value = conc, Arabinose:Sugar_all)
dt_tidy_long$Day <- factor(dt_tidy_long$Day, levels = c(0, 7, 14))
Sugars <- unique(dt_tidy_long$Sugar)
groupLevel <- c("CMF", "AIN", "vs_2_araC", "vs_2_malI", "vs_2_melR", 
                "vs_3_araCmalI", "vs_4_CMF", "vs_4_AIN")
dt_tidy_long$group <- factor(dt_tidy_long$group, levels = groupLevel)
dt_tidy_long$Sugar <- factor(dt_tidy_long$Sugar, levels = unique(dt_tidy_long$Sugar)) 

for (i in 1:nrow(dt_tidy_long)) {
        if(length(grep("AIN", dt_tidy_long$group[i])) == 1) {
                dt_tidy_long$CMF_AIN[i] <- "AIN"
        } else {
                dt_tidy_long$CMF_AIN[i] <- "CMF"
        }
}
dt_tidy_long$CMF_AIN <- factor(dt_tidy_long$CMF_AIN, levels = c("CMF", "AIN"))

plot_theme <- theme_classic() + 
        theme(axis.text.x = element_text(vjust = 0.5, colour = "black"),
              axis.text.y = element_text(colour = "black"), 
              plot.title = element_text(hjust = 0.5), 
              strip.background = element_blank())


# Draw boxplot ------------------------------------------------------------
fn <- "sugar_boxplot.pdf"
p <- ggplot2::ggplot(data = filter(dt_tidy_long, content == "feces", Day == "0"),
                     aes(x = CMF_AIN, y = conc / 1000, colour = CMF_AIN, fill = CMF_AIN)) + 
        stat_boxplot(geom = 'errorbar', width = 0.5, colour = "black", size = 0.3) +
        geom_boxplot(colour = "black", size = 0.3, outlier.colour = NA) + 
        geom_jitter(size = 0.8, colour = "black", shape = 1, height = 0) +
        theme_classic() + 
        summary_plot_theme +
        scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                          labels = c("High-MAC", "Low-MAC")) + 
        scale_x_discrete(labels = c("High-MAC", "Low-MAC")) + 
        ylab("Conc. (umol/g)") + 
        scale_y_continuous(limits = c(0, NA)) +
        facet_wrap(Sugar ~ ., nrow = 4, scales = "free")+
        NULL
ggsave(fn, p, height = 6)

