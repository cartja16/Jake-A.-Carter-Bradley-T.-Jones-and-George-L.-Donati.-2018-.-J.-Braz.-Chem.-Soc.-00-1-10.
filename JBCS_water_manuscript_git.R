# Section 1: Load libraries, read in data and format data objects #####################

rm(list=ls()) # Clear workspace
dev.off()

# for formatting
library(lattice)
library(latticeExtra)
library(extrafont)
mySettings <- trellis.par.get()

windowsFonts(Times=windowsFont("TT Times New Roman"))
par(family = "Times")

library(ggcorrplot)
library(mclust)
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(ggbiplot)
library(gridExtra)
library(zCompositions)

# read in data
# be sure working directory is accurate

library(readr)
water <- read.csv("G:/My Drive/WFU/Water samples/r/data_noHydrants_test.csv", row.names=1)
View(water)
str(water)

# change Age_of_building to year 
names(water)[1] <- "Year"
names(water)

# Let's consider samples and respective elemental concentrations from each spout 

cafeteria <- water[water$Spout == "Caf",]
fountain <- water[water$Spout == "Fount",]

# _stat objects for computation then refer back to spout splits 

cafeteria_stat <- water[water$Spout == "Caf",][names(water) != c("Spout", "longitude", "latitude")]
cafeteria_tab <- water[water$Spout == "Caf",][names(water) != c("Spout", "longitude", "latitude")]

fountain_stat <- water[water$Spout == "Fount",][names(water) != c("Spout", "longitude", "latitude")]
fountain_tab <- water[water$Spout == "Fount",][names(water) != c("Spout", "longitude", "latitude")]

# Section 2: LOD imputation using lrEM algorithm ###############################

# Cafeteria 

# to vizualize missingness 

# Figure S1

#tiff("caf_missingness.tiff", width = 7, height = 4, units = 'in', res = 300)
Caf.pattern.ID <- zPatterns(cafeteria_stat[,-1], label=0, bar.colors=c("#8B8878", "#8B8878"),
                             bar.labels=TRUE, cell.colors=c("#8B8878", "white"),
                             cell.labels=c("Nondetected", "Observed"),
                             cex.axis=1)
dev.off()

year_LOD <- 1 # "Any threshold value can be set for non-censored elements" from ?lrEM
Cr_LOD <- 0.08
Cu_LOD <- 0.3
As_LOD <- 0.04
Se_LOD <- 0.06
Cd_LOD <- 0.02
Sb_LOD <- 0.01
Tl_LOD <- 0.004
Pb_LOD <- 0.01

water_LOD <- c(year_LOD,Cr_LOD,Cu_LOD,As_LOD,Se_LOD,Cd_LOD,Sb_LOD,Tl_LOD,Pb_LOD)

water_LOD_Impute <- c(year_LOD, Cr_LOD, Cu_LOD, As_LOD, Sb_LOD, Pb_LOD)

cafeteria_stat <- lrEM(cafeteria_stat[,c(-5,-6,-8)], label=0, dl=water_LOD_Impute, ini.cov = "complete.obs",
                       tolerance = 0.0001, max.iter = 50)
cafeteria_stat

# Max and mins 
caf_max <- sapply(cafeteria_stat, function(x) max(x))
caf_min <- sapply(cafeteria_stat, function(x) min(x))
caf_mean <- sapply(cafeteria_stat, function(x) mean(x))

caf_max
caf_min
caf_mean <- data.frame(caf_mean)

# Fountain 

# to vizualize missingness

# Figure S2

#tiff("fount_missingness.tiff", width = 7, height = 4, units = 'in', res = 300)

Fount.pattern.ID <- zPatterns(fountain_stat[,-1], label=0, bar.colors=c("#8B8878", "#8B8878"),
                            bar.labels=TRUE, cell.colors=c("#8B8878", "white"),
                            cell.labels=c("Nondetected", "Observed"),
                            cex.axis=1)
dev.off()

fountain_stat <- lrEM(fountain_stat[,c(-5,-6,-8)], label=0, dl=water_LOD_Impute, ini.cov = "complete.obs",
                       tolerance = 0.0001, max.iter = 50)
fountain_stat

# Max and mins 

fount_max <- sapply(fountain_stat, function(x) max(x))
fount_min <- sapply(fountain_stat, function(x) min(x))
fount_mean <- sapply(fountain_stat, function(x) mean(x))

fount_max
fount_min
fount_mean <- data.frame(fount_mean)

# Section 3: Correlations, Multivariate analysis and labeling according to individual observations ##############

# correlation of features 

caf_cor <- cor(cafeteria_stat)
caf_pmat <- cor_pmat(cafeteria_stat)

fount_cor <- cor(fountain_stat)
fount_pmat <- cor_pmat(fountain_stat)

# ggplot 

# cafeteria 
caf_ggcorr <- ggcorrplot(caf_cor,
                         type = "lower",
                         lab_size = 6,
                         tl.cex = 12,
                         p.mat = caf_pmat,
                         ggtheme = theme_bw,
                         outline.color = "black",
                         show.legend = FALSE,
                         colors = c("white", "white", "white"),
                         lab = TRUE,
                         insig = "pch",
                         pch = 7,
                         pch.cex = 18,
                         title = "Cafeteria") +
  theme(#legend.direction = 'vertical',
    #legend.position = 'right',
    text=element_text(family="Times", face="bold", size = 18),
    axis.text.x=element_text(family="Times", face="bold", size = 18),
    axis.text.y=element_text(family="Times", face="bold", size = 18),
    #panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5))
caf_ggcorr
#ggsave("caf_ggcorr.tiff", plot = caf_ggcorr, units="in", width=10, height=8, dpi=300, compression = 'lzw')

# fountain 
fount_ggcorr <- ggcorrplot(fount_cor,
                           type = "lower",
                           lab_size = 6,
                           tl.cex = 12,
                           p.mat = fount_pmat,
                           ggtheme = theme_bw,
                           outline.color = "black",
                           show.legend = FALSE,
                           colors = c("white", "white", "white"),
                           lab = TRUE,
                           insig = "pch",
                           pch = 7,
                           pch.cex = 18,
                           title = "Fountain") +
  theme(#legend.direction = 'vertical',
    #legend.position = 'right',
    text=element_text(family="Times", face="bold", size = 18),
    axis.text.x=element_text(family="Times", face="bold", size = 18),
    axis.text.y=element_text(family="Times", face="bold", size = 18),
    #panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5))
fount_ggcorr
#ggsave("fount_ggcorr.tiff", plot = fount_ggcorr, units="in", width=10, height=8, dpi=300, compression = 'lzw')

# Figure 1

corrs <- arrangeGrob(grobs = list(caf_ggcorr, fount_ggcorr),
                     nrow = 1,
                     ncol = 2)
#ggsave("corrs.tiff", plot = corrs, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# pca of each spout 

# cafeteria_stat 

caf_scale <- scale(cafeteria_stat, center = TRUE, scale = TRUE)
rownames(caf_scale) <- c(1:nrow(caf_scale))
# scaled data; cor and cov are same 
caf_pc <- princomp(caf_scale, cor = TRUE)
summary(caf_pc)
print(caf_pc$loadings)
head(caf_pc$scores)
screeplot(caf_pc)

plot(caf_pc$scores[,1], caf_pc$scores[,2], xlab="PC1", ylab="PC2", type = "n")
text(caf_pc$scores[,1], caf_pc$scores[,2], labels = rownames(caf_scale))

plot(caf_pc$scores[,1], caf_pc$scores[,3], xlab="PC1", ylab="PC3", type = "n")
text(caf_pc$scores[,1], caf_pc$scores[,3], labels = rownames(caf_scale))

plot(caf_pc$scores[,2], caf_pc$scores[,3], xlab="PC2", ylab="PC3", type = "n")
text(caf_pc$scores[,2], caf_pc$scores[,3], labels = rownames(caf_scale))

biplot(caf_pc, choices = c(1,2), col = c("black"), xlim=c(-0.3,0.3))
biplot(caf_pc, choices = c(1,3), col = c("black"), xlim=c(-0.3,0.3))
biplot(caf_pc, choices = c(2,3), col = c("black"), xlim=c(-0.5,0.2))

# fountain_stat 

fount_scale <- scale(fountain_stat, center = TRUE, scale = TRUE)
rownames(fount_scale) <- c(1:nrow(fount_scale))
fount_pc <- princomp(fount_scale, cor = TRUE)
summary(fount_pc)
print(fount_pc$loadings)
head(fount_pc$scores)
screeplot(fount_pc)

plot(fount_pc$scores[,1], fount_pc$scores[,2], xlab="PC1", ylab="PC2", type = "n")
text(fount_pc$scores[,1], fount_pc$scores[,2], labels = rownames(fount_scale))

plot(fount_pc$scores[,1], fount_pc$scores[,3], xlab="PC1", ylab="PC3", type = "n")
text(fount_pc$scores[,1], fount_pc$scores[,3], labels = rownames(fount_scale))

plot(fount_pc$scores[,2], fount_pc$scores[,3], xlab="PC2", ylab="PC3", type = "n")
text(fount_pc$scores[,2], fount_pc$scores[,3], labels = rownames(fount_scale))

biplot(fount_pc, choices = c(1,2), col = c("black"))
biplot(fount_pc, choices = c(1,3), col = c("black"))
biplot(fount_pc, choices = c(2,3), col = c("black"))

# Section 4: Multivariate analysis and labeling according to group assignment from model based clustering ##############

# We load in a modified ggbiplot function to produce the PCA biplot figure.
# See the following for the original function:
# Vincent Q. Vu (2011). ggbiplot: A ggplot2 based biplot. R package version 0.55.
# http://github.com/vqv/ggbiplot

source("ggbiplot_custom_repel.R")

# model based clustering 

# cafeteria_stat 

caf_mclust <- Mclust(caf_scale, G=4) # gives four clusters
summary(caf_mclust)

# summary table 

caf_group1 <- nrow(cafeteria[caf_mclust$classification == 1,])
caf_group2 <- nrow(cafeteria[caf_mclust$classification == 2,])
caf_group3 <- nrow(cafeteria[caf_mclust$classification == 3,])
caf_group4 <- nrow(cafeteria[caf_mclust$classification == 4,])

caf_sumTab <- data.frame(rbind(caf_group1,caf_group2,caf_group3,caf_group4))

caf_mclust$classification

# plot(caf_mclust) 0 gets you out

plot(caf_pc$scores[,1], caf_pc$scores[,2], xlab="PC1", ylab="PC2", type = "n")
text(caf_pc$scores[,1], caf_pc$scores[,2], labels = as.character(caf_mclust$classification))

plot(caf_pc$scores[,1], caf_pc$scores[,3], xlab="PC1", ylab="PC3", type = "n")
text(caf_pc$scores[,1], caf_pc$scores[,3], labels = as.character(caf_mclust$classification))

plot(caf_pc$scores[,2], caf_pc$scores[,3], xlab="PC2", ylab="PC3", type = "n")
text(caf_pc$scores[,2], caf_pc$scores[,3], labels = as.character(caf_mclust$classification))

# Base

biplot(caf_pc, choices = c(1,2), xlabs = as.character(caf_mclust$classification), col = c("black"), xlim=c(-0.3,0.3))
biplot(caf_pc, choices = c(1,3), xlabs = as.character(caf_mclust$classification), col = c("black"), xlim=c(-0.3,0.3))
biplot(caf_pc, choices = c(2,3), xlabs = as.character(caf_mclust$classification), col = c("black"), xlim=c(-0.5,0.2))

# ggplot

# Figure 2

caf_biplot <- ggbiplot_custom(caf_pc, choices = c(1,2), obs.scale = 1, var.scale = 1, varname.size = 8, alpha = 0.9,
                              stroke = 2, groups = as.factor(caf_mclust$classification), ellipse = TRUE, circle = FALSE, shape = as.factor(caf_mclust$classification)) +
  scale_shape_manual(name = '', 
                     values=c(22, 24, 23, 21),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) + 
  scale_size_manual(name = '',
                    values = c(4,4,4,4),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) + 
  scale_color_manual(name = '',
                     values = c("black", "black", "black", "black"),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) +
  scale_fill_manual(name = '',
                    values = c("black", "black", "white", "grey"),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) +
  annotate("text", x = 2.4, y = -3.3, label = "Group 2") +
  annotate("text", x = -1.6, y = -1.1, label = "Group 3") +
  annotate("text", x = -1.6, y = 0.8, label = "Group 4") +
  annotate("text", x = 1, y = 1.75, label = "Group 1") +
  theme_bw() + 
  theme(legend.direction = 'vertical',
        legend.position = 'right',
        text=element_text(family="Times", face="bold", size = 20),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))
caf_biplot
#ggsave("caf_biplot.tiff", plot = caf_biplot, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# Make boxplots of cluster means for each group

# Figure 3

caf_box <- list()
for (i in 1:6) {
  #j=show[i]
  caf_box[[i]] <- ggplot(as.data.frame(caf_scale), aes_string(as.factor(caf_mclust$classification), colnames(as.data.frame(caf_scale)[i])))  + 
    geom_boxplot() +
    theme_bw() + 
    theme(text=element_text(family="Times", face="bold", size = 20),
          panel.grid.major = element_blank()) +
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(colnames(as.data.frame(caf_scale))[i]) +
    theme(plot.title = element_text(hjust = 0.5))
}

caf_box_gg <- arrangeGrob(grobs = caf_box,
                          nrow = 2,
                          ncol = 3)
#ggsave("caf_box.tiff", plot = caf_box_gg, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# fountain_stat 

fount_mclust <- Mclust(fount_scale, G=4) # gives 4 clusters
summary(fount_mclust)

# Let's align group assignment with cafeteria groups

fount_g1 <- which(fount_mclust$classification == 2)
fount_g2 <- which(fount_mclust$classification == 4)
fount_g3 <- which(fount_mclust$classification == 1)
fount_g4 <- which(fount_mclust$classification == 3)

fount_mclust$classification[fount_g1] <- 1
fount_mclust$classification[fount_g2] <- 2
fount_mclust$classification[fount_g3] <- 3
fount_mclust$classification[fount_g4] <- 4

summary(fount_mclust)

fount_group1 <- nrow(fountain[fount_mclust$classification == 1,])
fount_group2 <- nrow(fountain[fount_mclust$classification == 2,])
fount_group3 <- nrow(fountain[fount_mclust$classification == 3,])
fount_group4 <- nrow(fountain[fount_mclust$classification == 4,])

fount_sumTab <- data.frame(rbind(fount_group1,fount_group2,fount_group3,fount_group4))

fount_mclust$classification

# plot(fount_mclust) 0 gets you out

plot(fount_pc$scores[,1], fount_pc$scores[,2], xlab="PC1", ylab="PC2", type = "n")
text(fount_pc$scores[,1], fount_pc$scores[,2], labels = as.character(fount_mclust$classification))

plot(fount_pc$scores[,1], fount_pc$scores[,3], xlab="PC1", ylab="PC3", type = "n")
text(fount_pc$scores[,1], fount_pc$scores[,3], labels = as.character(fount_mclust$classification))

plot(fount_pc$scores[,2], fount_pc$scores[,3], xlab="PC2", ylab="PC3", type = "n")
text(fount_pc$scores[,2], fount_pc$scores[,3], labels = as.character(fount_mclust$classification))

# base 

biplot(fount_pc, choices = c(1,2), xlabs = as.character(fount_mclust$classification), xlim=c(-0.3,0.3), col = c("black"))
biplot(fount_pc, choices = c(1,3), xlabs = as.character(fount_mclust$classification), col = c("black"))
biplot(fount_pc, choices = c(2,3), xlabs = as.character(fount_mclust$classification), xlim=c(-0.6,0.2), col = c("black"))

# ggplot

# Figure S3

fount_biplot <- ggbiplot_custom(fount_pc, choices = c(1,2), obs.scale = 1, var.scale = 1, varname.size = 8, alpha = 0.9,
                                stroke = 2, groups = as.factor(fount_mclust$classification), ellipse = TRUE, circle = FALSE) +
  scale_shape_manual(name = '', 
                     values=c(22, 24, 23, 21),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) + 
  scale_size_manual(name = '',
                    values = c(4,4,4,4),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) + 
  scale_color_manual(name = '',
                     values = c("black", "black", "black", "black"),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) +
  scale_fill_manual(name = '',
                    values = c("black", "black", "white", "grey"),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) +
  annotate("text", x = 2.6, y = -3.3, label = "Group 2") +
  annotate("text", x = -1.5, y = -0.85, label = "Group 3") +
  annotate("text", x = -1.5, y = 1, label = "Group 4") +
  annotate("text", x = 2.8, y = 1, label = "Group 1") +
  annotate("segment", x = 2.1, xend = 2.45, y = 0.9, yend = 1) +
  theme_bw() + 
  theme(legend.direction = 'vertical',
        legend.position = 'right',
        text=element_text(family="Times", face="bold", size = 20),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))
fount_biplot
#ggsave("fount_biplot.tiff", plot = fount_biplot, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# Make boxplots of cluster means for each

# Figure S4

fount_box <- list()
for (i in 1:6) {
  #j=show[i]
  fount_box[[i]] <- ggplot(as.data.frame(fount_scale), aes_string(as.factor(fount_mclust$classification), colnames(as.data.frame(fount_scale)[i])))  + 
    geom_boxplot() +
    theme_bw() + 
    theme(text=element_text(family="Times", face="bold", size = 20),
          panel.grid.major = element_blank()) +
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(colnames(as.data.frame(fount_scale))[i]) +
    theme(plot.title = element_text(hjust = 0.5))
}
fount_box_gg <- arrangeGrob(grobs = fount_box,
                            nrow = 2,
                            ncol = 3)
#ggsave("fount_box.tiff", plot = fount_box_gg, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# Summary table of both spouts

class_summaryTab <- cbind(caf_sumTab,
                          fount_sumTab)

# Section 5: Spatial arrangement of school buildings according to GPS coordinates and labeling according to group assignment from model based clustering using elemental concentrations in drinking water as feature inputs  #############

# we consider all samples
map <- qmap(location = c(lon = -80.235000, lat = 36.120000),
            zoom = 11,
            maptype = "roadmap",
            color = "bw",
            source = "google",
            legend = "topright")

map + geom_point(data = water, aes(x = longitude, y = latitude), color="black", size=2, alpha=0.5)

# School buildings - cafeteria sinks

# Figure 4

caf_map <- map + geom_point(data = cafeteria,
                            aes(x = longitude, y = latitude, color = as.factor(caf_mclust$classification),
                                size = as.factor(caf_mclust$classification), shape = as.factor(caf_mclust$classification),
                                fill = as.factor(caf_mclust$classification)),
                            alpha = 0.9, stroke = 2) +
  scale_shape_manual(name = '', 
                     values=c(22, 24, 23, 21),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) + 
  scale_size_manual(name = '',
                    values = c(5,5,5,5),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) + 
  scale_color_manual(name = '',
                     values = c("black", "black", "black", "black"),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) +
  scale_fill_manual(name = '',
                    values = c("black", "black", "white", "grey"),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) +
  xlab(NULL) +
  ylab(NULL) + 
  theme(legend.direction = 'horizontal',
        #legend.position = 'right',
        text=element_text(family="Times", face="bold"))
caf_map
#ggsave("caf_map.tiff", plot = caf_map, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# School buildings - Water fountains

# Figure S5

fount_map <- map + geom_point(data = fountain,
                              aes(x = longitude, y = latitude, color = as.factor(fount_mclust$classification),
                                  size = as.factor(fount_mclust$classification), shape = as.factor(fount_mclust$classification),
                                  fill = as.factor(fount_mclust$classification)),
                              alpha = 0.9, stroke = 2) +
  scale_shape_manual(name = '', 
                     values=c(22, 24, 23, 21),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) + 
  scale_size_manual(name = '',
                    values = c(5,5,5,5),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) + 
  scale_color_manual(name = '',
                     values = c("black", "black", "black", "black"),
                     labels = c("Group 1",
                                "Group 2",
                                "Group 3",
                                "Group 4")) +
  scale_fill_manual(name = '',
                    values = c("black", "black", "white", "grey"),
                    labels = c("Group 1",
                               "Group 2",
                               "Group 3",
                               "Group 4")) +
  xlab(NULL) +
  ylab(NULL) + 
  theme(legend.direction = 'horizontal',
        #legend.position = 'right',
        text=element_text(family="Times", face="bold"))
fount_map
#ggsave("fount_map.tiff", plot = fount_map, units="in", width=10, height=8, dpi=300, compression = 'lzw')
dev.off()

# Section 6: Summer flush, read in data and format data objects ############################################################

summer_flush <- read_excel("summer_flush_1.xlsx")
View(summer_flush)

summer_flush$School <- as.factor(summer_flush$School)
str(summer_flush)

colnames(summer_flush)[c(1,2,3)] <- c("Time", "Cu", "Pb")
colnames(summer_flush)

flush_location_C <- dplyr::filter(summer_flush, summer_flush$School %in% c("Location C Cafeteria", "Location C Fountain"))

flush_SI <- dplyr::filter(summer_flush, summer_flush$School != "Location C Cafeteria" & summer_flush$School != "Location C Fountain")  

# Section 6.1: Copper and lead flush #################################################

Pb_Location_C <- ggplot(data = flush_location_C, aes(x = Time, y = Pb)) + 
  geom_point(size = 2) +
  geom_line() + 
  xlab(expression(bold("Time (min)"))) + 
  ylab(expression(bold(paste("Pb (", mu,"g " , L^-1,")")))) +
  #  geom_hline(yintercept = 15, size = 2) + 
  facet_wrap(~ School) + 
  theme_bw() + 
  theme(legend.direction = 'horizontal',
        legend.position = 'top',
        text=element_text(family="Times", face="bold", size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

Cu_Location_C <- ggplot(data = flush_location_C, aes(x = Time, y = Cu)) + 
  geom_point(size = 2) +
  geom_line() + 
  xlab(expression(bold("Time (min)"))) + 
  ylab(expression(bold(paste("Cu (", mu,"g " , L^-1,")")))) +
  #  geom_hline(yintercept = 15, size = 2) + 
  facet_wrap(~ School) + 
  theme_bw() + 
  theme(legend.direction = 'horizontal',
        legend.position = 'top',
        text=element_text(family="Times", face="bold", size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Figure 5

flush <- arrangeGrob(grobs = list(Cu_Location_C, Pb_Location_C),
                     nrow = 1,
                     ncol = 2)

#ggsave("flush.tiff", plot = flush, units="in", width=10, height=4, dpi=300, compression = 'lzw')
dev.off()

# Lead

Pb_Location_SI <- ggplot(data = flush_SI, aes(x = Time, y = Pb)) + 
  geom_point(size = 2) +
  geom_line() + 
  xlab(expression(bold("Time (min)"))) + 
  ylab(expression(bold(paste("Pb (", mu,"g " , L^-1,")")))) +
  #  geom_hline(yintercept = 15, size = 2) + 
  facet_wrap(~ School) + 
  theme_bw() + 
  theme(legend.direction = 'horizontal',
        legend.position = 'top',
        text=element_text(family="Times", face="bold", size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Copper 

Cu_Location_SI <- ggplot(data = flush_SI, aes(x = Time, y = Cu)) + 
  geom_point(size = 2) +
  geom_line() + 
  xlab(expression(bold("Time (min)"))) + 
  ylab(expression(bold(paste("Cu (", mu,"g " , L^-1,")")))) +
  #  geom_hline(yintercept = 15, size = 2) + 
  facet_wrap(~ School) + 
  theme_bw() + 
  theme(legend.direction = 'horizontal',
        legend.position = 'top',
        text=element_text(family="Times", face="bold", size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Figure S6

flush_SI <- arrangeGrob(grobs = list(Cu_Location_SI, Pb_Location_SI),
                        nrow = 1,
                        ncol = 2)

# ggsave("flush_SI.tiff", plot = flush_SI, units="in", width=10, height=4, dpi=300, compression = 'lzw')
dev.off()