library(tidyverse)
setwd("P:/RaBET/Results/")
##Data Visualizati
trainData_indices <- read.csv("P:/RaBET/RaBET_species/Outputs/final_results/trainData_102021.csv") %>% dplyr::select(-c(MNDWI_2019,MNDWI_2018,MNDWI_2017,NDII_2019,NDII_2018,NDII_2017,X.1,X))

names(trainData_indices) <- c('NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019', 
                              'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                              'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CHM_2019',
                              'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                              'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                              'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018',
                              'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017','PRI2_2017',
                              'CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017','CAI_2017','CELL_2017',
                              'CELL2_2017','NDNI_2017','CHM_2017','class')
trainData_indices_outlier.rm <- read.csv('P:/RaBET/Results/trainData_cln_ac_cleaned.csv')
trainData_indices_outlier.rm_CI <- read.csv('P:/RaBET/Results/trainData_102021_outliersrm_cln_ac.csv')

names(trainData_indices) <- c('ID', 'NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019', 
                         'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                         'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CHM_2019',
                         'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018',
                         'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017','PRI2_2017',
                         'CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017','CAI_2017','CELL_2017',
                         'CELL2_2017','NDNI_2017','CHM_2017','class')

#NEON_2019 <- trainData_indices %>% select(c(2:16,47))
#names(NEON_2019) <- c('NDVI','NDWI','PRI','SWIRI','SAVI', 
#                      'PRI2','CACTI','CACTI2','MTCI','CI',
#                      'CAI','CELL','CELL2','NDNI','CHM','class')
#NEON_2018 <- trainData_indices %>% select(c(17:31,47))
#names(NEON_2018) <- c('NDVI','NDWI','PRI','SWIRI','SAVI', 
#                    'PRI2','CACTI','CACTI2','MTCI','CI',
#                    'CAI','CELL','CELL2','NDNI','CHM','class')
#NEON_2017 <- trainData_indices %>% select(c(32:46,47))
#names(NEON_2017) <- c('NDVI','NDWI','PRI','SWIRI','SAVI', 
#                      'PRI2','CACTI','CACTI2','MTCI','CI',
#                      'CAI','CELL','CELL2','NDNI','CHM','class')

#trainData_indices <- rbind(NEON_2019,NEON_2018, NEON_2017)
#desert vegetation
val_bareground <- subset(trainData_indices, class == 1)

val_grass <- subset(trainData_indices, class == 2)

val_mesquite <- subset(trainData_indices, class == 3)


val_cactus <- subset(trainData_indices, class == 4)

val_lotebush <- subset(trainData_indices, class == 5)


val_paloverde <- subset(trainData_indices, class == 6)

val_creosote <- subset(trainData_indices, class == 7)

trainData_indices <- rbind(val_bareground, val_grass, val_mesquite,val_cactus,val_lotebush,val_paloverde,val_creosote)
#write.csv(trainData_indices, file = 'trainData_cln_ac_cleaned.csv')

hypersectral_2019 <- read.csv('P:/RaBET/Indices/Neon_2019_hyperspectral_subset.csv')
mean_hyperspectral_2019 <- hypersectral_2019 %>%
    group_by(class) %>%
    summarise_at(c(2:12), list(name = mean))

hypersectral_2018 <- read.csv('P:/RaBET/Indices/Neon_2018_hyperspectral_subset.csv')
mean_hyperspectral_2018 <- hypersectral_2018 %>%
    group_by(class) %>%
    summarise_at(c(2:12), list(name = mean))

hypersectral_2017 <- read.csv('P:/RaBET/Indices/Neon_2017_hyperspectral_subset.csv')
mean_hyperspectral_2017 <- hypersectral_2017 %>%
    group_by(class) %>%
    summarise_at(c(2:12), list(name = mean))

scatterplot(NDVI_2021 ~ CI_2021 | class, 
            ellipse=TRUE, regLine=FALSE, smooth=FALSE, data=trainData_indices, pch = c(1,1,1,1,1,1,1), 
            col = c('gray','blue','yellow','orange','green', 'brown'), ylab = "CACTI", xlab = "NDVI", main = "2019 Indices",
            cex.lab=1.5,cex.main=2, cex.axis=1.5)

#write.csv(mean_hyperspectral_2019, file = "mean_hyperspectral_2019.csv")
#write.csv(mean_hyperspectral_2018, file = "mean_hyperspectral_2018.csv")
write.csv(mean_hyperspectral_2017, file = "mean_hyperspectral_2017.csv")


#INDICES
library(car)
windows()
pdf(file = 'P:/RaBET/Results/indices.pdf')
par(mar=c(1,1,1,1))
scatterplot(NDVI_2019 ~ CACTI_2019 | class, 
            ellipse=TRUE, regLine=FALSE, smooth=FALSE, data=trainData_indices, pch = c(1,1,1,1,1,1,1), 
            col = c('grey', 'orange','green','black','blue', 'magenta', 'brown'), ylab = "CACTI", xlab = "NDVI", main = "2019 Indices",
            cex.lab=1.5,cex.main=2, cex.axis=1.5)


scatterplot(NDVI_2019 ~ CACTI_2019 | class, 
            ellipse=TRUE, regLine=FALSE, smooth=FALSE, data=trainData_indices_outlier.rm, pch = c(1,1,1,1,1,1,1), 
            col = c('grey', 'orange','green','black','blue', 'magenta', 'brown'), ylab = "CACTI", xlab = "NDVI", main = "2019 Indices",
            cex.lab=1.5,cex.main=2, cex.axis=1.5)

scatterplot(NDVI_2019 ~ CACTI_2019 | class, 
            ellipse=TRUE, regLine=FALSE, smooth=FALSE, data=trainData_indices_outlier.rm_CI, pch = c(1,1,1,1,1,1,1), 
            col = c('grey', 'orange','green','black','blue', 'magenta', 'brown'), ylab = "CACTI", xlab = "NDVI", main = "2019 Indices",
            cex.lab=1.5,cex.main=2, cex.axis=1.5)
dev.off()


#CHM
pdf(file = 'P:/RaBET/Results/CHM_boxplot_points.pdf')

boxplot(val_bareground$CHM_2018, val_grass$CHM_2018, 
        val_mesquite$CHM_2018, val_cactus$CHM_2018, 
        val_lotebush$CHM_2018, val_paloverde$CHM_2018, 
        val_creosote$CHM_2018, main="Class Canopy Height Model",
        xlab="class",
        ylab="CHM_2018 (m)",
        col="orange",
        border="black",
        names = c("bareground","grass","mesquite","cactus","lotebush","paloverde","creosote"),
        outline=FALSE
)

dev.off()


#INDEX PLOTS 2018
pdf(file = 'P:/RaBET/Results/CELL_NDNI_2018_points.pdf')
plot(CELL_2018 ~ NDNI_2018, data = val_mesquite, ylab = "NDNI", xlab = "CELL", main = "2018 Indices", 
    ylim = c(-0.1,0.1), xlim = c(-0.05,0),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#points(CELL_2018 ~ NDNI_2018, data = val_bareground, col="red")
#points(CELL_2018 ~ NDNI_2018, data = val_grass, col="red")
points(CELL_2018 ~ NDNI_2018, data = val_cactus, col="orange")
points(CELL_2018 ~ NDNI_2018, data = val_lotebush, col="purple")
points(CELL_2018 ~ NDNI_2018, data = val_paloverde, col="green")
points(CELL_2018 ~ NDNI_2018, data = val_creosote, col="black")


#abline(lm(CELL_2018 ~ NDNI_2018, data = val_bareground), col ='black')
#abline(lm(CELL_2018 ~ NDNI_2018, data = val_grass), col = 'red')
abline(lm(CELL_2018 ~ NDNI_2018, data = val_mesquite),  col = 'red')
abline(lm(CELL_2018 ~ NDNI_2018, data = val_cactus), col = 'orange')
abline(lm(CELL_2018 ~ NDNI_2018, data = val_lotebush), col = 'purple')
abline(lm(CELL_2018 ~ NDNI_2018, data = val_paloverde), col = 'green')
abline(lm(CELL_2018 ~ NDNI_2018, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('bottomright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()



#CHM
pdf(file = 'P:/RaBET/Results/CHM_boxplot_points.pdf')

boxplot(val_bareground$CHM_2018, val_grass$CHM_2018, 
        val_mesquite$CHM_2018, val_cactus$CHM_2018, 
        val_lotebush$CHM_2018, val_paloverde$CHM_2018, 
        val_creosote$CHM_2018, main="Class Canopy Height Model",
        xlab="class",
        ylab="CHM_2018 (m)",
        col="orange",
        border="black",
        names = c("bareground","grass","mesquite","cactus","lotebush","paloverde","creosote"),
        outline=FALSE
)

dev.off()

library(ggfortify)

pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)


#INDEX PLOTS 2019

#ggplot(data.frame(predicted))+aes(x=CACTI_2019,y=SWIRI_2019,color=iris$Species)+geom_point(aes(size=iris$Sepal.Length))+stat_ellipse()+stat_ellipse(level=0.8)
pdf(file = 'P:/RaBET/Results/CACTI_SWIRI_2019_points.pdf')
plot(CACTI_2019 ~ SWIRI_2019, data = val_mesquite, ylab = "SWIRI", xlab = "CACTI", main = "2019 Indices", 
     ylim = c(-0.05,0.27), xlim = c(0,0.17),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#points(CACTI_2019 ~ SWIRI_2019, data = val_bareground, col="red")
#points(CACTI_2019 ~ SWIRI_2019, data = val_grass, col="red")
points(CACTI_2019 ~ SWIRI_2019, data = val_cactus, col="orange")
points(CACTI_2019 ~ SWIRI_2019, data = val_lotebush, col="purple")
points(CACTI_2019 ~ SWIRI_2019, data = val_paloverde, col="green")
points(CACTI_2019 ~ SWIRI_2019, data = val_creosote, col="black")


#abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_bareground), col ='black')
#abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_grass), col = 'red')
abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_mesquite),  col = 'red')
abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_cactus), col = 'orange')
abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_lotebush), col = 'purple')
abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_paloverde), col = 'green')
abline(lm(CACTI_2019 ~ SWIRI_2019, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('topright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()





pdf(file = 'P:/RaBET/Results/savi_SWIRI_2019_points.pdf')
#windows()
plot(SAVI_2019 ~ SWIRI_2019, data = val_mesquite, ylab = "SWIRI", xlab = "SAVI", main = "2019 Indices", 
     ylim = c(0.1,0.6), xlim = c(0.02,0.17),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#pointS(SAVI_2019 ~ SWIRI_2019, data = val_bareground, col="red")
#pointS(SAVI_2019 ~ SWIRI_2019, data = val_grass, col="red")
points(SAVI_2019 ~ SWIRI_2019, data = val_cactus, col="orange")
points(SAVI_2019 ~ SWIRI_2019, data = val_lotebush, col="purple")
points(SAVI_2019 ~ SWIRI_2019, data = val_paloverde, col="green")
points(SAVI_2019 ~ SWIRI_2019, data = val_creosote, col="black")


#abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_bareground), col ='black')
#abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_grass), col = 'red')
abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_mesquite),  col = 'red')
abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_cactus), col = 'orange')
abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_lotebush), col = 'purple')
abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_paloverde), col = 'green')
abline(lm(SAVI_2019 ~ SWIRI_2019, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('bottomright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()






#INDEX PLOTS 2018
pdf(file = 'P:/RaBET/Results/CELL_NDVI_2018_points.pdf')
plot(CI_2018 ~ NDVI_2018, data = val_mesquite, ylab = "NDVI", xlab = "CELL", main = "2018 Indices", 
     ylim = c(0.57,01), xlim = c(0.2,0.6),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#points(CI_2018 ~ NDVI_2018, data = val_bareground, col="red")
#points(CI_2018 ~ NDVI_2018, data = val_grass, col="red")
points(CI_2018 ~ NDVI_2018, data = val_cactus, col="orange")
points(CI_2018 ~ NDVI_2018, data = val_lotebush, col="purple")
points(CI_2018 ~ NDVI_2018, data = val_paloverde, col="green")
points(CI_2018 ~ NDVI_2018, data = val_creosote, col="black")


#abline(lm(CI_2018 ~ NDVI_2018, data = val_bareground), col ='black')
#abline(lm(CI_2018 ~ NDVI_2018, data = val_grass), col = 'red')
abline(lm(CI_2018 ~ NDVI_2018, data = val_mesquite),  col = 'red')
abline(lm(CI_2018 ~ NDVI_2018, data = val_cactus), col = 'orange')
abline(lm(CI_2018 ~ NDVI_2018, data = val_lotebush), col = 'purple')
abline(lm(CI_2018 ~ NDVI_2018, data = val_paloverde), col = 'green')
abline(lm(CI_2018 ~ NDVI_2018, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('bottomright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()





#EXTRA
pdf(file = 'P:/RaBET/Results/CACTI_SAVI_2019_points.pdf')
plot(CACTI_2019 ~ SAVI_2019, data = val_mesquite, ylab = "SAVI", xlab = "CACTI", main = "2019 Indices", 
    ylim = c(-0.05,0.27), xlim = c(0.15,0.60),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#points(CACTI_2019 ~ SAVI_2019, data = val_bareground, col="red")
#points(CACTI_2019 ~ SAVI_2019, data = val_grass, col="red")
points(CACTI_2019 ~ SAVI_2019, data = val_cactus, col="orange")
points(CACTI_2019 ~ SAVI_2019, data = val_lotebush, col="purple")
points(CACTI_2019 ~ SAVI_2019, data = val_paloverde, col="green")
points(CACTI_2019 ~ SAVI_2019, data = val_creosote, col="black")


#abline(lm(CACTI_2019 ~ SAVI_2019, data = val_bareground), col ='black')
#abline(lm(CACTI_2019 ~ SAVI_2019, data = val_grass), col = 'red')
abline(lm(CACTI_2019 ~ SAVI_2019, data = val_mesquite),  col = 'red')
abline(lm(CACTI_2019 ~ SAVI_2019, data = val_cactus), col = 'orange')
abline(lm(CACTI_2019 ~ SAVI_2019, data = val_lotebush), col = 'purple')
abline(lm(CACTI_2019 ~ SAVI_2019, data = val_paloverde), col = 'green')
abline(lm(CACTI_2019 ~ SAVI_2019, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('topright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()








pdf(file = 'P:/RaBET/Results/CACTI_SAVI_2019_points.pdf')
plot(NDNI_2018 ~ SAVI_2019, data = val_mesquite, ylab = "SAVI", xlab = "CACTI", main = "2019 Indices", 
     #ylim = c(-0.05,0.27), xlim = c(0.15,0.60),
     cex.lab=1.5,cex.main=2, cex.axis=1.5, col = 'red')

#points(NDNI_2018 ~ SAVI_2019, data = val_bareground, col="red")
#points(NDNI_2018 ~ SAVI_2019, data = val_grass, col="red")
points(NDNI_2018 ~ SAVI_2019, data = val_cactus, col="orange")
points(NDNI_2018 ~ SAVI_2019, data = val_lotebush, col="purple")
points(NDNI_2018 ~ SAVI_2019, data = val_paloverde, col="green")
points(NDNI_2018 ~ SAVI_2019, data = val_creosote, col="black")


#abline(lm(NDNI_2018 ~ SAVI_2019, data = val_bareground), col ='black')
#abline(lm(NDNI_2018 ~ SAVI_2019, data = val_grass), col = 'red')
abline(lm(NDNI_2018 ~ SAVI_2019, data = val_mesquite),  col = 'red')
abline(lm(NDNI_2018 ~ SAVI_2019, data = val_cactus), col = 'orange')
abline(lm(NDNI_2018 ~ SAVI_2019, data = val_lotebush), col = 'purple')
abline(lm(NDNI_2018 ~ SAVI_2019, data = val_paloverde), col = 'green')
abline(lm(NDNI_2018 ~ SAVI_2019, data = val_creosote), col = 'black')

legend('topleft', legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), lty=1, cex=0.8, box.lty=1)
legend('topright',  legend=c("mesquite", "cactus", "lotebush", "paloverde", "creosote"),col=c("red", "orange", "purple", "green", "black"), pch=1, cex=0.8, box.lty=1)
dev.off()
library(car)
windows()
pdf(file = 'P:/RaBET/Results/CELL_NDWI_2019_points_cfelipse.pdf')

scatterplot(CELL_2019 ~ NDWI_2019 | class, 
            ellipse=TRUE, regLine=FALSE, smooth=FALSE, data=trainData_indices, pch = c(1,1,1,1,1), 
            col = c('red','orange','purple', 'green', 'black'), ylab = "CELL", xlab = "NDWI", main = "2019 Indices",
            cex.lab=1.5,cex.main=2, cex.axis=1.5)
dev.off()

###PCA
confidenceEllipse(lm(prestige~income+education, data=Duncan), 
                  L=c("income + education", "income - education"))
pc <- prcomp(trainData_indices[,c(-1,-47)],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)
library(devtools)
library(cluster)
library(ggfortify)
autoplot(pc)
autoplot(pam(trainData_indices[c(-1)], 5), frame = TRUE, frame.type = 'norm')
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)