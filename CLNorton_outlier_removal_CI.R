library(tidyverse)
library(mice)


##Clean dataset
trainData_indices <- read.csv("P:/RaBET/Results//trainData_102021_outliersrm_cln_ac.csv")
md.pattern(trainData_indices)
trainData_indices_mice <- mice(trainData_indices,m=5,maxit=50,meth='pmm',seed=500)
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
trainData_nolidar <- trainData_indices %>% select(-c(CHM_2019, CHM_2018, CHM_2017))
trainData_lidar <- trainData_indices %>% select(c(X,CHM_2019, CHM_2018, CHM_2017, class))


val_bareground <- subset(trainData_nolidar, class == 1)
val_bareground_outlier.rm <- val_bareground %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  #mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  #filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "1")%>%
  select(-row)

imputed_Data <- mice(val_bareground, m=5, maxit = 50, method = 'pmm', seed = 500)


boxplot(val_bareground_outlier.rm$NDVI_2019)
boxplot(val_bareground_outlier.rm$CAI_2019)


val_grass <- subset(trainData_nolidar, class == 2)
val_grass_outlier.rm <- val_grass %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  #mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  #filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "2")%>%
  select(-row)


boxplot(val_grass_outlier.rm$NDVI_2019)
boxplot(val_grass_outlier.rm$CAI_2019)



val_mesquite <- subset(trainData_nolidar, class == 3)
val_mesquite_outlier.rm <- val_mesquite %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "3")%>%
  select(-row)


boxplot(val_mesquite_outlier.rm$NDVI_2019)
boxplot(val_mesquite_outlier.rm$CAI_2019)



val_cactus <- subset(trainData_nolidar, class == 4)
val_cactus_outlier.rm <- val_cactus %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "4")%>%
  select(-row)


boxplot(val_cactus_outlier.rm$NDVI_2019)
boxplot(val_cactus_outlier.rm$CAI_2019)


val_lotebush <- subset(trainData_nolidar, class == 5)
val_lotebush_outlier.rm <- val_lotebush %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "5")%>%
  select(-row)


boxplot(val_lotebush_outlier.rm$NDVI_2019)
boxplot(val_lotebush_outlier.rm$CAI_2019)


val_paloverde <- subset(trainData_nolidar, class == 6)
val_paloverde_outlier.rm <- val_paloverde %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "6")%>%
  select(-row)


boxplot(val_paloverde_outlier.rm$NDVI_2019)
boxplot(val_paloverde_outlier.rm$CAI_2019)


val_creosote <- subset(trainData_nolidar, class == 7)
val_creosote_outlier.rm <- val_creosote %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "7")%>%
  select(-row)


boxplot(val_creosote_outlier.rm$NDVI_2019)
boxplot(val_creosote_outlier.rm$CAI_2019)




trainData_nolidar_outlier.rm <- rbind(val_bareground,val_grass_outlier.rm,val_mesquite_outlier.rm,val_cactus_outlier.rm,val_lotebush_outlier.rm,val_paloverde_outlier.rm,val_creosote_outlier.rm)
trainData_indices_oulier.rm <-merge(trainData_nolidar_outlier.rm, trainData_lidar)
trainData_indices_oulier.rm_na.omit <- na.omit(trainData_indices_oulier.rm)
write.csv(trainData_indices_oulier.rm_na.omit, file= "trainData_102021_ouliersrm_cln_ac.csv")



##PLOT
trainData_before_CI <-read.csv("C:/Users/BatCave/Documents/RaBET/trainData_092721_clean_ac.csv")
trainData_after_CI <-read.csv("C:/Users/BatCave/Documents/RaBET/trainData_101321_ouliersrm_cln_ac.csv")
