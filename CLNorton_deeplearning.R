# Deep Learning
library(keras)
library(tensorflow)
library(tfdatasets)
library(purrr)
library(ggplot2)
library(rsample)
library(stars)
library(raster)
library(reticulate)
library(mapview)


#If not done yet, download and unzip the tutorial data:

#download.file("https://uni-muenster.sciebo.de/s/SOjP7fRGBRztn9z/download",destfile = "tutorial_data.zip")
#unzip("./tutorial_data.zip")
#setwd("./tutorial_data/")

#initiate an empty model
first_model <- keras_model_sequential()
#add first layer, the expected input is of shape 128 by 128 on three channels (we will be dealing with RGB images)
layer_conv_2d(first_model,filters = 32,kernel_size = 3, activation = "relu",input_shape = c(128,128,3))

summary(first_model)

layer_max_pooling_2d(first_model, pool_size = c(2, 2)) 
layer_conv_2d(first_model, filters = 64, kernel_size = c(3, 3), activation = "relu") 
layer_max_pooling_2d(first_model, pool_size = c(2, 2)) 
layer_conv_2d(first_model, filters = 128, kernel_size = c(3, 3), activation = "relu") 
layer_max_pooling_2d(first_model, pool_size = c(2, 2)) 
layer_conv_2d(first_model, filters = 128, kernel_size = c(3, 3), activation = "relu")
layer_max_pooling_2d(first_model, pool_size = c(2, 2)) 
layer_flatten(first_model) 
layer_dense(first_model, units = 256, activation = "relu")
layer_dense(first_model, units = 1, activation = "sigmoid")


# get all file paths of the images containing our target
subset_list <- list.files("./training_part1/true", full.names = T)

# create a data.frame with two coloumns: file paths, and the labels (1)
data_true <- data.frame(img=subset_list,lbl=rep(1L,length(subset_list)))

# get all file paths of the images containing non-targets
subset_list <- list.files("./training_part1/false", full.names = T)

#creating a data.frame with two coloumns: file paths, and the labels (0)
data_false <- data.frame(img=subset_list,lbl=rep(0L,length(subset_list)))

#merge both data.frames
data <- rbind(data_true,data_false)

# randomly split data set into training (~75%) and validation data (~25%)
# use `lbl` as stratum, so that the split is being done proportional for
# targets and non-targets
set.seed(2020)
data <- initial_split(data,prop = 0.75, strata = "lbl")

head(training(data)) # returns the the first few entries in the training data.frame

head(testing(data)) # returns the first few entries of the data set aside for validation

# compare the number of files in the training data, that show non-targets vs, those that
# show targets -> should be similiar
c(nrow(training(data)[training(data)$lbl==0,]), nrow(training(data)[training(data)$lbl==1,]))

#prepare training dataset
training_dataset <- tensor_slices_dataset(training(data))

#if you want to get a list of all tensors, you can use the as_iterator() and iterate() functions
dataset_iterator <- as_iterator(training_dataset)
dataset_list <- iterate(dataset_iterator)
#check the first few items of that list, each item one is again a list with two items: img and lbl
head(dataset_list)



#get input shape expected by first_model
subset_size <- first_model$input_shape[2:3]

# apply function on each dataset element: function is list_modify()
#->modify list item "img" three times:

# 1 read decode jpeg
training_dataset <- 
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$decode_jpeg(tf$io$read_file(.x$img))))

# 2 convert data type
training_dataset <- 
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)))

# 3 resize to the size expected by model
training_dataset <- 
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$resize(.x$img, size = shape(subset_size[1], subset_size[2]))))



training_dataset <- dataset_shuffle(training_dataset, buffer_size = 10L*128)
training_dataset <- dataset_batch(training_dataset, 10L)
training_dataset <- dataset_map(training_dataset, unname)


dataset_iterator <- as_iterator(training_dataset)
dataset_list <- iterate(dataset_iterator)
dataset_list[[1]][[1]]



dataset_list[[1]][[1]]$shape

dataset_list[[1]][[2]]



#validation
validation_dataset <- tensor_slices_dataset(testing(data))

validation_dataset <- 
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$decode_jpeg(tf$io$read_file(.x$img))))

validation_dataset <- 
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)))

validation_dataset <- 
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$resize(.x$img, size = shape(subset_size[1], subset_size[2]))))

validation_dataset <- dataset_batch(validation_dataset, 10L)
validation_dataset <- dataset_map(validation_dataset, unname)



compile(
  first_model,
  optimizer = optimizer_rmsprop(lr = 5e-5),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)



diagnostics <- fit(first_model,
                   training_dataset,
                   epochs = 15,
                   validation_data = validation_dataset)

plot(diagnostics)



predictions <- predict(first_model,validation_dataset)
head(predictions)



tail(predictions)



par(mfrow=c(1,3),mai=c(0.1,0.1,0.3,0.1),cex=0.8)
for(i in 1:3){
  sample <- floor(runif(n = 1,min = 1,max = 56))
  img_path <- as.character(testing(data)[[sample,1]])
  img <- stack(img_path)
  plotRGB(img,margins=T,main = paste("prediction:",round(predictions[sample],digits=3)," | ","label:",as.character(testing(data)[[sample,2]])))
}



# get all file paths of the images of our test area
subset_list <- list.files(path="./testarea_part1/subsets/", full.names = T)



dataset <- tensor_slices_dataset(subset_list)
dataset <- dataset_map(dataset, function(.x) 
  tf$image$decode_jpeg(tf$io$read_file(.x))) 
dataset <- dataset_map(dataset, function(.x) 
  tf$image$convert_image_dtype(.x, dtype = tf$float32)) 
dataset <- dataset_map(dataset, function(.x) 
  tf$image$resize(.x, size = shape(128, 128))) 
dataset <- dataset_batch(dataset, 10L)
dataset <-  dataset_map(dataset, unname)

predictions <- predict(first_model, dataset)


save_model_hdf5(first_model,filepath = "./my_first_model.h5")





# load vgg16 as basis for feature extraction
vgg16_feat_extr <- application_vgg16(include_top = F,input_shape = c(128,128,3),weights = "imagenet")
#freeze weights
freeze_weights(vgg16_feat_extr)
#use only layers 1 to 15
pretrained_model <- keras_model_sequential(vgg16_feat_extr$layers[1:15])



# add flatten and dense layers for classification 
# -> these dense layers are going to be trained on our data only
pretrained_model <- layer_flatten(pretrained_model)
pretrained_model <- layer_dense(pretrained_model,units = 256,activation = "relu")
pretrained_model <- layer_dense(pretrained_model,units = 1,activation = "sigmoid")



pretrained_model


compile(
  pretrained_model,
  optimizer = optimizer_rmsprop(lr = 1e-5),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

diagnostics <- fit(pretrained_model,
                   training_dataset,
                   epochs = 6,
                   validation_data = validation_dataset)
plot(diagnostics)



diagnostics$metrics


## we start with the "contratcing path"##
# input
input_tensor <- layer_input(shape = c(448,448,3))

#conv block 1
unet_tensor <- layer_conv_2d(input_tensor,filters = 64,kernel_size = c(3,3), padding = "same",activation = "relu")
conc_tensor2 <- layer_conv_2d(unet_tensor,filters = 64,kernel_size = c(3,3), padding = "same",activation = "relu")
unet_tensor <- layer_max_pooling_2d(conc_tensor2)

#conv block 2
unet_tensor <- layer_conv_2d(unet_tensor,filters = 128,kernel_size = c(3,3), padding = "same",activation = "relu")
conc_tensor1 <- layer_conv_2d(unet_tensor,filters = 128,kernel_size = c(3,3), padding = "same",activation = "relu")
unet_tensor <- layer_max_pooling_2d(conc_tensor1)

#"bottom curve" of unet
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256,kernel_size = c(3,3), padding = "same",activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256,kernel_size = c(3,3), padding = "same",activation = "relu")

##  this is where the expanding path begins ##

# upsampling block 1
unet_tensor <- layer_conv_2d_transpose(unet_tensor,filters = 128,kernel_size = c(2,2),strides = 2,padding = "same") 
unet_tensor <- layer_concatenate(list(conc_tensor1,unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = c(3,3),padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = c(3,3),padding = "same", activation = "relu")

# upsampling block 2
unet_tensor <- layer_conv_2d_transpose(unet_tensor,filters = 64,kernel_size = c(2,2),strides = 2,padding = "same")
unet_tensor <- layer_concatenate(list(conc_tensor2,unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = c(3,3),padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = c(3,3),padding = "same", activation = "relu")

# output
unet_tensor <- layer_conv_2d(unet_tensor,filters = 1,kernel_size = 1, activation = "sigmoid")

# combine final unet_tensor (carrying all the transformations applied through the layers) 
# with input_tensor to create model

unet_model <- keras_model(inputs = input_tensor, outputs = unet_tensor)

## load pretrained vgg16 and use part of it as contracting path (feature extraction) ##
vgg16_feat_extr <- application_vgg16(weights = "imagenet", include_top = FALSE, input_shape = c (448,448,3))

# optionally freeze first layers to prevent changing of their weights, either whole convbase or only certain layers
# freeze_weights(vgg16_feat_extr) #or:
# freeze_weights(vgg16_feat_extr, to = "block1_pool") 

# we'll not use the whole model but only up to layer 15
unet_tensor <- vgg16_feat_extr$layers[[15]]$output 


## add the second part of 'U' for segemntation ##

# "bottom curve" of U-net
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 1
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 512, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[14]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 2
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 256, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[10]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 3
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 128, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[6]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 4
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 64, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[3]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")

# final output 
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1, kernel_size = 1, activation = "sigmoid")

# create model from tensors
pretrained_unet <- keras_model(inputs = vgg16_feat_extr$input, outputs = unet_tensor)


spectral_augmentation <- function(img) {
  img <- tf$image$random_brightness(img, max_delta = 0.3) 
  img <- tf$image$random_contrast(img, lower = 0.8, upper = 1.2)
  img <- tf$image$random_saturation(img, lower = 0.8, upper = 1.2) 
  # make sure we still are between 0 and 1
  img <- tf$clip_by_value(img,0, 1) 
}





#adapted from: https://blogs.rstudio.com/ai/posts/2019-08-23-unet/ (accessed 2020-08-12)

dl_prepare_data <- function(files=NULL, train, predict=FALSE, subsets_path=NULL, model_input_shape = c(448,448), batch_size = 10L) {
  
  if (!predict){
    
    #function for random change of saturation,brightness and hue, 
    #will be used as part of the augmentation
    spectral_augmentation <- function(img) {
      img <- tf$image$random_brightness(img, max_delta = 0.3)
      img <- tf$image$random_contrast(img, lower = 0.8, upper = 1.1)
      img <- tf$image$random_saturation(img, lower = 0.8, upper = 1.1)
      # make sure we still are between 0 and 1
      img <- tf$clip_by_value(img, 0, 1)
    }
    
    
    #create a tf_dataset from the input data.frame 
    #right now still containing only paths to images 
    dataset <- tensor_slices_dataset(files)
    
    #use dataset_map to apply function on each record of the dataset 
    #(each record being a list with two items: img and mask), the 
    #function is list_modify, which modifies the list items
    #'img' and 'mask' by using the results of applying decode_jpg on the img and the mask   
    #-> i.e. jpgs are loaded and placed where the paths to the files were (for each record in dataset)
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x,img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
                    mask = tf$image$decode_jpeg(tf$io$read_file(.x$mask)))) 
    
    #convert to float32:
    #for each record in dataset, both its list items are modyfied 
    #by the result of applying convert_image_dtype to them
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
                    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32))) 
    
    #resize:
    #for each record in dataset, both its list items are modified 
    #by the results of applying resize to them 
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x, img = tf$image$resize(.x$img, size = shape(model_input_shape[1], model_input_shape[2])),
                    mask = tf$image$resize(.x$mask, size = shape(model_input_shape[1], model_input_shape[2]))))
    
    
    # data augmentation performed on training set only
    if (train) {
      
      #augmentation 1: flip left right, including random change of 
      #saturation, brightness and contrast
      
      #for each record in dataset, only the img item is modified by the result 
      #of applying spectral_augmentation to it
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      #...as opposed to this, flipping is applied to img and mask of each record
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_left_right(.x$img),
                      mask = tf$image$flip_left_right(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset,augmentation)
      
      #augmentation 2: flip up down, 
      #including random change of saturation, brightness and contrast
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_up_down(.x$img),
                      mask = tf$image$flip_up_down(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset_augmented,augmentation)
      
      #augmentation 3: flip left right AND up down, 
      #including random change of saturation, brightness and contrast
      
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_left_right(.x$img),
                      mask = tf$image$flip_left_right(.x$mask)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_up_down(.x$img),
                      mask = tf$image$flip_up_down(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset_augmented,augmentation)
      
    }
    
    # shuffling on training set only
    if (train) {
      dataset <- dataset_shuffle(dataset_augmented, buffer_size = batch_size*128)
    }
    
    # train in batches; batch size might need to be adapted depending on
    # available memory
    dataset <- dataset_batch(dataset, batch_size)
    
    # output needs to be unnamed
    dataset <-  dataset_map(dataset, unname) 
    
  }else{
    #make sure subsets are read in in correct order 
    #so that they can later be reassembled correctly
    #needs files to be named accordingly (only number)
    o <- order(as.numeric(tools::file_path_sans_ext(basename(list.files(subsets_path)))))
    subset_list <- list.files(subsets_path, full.names = T)[o]
    
    dataset <- tensor_slices_dataset(subset_list)
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$decode_jpeg(tf$io$read_file(.x))) 
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$convert_image_dtype(.x, dtype = tf$float32)) 
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$resize(.x, size = shape(model_input_shape[1], model_input_shape[2]))) 
    
    dataset <- dataset_batch(dataset, batch_size)
    dataset <-  dataset_map(dataset, unname)
    
  }
  
}



#get paths 
files <- data.frame(
  img = list.files("./training_unet/imgs/", full.names = TRUE, pattern = "*.jpg"),
  mask = list.files("./training_unet/masks/", full.names = TRUE, pattern = "*.jpg")
)

# split the data into training and validation datasets. 

files <- initial_split(files, prop = 0.8)

# prepare data for training
training_dataset <- dl_prepare_data(training(files),train = TRUE,model_input_shape = c(448,448),batch_size = 10L)
validation_dataset <- dl_prepare_data(testing(files),train = FALSE,model_input_shape = c(448,448),batch_size = 10L)


# get all tensors through the python iterator
training_tensors <- training_dataset%>%as_iterator()%>%iterate()


compile(
  pretrained_unet,
  optimizer = optimizer_rmsprop(lr = 1e-5),
  loss = "binary_crossentropy",
  metrics = c(metric_binary_accuracy)
)


diagnostics <- fit(pretrained_unet,
                   training_dataset,
                   epochs = 15,
                   validation_data = validation_dataset)

plot(diagnostics)

pretrained_unet <- load_model_hdf5("./pretrained_unet.h5")

sample <- floor(runif(n = 1,min = 1,max = 4))
img_path <- as.character(testing(files)[[sample,1]])
mask_path <- as.character(testing(files)[[sample,2]])
img <- magick::image_read(img_path)
mask <- magick::image_read(mask_path)
pred <- magick::image_read(as.raster(predict(object = pretrained_unet,validation_dataset)[sample,,,]))

out <- magick::image_append(c(
  magick::image_append(mask, stack = TRUE),
  magick::image_append(img, stack = TRUE), 
  magick::image_append(pred, stack = TRUE)
)
)

plot(out)



test_dataset <- dl_prepare_data(train = F,predict = T,subsets_path="./testarea_unet/subsets/",model_input_shape = c(448,448),batch_size = 5L)

system.time(predictions <- predict(pretrained_unet,test_dataset))


plot_layer_activations <- function(img_path, model, activations_layers,channels){
  
  
  model_input_size <- c(model$input_shape[[2]], model$input_shape[[3]]) 
  
  #preprocess image for the model
  img <- image_load(img_path, target_size =  model_input_size) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, model_input_size[1], model_input_size[2], 3)) %>%
    imagenet_preprocess_input()
  
  layer_outputs <- lapply(model$layers[activations_layers], function(layer) layer$output)
  activation_model <- keras_model(inputs = model$input, outputs = layer_outputs)
  activations <- predict(activation_model,img)
  if(!is.list(activations)){
    activations <- list(activations)
  }
  
  #function for plotting one channel of a layer, adopted from: Chollet (2018): "Deep learning with R"
  plot_channel <- function(channel,layer_name,channel_name) {
    rotate <- function(x) t(apply(x, 2, rev))
    image(rotate(channel), axes = FALSE, asp = 1,
          col = terrain.colors(12),main=paste("layer:",layer_name,"channel:",channel_name))
  }
  
  for (i in 1:length(activations)) {
    layer_activation <- activations[[i]]
    layer_name <- model$layers[[activations_layers[i]]]$name
    n_features <- dim(layer_activation)[[4]]
    for (c in channels){
      
      channel_image <- layer_activation[1,,,c]
      plot_channel(channel_image,layer_name,c)
      
    }
  } 
  
}


par(mfrow=c(1,1))
plot(read_stars("./testarea_unet/subsets/25.jpg"),rgb=c(1,2,3))


#visualize layers 3 and 10, channels 1 to 20
par(mfrow=c(3,4),mar=c(1,1,1,1),cex=0.5)
plot_layer_activations(img_path = "./testarea_unet/subsets/25.jpg", model=pretrained_unet ,activations_layers = c(2,3,5,6,8,9,10,12,13,14), channels = 1:4)


dl_subsets <- function(inputrst, targetsize, targetdir, targetname="", img_info_only = FALSE, is_mask = FALSE){
  require(jpeg)
  require(raster)
  
  #determine next number of quadrats in x and y direction, by simple rounding
  targetsizeX <- targetsize[1]
  targetsizeY <- targetsize[2]
  inputX <- ncol(inputrst)
  inputY <- nrow(inputrst)
  
  #determine dimensions of raster so that 
  #it can be split by whole number of subsets (by shrinking it)
  while(inputX%%targetsizeX!=0){
    inputX = inputX-1  
  }
  while(inputY%%targetsizeY!=0){
    inputY = inputY-1    
  }
  
  #determine difference
  diffX <- ncol(inputrst)-inputX
  diffY <- nrow(inputrst)-inputY
  
  #determine new dimensions of raster and crop, 
  #cutting evenly on all sides if possible
  newXmin <- floor(diffX/2)
  newXmax <- ncol(inputrst)-ceiling(diffX/2)-1
  newYmin <- floor(diffY/2)
  newYmax <- nrow(inputrst)-ceiling(diffY/2)-1
  rst_cropped <- suppressMessages(crop(inputrst, extent(inputrst,newYmin,newYmax,newXmin,newXmax)))
  #writeRaster(rst_cropped,filename = target_dir_crop)
  
  #return (list(ssizeX = ssizeX, ssizeY = ssizeY, nsx = nsx, nsy =nsy))
  agg <- suppressMessages(aggregate(rst_cropped[[1]],c(targetsizeX,targetsizeY)))
  agg[]    <- suppressMessages(1:ncell(agg))
  agg_poly <- suppressMessages(rasterToPolygons(agg))
  names(agg_poly) <- "polis"
  
  pb <- txtProgressBar(min = 0, max = ncell(agg), style = 3)
  for(i in 1:ncell(agg)) {
    
    # rasterOptions(tmpdir=tmpdir)
    setTxtProgressBar(pb, i)
    e1  <- extent(agg_poly[agg_poly$polis==i,])
    
    subs <- suppressMessages(crop(rst_cropped,e1))
    #rescale to 0-1, for jpeg export
    if(is_mask==FALSE){
      
      subs <- suppressMessages((subs-cellStats(subs,"min"))/(cellStats(subs,"max")-cellStats(subs,"min")))
    } 
    #write jpg
    
    
    writeJPEG(as.array(subs),target = paste0(targetdir,targetname,i,".jpg"),quality = 1)
    
    #writeRaster(subs,filename=paste0(targetdir,"SplitRas_",i,".tif"),overwrite=TRUE) 
    #return(c(extent(rst_cropped),crs(rst_cropped)))
  }
  close(pb)
  #img_info <- list("tiles_rows"=nrow(rst_cropped)/targetsizeY, "tiles_cols"=ncol(rst_cropped)/targetsizeX,"crs"= crs(rst_cropped),"extent"=extent(rst_cropped))
  #writeRaster(rst_cropped,filename = paste0(targetdir,"input_rst_cropped.tif"))
  rm(subs,agg,agg_poly)
  gc()
  return(rst_cropped)
  
}




rebuild_img <- function(pred_subsets,out_path,target_rst){
  require(raster)
  require(gdalUtils)
  require(stars)
  
  
  subset_pixels_x <- ncol(pred_subsets[1,,,])
  subset_pixels_y <- nrow(pred_subsets[1,,,])
  tiles_rows <- nrow(target_rst)/subset_pixels_y
  tiles_cols <- ncol(target_rst)/subset_pixels_x
  
  # load target image to determine dimensions
  target_stars <- st_as_stars(target_rst,proxy=F)
  #prepare subfolder for output
  result_folder <- paste0(out_path,"out")
  if(dir.exists(result_folder)){
    unlink(result_folder,recursive = T)
  }
  dir.create(path = result_folder)
  
  #for each tile, create a stars from corresponding predictions, 
  #assign dimensions using original/target image, and save as tif: 
  for (crow in 1:tiles_rows){
    for (ccol in 1:tiles_cols){
      i <- (crow-1)*tiles_cols + (ccol-1) +1 
      
      dimx <- c(((ccol-1)*subset_pixels_x+1),(ccol*subset_pixels_x))
      dimy <- c(((crow-1)*subset_pixels_y+1),(crow*subset_pixels_y))
      cstars <- st_as_stars(t(pred_subsets[i,,,1]))
      attr(cstars,"dimensions")[[2]]$delta=-1
      #set dimensions using original raster
      st_dimensions(cstars) <- st_dimensions(target_stars[,dimx[1]:dimx[2],dimy[1]:dimy[2]])[1:2]
      
      write_stars(cstars,dsn = paste0(result_folder,"/_out_",i,".tif")) 
    }
  }
  
  starstiles <- as.vector(list.files(result_folder,full.names = T),mode = "character")
  gdalbuildvrt(starstiles,paste0(result_folder,"/mosaic.vrt"))
  gdalwarp(paste0(result_folder,"/mosaic.vrt"), paste0(result_folder,"/mosaic.tif"))
}



#how many tensors?
length(training_tensors)
