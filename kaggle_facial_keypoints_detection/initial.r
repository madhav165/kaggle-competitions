data_dir   <- './data/'
train_file <- paste0(data_dir, 'training.csv')
test_file  <- paste0(data_dir, 'test.csv')

d_train <- read.csv(train_file, stringsAsFactors=F)

im_train  <- d_train$Image
d_train$Image <- NULL

library(doParallel)
registerDoParallel()
im_train <- foreach(im = im_train, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(im, " ")))
}

d_test  <- read.csv(test_file, stringsAsFactors=F)
im_test <- foreach(im = d_test$Image, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(im, " ")))
}

d_test$Image <- NULL

save(d_train, im_train, d_test, im_test, file="data.Rd")

im <- matrix(data=rev(im_train[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-d_train$nose_tip_x[1],         96-d_train$nose_tip_y[1],         col="red")
points(96-d_train$left_eye_center_x[1],  96-d_train$left_eye_center_y[1],  col="blue")
points(96-d_train$right_eye_center_x[1], 96-d_train$right_eye_center_y[1], col="green")

for(i in 1:nrow(d_train)) {
    points(96-d_train$nose_tip_x[i], 96-d_train$nose_tip_y[i], col="red")
}

idx <- which.max(d_train$nose_tip_x)
im  <- matrix(data=rev(im_train[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-d_train$nose_tip_x[idx], 96-d_train$nose_tip_y[idx], col="red")

colMeans(d_train, na.rm=T)

p           <- matrix(data=colMeans(d_train, na.rm=T), nrow=nrow(d_test), ncol=ncol(d_train), byrow=T)
colnames(p) <- names(d_train)
predictions <- data.frame(ImageId = 1:nrow(d_test), p)
head(predictions)

library(reshape2)
submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(submission)

example.submission <- read.csv(paste0(data_dir, 'submissionFileFormat.csv'))
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]
write.csv(submission, file="submission_means.csv", quote=F, row.names=F)
