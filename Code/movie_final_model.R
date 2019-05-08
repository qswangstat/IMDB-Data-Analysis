library(MASS)
library(dplyr)
library(readr)
library(wordcloud)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(GGally)
library(RColorBrewer)
library(ggcorrplot)
library(randomForest)
library(e1071)
library(rpart)
library(glmnet)
library(PerformanceAnalytics)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(car)
library(kernlab)
library(keras)
library(xgboost)
library(stringr)
library(caret)
library(car)
library(Matrix)
library(ROCR)
library(pROC)

setwd("Data")
movie = read_csv("movie.csv")
colnames(movie)
movie$genres = factor(movie$genres)
movie$color = factor(movie$color)
movie$content_rating = factor(movie$content_rating)
movie$language = factor(movie$language)
idx = c("movie_title", "overview", "plot_keywords", "popularity", "gross",
        "production_companies", "title_year", "content_rating", "country",
        "language", "movie_facebook_likes", "total_fb_likes")

movie_simp = movie %>% 
  mutate(title_length = unlist(lapply(str_split(movie_title," "), length))) %>%
  mutate(kw_length = unlist(lapply(str_split(plot_keywords,"|"), length))) %>%
  select(-ends_with("_name")) %>%
  select(-starts_with("num_")) %>%
  filter(language == "English") %>%
  select(-idx)
### correlation matrix visualization 
dim(movie_simp)
colnames(movie_simp)
corrplot(cor(movie_simp %>% select(-color, -genres)), tl.cex = 0.5, method = "ellipse")
### dimension reduction 
pca_res = PCA(movie_simp %>% select(-imdb_score, -if_profit, -return_rate, -color, -genres),
              graph = FALSE)
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 50))
var = get_pca_var(pca_res)
corrplot(var$cos2, is.corr=FALSE, method = "ellipse")
fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

### split the dataset
set.seed(123)
train_num = ceiling(0.8 * nrow(movie_simp))
test_num = nrow(movie_simp) - train_num
train_id = sample(1:nrow(movie_simp), train_num)
train = movie_simp[train_id, ]
test = movie_simp[-train_id, ]

##########################
### predict IMDB score ###
##########################

### Linear Regression
#### display regression plots
view_model = function(model) {
  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  plot(model)
  par(mfrow=c(1,1)) # Change back to 1 x 1
}
train_data = train %>% select(-return_rate, -if_profit)
lfit = lm(imdb_score ~ ., train_data)
summary(lfit)
view_model(lfit)
pred_score = predict(lfit, test)
print(paste("Linear Regression: RMSE =", sqrt(1 / test_num * sum((pred_score - test$imdb_score)^2))))
### box-cox transformation
lambda_seq = seq(-5,5,0.05)
bc = boxcox(lfit, lambda_seq)
lam = lambda_seq[which.max(bc$y)]
lfit_bc = lm((imdb_score^lam - 1)/lam ~ ., train_data)
summary(lfit_bc)
view_model(lfit_bc)
pred_score = predict(lfit_bc, test)
print(paste("Linear Regression with Box-Cox: RMSE =", sqrt(1 / test_num * sum(((pred_score*lam+1)^(1/lam) - test$imdb_score)^2))))
### LASSO
grid = 10^seq(-5, 5, 0.05)
xfactor = model.matrix(imdb_score ~ color + genres, train_data)[, -1]
design = as.matrix(data.frame(train_data %>% select(-imdb_score, -color, -genres), 
                              xfactor))
lsfit = glmnet(design, train_data$imdb_score, alpha = 1, lambda = grid)
plot(lsfit)
lscv = cv.glmnet(design, train_data$imdb_score, alpha = 1, lambda = grid, nfolds = 10)
plot(lscv)
lam_opt = lscv$lambda.min

xfactor2 = model.matrix(imdb_score ~ color + genres, test)[, -1]
design2 = as.matrix(data.frame(test %>% select(-imdb_score, -color, -genres, -return_rate, -if_profit), 
                              xfactor2))
coef = coef(lsfit, s = lscv$lambda.min)
act_idx = which(coef != 0)
row.names(coef)[act_idx]
pred_score = predict(lsfit, s = lam_opt, newx = design)
print(paste("LASSO: RMSE =", sqrt(1 / test_num * sum((pred_score - test$imdb_score)^2))))

### SVM
svmfit = ksvm(imdb_score ~ ., train_data, kernel = "rbfdot",                    
                  kpar = list(sigma = 0.05), C = 5,                    
                  prob.model = TRUE, cross = 10) 
pred_score = predict(svmfit, test)
print(paste("SVM: RMSE =", sqrt(1 / test_num * sum((pred_score - test$imdb_score)^2))))
### Random Forest
rffit = randomForest(imdb_score ~ ., train_data, ntree=80,
                     importance=TRUE, proximity=TRUE, na.action = na.omit, 
                     norm.votes=TRUE)
pred_score = predict(rffit, test)
print(paste("Random Forest: RMSE =", sqrt(1 / test_num * sum((pred_score - test$imdb_score)^2))))
varImpPlot(rffit, cex = 0.6)
### Deep Learning
train_dl = scale(train_data %>% select(-color, -genres, -imdb_score))
test_dl = test %>% select(-imdb_score, -color, -genres, -return_rate, -if_profit)
# Use means and standard deviations from training set to normalize test set
col_means_train = attr(train_dl, "scaled:center") 
col_stddevs_train = attr(train_dl, "scaled:scale")
test_dl = scale(test_dl, center = col_means_train, scale = col_stddevs_train)
build_model = function() {
  
  model = keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu",
                input_shape = dim(train_dl)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mean_absolute_error")
  )
  model
}

model = build_model()
model %>% summary()
print_dot_callback = callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 100 == 0) cat("\n")
    cat(".")
  }
)    
epochs = 500
early_stop = callback_early_stopping(monitor = "val_loss", patience = 50)

history = model %>% fit(
  train_dl,
  train$imdb_score,
  batch_size = 100,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE)
pred_score = model %>% predict(test_dl)
print(paste("Deep Learning: RMSE =", sqrt(1 / test_num * sum((pred_score[, 1] - test$imdb_score)^2))))

##############################
### classify if profitable ###
##############################
### Logistic Regression
train_data = train %>% select(-imdb_score, -return_rate)
logfit = glm(as.factor(if_profit) ~ ., train_data,
             family=binomial(link="logit"))
pred_if = predict(logfit, test, type = 'response') >= 0.5
print(paste("Logistic Regression: Rate =", 1 / test_num * sum(pred_if == test$if_profit)))
roc3 = roc(test$if_profit, as.numeric(pred_if))
plot(roc3, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
### Random Forest
rffit = randomForest(as.factor(if_profit) ~ ., train_data, ntree = 100, 
                     importance=TRUE, proximity=TRUE, na.action = na.omit, 
                     norm.votes=TRUE)
pred_if = predict(rffit, test)
print(paste("Random Forest: RMSE =", 1 / test_num * sum(pred_if == test$if_profit)))
varImpPlot(rffit, cex = 0.6)
roc4 = roc(test$if_profit, as.numeric(pred_if))
plot(roc4, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
### Xgboost
traindata1 = data.matrix(train_data %>% select(-if_profit)) 
traindata2 = Matrix(traindata1, sparse = T)
traindata3 = data.matrix(train_data$if_profit) 
traindata4 = list(data = traindata2, label = traindata3) 
dtrain = xgb.DMatrix(data = traindata4$data, label = traindata4$label)

test_data = test %>% select(-imdb_score, -return_rate)
testdata1 = data.matrix(test_data %>% select(-if_profit)) 
testdata2 = Matrix(testdata1, sparse = T) 
testdata3 = data.matrix(test_data$if_profit)
testdata4 = list(data = testdata2, label = testdata3) 
dtest = xgb.DMatrix(data = testdata4$data, label = testdata4$label) 

param = list(max_depth = 6, eta = 0.01, objective='binary:logistic')
xgbfit = xgb.train(params = param, data = dtrain, nrounds = 200)
pred_if = predict(xgbfit, dtest) >= 0.5
print(paste("Xgboost: RMSE =", 1 / test_num * sum(pred_if == test$if_profit)))

roc5 = roc(test_data$if_profit, as.numeric(pred_if))
plot(roc5, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

names = dimnames(data.matrix(train_data %>% select(-if_profit)))[[2]]
importance_matrix = xgb.importance(names, model = model)
xgb.plot.importance(importance_matrix[1:10,])

### Deep Learning
train_dl = scale(train_data %>% select(-color, -genres, -if_profit))
test_dl = test %>% select(-imdb_score, -color, -genres, -return_rate, -if_profit)
# Use means and standard deviations from training set to normalize test set
col_means_train = attr(train_dl, "scaled:center") 
col_stddevs_train = attr(train_dl, "scaled:scale")
test_dl = scale(test_dl, center = col_means_train, scale = col_stddevs_train)
build_model = function() {
  
  model = keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu",
                input_shape = dim(train_dl)[2]) %>%
    layer_dense(units = 32, activation = "sigmoid") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("accuracy")
  )
  model
}

model = build_model()
model %>% summary()
print_dot_callback = callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 100 == 0) cat("\n")
    cat(".")
  }
)    
epochs = 500
early_stop = callback_early_stopping(monitor = "val_loss", patience = 50)

history = model %>% fit(
  train_dl,
  train$if_profit,
  batch_size = 100,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, smooth = FALSE)
pred_if = model %>% predict(test_dl) >= 0.5
print(paste("Deep Learning: RMSE =", 1 / test_num * sum(pred_if == test$if_profit)))
roc1 = roc(test$if_profit, as.numeric(pred_if))
plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
