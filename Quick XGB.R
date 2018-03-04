require(tidyverse)
require(skimr)
require(xgboost)
require(caret)
require(ROCR)
require(pROC)
require(dummies)

input_data_orig <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/train.csv')
dictionary <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/WiDS data dictionary v2.csv')
input_data <- input_data_orig
#str(input_data)
#skim(input_data)

prop.table(table(input_data$is_female)) #54% is female

table(input_data$is_female, input_data$DG6) #65% of men are HOH, 64% of women's spouses are HOH
table(is.na(input_data$DG6))
table(input_data$DG6)
table(input_data$is_female, input_data$DL0)

# #Try some feature engineering
# 
# #Step 1: Just replace the 96/other with something closer to distribution
# 
# # max_table <- input_data_orig %>%
# #   summarise_all(funs(max))
# # 
# # max_table <- t(data.frame(max_table))
# # table(max_table)
# 
# #Simple logic for now. If max is 99 or 96, replace 96 with 50 since this is outside the distribution but farther from 99
# 
# replace_96 <- function(x) {
#   ifelse((max(x) == 96 | max(x) == 99) & x == 96, 50, x)
# }
# 
# # table(input_data$DG3A)
# # input_data$test <- replace_96(input_data$DG3A)
# # table(input_data$test) #Works!
# # input_data$test <- NULL
# # 
# # temp <- data.frame(sapply(input_data, replace_96))
# # table(temp$DG3A) #Works!
# 
# input_data <- data.frame(sapply(input_data, replace_96))
# look <- data.frame(sapply(input_data, typeof))
# 
# asNumeric <- function(x) as.numeric(as.character(x))
# factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
#                                                    asNumeric))
# 
# input_data <- factorsNumeric(input_data) #Hmm.. what is this error?

#Alternative Step 1: Change non-ordinal chars to OHE

#Drop variables w/ no importance after initial xgb

#Run w/ only top ~150 variables

imp_matrix <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission7_imp.csv')

imp_matrix$var <- imp_matrix$X1
  imp_matrix$cum <- cumsum(imp_matrix$Overall)
to_keep <- imp_matrix$var[imp_matrix$cum <= 0.9]

input_data <- input_data[, c('train_id', 'is_female', to_keep)]

ohe_feats <- filter(dictionary, Ordinal == 'No')
ohe_feats <- ohe_feats$`Column Name`
ohe_feats <- intersect(ohe_feats, names(input_data))

ohe_data <- select(input_data, ohe_feats)## START HERE- Need a quick way to encode all of these

ohe_data <- data.frame(sapply(ohe_data, as.factor))

find_levels <- function(x) {
  length(levels(x))
}

not_enough_levels <- data.frame(levels = sapply(ohe_data, find_levels), var = names(ohe_data))
not_enough_levels <- filter(not_enough_levels, levels < 2)
not_enough_levels <- as.character(not_enough_levels$var)

ohe_data <- select(ohe_data, -one_of(not_enough_levels))

dummies <- dummy.data.frame(ohe_data, sep = '_')

#Join back to original data

input_data <- select(input_data, -one_of(names(ohe_data)))
input_data <- cbind(input_data, dummies)

#Unsupervised feature reduction - Simple for now, high missings and low variance 

#Unsupervised Variable Reduction ---------------------------------------------------

target <- input_data$is_female
drp_rsn <- data.frame(col = character(), rsn = character())

# Handling binaries that are characters
charVars <- names(input_data)[sapply(input_data,class) == 'character']

#Drop those w/ too many levels
#charVars[sapply(input_data[,charVars], function(x) length(unique(x))) <= 5]
#drp_rsn <- rbind(drp_rsn, data.frame(col = charVars[sapply(df[,charVars], function(x) length(unique(x))) <= 5],rsn = 'OHE_created'))
#drp_rsn <- rbind(drp_rsn, data.frame(col = charVars[sapply(df[,charVars], function(x) length(unique(x))) > 5],rsn = 'Too_many_levels'))

# # Creating dummy for char vars (all are ibe variables with levels <= 5)
# for (col in charVars[sapply(df[,charVars], function(x) length(unique(x))) <= 5]){
#   df[is.na(df[,col]),col] <- "Missing"
#   df <- cbind(df,model.matrix(as.formula(paste("~ ",col," -1")),data = df))
#   df[,col] <- NULL
# }
# 
# charVars <- setdiff(names(df)[sapply(df,class) == 'character'],keep_cols)
# df[,charVars[sapply(df[,charVars], function(x) length(unique(x))) > 5]] <- NULL # Deleting too many level columns

# Replacing empty string with NA
is.na(input_data) <- input_data==''


# Drop 0 variance features
allSame <- sapply(input_data,function(x)length(unique(x)) == 1)
allSame <- names(allSame[allSame])
drp_rsn <- data.frame(col = allSame,rsn = 'all_missing')
input_data[,allSame] <- NULL

# Remove features that has less than 5% of non-missing or zero values

#95% missing, 95% same value
cutoff <- 0.01

#Missing
percNonNaZero <- sapply(input_data, function(x)(sum(!is.na(x)) - sum(x %in% c(""," "), na.rm = T))/nrow(input_data))
review <- data.frame(percNonNaZero) #none

percNonNaZero <- review %>%
  mutate(var = row.names(review)) %>%
  filter(percNonNaZero < cutoff) %>%
  select(var)

drp_rsn <- rbind(drp_rsn, data.frame(col = percNonNaZero$var,rsn = '95% or more missing')) #This drops out half of the variables. Might be a good place to do some feature engineering later
input_data[,percNonNaZero$var] <- NULL

#Same value
percNonNaZero <- sapply(input_data, function(x)max(table(x)/length(x)))
review <- data.frame(percNonNaZero)

percNonNaZero <- review %>%
  mutate(var = row.names(review)) %>%
  filter(percNonNaZero > 1 - cutoff) %>%
  select(var)

drp_rsn <- rbind(drp_rsn, data.frame(col = percNonNaZero$var,rsn = '95% or same value')) #This drops out half of the variables. Might be a good place to do some feature engineering later
input_data[,percNonNaZero$var] <- NULL

#Now down to 406 variables

#Remove those with high corr

elvt_correlated_feats <- function (df, nSubsequent = 10, corCutoff = 0.98, chainDrop = F, 
          returnNamesOnly = T) 
{
  df <- df[, names(df)[sapply(df, class) %in% c("numeric", 
                                                "integer")]]
  df_cor <- data.frame(V1 = "", V2 = "", Correlation = 0, missingMatchRate = 0, 
                       stringsAsFactors = F)
  i <- 0
  nm <- names(df)
  for (col in 1:(ncol(df) - 1)) {
    lst <- min((col + nSubsequent), ncol(df))
    for (col2 in (col + 1):lst) {
      i <- i + 1
      df_cor[i, 1] <- nm[col]
      df_cor[i, 2] <- nm[col2]
      df_cor[i, 3] <- cor(df[, col], df[, col2], use = "pairwise.complete.obs")
      missPattern <- paste(is.na(df[, col]) * 1, is.na(df[, 
                                                          col2]) * 1, sep = "")
      df_cor[i, 4] <- sum(missPattern == "11")/sum(missPattern != 
                                                     "00")
    }
  }
  df_cor <- df_cor[df_cor$Correlation > corCutoff & !is.na(df_cor$Correlation) & 
                     df_cor$missingMatchRate > 0.8 & !is.na(df_cor$missingMatchRate), 
                   ]
  df_cor$ord <- rank(-df_cor$Correlation, ties.method = "min")
  df_cor <- df_cor[order(df_cor$Correlation, decreasing = T), 
                   ]
  df_cor$rem <- 0
  df_cor[1, "rem"] <- 2
  for (i in 2:nrow(df_cor)) {
    df_cor$rem[i] <- ifelse(df_cor$V2[i] %in% c(df_cor$V2[df_cor$rem == 
                                                            2], df_cor$V1[df_cor$rem == 1]), 2, ifelse(df_cor$V1[i] %in% 
                                                                                                         c(df_cor$V2[df_cor$rem == 2], df_cor$V1[df_cor$rem == 
                                                                                                                                                   1]), 1, 0))
    if (df_cor$rem[i] != 0) {
      next
    }
    if (df_cor$rem[i] == 0) {
      V1_has_pair_dropped <- df_cor$V1[i] %in% c(df_cor$V2[df_cor$rem == 
                                                             1], df_cor$V1[df_cor$rem == 2])
      V2_has_pair_dropped <- df_cor$V2[i] %in% c(df_cor$V2[df_cor$rem == 
                                                             1], df_cor$V1[df_cor$rem == 2])
      if (!(V1_has_pair_dropped) & (V2_has_pair_dropped)) {
        df_cor$rem[i] <- 1
        next
      }
      if ((V1_has_pair_dropped) & !(V2_has_pair_dropped)) {
        df_cor$rem[i] <- 2
        next
      }
    }
    if ((!(V1_has_pair_dropped) & !(V2_has_pair_dropped)) | 
        chainDrop) {
      v1_nonmiss <- sum(!is.na(df[, df_cor$V1[i]]))
      v2_nonmiss <- sum(!is.na(df[, df_cor$V2[i]]))
      df_cor$rem[i] <- ifelse(v1_nonmiss >= v2_nonmiss, 
                              2, ifelse(v1_nonmiss < v2_nonmiss, 1, 0))
      next
    }
  }
  ifelse(returnNamesOnly, return(unique(c(df_cor$V2[df_cor$rem == 
                                                      2], df_cor$V1[df_cor$rem == 1]))), return(df_cor))
}

test_corr <- elvt_correlated_feats(input_data, corCutoff = 0.75)

input_data[, test_corr] <- NULL

#Split data

input_data$target <- target


#Quick xgb

#prop.table(table(labels_train))
#prop.table(table(labels_test)) #Nice split

review <- data.frame(sapply(train, typeof)) #3 characters

length(unique(input_data$MM12_REC))
length(unique(input_data$LN2_RIndLngBEOth))
length(unique(input_data$LN2_WIndLngBEOth))

#Drop them for now-- revisit for feature work later 

input_data[, c('MM12_REC', 'LN2_RIndLngBEOth', 'LN2_WIndLngBEOth', 'DL1_OTHERS', 'DL2_96_OTHERS', 'DL4_OTHERS', 'FL9A_OTHERS', 'FB19_6_OTHERS')] <- NULL

train_split <- 0.7

set.seed(8675309)
train <- input_data[sample(1:nrow(input_data), nrow(input_data) * train_split, replace=FALSE),]
test <- filter(input_data, !train_id %in% train$train_id)

labels_train <- train$is_female
labels_test <- test$is_female

train$target <- NULL
train$is_female <- NULL
train_ids <- train$train_id
train$train_id <- NULL

test <- select(test, one_of(names(train)))

DMMatrix_train <- xgb.DMatrix(data = as.matrix(sapply(train, as.numeric)), label = labels_train, missing = NA) 
DMMatrix_test <- xgb.DMatrix(data = as.matrix(sapply(test, as.numeric)), label = labels_test, missing = NA) 

xgboostModelCV<-xgb.cv(data=DMMatrix_train, nrounds=2500, nfold=5, showsd=FALSE, 
                                     verbose=TRUE, eval_metric='auc',
                       objective="binary:logistic", max.depth=5, eta=0.01,
                       early_stopping_rounds=20, maximize=TRUE, prediction = TRUE, nthread = 5)

#Control 
xgb_trcontrol <- trainControl(
  method="cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final", 
  classProbs = T, 
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


#scale_pos_weight <- nrow(retro_train)/sum(y==1)
#for submission 3 
# xgb_grid_1 <- expand.grid(
#   nrounds = 200, #was 200 for 5% missing rate
#   max_depth = seq(1,6,2),
#   eta = seq(0, 1, .33),
#   gamma = seq(0, 1, 0.33),
#   colsample_bytree = c(1, 0.5, 0.8),
#   #nthread = 10,
#   #silent = 0,
#   min_child_weight = c(2,5,8),
#   #scale_pos_weight = c(scale_pos_weight*0.8, scale_pos_weight,scale_pos_weight*1.2),
#   #alpha = seq(0,1, .2),
#   subsample = c(0.5, 0.8)
# ) #864 combos

xgb_grid_1 <- expand.grid(
  nrounds = 2500, #was 200 for 5% missing rate
  max_depth = 6,
  eta = seq(0.01, 0.05, 0.01),
  gamma = seq(0.1, 0.9, 0.2),
  colsample_bytree = 0.8,
  #nthread = 10,
  #silent = 0,
  min_child_weight = c(2, 4),
  #scale_pos_weight = c(scale_pos_weight*0.8, scale_pos_weight,scale_pos_weight*1.2),
  #alpha = seq(0,1, .2),
  subsample = 0.8
) #200 combos


y <- ifelse(labels_train==1, 'Female', 'Male')

start <- Sys.time()
#Train the model
boost <- train(
  x = as.matrix(train),
  y = as.factor(y),
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid_1,
  method="xgbTree", missing = NA, nthread = 5
)

print(Sys.time() - start)

#List chosen parameters
boost$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 524     200         6 0.1     1              0.5                1       0.8

#Option 2: Run a specific set of criteria rather than tuning

params <- list(objective        = "binary:logistic",
               eval_metric      = "auc"            ,
               max_depth        = 6                ,
               eta              = 0.02     ,
               gamma = 0.3,
               subsample        = 0.8           ,
               colsample_bytree = 0.8          ,
               min_child_weight = 2            )

  
  

set.seed(8675309)
boost <- xgb.train(       data              = DMMatrix_train           ,
                              nround            = 1000                      , 
                              nthread           = 5                       ,
                              early.stop.rounds = 20                      ,
                          params = params,
                          silent = 0)

#Assess Option 2

names<-dimnames(data.matrix(train[, 1:ncol(train)]))[[2]]
# Get the important matrix
imp_matrix_final_constrained <-xgb.importance(names, model=boost)
write.csv(imp_matrix_final_constrained, 'C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/feature_eng_2_imp.csv')

pred_train <- as.data.frame(predict(boost, DMMatrix_train))
true_train <- as.data.frame(labels_train)
df_pred_true_train <- cbind(pred_train,true_train )
colnames(df_pred_true_train)<-c('score','target')
df_pred_true_train = df_pred_true_train%>%
  mutate(target = ifelse(target==1, 'Female', 'Male'))

pred_test <- as.data.frame(predict(boost, DMMatrix_test))
true_test <- as.data.frame(labels_test)
df_pred_true_test <- cbind(pred_test,true_test )
colnames(df_pred_true_test)<-c('score','target')
df_pred_true_test = df_pred_true_test%>%
  mutate(target = ifelse(target==1, 'Female', 'Male'))

ROC_train <- roc(predictor=pred_train$`predict(boost, DMMatrix_train)`,
                 response=labels_train)

ROC_train$auc

ROC_test <- roc(predictor=pred_test$`predict(boost, DMMatrix_test)`,
                response=labels_test)

ROC_test$auc

#Assess Option 1

#Get importance
imp_matrix <- varImp(boost, scale = FALSE)
imp_matrix <- imp_matrix$importance

#Get lift and auc
target_test <- test$target
#target_test <- results$target
target_test <- ifelse(target_test==1, 'Female', 'Male')

test <- select(test, -train_id, -is_female, -target)
scores <- predict(boost, test , type = "prob")[, 'Female']

results <- data.frame(score = scores, target = labels_test)
ROC <- roc(predictor=scores,
           response=labels_test,
           levels=rev(levels(as.factor(labels_test))))

ROC$auc #0.9655 not bad (with 5% cutoff)
ROC$auc #0.9907 for 1% cutoff (submit 2)
ROC$auc #0.9719 for sub 4 on the 1,080 grid, 524 rounds above

#Score holdout

holdout <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/test.csv')

#Same feature engineering performed for test/train

ohe_data <- select(holdout, ohe_feats)## START HERE- Need a quick way to encode all of these

ohe_data <- data.frame(sapply(ohe_data, as.factor))

dummies <- dummy.data.frame(ohe_data, sep = '_')

#Join back to original data

holdout <- select(holdout, -one_of(names(ohe_data)))
holdout <- cbind(holdout, dummies)

holdout_toscore <- select(holdout, names(train))
holdout_scores <- predict(boost, holdout_toscore , type = "prob")[, 'Female']
plot(density(holdout_scores))
plot(density(scores)) #distributions are similar

submission <- data.frame(test_id = holdout$test_id, is_female = holdout_scores)
write.csv(submission, 'C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission13.csv', row.names = FALSE)

write.csv(imp_matrix, 'C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission7_imp.csv')


