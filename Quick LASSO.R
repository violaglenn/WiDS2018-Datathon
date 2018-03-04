require(MASS)
require(tidyverse)
require(glmnet)
require(caret)
require(ROCR)
require(pROC)
require(dummies)

input_data_orig <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/train.csv')
holdout <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/test.csv')
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

input_data$tag <- 'input data'
holdout$tag <- 'holdout'
holdout$train_id <- NA
holdout$is_female <- NA
input_data$test_id <- NA

to_process <- rbind(input_data, holdout)

ohe_feats <- filter(dictionary, Ordinal == 'No')
ohe_feats <- ohe_feats$`Column Name`
ohe_feats <- intersect(ohe_feats, names(to_process))

ohe_data <- select(to_process, ohe_feats)## START HERE- Need a quick way to encode all of these

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

to_process <- select(to_process, -one_of(names(ohe_data)))
to_process <- cbind(to_process, dummies)



#Since this is linear, need to take a look at missing values ---------------------------------------------------

count_em <- function(x) {
  sum(is.na(x))
}

missing_count <- data.frame(count = sapply(to_process, count_em), var = names(to_process))
missing_count$percentage <- missing_count$count/nrow(to_process)
plot(density(missing_count$percentage, na.rm = T))
nrow(missing_count[missing_count$percentage <= 0.5, ]) #Start with 0.5, this gives is 1,654 variables (compared to 1544 before, not as big of a jump as I'd think)

table(missing_count$count) #1544 of 2411 have no missing data. Start there
#missing_keeps <- as.character(missing_count$var[missing_count$count == 0])
missing_keeps <- as.character(missing_count$var[missing_count$percentage <= 0.5])

to_process <- select(to_process, one_of('is_female', 'train_id', 'test_id', missing_keeps))

Mode <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux)) 
    ux[tab == max(tab)]
  }

mode_replace <- function(x) {
  ifelse(is.na(x), Mode(x), x)
} #mode selected for now because most are actually categorical dressed up as numeric

prop.table(table(is.na(to_process$FF19_5))) #19,575 NAs
Mode(to_process$FF19_5) #1.97
table(to_process$FF19_5)

Mode(to_process$IFI17_2) #Ah, for a few mode is NA

look <- data.frame(sapply(to_process, Mode))
look$var <- rownames(look)
names(look) <- c('Mode', 'var')
look <- filter(look, is.na(Mode))

drops <- look[4:15, 'var']

to_process[, drops] <- NULL #Drop those 12 for now

to_process <- data.frame(sapply(to_process, mode_replace))
tag <- to_process$tag
test_id <- to_process$test_id
train_id <- to_process$train_id
flag <- to_process$is_female
to_process <- data.frame(sapply(to_process, as.numeric))
to_process$tag <- tag
to_process$test_id <- test_id
to_process$train_id <- train_id
to_process$is_female <- flag

missing_test <- data.frame(sapply(to_process, count_em)) #looks goods


#Split back out

input_data <- filter(to_process, tag == 'input data')
holdout <- filter(to_process, tag == 'holdout')
input_data$tag <- NULL
holdout$tag <- NULL
holdout$is_female <- NULL
input_data$test_id <- NULL



#Unsupervised Variable Reduction ---------------------------------------------------

target <- input_data$is_female
#drp_rsn <- data.frame(col = character(), rsn = character())

# Handling binaries that are characters
charVars <- names(input_data)[sapply(input_data,class) == 'character'] #none

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
# is.na(input_data) <- input_data==''
# 
# 
# # Drop 0 variance features
# allSame <- sapply(input_data,function(x)length(unique(x)) == 1)
# allSame <- names(allSame[allSame])
# drp_rsn <- data.frame(col = allSame,rsn = 'all_missing')
# input_data[,allSame] <- NULL
# 
# # Remove features that has less than 5% of non-missing or zero values
# 
# #95% missing, 95% same value
# cutoff <- 0.01
# 
# #Missing
# percNonNaZero <- sapply(input_data, function(x)(sum(!is.na(x)) - sum(x %in% c(""," "), na.rm = T))/nrow(input_data))
# review <- data.frame(percNonNaZero) #none
# 
# percNonNaZero <- review %>%
#   mutate(var = row.names(review)) %>%
#   filter(percNonNaZero < cutoff) %>%
#   select(var)
# 
# drp_rsn <- rbind(drp_rsn, data.frame(col = percNonNaZero$var,rsn = '95% or more missing')) #This drops out half of the variables. Might be a good place to do some feature engineering later
# input_data[,percNonNaZero$var] <- NULL
# 
# #Same value
# percNonNaZero <- sapply(input_data, function(x)max(table(x)/length(x)))
# review <- data.frame(percNonNaZero)
# 
# percNonNaZero <- review %>%
#   mutate(var = row.names(review)) %>%
#   filter(percNonNaZero > 1 - cutoff) %>%
#   select(var)
# 
# drp_rsn <- rbind(drp_rsn, data.frame(col = percNonNaZero$var,rsn = '95% or same value')) #This drops out half of the variables. Might be a good place to do some feature engineering later
# input_data[,percNonNaZero$var] <- NULL
# 
# #Now down to 406 variables
# 
# #Split data
# 
input_data$target <- target
# 
# 
# #Quick xgb
# 
# #prop.table(table(labels_train))
# #prop.table(table(labels_test)) #Nice split
# 
# review <- data.frame(sapply(train, typeof)) #3 characters
# 
# length(unique(input_data$MM12_REC))
# length(unique(input_data$LN2_RIndLngBEOth))
# length(unique(input_data$LN2_WIndLngBEOth))
# 
# #Drop them for now-- revisit for feature work later 

#input_data[, c('LN2_RIndLngBEOth', 'LN2_WIndLngBEOth')] <- NULL
 
# input_data[, c('MM12_REC', 'LN2_RIndLngBEOth', 'LN2_WIndLngBEOth', 'DL1_OTHERS', 'DL2_96_OTHERS', 'DL4_OTHERS', 'FL9A_OTHERS', 'FB19_6_OTHERS')] <- NULL

train_split <- 0.7

set.seed(8675309)
train <- input_data[sample(1:nrow(input_data), nrow(input_data) * train_split, replace=FALSE),]
test <- filter(input_data, !train_id %in% train$train_id)

labels_train <- train$target
labels_test <- test$target

train$target <- NULL
train$is_female <- NULL
train_ids <- train$train_id
train$train_id <- NULL

test <- select(test, one_of(names(train)))

#Start Lasso tests
train <- as.matrix(train)
labels_train <- as.factor(labels_train)

fit.lasso <- glmnet(train, labels_train, family="binomial", alpha=1)
fit.ridge <- glmnet(train, labels_train, family="binomial", alpha=0)
fit.enet <- glmnet(train, labels_train, family="binomial", alpha=0.5)

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(train, labels_train, type.measure="auc", 
                                            alpha=i/10,family="binomial"))
  
  print(paste('Finished ', i))
}

par(mfrow=c(3,2))

plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.enet, xvar="lambda")
plot(fit5, main="Elastic Net")

#Get lift and auc
test <- as.matrix(test)
scores <- predict(fit7, s=fit7$lambda.1se, newx=test, type = 'response')

results <- data.frame(score = scores, target = labels_test)
ROC <- roc(predictor=scores,
           response=labels_test,
           levels=rev(levels(as.factor(labels_test))))

ROC$auc 

#Score holdout

holdout_toscore <- select(holdout, names(data.frame(train)))
holdout_toscore <- as.matrix(holdout_toscore)

holdout_scores <- predict(fit6, s=fit6$lambda.1se, newx=holdout_toscore, type = 'response')
plot(density(holdout_scores))
plot(density(scores)) #distributions are similar

submission <- data.frame(test_id = holdout$test_id, is_female = holdout_scores)
names(submission) <- c('test_id', 'is_female')
write.csv(submission, 'C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission10.csv', row.names = FALSE)

#First ensemble 

xgb_scores <- read_csv('C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission12.csv')

submission$test_id <- as.numeric(as.character(submission$test_id))

combined_scores <- left_join(xgb_scores, submission, by = 'test_id')
plot(combined_scores$is_female.x, combined_scores$is_female.y)

combined_scores$average <- (combined_scores$is_female.x + combined_scores$is_female.y)/2
par(mfrow=c(3,1))
plot(combined_scores$is_female.x, combined_scores$is_female.y)
plot(combined_scores$is_female.x, combined_scores$average)
plot(combined_scores$is_female.y, combined_scores$average)

submission <- data.frame(test_id = combined_scores$test_id, is_female = combined_scores$average)
write.csv(submission, 'C:/Users/vglenn/Documents/Misc/Continued Ed/WiDS Datathon 2018/submission11.csv', row.names = FALSE)

#Get coeffs

look <- coefficients(fit6)
results <- data.frame(row = as.character(look@i), coeff = look@x)
names <- data.frame(look@Dimnames[1])
names$row <- row(names) - 1
names$row <- as.character(names$row)
results <- left_join(results, names, by = 'row')
