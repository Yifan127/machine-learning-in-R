# Assignment1 Question1
library(caret)
# ----------------read the data into data frame----------------
url = "https://www.cs.auckland.ac.nz/courses/compsci760s2c/assignments/2017/Pat/"
obscuredx = read.csv(paste0(url, "obscuredx.dta"), header = FALSE, sep = ",")
dim(obscuredx)

attributeName = setNames(read.csv(paste0(url, "obscured.attr"), header = FALSE, sep = ":"), 
                         c("name", "value"))

# set colnames for data frame
names(obscuredx) = attributeName$name

# ---------------------------missing value analysis---------------
# handle missing value
obscuredx[obscuredx == "?"] = NA
obscuredx = droplevels(obscuredx)
# ------------------------------data splitting----------------
# first, split the training set off
set.seed(760)
split1.x = createDataPartition(obscuredx$contam_code, 
                               p = .8,
                               list = FALSE)
training.x = obscuredx[split1.x, ]
testing.x = obscuredx[-split1.x, ]
# -----------------------------zero variance----------------
nzv = nearZeroVar(obscuredx, saveMetrics = TRUE)
nzv

zero.var = c("non_risk_goods") 
training.x = training.x[ , -which(names(training.x) %in% zero.var)]

# Determine the predictor names
predictors = names(training.x)[names(training.x) != "contam_code"]
# ------------------------summarize data----------------
dim(training.x)
sapply(training.x, class)
dim(testing.x)
levels(training.x$contam_code)
length(levels(training.x$full_name))
length(levels(training.x$country_name))
percentage <- prop.table(table(obscuredx$contam_code)) * 100
cbind(freq=table(obscuredx$contam_code), percentage=percentage)
#---------------------------performance wrapper-----------
fiveStats = function(...) c(multiClassSummary(...),
                            defaultSummary(...))

# ----------------training control----------------
ctrl1 = trainControl(method = "repeatedcv", 
                     number = 10,
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     verboseIter = TRUE)

ctrl2 = trainControl(method = "cv", 
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     verboseIter = TRUE)

# ---------------- base model  ----------------
# -------------------- c5.0 -------------------
library(C50)
library(plyr)

# training model
c50grid = expand.grid(winnow = TRUE, 
                      trials = 1:10, 
                      model = "rules")

c50base.x = train(x = training.x[ , predictors],
                  y = training.x$contam_code,
                  method = "C5.0",
                  trControl = ctrl1,
                  tuneGrid = c50grid,
                  verbose = TRUE,
                  metric = "Accuracy", 
                  na.action = na.pass)

c50base.x
setwd("D:/UoA/COMSCI760/Assignments/R/c50")
write.csv(c50base.x$results, "c50_base_training_overall_x.csv")
write.csv(c50base.x$bestTune, "c50_base_besttune_x.csv")

saveRDS(c50base.x, "c50xbasemodel.rds")

# evaluation model
c50testing.x = data.frame(contam_code = testing.x$contam_code)
c50testing.x$c50 = predict(c50base.x,
                           newdata = testing.x[ ,predictors])

# confusionMatrix
c50cm.x = confusionMatrix(data = c50testing.x$c50, 
                          reference = c50testing.x$contam_code,
                          mode = "prec_recall")

write.csv(c50cm.x$table, "c50_base_confusionmatrix_x.csv")
write.csv(c50cm.x$byClass, "c50_base_byclass_x.csv")
write.csv(c50cm.x$overall, "c50_base_testing_overall_x.csv")

# -------------------- naive bayes -------------------
# training model
library(naivebayes)
nbgrid = expand.grid(fL = c(0, 1), 
                     usekernel = c(TRUE, FALSE), 
                     adjust = 1)

nbbase.x = train(x = training.x[ , predictors],
                 y = training.x$contam_code,
                 method = "naive_bayes",
                 trControl = ctrl1,
                 tuneGrid = nbgrid,
                 verbose = TRUE,
                 metric = "Accuracy", 
                 na.action = na.pass)

nbbase.x

setwd("D:/UoA/COMSCI760/Assignments/R/naivebayes")
write.csv(nbbase.x$results, "nb_base_training_overall_x.csv")
write.csv(nbbase.x$bestTune, "nb_base_besttune_x.csv")
saveRDS(nbbase.x, "nbbasemodel.rds")

# evaluation model
nbtesting.x = data.frame(contam_code = testing.x$contam_code)
nbtesting.x$nb = predict(nbbase.x,
                         newdata = testing.x[ ,predictors])

# confusionMatrix
nbcm.x = confusionMatrix(nbtesting.x$nb, nbtesting.x$contam_code)

write.csv(nbcm.x$table, "nb_base_confusionmatrix_x.csv")
write.csv(nbcm.x$byClass, "nb_base_byclass_x.csv")
write.csv(nbcm.x$overall, "nb_base_testing_overall_x.csv")


#--------------------------rpart-------------------------
library(rpart)
# training model
rpgrid = expand.grid(cp = seq(0.001, 0.02, 0.001))

rpbase.x = train(contam_code ~ .,
                 data = training.x,
                 method = "rpart",
                 trControl = ctrl1,
                 tuneGrid = rpgrid,
                 metric = "Accuracy", 
                 na.action = na.pass)

rpbase.x
setwd("D:/UoA/COMSCI760/Assignments/R/rpart")
write.csv(rpbase.x$results, "rpart_base_training_overall_x.csv")
write.csv(rpbase.x$bestTune, "rpart_base_besttune_x.csv")
saveRDS(rpbase.x, "rpartbasemodel.rds")

library(rattle)
fancyRpartPlot(rpbase.x$finalModel)

# evaluation model
# rptest.x = na.omit(testing.x)
rptesting.x = data.frame(contam_code = testing.x$contam_code)
rptesting.x$rpart = predict(rpbase.x,
                            newdata = testing.x[ ,predictors],
                            na.action = na.pass)

# confusionMatrix
rpartcm.x = confusionMatrix(rptesting.x$rpart, rptesting.x$contam_code)

write.csv(rpartcm.x$table, "rpart_base_confusionmatrix_x.csv")
write.csv(rpartcm.x$byClass, "rpart_base_byclass_x.csv")
write.csv(rpartcm.x$overall, "rpart_base_testing_overall_x.csv")

#--------------------------compare model-------------------------
# collect resamples
results = resamples(list(C50 = c50base.x, RPART = rpbase.x))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results, metric = c("logLoss", "Accuracy", "Kappa") , 
       layout = (c(3, 1)), main = "Models Comparison")
bwplot(results)
# dot plots of results
dotplot(results)

library(lattice)
histogram(~ country_name| contam_code, obscuredx)