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

# ----------------missing value analysis---------------
# handle missing value
obscuredx[obscuredx == "?"] = NA
obscuredx = droplevels(obscuredx)

library(reshape2)
missing = melt(missing.prop, id = "attribute")

library(ggplot2)
# Basic barplot
ggplot(data = missing, aes(x = attribute, y = value, fill = variable)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_brewer(palette="Blues") +
  labs(title="Attributes with Missing Values", 
       x="Attribute", y = "Proportion") +
  theme_classic()


library(VIM)
options(op)
aggr_plot = aggr(obscuredx[, 15:19], col = c('navyblue','red'), prop = FALSE,
                 numbers = TRUE, sortVars = TRUE, 
                 labels = names(obscuredx[, 15:19]), cex.axis = .7, cex.numbers = 0.7,
                 gap = 3, 
                 ylab = c("Histogram of missing data","Pattern"))

aggr_plot = aggr(testing.x[, 15:19], col = c('navyblue','red'), prop = FALSE,
                 numbers = TRUE, sortVars = TRUE, 
                 labels = names(testing.x[, 15:19]), cex.axis = .7, cex.numbers = 0.7,
                 gap = 3, 
                 ylab = c("Histogram of missing data","Pattern"))

# ----------------frequency table----------------
Freq = function(df){
  freq = data.frame(table(df$contam_code))
  prop = data.frame(round(prop.table(table(df$contam_code)) * 100, digits = 3))
  freq = merge(freq, prop, by = "Var1")
  colnames(freq) = c("contam_code", "freq", "prop")
  freq = freq[order(freq$freq), ]
}

percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

freq = Freq(obscuredx)
freq

# Basic barplot
freq.plot = function(df, title){
  ggplot(data = df, aes(x = contam_code, y = prop)) +
    geom_bar(stat = "identity", fill = "steelblue") + 
    geom_text(aes(label = prop), vjust = -0.3, size = 3.5, angle = 90) +
    labs(title = title, 
         x = "contam_code", y = "Proportion") +
    theme_classic()
}
freq.plot(freq, "Imbalanced Classification")

# country_name and code
code.country = obscuredx[, c(1, 21)]
code.bycountry = as.data.frame(table(code.country))

library(RColorBrewer)
colourCount = length(unique(code.bycountry$country_name))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(data = code.bycountry, aes(x = country_name, y = Freq, fill = contam_code)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual(values = getPalette(colourCount)) + 
  labs(title="DatasetX: Country name and Contam Code Frequency Table", 
       x="Country Name", y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


cc.y = obscuredy[, c(1, 21)]
code.conty.y = as.data.frame(table(cc.y))

ggplot(data = code.conty.y, aes(x = country_name, y = Freq, fill = contam_code)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual(values = getPalette(colourCount)) + 
  labs(title="DatasetY: Country name and Contam Code Frequency Table", 
       x="Country Name", y = "Frequency") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

# ----------------data splitting----------------
set.seed(760)
split1.x = createDataPartition(obscuredx$contam_code, 
                               p = .8,
                               list = FALSE)
training.x = obscuredx[split1.x, ]
testing.x = obscuredx[-split1.x, ]
# --------------- three parts ------------------
set.seed(760)
split1.x = createDataPartition(obscuredx$contam_code, 
                               p = .8,
                               list = FALSE)
training.x = obscuredx[split1.x, ]
other.x = obscuredx[-split1.x, ]

# Now create the evaluation and test sets
set.seed(760)
split2.x <- createDataPartition(other.x$contam_code, 
                                p = 1/3,
                                list = FALSE)
evaluation.x <- other.x[split2.x,]
testing <- other.x[-split2.x,]

# ----------------zero variance----------------
nzv = nearZeroVar(obscuredx, saveMetrics = TRUE)
nzv

zero.var = c("non_risk_goods") 
training.x = training.x[ , -which(names(training.x) %in% zero.var)]

# Determine the predictor names
predictors = names(training.x)[names(training.x) != "contam_code"]
setwd("D:/UoA/COMSCI760/Assignments/Data")

write.csv(training.x, "trainingx.csv", na = "?")
write.csv(testing.x, "testingx.csv",na = "?")
# ----------------impute missing value----------------

library(Hmisc)

# using argImpute
impute.pmm = aregImpute(~ animal_products + agricultural_compounds +
                          car_parts + fertiliser + nursery_stock + plant_products + 
                          produce + seed_grain + stored_products + tyres + 
                          vehicles + equipment + clean + Pack_Mat + Pack_Wood +
                          Wood_Treat + Cert_Avail, 
                        data = training.x, 
                        n.impute = 5, type = "pmm")

library(missForest)
impute.rf = missForest(training.x, maxiter = 5, ntree = 10,
                       variablewise = TRUE)

library(mice)
imputed = mice(training.x, m = 5, maxit = 10, method = 'logreg', seed = 500)
complete = complete(imputed, "long")
imp.1 = complete(imputed, 1)
imp.2 = complete(imputed, 2)
imp.3 = complete(imputed, 3)
imp.4 = complete(imputed, 4)
imp.5 = complete(imputed, 5)

densityplot(imputed, ~ clean + Pack_Mat + Pack_Wood + Wood_Treat + Cert_Avail, 
            scales = list(x = list(relation = "free")),
            main = "Imputation Check")

#-----------------load imputation data-------------------
# write
setwd("D:/UoA/COMSCI760/Assignments/Data/imputation")
write.csv(complete, "mice_imputed.csv")
for(i in 1:5){
  write.csv(complete(imputed, i), paste0("mice_imputed", i, ".csv"))
}

# read
imp = vector("list", 5)
for(i in 1:5){
  imp[[i]] = read.csv(paste0("mice_imputed", i, ".csv"), header = TRUE, sep = ",")
}
# read all
complete = read.csv("mice_imputed.csv", header = TRUE, sep = ",")
complete.m1 = complete.x[complete.x$.imp == 1, -(1:2)]
dim(complete.m1)
#-----------------subsampling------------------
freq.train = Freq(training.x)
freq.plot(freq.train, "Training Imbalanced Classification")

# smote sampling
library(DMwR)

code = unique(training.x$contam_code)
train.smote = vector("list", length(code)-1)
for(i in code){
  if(i != "c")
  train.pairx = imp.1[imp.1$contam_code %in% c("c", i), ]
  train.pairx$contam_code = factor(train.pairx$contam_code)
  index = which(code == i)
  train.smote[[index]] = SMOTE(contam_code ~ ., 
                               data  = train.pairx,
                               perc.over = 1000,
                               perc.under = 100) 
}
train.smote = do.call(rbind, train.smote)
train.smote6 = na.omit(train.smote)

setwd("D:/UoA/COMSCI760/Assignments/Data/smote")
write.csv(train.smote5, "smote_imp5_1000.csv")

freq.smote6 = Freq(train.smote6)
freq.plot(freq.smote6, "SMOTE Resampling")

# load smote data
smote2 = read.csv("D:/UoA/COMSCI760/Assignments/Data/smote/smote_imp2_1000.csv",
                  header = TRUE, sep = ",")

#-----------------performance wrapper-----------
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
# -------------------- c5.0 -------------------
# --------------------- training ---------------------
library(C50)
library(plyr)

# training model
c50grid = expand.grid(winnow = TRUE, 
                       trials = 1:10, 
                       model = c("rules"))

c50.imp2 = train(x = smote2[ , predictors],
                 y = smote2$contam_code,
                 method = "C5.0",
                 trControl = ctrl1,
                 tuneGrid = c50grid,
                 verbose = TRUE,
                 metric = "Accuracy", 
                 na.action = na.pass)

c50.imp2
setwd("D:/UoA/COMSCI760/Assignments/R/c50")
write.csv(c50.imp3$results, "c50_imp2_1000_overall.csv")
write.csv(c50.imp3$bestTune, "c50_imp2_1000_besttune.csv")

saveRDS(c50.imp2, "c50imp2_1000model.rds")

#---------------------- load models --------------------
# load the model
c50.imp5 = readRDS("c50imp5_1000model.rds")
print(c50.imp1)
write(c50.imp1$finalModel$output, file = "smote1_output")

# --------------------- evaluation ---------------------
# evaluation model
c50testing.imp2 = data.frame(contam_code = testing.x$contam_code)
c50testing.imp2$c50 = predict(c50.imp2,
                              newdata = testing.x[ ,predictors],
                              na.action = na.pass)
                              #type = "prob")

library(jsonlite)
c50testing.imp1f = flatten(c50testing.imp1)
predicted.class = apply(c50testing.imp1f[,-1], 1, which.max)
multiclass.roc(response = testing.x$contam_code, predictor = predicted.class)
# write.csv(c50testing.imp1$c50, "c50prob_imp1.csv")

c50testing.imp2$c50 = factor(c50testing.imp2$c50, 
                             levels = levels(c50testing.imp1$contam_code))

# confusionMatrix
c50cm.imp1 = confusionMatrix(data = c50testing.imp1$c50, 
                             reference = c50testing.imp1$contam_code,
                             mode = "prec_recall")

write.csv(c50cm.imp5$table, "c50_imp5_1000_confusionmatrix.csv")
write.csv(c50cm.imp5$byClass, "c50_imp5_1000_byclass.csv")
write.csv(c50cm.imp5$overall, "c50_imp5_1000_testing_overall.csv")

#--------------------------rpart-------------------------
library(rpart)
# training model
rpgrid = expand.grid(cp = seq(0.01, 0.02, 0.001))

rp.imp2 = train(contam_code ~ .,
               data = train.smote,
               method = "rpart",
               trControl = ctrl1,
               tuneGrid = rpgrid,
               metric = "Accuracy", 
               na.action = na.pass)

rp.imp2
setwd("D:/UoA/COMSCI760/Assignments/R/rpart")
write.csv(rpbase.x$results, "rpart_imp2_training_overall.csv")
write.csv(rpbase.x$bestTune, "rpart_imp2_besttune.csv")
saveRDS(rpbase.x, "rpartimp2model.rds")

library(rattle)
fancyRpartPlot(rp.imp2$finalModel)

# evaluation model
# rptest.x = na.omit(testing.x)
rptesting = data.frame(contam_code = testing.x$contam_code)
rptesting$rpart = predict(rp.imp2,
                            newdata = testing.x[ ,predictors],
                            na.action = na.pass)

# confusionMatrix
rpartcm.x = confusionMatrix(rptesting.x$rpart, rptesting.x$contam_code, 
                            mode = "prec_recall")

write.csv(rpartcm.x$table, "rpart_imp2_confusionmatrix_x.csv")
write.csv(rpartcm.x$byClass, "rpart_imp2_byclass_x.csv")
write.csv(rpartcm.x$overall, "rpart_imp2_testing_overall_x.csv")

#--------------------------compare model-------------------------
# collect resamples
results = resamples(list(C50 = c50.imp2, RPART = rp.imp2))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results, metric = c("logLoss", "Accuracy", "Kappa") , 
       layout = (c(3, 1)), main = "Models Comparison")

# compare imptation set 1-5
results = resamples(list(imp1 = c50.imp1, imp2 = c50.imp2,
                         imp3 = c50.imp3, imp4 = c50.imp4,
                         imp5 = c50.imp5))
summary(results)
bwplot(results, metric = c("Accuracy", "Mean_Balanced_Accuracy", 
                           "Kappa", "AUC", "Mean_Sensitivity", "Mean_Specificity"),
       layout = (c(3, 2)), main = "Comparing Models on Five Imputed Datasets",
       xlim = c(0, 1))

library(pROC)
multiclass.roc(testing.x$contam_code,
               as.ordered(predict(c50.imp1,
                       newdata = testing.x[ ,predictors],
                       na.action = na.pass,
                       type = "raw")))

#-------------------------by class mean-----------------------
setwd("D:/UoA/COMSCI760/Assignments/R/c50")
byclass = read.csv("overall_byclass.csv", header = TRUE, sep = ",")
meanbyclass = aggregate(byclass[, -(1:2)], by = list(byclass$Class), mean)
write.csv(meanbyclass, "meanbyclass.csv")