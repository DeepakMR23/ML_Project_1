library(caret)

Sigmoid <- function(X)
{
	1/(1 + exp(-X))
}

ProjectSolution <- function()
{
	currentWorkDir <- getwd()
	setwd('C:\\Users\\new\\Desktop\\Studies\\Sem2\\ML\\Project\\BlogFeedback')

	blog.train <- read.csv('blogData_train.csv', header=FALSE)

	#select the base features and target variable 
	blog.trainExp1 <- blog.train[,c(51:60,281)]

	Exp1Names <- c("Cmnts", "Cmnts24", "Cmnts48", "CmntsDayBfr", "CmntsChng", "trackBacks", "trackBacks24", "trackBacks48", "trackBacksDayBfr", "TrackBacksChng", "ExpCmmnts")
	
	names(blog.trainExp1) <- Exp1Names
	
	print(nrow(blog.trainExp1))
	print(ncol(blog.trainExp1))
	
	#read test data from different files and append to one data frame
	
	testfiles <- list.files(pattern = "*_test*")
	
	testdf <- read.csv(testfiles[1], header=FALSE)
	columnNms <- names(testdf)
	
	for( i in 2:length(testfiles) ) {
		temp <- read.csv(testfiles[i], header=FALSE)
		testdf <- rbind(testdf, setNames(temp, columnNms))
		#print(nrow(testdf))
	}

	
	#EXPERIMENT 1
	#============
	
	#Subset the test data for Experiment 1
	blog.testExp1 <- testdf[, c(51:60,281)]
	
	#Make the column names for exp1 train and test data the same
	trainNms <- names(blog.trainExp1)
	blog.testExp1 <- setNames(blog.testExp1, trainNms)
	
	print(ncol(blog.testExp1))
	print(nrow(blog.testExp1))
	print(names(blog.testExp1))
	
	blog.linearfit <- lm(formula = ExpCmmnts ~ Cmnts + Cmnts24 + Cmnts48 + CmntsDayBfr + CmntsChng + trackBacks + trackBacks24 + trackBacks48 + trackBacksDayBfr + TrackBacksChng, data=blog.trainExp1)
	summary(blog.linearfit)
	
	#plot(blog.linearfit)
	
	#Test the fit
	blog.predictExp1 <- predict(blog.linearfit, blog.testExp1, se.fit=TRUE)
	
	#print(blog.predictExp1$fit)
	
	MSE = mean((blog.predictExp1$fit - blog.testExp1$ExpCmmnts)^2)
	print(cat("MSE For Experiment 1: ",MSE))
	
	#Logistic Regression
	
	blog.trainExp1Log <- blog.trainExp1
	blog.testExp1Log <- blog.testExp1
	meanCmnts <- mean(blog.trainExp1Log[,"ExpCmmnts"])
	
	blog.trainExp1Log$ExpCmmnts <- sapply(blog.trainExp1Log$ExpCmmnts, function(x) {ifelse(x >= meanCmnts, 1, 0)})
	blog.testExp1Log$ExpCmmnts <- sapply(blog.testExp1Log$ExpCmmnts, function(x) {ifelse(x >= meanCmnts, 1, 0)})

	#print(blog.testExp1Log$ExpCmmnts)
	
	blog.logisticfit <- glm(formula = ExpCmmnts ~ Cmnts + Cmnts24 + Cmnts48 + CmntsDayBfr + CmntsChng + trackBacks + trackBacks24 + trackBacks48 + trackBacksDayBfr + TrackBacksChng, 
			data=blog.trainExp1Log, family = binomial())
	summary(blog.logisticfit)
	
	blog.predictExp1Log <- predict.glm(blog.logisticfit, blog.testExp1Log, type="response", se.fit=TRUE)
	#print(blog.predictExp1Log$fit)
	
	predictionsExp1 = sapply(blog.predictExp1Log$fit, function(x) {ifelse(x > .5, 1, 0)})
	
	print(confusionMatrix(blog.testExp1Log$ExpCmmnts, predictionsExp1))
	
	#EXPERIMENT 2
	#============
	
	#Subset the test data for Experiment 2
	blog.trainExp2 <- blog.train[,c(63:262,281)]
	blog.testExp2 <- testdf[, c(63:262,281)]
	
	#Make the column names for exp1 train and test data the same
	trainNms <- names(blog.trainExp2)
	blog.testExp2 <- setNames(blog.testExp2, trainNms)
	
	#print(ncol(blog.testExp2))
	#print(nrow(blog.testExp2))
	#print(names(blog.testExp2))
	
	blog.linearfit2 <- lm(formula = V281 ~ ., data=blog.trainExp2)
	summary(blog.linearfit2)
	
	#Test the fit
	blog.predictExp2 <- predict(blog.linearfit2, blog.testExp2, se.fit=TRUE)
	
	MSE2 = mean((blog.predictExp2$fit - blog.testExp2$V281)^2)
	print(cat("MSE For Experiment 2: ",MSE2))
	
	#Logistic Regression
	
	blog.trainExp2Log <- blog.trainExp2
	blog.testExp2Log <- blog.testExp2
	
	blog.trainExp2Log$V281 <- sapply(blog.trainExp2Log$V281, function(x) {ifelse(x >= meanCmnts, 1, 0)})
	blog.testExp2Log$V281 <- sapply(blog.testExp2Log$V281, function(x) {ifelse(x >= meanCmnts, 1, 0)})
	
	blog.logisticfit2 <- glm(formula = V281 ~ ., data=blog.trainExp2Log, family = binomial())
	summary(blog.logisticfit2)
	
	blog.predictExp2Log <- predict.glm(blog.logisticfit2, blog.testExp2Log, type="response", se.fit=TRUE)
	
	predictionsExp2 = sapply(blog.predictExp2Log$fit, function(x) {ifelse(x > .5, 1, 0)})
	
	print(confusionMatrix(blog.testExp2Log$V281, predictionsExp2))
	
}


