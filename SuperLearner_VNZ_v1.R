dat <- read.csv('Venezuela_Matriz_260919b.csv')

library(SuperLearner)
x <-dat[,9:133]
x$response <-  dat[,8] 

NA2median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
x[] <- lapply(x, NA2median)

fit_cv<- CV.SuperLearner(
    log1p(x[,126]),
                                     x[,1:125],	
                                     V=5,
                                     SL.library=list("SL.xgboost", "SL.ranger",
                                                     "SL.ksvm", "SL.kernelKnn" ,"SL.bayesglm"))

plot(fit_cv)

fit_all<- SuperLearner(
    log1p(x[,126]),
                                     x[,1:125],	
                                    
                                     SL.library=list("SL.xgboost", "SL.ranger",
                                                     "SL.ksvm", "SL.kernelKnn" ,"SL.bayesglm"))


test<- read.csv('Venezuela_validacion_290919d.csv')
val <-test[,9:133]
val[] <- lapply(val, NA2median)
pred <- predict(fit_all, val, x[,1:125], log1p(x[,126]), onlySL=TRUE)
#evaluation

	obs <- data.frame(obs=test$CO_STOCK)
	#modeled data
	 mod <-  data.frame(mod=expm1(as.numeric(pred[[1]])))
	#conditional quantile 
	 conditionalQuantile(cbind(obs, mod), obs = "obs", mod = "mod")
	#evaluation statistics 
	modStats(cbind(obs, mod), obs = "obs", mod = "mod")


