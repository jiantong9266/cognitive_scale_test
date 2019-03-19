library(tidyverse)
library(changepoint)
library(forecast)
library(ecp)
library(marima)

#::::::::::::::::: Function to prepare lookup table for ARIMA modeling

create_arima_lookuptable <- function(max_arima_order,
                                     max_seasonal_order,
                                     periods = 0){
  
  #' @description this function prepares complete list of arima combinations based on
  #' given arima orders, seasonal orders and periods
  #' 
  #' @param max_arima_order of the form (p,d,q) to denote maximum AR order, differencing order and MA order, resp
  #' @param max_seasonal_order of the form (P,D,Q) to denote maximum seasonal AR order, seasonal differencing and seasonal MA order, resp
  #' @param periods denotes the seasonal periods to consider, can be a vector
  #' 
  #' @return R dataframe with 7 columns "p","d","q","P","D","Q","period"
  
  if (length(max_arima_order)!=3 | length(max_seasonal_order)!=3){
    stop("order arguments must have length 3")
  } else {
    ARIMA_p = c(0:max_arima_order[1])
    ARIMA_d = c(0:max_arima_order[2])
    ARIMA_q = c(0:max_arima_order[3])
    Seasonal_P = c(0:max_seasonal_order[1])
    Seasonal_D = c(0:max_seasonal_order[2])
    Seasonal_Q = c(0:max_seasonal_order[3])
    Period = periods
    out = expand.grid(ARIMA_p,ARIMA_d,ARIMA_q,Seasonal_P,Seasonal_D,Seasonal_Q,Period)
    colnames(out) = c("p","d","q","P","D","Q","period")
    rownames(out) = c(1:nrow(out))
    return(out)
  }
  
}


#:::::::::::::::::::: FIND CHAMPION MODEL FROM REGULAR UNIVARIATE TIME SERIES DATA

find_champion_arima <- function(data,
                                show_variable = "SC3_Impressions",
                                agg_timescale = "Date",
                                log_transformation = 1,
                                OOS_start,
                                regressors,
                                keep_regressors = 'All',
                                max_changepoints = 0,
                                changepoint_dates = NULL,
                                lookuptable = NULL){
  
  #' @description finds best ARIMA model for the given data according to out-of-sample date 
  #' and conditions provided on regressors and change-points
  #' 
  #' @param data R data-frame, typically an output from `create_aggregated_data()` function (see `data_prep_function.R`)
  #' @param show_variable denotes which variable to fit the model to, default is `SC3_Impressions`
  #' @param agg_timescale denotes the time-scale of the aggregated data - choices are `Week`, `Date`, `Hour` or `Half_Hr`
  #' @param log_transformation denotes whether to use log-transformation to the variable, choices are 0 (no) and 1 (yes), default is 1
  #' @param OOS_start the first date for the out-of-sample, format has to be `as.Date()`
  #' @param regressors vector of all regressors to be used, include `trend` in this vector if you want to use drift/trend in the model
  #' @param keep_regressors vector of all regressors to be forced to use in the model, default is all regressors 
  #' @param max_changepoints no of changepoints to be used in piecewise linear trend, default is 0 (not to use piecewise linear trend)
  #' @param changepoint_dates dates where trend behavior changes, default is `NULL` (changepoints to be found using statistical means)
  #' @param lookuptable R dataframe with candidate ARIMA models (typically an output from `create_arima_lookuptable()`)
  #'                    default is NULL (will return best model from `auto.arima()` function)
  #' 
  #' @return list containing the following:
  #' - ARIMA model object
  #' - R dataframe with all details and model fit and forecasts for train/test data
  #' - vector of regressors eventually used in the model
  #' - changepoint dates for piecewise linear function
  
  # check if max_changepoints correspond to changepoint_dates
  if (is.null(changepoint_dates)!=0 & length(changepoint_dates)>max_changepoints){
    stop("no of changepoint-dates is more than allowed no of changepoints")
  }
  if (!"trend" %in% regressors & max_changepoints > 0){
    stop("'trend' has to be included in regressors to allow for piecewise linear trend")
  }
  
  # prepare train and test data 
  if (agg_timescale == "Week"){
    train <- data %>% filter(Week < OOS_start)
    test <- data %>% filter(Week >= OOS_start)
  } else{
    train <- data %>% filter(Date < OOS_start)
    test <- data %>% filter(Date >= OOS_start)
  }
  train_periods = nrow(train)
  test_periods = nrow(test)
  
  # select appropriate regressor subset
  if (keep_regressors == "All") keep_regressors = regressors
  full_formula <- paste(show_variable,paste(regressors[! regressors == "trend"],collapse = "+"),sep = "~")
  reduced_formula <- paste(show_variable,paste(keep_regressors[! keep_regressors == "trend"],collapse = "+"),sep = "~")
  full_model <- lm(formula(full_formula),data = train)
  lower_model <- lm(formula(reduced_formula),data = train)
  reduced_model <- step(full_model,scope = list(lower = lower_model,upper = full_model),trace = 0)
  regressors_subset <- names(reduced_model$coefficients)[-1]
  
  # prepare xreg matrices for train/test for the arima models
  reg_train <- train[,regressors_subset]
  reg_test <- test[,regressors_subset]
  if ("trend" %in% regressors){
    x1 = c(1:train_periods)
    reg_train <- cbind(train[,regressors_subset],x1)
    colnames(reg_train)[ncol(reg_train)] = "trend"
    x2 = seq(train_periods+1,train_periods+test_periods,1)
    reg_test <- cbind(test[,regressors_subset],x2)
    colnames(reg_test)[ncol(reg_test)] = "trend"
  }
  
  # adjust xreg matrices for piecewise linear trends
  if (max_changepoints > 0){
    if (length(changepoint_dates)==0){
      changepoints <- cpt.mean(as.numeric(unlist(train[,show_variable])),method = "BinSeg",pen.value = "2*log(n)",Q = max_changepoints)
      knotpoints = cpts(changepoints)
    } else{
      knotpoints = which(train[,agg_timescale] %in% changepoint_dates)
    }
    if (length(knotpoints)>0){
      for (i in 1:length(knotpoints)){
        reg_train <- cbind(reg_train,pmax(0,x1-knotpoints[i]))
        newname <- paste("trend_after",knotpoints[i],sep = "_")
        colnames(reg_train)[ncol(reg_train)] = newname
        reg_test <- cbind(reg_test,pmax(0,x2-knotpoints[i]))
        colnames(reg_test)[ncol(reg_test)] = newname
      }
      changepoint_dates = train[knotpoints,agg_timescale]
    }
  }
  
  # adjust xreg matrices as needed
  idx = which(colSums(reg_train)==0)
  if (length(idx)>0){
    reg_train <- reg_train[,-idx]
    reg_test <- reg_test[,-idx]
  }
  
  # prepare data based on log-transformation
  if (log_transformation == 1){
    train_series = log(as.numeric(unlist(train[,show_variable])) + 0.01)
    test_series = log(as.numeric(unlist(test[,show_variable])) + 0.01)
  } else{
    train_series = as.numeric(unlist(train[,show_variable]))
    test_series = as.numeric(unlist(test[,show_variable]))
  }
  
  # auto-arima out of sample selection
  fit_auto <- auto.arima(train_series,
                         max.p = 7,max.P = 7,max.q = 7,max.Q = 7,max.d = 1,max.D = 0,
                         seasonal = TRUE,
                         trace = FALSE,
                         approximation = FALSE,
                         allowdrift = FALSE,
                         allowmean = FALSE,
                         stepwise = TRUE, 
                         biasadj = FALSE,
                         ic = "aic",
                         lambda = "auto",
                         xreg = as.matrix(reg_train))
  evaluate_auto <- forecast::forecast(fit_auto,h = test_periods,xreg = as.matrix(reg_test))
  ModelAccuracy_auto <- sum(accuracy(evaluate_auto,x = test_series)[,5]) 
  champion_model = fit_auto
  ModelAccuracy_champion = ModelAccuracy_auto
  
  # find the champion model using set of pre-defined lookup table of difference ARMA combinations
  if (is.null(lookuptable) != 0){
    total_loop <- nrow(lookuptable) 
    for(i in 1:total_loop){
      fit_test <- Arima(train_series,
                        order = lookuptable[i,c("p","d","q")],
                        seasonal = list(order = lookuptable[i,c("P","D","Q")],period = lookuptable$period[i]),
                        include.drift = model_drift, 
                        include.constant = FALSE,
                        lambda = 'auto',
                        xreg = as.matrix(reg_train),
                        method = 'CSS')
      if (fit_test$code == 0){
        evaluate_test <- forecast::forecast(fit_test,h = test_periods,xreg = as.matrix(reg_test))
        ModelAccuracy <- sum(accuracy(evaluate_test,x = test_series)[2,5])
        if (ModelAccuracy < ModelAccuracy_champion){
          champion_model = fit_test
          ModelAccuracy_champion = ModelAccuracy
        }
      }
    }
  }
  
  # forecast champion model - test set
  champion_forecast <- champion_model %>% forecast::forecast(h = test_periods, xreg = as.matrix(reg_test))
  
  if (log_transformation == 1){
    trainfit_champion <- exp(champion_forecast$fitted[1:nrow(train)])
    Predict_champion <- exp(champion_forecast$mean[1:nrow(test)])
  } else{
    trainfit_champion <- champion_forecast$fitted[1:nrow(train)]
    Predict_champion <- champion_forecast$mean[1:nrow(test)]
  }
  combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
  names(combinedfit_champion) = "Predict"
  
  full_data_champion <- as.data.frame(cbind(as.data.frame(data),combinedfit_champion)) 
  full_data_champion$Stream = show_variable
  full_data_champion$Error = ((full_data_champion[,show_variable] - full_data_champion$Predict)/full_data_champion[,show_variable])*100
  full_data_champion$APE = abs(full_data_champion$Error)
  
  return(list(champion_model = champion_model,
              champion_result = full_data_champion,
              regressors = regressors_subset,
              changepoints = changepoint_dates))
}

#:::::::::::::::::::: FIND CHAMPION MODEL FROM MULTIVARIATE DATA

find_champion_arima_BV <- function(data,
                                   show_names,
                                   show_variable = "SC3_Impressions",
                                   agg_timescale = "Week",
                                   log_transformation = 1,
                                   OOS_start,
                                   regressors,
                                   piecewsise_trend = FALSE,
                                   changepoint_dates = NULL,
                                   maximum_AR,
                                   maximum_MA){
  
  #' @description finds best bivariate ARIMA model for the given data according to out-of-sample date 
  #' and conditions provided on regressors and change-points
  #' 
  #' @param data R data-frame, typically an output from `create_aggregated_data()` function (see `data_prep_function.R`)
  #' @param show_names denotes the names of the first and second show, in that order
  #' @param show_variable denotes which variable to fit the model to, default is `SC3_Impressions`
  #' @param agg_timescale denotes the time-scale of the aggregated data - choices are `Week` or `Date`
  #' @param log_transformation denotes whether to use log-transformation to the variable, choices are 0 (no) and 1 (yes), default is 1
  #' @param OOS_start the first date for the out-of-sample, format has to be `as.Date()`
  #' @param regressors vector of all regressors to be used in the model
  #' @param piecewise_trend logical; whether to use piecewise linear trend, default is FALSE
  #' @param changepoint_dates dates where trend behavior changes, default is `NULL` (changepoints to be found using statistical means)
  #' @param maximum_AR denotes maximum auto-regressive order to consider, must be greater than 1
  #' @param maximum_MA denotes maximum moving-average order to consider, must be greater than 1
  #' 
  #' @return list containing the following:
  #' - best AR/MA order
  #' - linear model object from the first step for first show
  #' - linear model object from the first step for second show
  #' - multivariate ARIMA model object
  #' - multivariate ARIMA model forecasts
  #' - R dataframe with all details and model fit and forecasts for train/test data for first show
  #' - R dataframe with all details and model fit and forecasts for train/test data for first show
  #' - changepoint dates for piecewise linear function
  
  if (length(show_names) != 2){
    stop("this method works only for two consecutive shows")
  }
  if (min(maximum_AR,maximum_MA) < 2){
    stop("AR/MA orders have to be integers greater than 1")
  }
  
  # create data files for the two shows separately
  agg_dataA <- subset(data,Show_Name == show_names[1])
  agg_dataB <- subset(data,Show_Name == show_names[2])
  
  # prepare train and test data 
  if (agg_timescale == "Week"){
    commondates <- intersect(agg_dataA$Week,agg_dataB$Week)
    idxA = which(agg_dataA$Week %in% commondates)
    idxB = which(agg_dataB$Week %in% commondates)
    partA <- agg_dataA[idxA,]
    trainA <- partA %>% filter(Week < OOS_start)
    testA <- partA %>% filter(Week >= OOS_start)
    partB <- agg_dataB[idxB,]
    trainB <- partB %>% filter(Week < OOS_start)
    testB <- partB %>% filter(Week >= OOS_start)
  } else{
    commondates <- intersect(agg_dataA$Date,agg_dataB$Date)
    idxA = which(agg_dataA$Date %in% commondates)
    idxB = which(agg_dataB$Date %in% commondates)
    partA <- agg_dataA[idxA,]
    trainA <- partA %>% filter(Date < OOS_start)
    testA <- partA %>% filter(Date >= OOS_start)
    partB <- agg_dataB[idxB,]
    trainB <- partB %>% filter(Date < OOS_start)
    testB <- partB %>% filter(Date >= OOS_start)
  }
  
  train_periods <- nrow(trainA)
  test_periods <- nrow(testA)
  
  # get the set of xreg
  regressors_subset = regressors[! regressors == "trend"]
  x1 = c(1:train_periods)
  reg_train <- cbind(trainA[,regressors_subset],x1)
  colnames(reg_train)[ncol(reg_train)] = "trend"
  x2 = seq(train_periods+1,train_periods+test_periods,1)
  reg_test <- cbind(testA[,regressors_subset],x2)
  colnames(reg_test)[ncol(reg_test)] = "trend"
  
  if (piecewsise_trend == TRUE){
    if (length(changepoint_dates) > 0){
      knotpoints = which(trainA[,agg_timescale] %in% changepoint_dates)
    } else {
      changepoints <- e.divisive(cbind(c(trainA[,show_variable]),c(trainB[,show_variable])),R = 499)
      knotpoints = changepoints$estimates[-c(1,length(changepoints$estimates))]
    }
    if (length(knotpoints)>0){
      for (i in 1:length(knotpoints)){
        reg_train <- cbind(reg_train,pmax(0,x1-knotpoints[i]))
        newname <- paste("trend_after",knotpoints[i],sep = "_")
        colnames(reg_train)[ncol(reg_train)] = newname
        reg_test <- cbind(reg_test,pmax(0,x2-knotpoints[i]))
        colnames(reg_test)[ncol(reg_test)] = newname
      }
      changepoint_dates = trainA[knotpoints,agg_timescale]
    }
  } 
  
  # update xreg matrices if needed
  idx = which(colSums(reg_train)==0)
  if (length(idx)>0){
    reg_train <- reg_train[,-idx]
    reg_test <- reg_test[,-idx]
  }
  
  # use log-transformation if needed
  if (log_transformation == 1){
    trainA_series = log(as.numeric(unlist(trainA[,show_variable])) + 0.01)
    testA_series = log(as.numeric(unlist(testA[,show_variable])) + 0.01)
    trainB_series = log(as.numeric(unlist(trainB[,show_variable])) + 0.01)
    testB_series = log(as.numeric(unlist(testB[,show_variable])) + 0.01)
  } else{
    trainA_series = as.numeric(unlist(trainA[,show_variable]))
    testA_series = as.numeric(unlist(testA[,show_variable]))
    trainB_series = as.numeric(unlist(trainB[,show_variable]))
    testB_series = as.numeric(unlist(testB[,show_variable]))
  }
  
  # combine the two shows data into one data.frame
  Y_train <- data.frame(A = trainA_series,B = trainB_series,reg_train)
  Y_test <- data.frame(A = testA_series,B = testB_series,reg_test)
  
  # OLS for the shows, use assigned regressors for A, add A as a regressor for B
  LM_A <- lm(formula(paste("A",paste(names(reg_train),collapse = "+"),sep = "~")),data = Y_train)
  LM_B <- lm(formula(paste("B",paste(c("A",names(reg_train)),collapse = "+"),sep = "~")),data = Y_train)
  
  # fit VARMA for the bivariate vectors for the residuals
  errors <- data.frame(e1 = LM_A$residuals,e2 = LM_B$residuals)
  cutoff = 1e10
  
  # check performances for different AR/MA orders and find the best model
  for (i in 1:maximum_AR){
    for (j in 1:maximum_MA){
      if (i+j>2){
        # fit the MARIMA model to the residuals
        Model1 <- define.model(kvar = ncol(errors), ar = c(0:(i-1)), ma = c(0:(j-1)))
        Marima_fit <- marima(ts(errors),Model1$ar.pattern,Model1$ma.pattern,penalty = 2)
        
        # get the forecasts from the marima model
        arima1 <- arima(errors[,1],order = c((i-1),0,(j-1)))
        f1 <- forecast::forecast(arima1,h = test_periods)
        arima2 <- arima(errors[,2],order = c((i-1),0,(j-1)))
        f2 <- forecast::forecast(arima2,h = test_periods)
        Marima_forecast <- arma.forecast(cbind(f1$mean,f2$mean),
                                         marima = Marima_fit,nstart = 0,nstep = test_periods,check = F)
        
        # add back the mean estimates from the previous linear models
        A_forecast = Marima_forecast$forecasts[1,] + predict(LM_A,Y_test)
        Bnewdata = Y_test
        Bnewdata$A = A_forecast
        B_forecast = Marima_forecast$forecasts[2,] + predict(LM_B,Bnewdata)
        A_pred <- rowSums(cbind(fitted(Marima_fit)[1,],fitted(LM_A)),na.rm = T)
        B_pred <- rowSums(cbind(fitted(Marima_fit)[2,],fitted(LM_B)),na.rm = T)
        
        # get the train/test predictions for the original series
        if (log_transformation == 1){
          A_train = mape(actual = exp(Y_train$A),pred = exp(A_pred))
          B_train = mape(actual = exp(Y_train$B),pred = exp(B_pred))
          A_test = mape(actual = exp(Y_test$A),pred = exp(A_forecast))
          B_test = mape(actual = exp(Y_test$B),pred = exp(B_forecast))
        } else {
          A_train = mape(actual = (Y_train$A),pred = (A_pred))
          B_train = mape(actual = (Y_train$B),pred = (B_pred))
          A_test = mape(actual = (Y_test$A),pred = (A_forecast))
          B_test = mape(actual = (Y_test$B),pred = (B_forecast))
        }
        temp = A_train + A_test + B_train + B_test
        
        # check if this beats the current champion
        if(temp < cutoff){
          cutoff = temp
          champion <- data.frame(ARorder = (i-1),MAorder = (j-1))
          if(log_transformation == 1){
            trainfit_champion = exp(A_pred)
            Predict_champion = exp(A_forecast)
            combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
            names(combinedfit_champion) = "Predict"
            champA <- as.data.frame(cbind(as.data.frame(partA),combinedfit_champion)) 
            champA$Stream = show_variable
            champA$Error = ((champA[,show_variable] - champA$Predict)/champA[,show_variable])*100
            champA$APE = abs(champA$Error)
            trainfit_champion = exp(B_pred)
            Predict_champion = exp(B_forecast)
            combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
            names(combinedfit_champion) <- "Predict"
            champB <- as.data.frame(cbind(as.data.frame(partB), combinedfit_champion)) 
            champB$Stream = show_variable
            champB$Error = ((champB[,show_variable] - champB$Predict)/champB[,show_variable])*100
            champB$APE = abs(champB$Error)
            champion_model = list(arma = champion,
                                  LM_A = LM_A,
                                  LM_B = LM_B,
                                  MARIMA = Marima_fit,
                                  MARIMA_forecast = Marima_forecast,
                                  full_data_champion_A = champA,
                                  full_data_champion_B = champB,
                                  changepoints = changepoint_dates)
          } else{
            trainfit_champion = (A_pred)
            Predict_champion = (A_forecast)
            combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
            names(combinedfit_champion) = "Predict"
            champA <- as.data.frame(cbind(as.data.frame(partA),combinedfit_champion)) 
            champA$Stream = show_variable
            champA$Error = ((champA[,show_variable] - champA$Predict)/champA[,show_variable])*100
            champA$APE = abs(champA$Error)
            trainfit_champion = (B_pred)
            Predict_champion = (B_forecast)
            combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
            names(combinedfit_champion) <- "Predict"
            champB <- as.data.frame(cbind(as.data.frame(partB), combinedfit_champion)) 
            champB$Stream = show_variable
            champB$Error = ((champB[,show_variable] - champB$Predict)/champB[,show_variable])*100
            champB$APE = abs(champB$Error)
            champion_model = list(arma_order = champion,
                                  LM_A = LM_A,
                                  LM_B = LM_B,
                                  MARIMA = Marima_fit,
                                  MARIMA_forecast = Marima_forecast,
                                  champion_result_A = champA,
                                  champion_result_B = champB,
                                  changepoints = changepoint_dates)
          }
        }
      }
    }
  }
  
  return(champion_model)
}



#:::::::::::::::::::::::::: FUNCTIONS TO FIT THE CHAMPION MODEL

fit_champion_arima <- function(data,
                               show_variable = "SC3_Impressions",
                               agg_timescale = "Date",
                               log_transformation = 1,
                               OOS_start,
                               regressors,
                               changepoint_dates = NULL,
                               ARIMA_order,
                               ARIMA_seasonal){
  
  #' @description fits the best ARIMA model for the given data according to 
  #' conditions provided on regressors, arima orders and change-points
  #' 
  #' @param data R data-frame, typically an output from `create_aggregated_data()` function (see `data_prep_function.R`)
  #' @param show_variable denotes which variable to fit the model to, default is `SC3_Impressions`
  #' @param agg_timescale denotes the time-scale of the aggregated data - choices are `Week`, `Date`, `Hour` or `Half_Hr`
  #' @param log_transformation denotes whether to use log-transformation to the variable, choices are 0 (no) and 1 (yes), default is 1
  #' @param OOS_start the first date for the out-of-sample, format has to be `as.Date()`
  #' @param regressors vector of all regressors to be used in the model
  #' @param changepoint_dates dates where trend behavior changes, default is `NULL` (not to use piecewise linear trend)
  #' @param ARIMA_order of the form (p,d,q) to denote AR order, differencing order and MA order, respectively
  #' @param ARIMA_seasonal list with two compponents `order` [seasonal ARIMA order, of the form (P,D,Q)], and `period`
  #' 
  #' @return list containing the following:
  #' - ARIMA model object
  #' - R dataframe with all details and model fit and forecasts for train/test data
  #' - changepoint dates for piecewise linear function
  
  # check if changepoints correspond to regressors
  if (!"trend" %in% regressors & length(changepoint_dates) > 0){
    stop("'trend' has to be included in regressors to allow for piecewise linear trend")
  }
  if (length(ARIMA_order) != 3){
    stop("ARIMA_order should have exactly three components")
  }
  if (names(ARIMA_seasonal) != c("order","period")){
    stop("ARIMA_seasonal does not match allowed format")
  }
  if (length(ARIMA_seasonal$order) != 3 | length(ARIMA_seasonal$period) != 1){
    stop("ARIMA_seasonal does not match allowed format")
  }
  
  # prepare train and test data 
  if (agg_timescale == "Week"){
    train <- data %>% filter(Week < OOS_start)
    test <- data %>% filter(Week >= OOS_start)
  } else{
    train <- data %>% filter(Date < OOS_start)
    test <- data %>% filter(Date >= OOS_start)
  }
  train_periods = nrow(train)
  test_periods = nrow(test)
  
  # prepare xreg matrices for train/test for the arima models
  regressors_subset = regressors[! regressors == "trend"]
  reg_train <- train[,regressors_subset]
  reg_test <- test[,regressors_subset]
  if ("trend" %in% regressors){
    x1 = c(1:train_periods)
    reg_train <- cbind(reg_train,x1)
    colnames(reg_train)[ncol(reg_train)] = "trend"
    x2 = seq(train_periods+1,train_periods+test_periods,1)
    reg_test <- cbind(reg_test,x2)
    colnames(reg_test)[ncol(reg_test)] = "trend"
  }
  
  # adjust xreg matrices for piecewise linear trends
  if (is.null(changepoint_dates) == F){
    knotpoints = which(as.Date(unlist(train[,agg_timescale])) %in% changepoint_dates)
    for (i in 1:length(knotpoints)){
      reg_train <- cbind(reg_train,pmax(0,x1-knotpoints[i]))
      newname <- paste("trend_after",knotpoints[i],sep = "_")
      colnames(reg_train)[ncol(reg_train)] = newname
      reg_test <- cbind(reg_test,pmax(0,x2-knotpoints[i]))
      colnames(reg_test)[ncol(reg_test)] = newname
    }
  }
  
  # adjust xreg matrices as needed
  idx = which(colSums(reg_train)==0)
  if (length(idx)>0){
    reg_train <- reg_train[,-idx]
    reg_test <- reg_test[,-idx]
  }
  
  # prepare data based on log-transformation
  if (log_transformation == 1){
    our_series = log(as.numeric(unlist(train[,show_variable])) + 0.01)
  } else{
    our_series = as.numeric(unlist(train[,show_variable]))
  }
  
  # fit the best Arima model and get forecast
  fit_champion <- Arima(our_series,
                        order = ARIMA_order,
                        seasonal = ARIMA_seasonal,
                        include.drift = FALSE,
                        include.constant = TRUE,
                        lambda = 'auto',
                        xreg = as.matrix(reg_train),
                        method = 'CSS')
  forecasts_champion <- fit_champion %>% forecast::forecast(h = test_periods, xreg = as.matrix(reg_test))
  if (log_transformation == 1){
    trainfit_champion <- exp(forecasts_champion$fitted[1:nrow(train)])
    estimate_champion <- exp(forecasts_champion$mean[1:nrow(test)])
  } else {
    trainfit_champion <- forecasts_champion$fitted[1:nrow(train)]
    estimate_champion <- forecasts_champion$mean[1:nrow(test)]
  }
  combinedfit_champion <- as.data.frame(append(trainfit_champion, estimate_champion))
  names(combinedfit_champion) = "Predict"
  
  full_data_champion <- as.data.frame(cbind(as.data.frame(data),combinedfit_champion)) 
  full_data_champion$Stream = show_variable
  full_data_champion$Error = ((full_data_champion[,show_variable] - full_data_champion$Predict)/full_data_champion[,show_variable])*100
  full_data_champion$APE = abs(full_data_champion$Error)
  
  return(list(champion_model = fit_champion,
              champion_result = full_data_champion,
              changepoints = changepoint_dates)
  )
}

fit_champion_arima_BV <- function(data,
                                  show_names,
                                  show_variable = "SC3_Impressions",
                                  agg_timescale = "Week",
                                  log_transformation = 1,
                                  OOS_start,
                                  regressors,
                                  piecewsise_trend = FALSE,
                                  changepoint_dates = NULL,
                                  ARorder,
                                  MAorder){
  
  #' @description fits bivariate ARIMA model for the given data according to 
  #' conditions provided on regressors, parameters and change-points
  #' 
  #' @param data R data-frame, typically an output from `create_aggregated_data()` function (see `data_prep_function.R`)
  #' @param show_names denotes the names of the first and second show, in that order
  #' @param show_variable denotes which variable to fit the model to, default is `SC3_Impressions`
  #' @param agg_timescale denotes the time-scale of the aggregated data - choices are `Week` or `Date`, default is `Week`
  #' @param log_transformation denotes whether to use log-transformation to the variable, choices are 0 (no) and 1 (yes), default is 1
  #' @param OOS_start the first date for the out-of-sample, format has to be `as.Date()`
  #' @param regressors vector of all regressors to be used in the model
  #' @param piecewise_trend logical; whether to use piecewise linear trend, default is FALSE
  #' @param changepoint_dates dates where trend behavior changes, default is `NULL` (changepoints to be found using statistical means)
  #' @param ARorder denotes the auto-regressive order to use, must be greater than 1
  #' @param MAorder denotes the moving-average order to use, must be greater than 1
  #' 
  #' @return list containing the following:
  #' - linear model object from the first step for first show
  #' - linear model object from the first step for second show
  #' - multivariate ARIMA model object
  #' - multivariate ARIMA model forecasts
  #' - R dataframe with all details and model fit and forecasts for train/test data for first show
  #' - R dataframe with all details and model fit and forecasts for train/test data for first show
  #' - changepoint dates for piecewise linear function
  
  if (length(show_names) != 2){
    stop("this method works only for two consecutive shows")
  }
  if (min(ARorder,MAorder) < 2){
    stop("AR/MA orders have to be integers greater than 1")
  }
  if (piecewsise_trend == T & is.null(changepoint_dates) == T){
    stop("no changepoint date has been provided")
  }
  
  # create data files for the two shows separately
  agg_dataA <- subset(data,Show_Name == show_names[1])
  agg_dataB <- subset(data,Show_Name == show_names[2])
  
  # prepare train and test data 
  if (agg_timescale == "Week"){
    commondates <- intersect(agg_dataA$Week,agg_dataB$Week)
    idxA = which(agg_dataA$Week %in% commondates)
    idxB = which(agg_dataB$Week %in% commondates)
    partA <- agg_dataA[idxA,]
    trainA <- partA %>% filter(Week < OOS_start)
    testA <- partA %>% filter(Week >= OOS_start)
    partB <- agg_dataB[idxB,]
    trainB <- partB %>% filter(Week < OOS_start)
    testB <- partB %>% filter(Week >= OOS_start)
  } else{
    commondates <- intersect(agg_dataA$Date,agg_dataB$Date)
    idxA = which(agg_dataA$Date %in% commondates)
    idxB = which(agg_dataB$Date %in% commondates)
    partA <- agg_dataA[idxA,]
    trainA <- partA %>% filter(Date < OOS_start)
    testA <- partA %>% filter(Date >= OOS_start)
    partB <- agg_dataB[idxB,]
    trainB <- partB %>% filter(Date < OOS_start)
    testB <- partB %>% filter(Date >= OOS_start)
  }
  
  train_periods <- nrow(trainA)
  test_periods <- nrow(testA)
  
  # get the set of xreg
  regressors_subset = regressors[! regressors == "trend"]
  x1 = c(1:train_periods)
  reg_train <- cbind(trainA[,regressors_subset],x1)
  colnames(reg_train)[ncol(reg_train)] = "trend"
  x2 = seq(train_periods+1,train_periods+test_periods,1)
  reg_test <- cbind(testA[,regressors_subset],x2)
  colnames(reg_test)[ncol(reg_test)] = "trend"
  
  if (piecewsise_trend == TRUE){
    knotpoints = which(trainA[,agg_timescale] %in% changepoint_dates)
    for (i in 1:length(knotpoints)){
      reg_train <- cbind(reg_train,pmax(0,x1-knotpoints[i]))
      newname <- paste("trend_after",knotpoints[i],sep = "_")
      colnames(reg_train)[ncol(reg_train)] = newname
      reg_test <- cbind(reg_test,pmax(0,x2-knotpoints[i]))
      colnames(reg_test)[ncol(reg_test)] = newname
    }
  } 
  
  # update xreg matrices if needed
  idx = which(colSums(reg_train)==0)
  if (length(idx)>0){
    reg_train <- reg_train[,-idx]
    reg_test <- reg_test[,-idx]
  }
  
  # use log-transformation if needed
  if (log_transformation == 1){
    trainA_series = log(as.numeric(unlist(trainA[,show_variable])) + 0.01)
    testA_series = log(as.numeric(unlist(testA[,show_variable])) + 0.01)
    trainB_series = log(as.numeric(unlist(trainB[,show_variable])) + 0.01)
    testB_series = log(as.numeric(unlist(testB[,show_variable])) + 0.01)
  } else{
    trainA_series = as.numeric(unlist(trainA[,show_variable]))
    testA_series = as.numeric(unlist(testA[,show_variable]))
    trainB_series = as.numeric(unlist(trainB[,show_variable]))
    testB_series = as.numeric(unlist(testB[,show_variable]))
  }
  
  # combine the two shows data into one data.frame
  Y_train <- data.frame(A = trainA_series,B = trainB_series,reg_train)
  Y_test <- data.frame(A = testA_series,B = testB_series,reg_test)
  
  # OLS for the shows, use assigned regressors for A, add A as a regressor for B
  LM_A <- lm(formula(paste("A",paste(names(reg_train),collapse = "+"),sep = "~")),data = Y_train)
  LM_B <- lm(formula(paste("B",paste(c("A",names(reg_train)),collapse = "+"),sep = "~")),data = Y_train)
  
  # fit VARMA for the bivariate vectors for the residuals
  errors <- data.frame(e1 = LM_A$residuals,e2 = LM_B$residuals)
  
  i = ARorder; j = MAorder
  if (i+j>2){
    # fit the MARIMA model to the residuals
    Model1 <- define.model(kvar = ncol(errors), ar = c(0:(i-1)), ma = c(0:(j-1)))
    Marima_fit <- marima(ts(errors),Model1$ar.pattern,Model1$ma.pattern,penalty = 2)
    
    # get the forecasts from the marima model
    arima1 <- arima(errors[,1],order = c((i-1),0,(j-1)))
    f1 <- forecast::forecast(arima1,h = test_periods)
    arima2 <- arima(errors[,2],order = c((i-1),0,(j-1)))
    f2 <- forecast::forecast(arima2,h = test_periods)
    Marima_forecast <- arma.forecast(cbind(f1$mean,f2$mean),
                                     marima = Marima_fit,nstart = 0,nstep = test_periods,check = F)
    
    # add back the mean estimates from the previous linear models
    A_forecast = Marima_forecast$forecasts[1,] + predict(LM_A,Y_test)
    Bnewdata = Y_test
    Bnewdata$A = A_forecast
    B_forecast = Marima_forecast$forecasts[2,] + predict(LM_B,Bnewdata)
    A_pred <- rowSums(cbind(fitted(Marima_fit)[1,],fitted(LM_A)),na.rm = T)
    B_pred <- rowSums(cbind(fitted(Marima_fit)[2,],fitted(LM_B)),na.rm = T)
    
    if(log_transformation == 1){
      trainfit_champion = exp(A_pred)
      Predict_champion = exp(A_forecast)
      combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
      names(combinedfit_champion) = "Predict"
      champA <- as.data.frame(cbind(as.data.frame(partA),combinedfit_champion)) 
      champA$Stream = show_variable
      champA$Error = ((champA[,show_variable] - champA$Predict)/champA[,show_variable])*100
      champA$APE = abs(champA$Error)
      trainfit_champion = exp(B_pred)
      Predict_champion = exp(B_forecast)
      combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
      names(combinedfit_champion) <- "Predict"
      champB <- as.data.frame(cbind(as.data.frame(partB), combinedfit_champion)) 
      champB$Stream = show_variable
      champB$Error = ((champB[,show_variable] - champB$Predict)/champB[,show_variable])*100
      champB$APE = abs(champB$Error)
      champion_model = list(LM_A = LM_A,
                            LM_B = LM_B,
                            MARIMA = Marima_fit,
                            MARIMA_forecast = Marima_forecast,
                            full_data_champion_A = champA,
                            full_data_champion_B = champB,
                            changepoints = changepoint_dates)
    } else{
      trainfit_champion = (A_pred)
      Predict_champion = (A_forecast)
      combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
      names(combinedfit_champion) = "Predict"
      champA <- as.data.frame(cbind(as.data.frame(partA),combinedfit_champion)) 
      champA$Stream = show_variable
      champA$Error = ((champA[,show_variable] - champA$Predict)/champA[,show_variable])*100
      champA$APE = abs(champA$Error)
      trainfit_champion = (B_pred)
      Predict_champion = (B_forecast)
      combinedfit_champion <- as.data.frame(append(trainfit_champion, Predict_champion))  
      names(combinedfit_champion) <- "Predict"
      champB <- as.data.frame(cbind(as.data.frame(partB), combinedfit_champion)) 
      champB$Stream = show_variable
      champB$Error = ((champB[,show_variable] - champB$Predict)/champB[,show_variable])*100
      champB$APE = abs(champB$Error)
      champion_model = list(LM_A = LM_A,
                            LM_B = LM_B,
                            MARIMA = Marima_fit,
                            MARIMA_forecast = Marima_forecast,
                            full_data_champion_A = champA,
                            full_data_champion_B = champB,
                            changepoints = changepoint_dates)
    }
  }
  
  return(champion_model)
}
