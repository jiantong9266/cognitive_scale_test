#  #######################################################################
#       File-Name:      MAIN.R
#       Purpose:        Run testing for cortex 5
#       Dependencies:   parameters.R
#                       forecasting_functions.R
#  #######################################################################

set.seed(1221)

# source all dependent files
source(here::here("OXYG","parameters.R"))
source(here::here("OXYG","forecasting_functions.R"))

options(warn = -1,digits = 3,error = NULL,verbose = F)

# read data from S3 --- directly read the data needed for modeling
# set-up spark connection
#SparkR::sparkR.session()
#sc <- sparklyr::spark_connect(method = "databricks")
#df_source <- sparklyr::spark_read_csv(sc, name = inputFileName, path = inputFilePath)

# read data from local
df_source <- read_csv(here::here("OXYG","OXYG_TEST_DATA.csv"))
inputFile <- collect(df_source)

# formatting the data
inputFile$Week <- as.Date(inputFile$Week,format = "%Y-%m-%d")
inputFile$Date <- as.Date(inputFile$Date,format = "%Y-%m-%d")
inputFile <- inputFile[order(inputFile$Week,inputFile$Date),]

OOS_start <- subset(inputFile,Source == "Compass")$Week[1]
all_models <- readxl::read_xlsx(here::here("OXYG","OXYG_model_details.xlsx"))
version_models <- subset(all_models,Model_version == current_model & Data_version == current_data)
column_ids = c("Network","Demo","Model_type","Data_version","Model_version",
               "HH_NT_Daypart","program_type","Show_Name")

#::::::::::::::: Get all results for all shows

all_outputs = list()

for (showidx in 1:nrow(version_models)){
  data_model = version_models[showidx,]
  idx = which(apply(inputFile[,column_ids],1,
                    function(x) identical(as.character(x),as.character(data_model[,column_ids]))) == T)
  case_data = inputFile[idx,]
  
  # get the set of regressors
  regressor_part <- data_model %>% select(starts_with("REG_"))
  regressors <- names(regressor_part)[which(regressor_part == 1)]
  regressors <- regressors %>% str_replace("REG_","")
  
  # get the arima and seasonal orders
  arima_pdq <- as.numeric(data_model[,c("arima_P","arima_D","arima_Q")])
  arima_seasonal <- list(order = as.numeric(data_model[,c("seasonal_p","seasonal_d","seasonal_q")]),
                         period = data_model$arima_period)
  
  # get the changepoint dates
  if (is.na(data_model$changepoints) == T){
    cp_dates <- NULL
  } else {
    cp_dates <- as.Date(unlist(strsplit(data_model$changepoints,",")))
  }
  
  # get the results from fitting the best model
  results <- fit_champion_arima(data = case_data,
                                show_variable = "SC3_Impressions",
                                agg_timescale = data_model$Time_ID,
                                log_transformation = data_model$log_transformation,
                                OOS_start = OOS_start,
                                regressors = regressors,
                                changepoint_dates = cp_dates,
                                ARIMA_order = arima_pdq,
                                ARIMA_seasonal = arima_seasonal)
  all_outputs[[showidx]] = results$champion_result %>%
    mutate(
      STREAM = data_model$Stream,
      LAST_UPDATE = Sys.Date(), 
      QTRYEAR = paste0(Broadcast_Year, "-", Cable_Qtr)
    ) %>%
    rename(
      BROADCAST_WEEK = Week,
      BROADCAST_DATE = Date,
      ACTUAL = SC3_Impressions
    ) 
  all_outputs[[showidx]] = setNames(all_outputs[[showidx]], toupper(names(all_outputs[[showidx]])))
}

#::::::::::::::::::::: Prepare the output

s3_output <- do.call(rbind,lapply(all_outputs,function(x) return(x[,output_cols])))

head(s3_output)

# Plugin: store the data into csv
write.csv(s3_output,here::here("OXYG","OXYG_TEST_OUTPUTS.csv"))

# Plugin: store the data into RDS or S3 ???
