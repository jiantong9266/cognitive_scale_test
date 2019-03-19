# :::::::::MODEL DETAILS

current_model = "V1.1_190205"
current_data = "19W018"
inputFilePath = "s3a://cs-demo-bucket/OXYG_TEST_DATA.csv"
inputFileName = "OXYG_DATA"


# ::::::::: VARIABLES TO KEEP IN THE OUTPUT DATA
output_cols <- c("NETWORK","DEMO","MODEL_TYPE","LAST_UPDATE","DATA_VERSION","MODEL_VERSION","STREAM","TIME_ID","SHOW_NAME",
                 "PROGRAM_TYPE","HH_NT_DAYPART","BROADCAST_YEAR","CABLE_QTR","QTRYEAR","BROADCAST_WEEK","BROADCAST_DATE",
                 "HOURS","HALF_HR","ACTUAL","PREDICT")
