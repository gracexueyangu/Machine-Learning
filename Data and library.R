## this r file is used to import needed library and variables for shiny.

######## import needed library
library(readxl)
library(readr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(gridExtra)
library(ggrepel)
library(ggalt)
library(maps)
library(ggmap)
library(zipcode)

######## import data
data = read.csv("data_for_shiny.csv")
name_list = read.csv("name_list.csv")
data_class = read_excel("classification var.xlsx")
names(name_list) = c("COL", "ALIAS")
name_list$ALIAS = as.character(name_list$ALIAS)
name_list$COL = as.character(name_list$COL)



######## Classify variables

## Type: Binary/ Numeric/ Categorical
var_binary = unname(unlist(na.omit(data_class[,1])))
var_numeric = unname(unlist(na.omit(data_class[,2])))
var_categorical = unname(unlist(na.omit(data_class[,3])))


## Cateogry: Policy/ Property Safeness/ Property Condition/ Client
var_policy = unname(unlist(na.omit(data_class[,4])))
var_prosafeness = unname(unlist(na.omit(data_class[,5])))
var_procondition = unname(unlist(na.omit(data_class[,6])))
var_client = unname(unlist(na.omit(data_class[,7])))

## List for distribution graphs part
var_binary_dis = 
c("CLM_DISC_SUR_IND","BSMNT_PRESNC_IND","SWIM_POOL_IND","SPRKLR_DISC_IND",
  "NON_SMKR_DISC_IND","CNTRL_BRG_ALRM_IND","ELECTR_SYS_RPLCMT_DISC_IND",
  "HVAC_SYS_RPLCMT_DISC_IND","RENOV_DISC_IND_PLMBNG","HM_PRTCTN_PRVNTN_LOSS_IND",
  "FORTFY_HM_IND","IRON_BAR_DISC_IND","GOOD_PAYER_DISC_IND","CLM_FREE_DISC_IND","EPLCY_IND",
  "HM_UMB_DISC_IND","HM_BUS_DISC_IND","HM_SPLTY_BOAT_WTRCRFT_DISC_IND",
  "AH_DISC_IND","PREFRD_PYMT_PLAN_DISC_IND","HM_LIFE_DISC_IND","ET_EFT_DISC_IND")

var_numeric_dis = 
c("PREM","DED_AMT_1","HEAD_HH_AGE","CNSTRCT_YR","LVNG_AREA_SQRFT","TTL_BTHS_HH",
  "ROOF_AGE","AGE_HM","COA_Limit","NUM_DISC")

var_categorical_dis_1 = 
c("PKG_CD","CNSTRCT_CLS_DESC","COE_Limit","ERC_Limit")

var_categorical_dis_2 = 
c("FIRE_CS_CD","PRD_TYP_DESC","DWL_STYL_DESC","FPPS_OCCUP_CD","CNSTRCT_MATL_DESC",
  "PRIM_HEAT_FUEL_DESC","ROOF_TYP_DESC","PRTCTN_CLS_DESC","FIRELN_ACCS_CD","FIRELN_FUEL_CD",
  "FIRELN_MTCH_DESC","FIRELN_SLOPE_CD","GRG_DESC")

var_binacat_dis = 
c("CLM_DISC_SUR_IND","BSMNT_PRESNC_IND","SWIM_POOL_IND","SPRKLR_DISC_IND",
  "NON_SMKR_DISC_IND","CNTRL_BRG_ALRM_IND","ELECTR_SYS_RPLCMT_DISC_IND",
  "HVAC_SYS_RPLCMT_DISC_IND","RENOV_DISC_IND_PLMBNG","HM_PRTCTN_PRVNTN_LOSS_IND",
  "FORTFY_HM_IND","IRON_BAR_DISC_IND","GOOD_PAYER_DISC_IND","CLM_FREE_DISC_IND","EPLCY_IND",
  "HM_UMB_DISC_IND","HM_BUS_DISC_IND","HM_SPLTY_BOAT_WTRCRFT_DISC_IND",
  "AH_DISC_IND","PREFRD_PYMT_PLAN_DISC_IND","HM_LIFE_DISC_IND","ET_EFT_DISC_IND",
  "PKG_CD","CNSTRCT_CLS_DESC","COE_Limit","ERC_Limit",
  "FIRE_CS_CD","PRD_TYP_DESC","DWL_STYL_DESC","FPPS_OCCUP_CD","CNSTRCT_MATL_DESC",
  "PRIM_HEAT_FUEL_DESC","ROOF_TYP_DESC","PRTCTN_CLS_DESC","FIRELN_ACCS_CD","FIRELN_FUEL_CD",
  "FIRELN_MTCH_DESC","FIRELN_SLOPE_CD","DED_AMT_3","GRG_DESC")

var_policy_dis = 
c('Package Code' = 'PKG_CD',
  'First Deductible Amount' = 'DED_AMT_1',
  'Third Deductible Amount' = 'DED_AMT_3',
  'Product Type' = 'PRD_TYP_DESC',
  'Premium Chargeable' = 'PREM',
  'Claims Surcharge Indicator' = 'CLM_DISC_SUR_IND',
  'Non Smoker Discount' = 'NON_SMKR_DISC_IND',
  'Electrical System Replacement Discount' = 'ELECTR_SYS_RPLCMT_DISC_IND',
  'HVAC System Replacement Discount' = 'HVAC_SYS_RPLCMT_DISC_IND',
  'Renovation Discount - Plumbing' = 'RENOV_DISC_IND_PLMBNG',
  'Home Protection and Loss Prevention Discount' = 'HM_PRTCTN_PRVNTN_LOSS_IND',
  'Fortified Home Discount' = 'FORTFY_HM_IND',
  'Iron Home Discount' = 'IRON_BAR_DISC_IND',
  'Good Payer Discount' = 'GOOD_PAYER_DISC_IND',
  'Claim Free Discount' = 'CLM_FREE_DISC_IND',
  'Electronic Policy Indicator' = 'EPLCY_IND',
  'Home/Umbrella Discount' = 'HM_UMB_DISC_IND',
  'Home/Business Discount' = 'HM_BUS_DISC_IND',
  'Home Protection and Loss Prevention Discount' = 'HM_SPLTY_BOAT_WTRCRFT_DISC_IND',
  'Home/Auto Discount' = 'AH_DISC_IND',
  'Preferred Payment Plan Discount' = 'PREFRD_PYMT_PLAN_DISC_IND',
  'EFT Discount' = 'ET_EFT_DISC_IND',
  'Home/Life Discount' = 'HM_LIFE_DISC_IND',
  'Dwelling Limit' = 'COA_Limit',
  'Liability Limit' = 'COE_Limit',
  'Number of Discount' = 'NUM_DISC',
  'Cost Limit' = 'ERC_Limit')

var_prosafeness_dis = 
c('Fire Credit Score' = 'FIRE_CS_CD',
  'Fire Safe Score' = 'PRTCTN_CLS_DESC',
  'Access Score' = 'FIRELN_ACCS_CD',
  'Fuel Score' = 'FIRELN_FUEL_CD',
  'Fireline Code Matching' = 'FIRELN_MTCH_DESC',
  'Slope Score' = 'FIRELN_SLOPE_CD')

var_procondition_dis = 
  c('Construction Year' = 'CNSTRCT_YR',
    'Garage Code' = 'GRG_DESC',
    'Style of Dwelling' = 'DWL_STYL_DESC',
    'Usage Type of Property' = 'FPPS_OCCUP_CD',
    'Building Material' = 'CNSTRCT_MATL_DESC',
    'Material of Dwelling' = 'CNSTRCT_CLS_DESC',
    'Square Footage of Living Area' = 'LVNG_AREA_SQRFT',
    'Type of Fuel' = 'PRIM_HEAT_FUEL_DESC',
    'Material of Dwelling Roof' = 'ROOF_TYP_DESC',
    'Basement Indicator' = 'BSMNT_PRESNC_IND',
    'Quantity of Baths' = 'TTL_BTHS_HH',
    'Sprinkler Indicator' = 'SPRKLR_DISC_IND',
    'Indoor Swimming Pool Indicator' = 'SWIM_POOL_IND',
    'Age of Roof' = 'ROOF_AGE',
    'Age of Property' = 'AGE_HM',
    'Central Burglar Alarm Indicator' = 'CNTRL_BRG_ALRM_IND')

var_client_dis = 
  c('Age of Household Head' = 'HEAD_HH_AGE')

# for comparison part
var_binary_com =
  c("CLM_DISC_SUR_IND","CNSTRCT_CLS_DESC","BSMNT_PRESNC_IND","SWIM_POOL_IND",
    "NON_SMKR_DISC_IND","CNTRL_BRG_ALRM_IND","ELECTR_SYS_RPLCMT_DISC_IND",
    "HVAC_SYS_RPLCMT_DISC_IND","RENOV_DISC_IND_PLMBNG","HM_PRTCTN_PRVNTN_LOSS_IND",
    "FORTFY_HM_IND","GOOD_PAYER_DISC_IND","CLM_FREE_DISC_IND","EPLCY_IND",
    "HM_UMB_DISC_IND","HM_BUS_DISC_IND","HM_SPLTY_BOAT_WTRCRFT_DISC_IND",
    "AH_DISC_IND","PREFRD_PYMT_PLAN_DISC_IND","HM_LIFE_DISC_IND")

var_numeric_com = 
  c("PREM","HEAD_HH_AGE","CNSTRCT_YR","LVNG_AREA_SQRFT","TTL_BTHS_HH",
    "ROOF_AGE","AGE_HM","COA_Limit","COE_Limit","ERC_Limit","NUM_DISC","DED_AMT_1","DED_AMT_3")

var_categorical_com =
  c("PKG_CD","PRD_TYP_DESC","GRG_DESC","FPPS_OCCUP_CD","FIRELN_ACCS_CD",
    "FIRELN_FUEL_CD","PRIM_HEAT_FUEL_DESC","FIRELN_MTCH_DESC","FIRELN_SLOPE_CD",
    "FIRE_CS_CD","DWL_STYL_DESC","CNSTRCT_MATL_DESC","ROOF_TYP_DESC",
    "PRTCTN_CLS_DESC")

var_policy_com = 
  c("PREM","CLM_DISC_SUR_IND","NON_SMKR_DISC_IND","ELECTR_SYS_RPLCMT_DISC_IND","HVAC_SYS_RPLCMT_DISC_IND",
    "RENOV_DISC_IND_PLMBNG","HM_PRTCTN_PRVNTN_LOSS_IND","FORTFY_HM_IND",
    "GOOD_PAYER_DISC_IND","CLM_FREE_DISC_IND","EPLCY_IND",
    "HM_UMB_DISC_IND","HM_BUS_DISC_IND","HM_SPLTY_BOAT_WTRCRFT_DISC_IND",
    "AH_DISC_IND","PREFRD_PYMT_PLAN_DISC_IND","HM_LIFE_DISC_IND",
    "PKG_CD","DED_AMT_1","DED_AMT_3","PRD_TYP_DESC","COA_Limit","COE_Limit","ERC_Limit","NUM_DISC")

var_prosafeness_com = 
  c("FIRE_CS_CD","PRTCTN_CLS_DESC","FIRELN_ACCS_CD","FIRELN_FUEL_CD",
    "FIRELN_MTCH_DESC","FIRELN_SLOPE_CD")

var_procondition_com = 
  c("CNSTRCT_YR","GRG_DESC","DWL_STYL_DESC","FPPS_OCCUP_CD",
    "CNSTRCT_MATL_DESC","CNSTRCT_CLS_DESC","LVNG_AREA_SQRFT",
    "PRIM_HEAT_FUEL_DESC","ROOF_TYP_DESC","BSMNT_PRESNC_IND",
    "TTL_BTHS_HH","SWIM_POOL_IND","ROOF_AGE",
    "AGE_HM","CNTRL_BRG_ALRM_IND")

var_client_com = 
  c("HEAD_HH_AGE")

# Divide data into groups based on var category & type
X_data_policy = data[,var_policy_com]
X_data_prosafeness = data[,var_prosafeness_com]
X_data_procondition = data[,var_procondition_com]
X_data_client = as.data.frame(data[,var_client_com])
X_data_binary = data[,var_binary_com]
X_data_numeric = data[,var_numeric_com]
X_data_categorical = data[,var_categorical_com]

##change col name into alias for each subgroup dataframe
df = as.data.frame(colnames(X_data_policy))
df$`colnames(X_data_policy)` = as.character(df$`colnames(X_data_policy)`)
df = left_join(df, name_list, by=c('colnames(X_data_policy)'='COL'))
names(X_data_policy) = df[,2]

df = as.data.frame(colnames(X_data_prosafeness))
df$`colnames(X_data_prosafeness)` = as.character(df$`colnames(X_data_prosafeness)`)
df = left_join(df, name_list, by=c('colnames(X_data_prosafeness)'='COL'))
names(X_data_prosafeness) = df[,2]

df = as.data.frame(colnames(X_data_procondition))
df$`colnames(X_data_procondition)` = as.character(df$`colnames(X_data_procondition)`)
df = left_join(df, name_list, by=c('colnames(X_data_procondition)'='COL'))
names(X_data_procondition) = df[,2]

df = as.data.frame(colnames(X_data_client))
df$`colnames(X_data_client)` = as.character(df$`colnames(X_data_client)`)
df = left_join(df, name_list, by=c('colnames(X_data_client)'='COL'))
names(X_data_client) = df[,2]

df = as.data.frame(colnames(X_data_binary))
df$`colnames(X_data_binary)` = as.character(df$`colnames(X_data_binary)`)
df = left_join(df, name_list, by=c('colnames(X_data_binary)'='COL'))
names(X_data_binary) = df[,2]

df = as.data.frame(colnames(X_data_numeric))
df$`colnames(X_data_numeric)` = as.character(df$`colnames(X_data_numeric)`)
df = left_join(df, name_list, by=c('colnames(X_data_numeric)'='COL'))
names(X_data_numeric) = df[,2]

df = as.data.frame(colnames(X_data_categorical))
df$`colnames(X_data_categorical)` = as.character(df$`colnames(X_data_categorical)`)
df = left_join(df, name_list, by=c('colnames(X_data_categorical)'='COL'))
names(X_data_categorical) = df[,2]

names(X_data_client) = 'Age of Household Head'


## for map part

# read files about zipcode
data(zipcode)
zip = zipcode
farm = read.csv("data_cleaned_02.csv") # version of previous file

# get zipcode for county
county_zip = read.csv('county_zip.csv') # reading an external file to have zip codes for city/ county
cnames = read.csv('cnames.csv') #lat and long for sub regions

############
map = map_data("county")  #take county data
theme_set(theme_bw())

map1 = map[map$region == "arizona",] # splitting county into Arizona

colnames(zip)[1] = "PROP_ZIP_CD"  # changing column in zip table
zip$PROP_ZIP_CD = as.integer(zip$PROP_ZIP_CD)

farmMap = left_join(zip,farm,by ="PROP_ZIP_CD")  # left join Zip with farmers' data to add 
# zip data in farmer's table

farmMap11 =farmMap[farmMap$state == "AZ",]

#Left Join with County_zip file to add county name
farmMap9 = left_join(farmMap11,county_zip[,c(1,3)], by = c('PROP_ZIP_CD' = 'Zip.Code'))



