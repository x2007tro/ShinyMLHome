##
# Manual training for xgbtree model
##

# ##
# # Saving data
# ##
# full_testdata <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_test_predictors")
# full_predictors <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_train_predictors01")
# full_target <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_train_target")
# tgt_map <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "input02_target_map")
# save(full_predictors, full_target, full_testdata, tgt_map, all_models, file = "hcdr_full04.RData")

##
# first things first, parameters
mlh_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/Data Science/ShinyMLHome/")
n_val_size <- 0.1
dataset_nm <- c("hcdr_sample","hcdr_full03")[2]
# rmv_fs <- c('SK_ID_CURR', 'NAME_TYPE_SUITE', 'HOUR_APPR_PROCESS_START', 'FLAG_MOBIL', 
#             'FLAG_CONT_MOBILE', 'FLAG_DOCUMENT_2', 'FLAG_DOCUMENT_4', 
#             'FLAG_DOCUMENT_7', 'FLAG_DOCUMENT_10', 
#             'FLAG_DOCUMENT_12', 'FLAG_DOCUMENT_14', 'FLAG_DOCUMENT_15', 
#             'FLAG_DOCUMENT_17', 'FLAG_DOCUMENT_19', 
#             'FLAG_DOCUMENT_20', 'FLAG_DOCUMENT_21', 'appl_process_weekday')

rmv_fs <- c('SK_ID_CURR')

run_test <- FALSE
smote_flag <- FALSE

##
# Load required source files and data
source(paste0(mlh_dir, "global.R"))
setwd(proj_dir)
load(paste0(proj_dir, dataset_nm, ".RData"))

AddFeatures <- function(dataset, do){
  
  if(do == TRUE){
    
    res_tmp <- dataset %>% 
      mutate(
        eqw_avg_bb_avg_dpd1 = pmax(eqw_avg_bb_avg_dpd, 0),
        eqw_avg_bb_dpd_occur_pct1 = pmax(eqw_avg_bb_dpd_occur_pct, 0),
        eqw_avg_bb_mxdpd1 = pmax(eqw_avg_bb_mxdpd, 0),
        eqw_avg_bb_mxdpd_duration1 = pmax(eqw_avg_bb_mxdpd_duration, 0),
        eqw_avg_ccf_avg_dpd_serious1 = pmax(eqw_avg_ccf_avg_dpd_serious, 0),
        eqw_avg_ccf_dpd_occur_pct1 = pmax(eqw_avg_ccf_dpd_occur_pct, 0),
        eqw_avg_ccf_mxdpd1 = pmax(eqw_avg_ccf_mxdpd, 0),
        eqw_avg_ccf_mxdpd_serious1 = pmax(eqw_avg_ccf_mxdpd_serious, 0),
        eqw_avg_ccf_mxdpd_duration1 = pmax(eqw_avg_ccf_mxdpd_duration, 0),
        eqw_avg_ip_avg_dpd1 = pmax(eqw_avg_ip_avg_dpd, 0),
        eqw_avg_ip_dpd_occur_pct1 = pmax(eqw_avg_ip_dpd_occur_pct, 0),
        eqw_avg_ip_mxdpd1 = pmax(eqw_avg_ip_mxdpd, 0),
        eqw_avg_ip_mxdpd_duration1 = pmax(eqw_avg_ip_mxdpd_duration, 0),
        eqw_avg_ip_avg_apd1 = pmax(eqw_avg_ip_avg_apd, 0),
        eqw_avg_ip_avg_apd_as_pct1 = pmax(eqw_avg_ip_avg_apd_as_pct, 0),
        eqw_avg_ip_apd_occur_pct1 = pmax(eqw_avg_ip_apd_occur_pct, 0),
        eqw_avg_ip_mxapd1 = pmax(eqw_avg_ip_mxapd, 0),
        eqw_avg_ip_mxapd_duration1 = pmax(eqw_avg_ip_mxapd_duration, 0),
        eqw_avg_pcb_avg_dpd1 = pmax(eqw_avg_pcb_avg_dpd, 0),
        eqw_avg_pcb_avg_dpd_serious1 = pmax(eqw_avg_pcb_avg_dpd_serious, 0),
        eqw_avg_pcb_dpd_occur_pct1 = pmax(eqw_avg_pcb_dpd_occur_pct, 0),
        eqw_avg_pcb_mxdpd1 = pmax(eqw_avg_pcb_mxdpd, 0),
        eqw_avg_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_avg_pcb_mxdpd_avg_comp_pct, 0),
        eqw_avg_pcb_mxdpd_serious1 = pmax(eqw_avg_pcb_mxdpd_serious, 0),
        eqw_avg_pcb_mxdpd_duration1 = pmax(eqw_avg_pcb_mxdpd_duration, 0)
      ) %>%
      # transfer the dpd happen time into weight step 1
      # lower number means either no dpd or no data, thus better than positive number
      mutate(
        eqw_avg_bb_mxdpd_cap = ifelse(eqw_avg_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_bb_mxdpd_earliest_occur + eqw_avg_bb_mxdpd_latest_occur)/eqw_avg_bb_mxdpd_duration1)),
        eqw_avg_ccf_mxdpd_cap = ifelse(eqw_avg_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_ccf_mxdpd_earliest_occur + eqw_avg_ccf_mxdpd_latest_occur)/eqw_avg_ccf_mxdpd_duration1)),
        eqw_avg_ip_mxdpd_cap = ifelse(eqw_avg_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_ip_mxdpd_earliest_occur + eqw_avg_ip_mxdpd_latest_occur)/eqw_avg_ip_mxdpd_duration1)),
        eqw_avg_ip_mxapd_cap = ifelse(eqw_avg_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_avg_ip_mxapd_earliest_occur + eqw_avg_ip_mxapd_latest_occur)/eqw_avg_ip_mxapd_duration1)),
        eqw_avg_pcb_mxdpd_cap = ifelse(eqw_avg_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_pcb_mxdpd_earliest_occur + eqw_avg_pcb_mxdpd_latest_occur)/eqw_avg_pcb_mxdpd_duration1))
      ) %>% 
      # transfer the dpd happen time into weight step 2
      mutate(
        eqw_avg_bb_mxdpd_wgt = eqw_avg_bb_mxdpd_cap/max(eqw_avg_bb_mxdpd_cap, na.rm = TRUE),
        eqw_avg_ccf_mxdpd_wgt = eqw_avg_ccf_mxdpd_cap/max(eqw_avg_ccf_mxdpd_cap, na.rm = TRUE),
        eqw_avg_ip_mxdpd_wgt = eqw_avg_ip_mxdpd_cap/max(eqw_avg_ip_mxdpd_cap, na.rm = TRUE),
        eqw_avg_ip_mxapd_wgt = eqw_avg_ip_mxapd_cap/max(eqw_avg_ip_mxapd_cap, na.rm = TRUE),
        eqw_avg_pcb_mxdpd_wgt = eqw_avg_pcb_mxdpd_cap/max(eqw_avg_pcb_mxdpd_cap, na.rm = TRUE)
      ) %>% 
      # calculate weighted mxdpd related
      mutate(
        eqw_avg_bb_mxdpd2=eqw_avg_bb_mxdpd1*eqw_avg_bb_mxdpd_wgt,
        eqw_avg_ccf_mxdpd2=eqw_avg_ccf_mxdpd1*eqw_avg_ccf_mxdpd_wgt,
        eqw_avg_ccf_mxdpd_serious2=eqw_avg_ccf_mxdpd_serious1*eqw_avg_ccf_mxdpd_wgt,
        eqw_avg_ip_mxdpd2=eqw_avg_ip_mxdpd1*eqw_avg_ip_mxdpd_wgt,
        eqw_avg_ip_mxapd2=eqw_avg_ip_mxapd1*eqw_avg_ip_mxapd_wgt,
        eqw_avg_pcb_mxdpd2=eqw_avg_pcb_mxdpd1*eqw_avg_pcb_mxdpd_wgt,
        eqw_avg_pcb_mxdpd_avg_comp_pct2=eqw_avg_pcb_mxdpd_avg_comp_pct1*eqw_avg_pcb_mxdpd_wgt,
        eqw_avg_pcb_mxdpd_serious2=eqw_avg_pcb_mxdpd_serious1*eqw_avg_pcb_mxdpd_wgt
      ) %>% 
      # calculate weight for each dpd related
      mutate(
        eqw_avg_bb_avg_dpd1_wgt =  eqw_avg_bb_avg_dpd1/max(eqw_avg_bb_avg_dpd1, na.rm = TRUE),
        eqw_avg_bb_dpd_occur_pct1_wgt =  eqw_avg_bb_dpd_occur_pct1/max(eqw_avg_bb_dpd_occur_pct1, na.rm = TRUE),
        eqw_avg_bb_mxdpd2_wgt =  eqw_avg_bb_mxdpd2/max(eqw_avg_bb_mxdpd2, na.rm = TRUE),
        eqw_avg_ccf_avg_dpd_serious1_wgt =  eqw_avg_ccf_avg_dpd_serious1/max(eqw_avg_ccf_avg_dpd_serious1, na.rm = TRUE),
        eqw_avg_ccf_dpd_occur_pct1_wgt =  eqw_avg_ccf_dpd_occur_pct1/max(eqw_avg_ccf_dpd_occur_pct1, na.rm = TRUE),
        eqw_avg_ccf_mxdpd2_wgt =  eqw_avg_ccf_mxdpd2/max(eqw_avg_ccf_mxdpd2, na.rm = TRUE),
        eqw_avg_ccf_mxdpd_serious2_wgt =  eqw_avg_ccf_mxdpd_serious2/max(eqw_avg_ccf_mxdpd_serious2, na.rm = TRUE),
        eqw_avg_ip_avg_dpd1_wgt =  eqw_avg_ip_avg_dpd1/max(eqw_avg_ip_avg_dpd1, na.rm = TRUE),
        eqw_avg_ip_dpd_occur_pct1_wgt =  eqw_avg_ip_dpd_occur_pct1/max(eqw_avg_ip_dpd_occur_pct1, na.rm = TRUE),
        eqw_avg_ip_mxdpd2_wgt =  eqw_avg_ip_mxdpd2/max(eqw_avg_ip_mxdpd2, na.rm = TRUE),
        eqw_avg_ip_avg_apd1_wgt = eqw_avg_ip_avg_apd1/max(eqw_avg_ip_avg_apd1, na.rm = TRUE),
        eqw_avg_ip_avg_apd_as_pct1_wgt = eqw_avg_ip_avg_apd_as_pct1/max(eqw_avg_ip_avg_apd_as_pct1, na.rm = TRUE),
        eqw_avg_ip_apd_occur_pct1_wgt = eqw_avg_ip_apd_occur_pct1/max(eqw_avg_ip_apd_occur_pct1, na.rm = TRUE),
        eqw_avg_ip_mxapd2_wgt =  eqw_avg_ip_mxapd2/max(eqw_avg_ip_mxapd2, na.rm = TRUE),
        eqw_avg_pcb_avg_dpd1_wgt =  eqw_avg_pcb_avg_dpd1/max(eqw_avg_pcb_avg_dpd1, na.rm = TRUE),
        eqw_avg_pcb_avg_dpd_serious1_wgt =  eqw_avg_pcb_avg_dpd_serious1/max(eqw_avg_pcb_avg_dpd_serious1, na.rm = TRUE),
        eqw_avg_pcb_dpd_occur_pct1_wgt =  eqw_avg_pcb_dpd_occur_pct1/max(eqw_avg_pcb_dpd_occur_pct1, na.rm = TRUE),
        eqw_avg_pcb_mxdpd2_wgt =  eqw_avg_pcb_mxdpd2/max(eqw_avg_pcb_mxdpd2, na.rm = TRUE),
        eqw_avg_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_avg_pcb_mxdpd_avg_comp_pct2/max(eqw_avg_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
        eqw_avg_pcb_mxdpd_serious2_wgt =  eqw_avg_pcb_mxdpd_serious2/max(eqw_avg_pcb_mxdpd_serious2, na.rm = TRUE)
      ) %>% 
      # calculate penalty for each dpd related
      mutate(
        eqw_avg_bb_avg_dpd1_pen = 10 * eqw_avg_bb_avg_dpd1_wgt,
        eqw_avg_bb_dpd_occur_pct1_pen = 10 * eqw_avg_bb_dpd_occur_pct1_wgt,
        eqw_avg_bb_mxdpd2_pen = 10 * eqw_avg_bb_mxdpd2_wgt,
        eqw_avg_ccf_avg_dpd_serious1_pen = 10 * eqw_avg_ccf_avg_dpd_serious1_wgt,
        eqw_avg_ccf_dpd_occur_pct1_pen = 10 * eqw_avg_ccf_dpd_occur_pct1_wgt,
        eqw_avg_ccf_mxdpd2_pen = 10 * eqw_avg_ccf_mxdpd2_wgt,
        eqw_avg_ccf_mxdpd_serious2_pen = 10 * eqw_avg_ccf_mxdpd_serious2_wgt,
        eqw_avg_ip_avg_dpd1_pen = 10 * eqw_avg_ip_avg_dpd1_wgt,
        eqw_avg_ip_dpd_occur_pct1_pen = 10 * eqw_avg_ip_dpd_occur_pct1_wgt,
        eqw_avg_ip_mxdpd2_pen = 10 * eqw_avg_ip_mxdpd2_wgt,
        eqw_avg_ip_avg_apd1_pen = 10 * eqw_avg_ip_avg_apd1_wgt,
        eqw_avg_ip_avg_apd_as_pct1_pen = 10 * eqw_avg_ip_avg_apd_as_pct1_wgt,
        eqw_avg_ip_apd_occur_pct1_pen = 10 * eqw_avg_ip_apd_occur_pct1_wgt,
        eqw_avg_ip_mxapd2_pen = 10 * eqw_avg_ip_mxapd2_wgt,
        eqw_avg_pcb_avg_dpd1_pen = 10 * eqw_avg_pcb_avg_dpd1_wgt,
        eqw_avg_pcb_avg_dpd_serious1_pen = 10 * eqw_avg_pcb_avg_dpd_serious1_wgt,
        eqw_avg_pcb_dpd_occur_pct1_pen = 10 * eqw_avg_pcb_dpd_occur_pct1_wgt,
        eqw_avg_pcb_mxdpd2_pen = 10 * eqw_avg_pcb_mxdpd2_wgt,
        eqw_avg_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_avg_pcb_mxdpd_avg_comp_pct2_wgt,
        eqw_avg_pcb_mxdpd_serious2_pen = 10 * eqw_avg_pcb_mxdpd_serious2_wgt
      ) %>% 
      # remove na
      mutate(
        eqw_avg_bb_avg_dpd1_pen = ifelse(is.na(eqw_avg_bb_avg_dpd1_pen), 0, eqw_avg_bb_avg_dpd1_pen),
        eqw_avg_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_bb_dpd_occur_pct1_pen), 0, eqw_avg_bb_dpd_occur_pct1_pen),
        eqw_avg_bb_mxdpd2_pen = ifelse(is.na(eqw_avg_bb_mxdpd2_pen), 0, eqw_avg_bb_mxdpd2_pen),
        eqw_avg_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_avg_ccf_avg_dpd_serious1_pen), 0, eqw_avg_ccf_avg_dpd_serious1_pen),
        eqw_avg_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_ccf_dpd_occur_pct1_pen), 0, eqw_avg_ccf_dpd_occur_pct1_pen),
        eqw_avg_ccf_mxdpd2_pen = ifelse(is.na(eqw_avg_ccf_mxdpd2_pen), 0, eqw_avg_ccf_mxdpd2_pen),
        eqw_avg_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_avg_ccf_mxdpd_serious2_pen), 0, eqw_avg_ccf_mxdpd_serious2_pen),
        eqw_avg_ip_avg_dpd1_pen = ifelse(is.na(eqw_avg_ip_avg_dpd1_pen), 0, eqw_avg_ip_avg_dpd1_pen),
        eqw_avg_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_ip_dpd_occur_pct1_pen), 0, eqw_avg_ip_dpd_occur_pct1_pen),
        eqw_avg_ip_mxdpd2_pen = ifelse(is.na(eqw_avg_ip_mxdpd2_pen), 0, eqw_avg_ip_mxdpd2_pen),
        
        eqw_avg_ip_avg_apd1_pen = ifelse(is.na(eqw_avg_ip_avg_apd1_pen), 0, eqw_avg_ip_avg_apd1_pen),
        eqw_avg_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_avg_ip_avg_apd_as_pct1_pen), 0, eqw_avg_ip_avg_apd_as_pct1_pen),
        eqw_avg_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_avg_ip_apd_occur_pct1_pen), 0, eqw_avg_ip_apd_occur_pct1_pen),
        eqw_avg_ip_mxapd2_pen = ifelse(is.na(eqw_avg_ip_mxapd2_pen), 0, eqw_avg_ip_mxapd2_pen),
        
        eqw_avg_pcb_avg_dpd1_pen = ifelse(is.na(eqw_avg_pcb_avg_dpd1_pen), 0, eqw_avg_pcb_avg_dpd1_pen),
        eqw_avg_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_avg_pcb_avg_dpd_serious1_pen), 0, eqw_avg_pcb_avg_dpd_serious1_pen),
        eqw_avg_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_pcb_dpd_occur_pct1_pen), 0, eqw_avg_pcb_dpd_occur_pct1_pen),
        eqw_avg_pcb_mxdpd2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd2_pen), 0, eqw_avg_pcb_mxdpd2_pen),
        eqw_avg_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_avg_pcb_mxdpd_avg_comp_pct2_pen),
        eqw_avg_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd_serious2_pen), 0, eqw_avg_pcb_mxdpd_serious2_pen)
      ) %>% 
      mutate(
        penalty = eqw_avg_bb_avg_dpd1_pen + eqw_avg_bb_dpd_occur_pct1_pen + eqw_avg_bb_mxdpd2_pen + 
          eqw_avg_ccf_avg_dpd_serious1_pen + eqw_avg_ccf_dpd_occur_pct1_pen + eqw_avg_ccf_mxdpd2_pen + eqw_avg_ccf_mxdpd_serious2_pen + 
          eqw_avg_ip_avg_dpd1_pen + eqw_avg_ip_dpd_occur_pct1_pen + eqw_avg_ip_mxdpd2_pen + 
          eqw_avg_ip_avg_apd_as_pct1_pen + eqw_avg_ip_apd_occur_pct1_pen + eqw_avg_ip_mxapd2_pen +
          eqw_avg_pcb_avg_dpd1_pen + eqw_avg_pcb_avg_dpd_serious1_pen + 
          eqw_avg_pcb_dpd_occur_pct1_pen + eqw_avg_pcb_mxdpd2_pen + eqw_avg_pcb_mxdpd_avg_comp_pct2_pen +
          eqw_avg_pcb_mxdpd_serious2_pen
      ) %>% 
      mutate(score_from_dpd_avg = 100 - penalty) %>% 
      mutate(
        eqw_max_bb_avg_dpd1 = pmax(eqw_max_bb_avg_dpd, 0),
        eqw_max_bb_dpd_occur_pct1 = pmax(eqw_max_bb_dpd_occur_pct, 0),
        eqw_max_bb_mxdpd1 = pmax(eqw_max_bb_mxdpd, 0),
        eqw_max_bb_mxdpd_duration1 = pmax(eqw_max_bb_mxdpd_duration, 0),
        eqw_max_ccf_avg_dpd_serious1 = pmax(eqw_max_ccf_avg_dpd_serious, 0),
        eqw_max_ccf_dpd_occur_pct1 = pmax(eqw_max_ccf_dpd_occur_pct, 0),
        eqw_max_ccf_mxdpd1 = pmax(eqw_max_ccf_mxdpd, 0),
        eqw_max_ccf_mxdpd_serious1 = pmax(eqw_max_ccf_mxdpd_serious, 0),
        eqw_max_ccf_mxdpd_duration1 = pmax(eqw_max_ccf_mxdpd_duration, 0),
        eqw_max_ip_avg_dpd1 = pmax(eqw_max_ip_avg_dpd, 0),
        eqw_max_ip_dpd_occur_pct1 = pmax(eqw_max_ip_dpd_occur_pct, 0),
        eqw_max_ip_mxdpd1 = pmax(eqw_max_ip_mxdpd, 0),
        eqw_max_ip_mxdpd_duration1 = pmax(eqw_max_ip_mxdpd_duration, 0),
        eqw_max_ip_avg_apd1 = pmax(eqw_max_ip_avg_apd, 0),
        eqw_max_ip_avg_apd_as_pct1 = pmax(eqw_max_ip_avg_apd_as_pct, 0),
        eqw_max_ip_apd_occur_pct1 = pmax(eqw_max_ip_apd_occur_pct, 0),
        eqw_max_ip_mxapd1 = pmax(eqw_max_ip_mxapd, 0),
        eqw_max_ip_mxapd_duration1 = pmax(eqw_max_ip_mxapd_duration, 0),
        eqw_max_pcb_avg_dpd1 = pmax(eqw_max_pcb_avg_dpd, 0),
        eqw_max_pcb_avg_dpd_serious1 = pmax(eqw_max_pcb_avg_dpd_serious, 0),
        eqw_max_pcb_dpd_occur_pct1 = pmax(eqw_max_pcb_dpd_occur_pct, 0),
        eqw_max_pcb_mxdpd1 = pmax(eqw_max_pcb_mxdpd, 0),
        eqw_max_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_max_pcb_mxdpd_avg_comp_pct, 0),
        eqw_max_pcb_mxdpd_serious1 = pmax(eqw_max_pcb_mxdpd_serious, 0),
        eqw_max_pcb_mxdpd_duration1 = pmax(eqw_max_pcb_mxdpd_duration, 0)
      ) %>%
      # transfer the dpd happen time into weight step 1
      # lower number means either no dpd or no data, thus better than positive number
      mutate(
        eqw_max_bb_mxdpd_cap = ifelse(eqw_max_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_bb_mxdpd_earliest_occur + eqw_max_bb_mxdpd_latest_occur)/eqw_max_bb_mxdpd_duration1)),
        eqw_max_ccf_mxdpd_cap = ifelse(eqw_max_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_ccf_mxdpd_earliest_occur + eqw_max_ccf_mxdpd_latest_occur)/eqw_max_ccf_mxdpd_duration1)),
        eqw_max_ip_mxdpd_cap = ifelse(eqw_max_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_ip_mxdpd_earliest_occur + eqw_max_ip_mxdpd_latest_occur)/eqw_max_ip_mxdpd_duration1)),
        eqw_max_ip_mxapd_cap = ifelse(eqw_max_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_max_ip_mxapd_earliest_occur + eqw_max_ip_mxapd_latest_occur)/eqw_max_ip_mxapd_duration1)),
        eqw_max_pcb_mxdpd_cap = ifelse(eqw_max_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_pcb_mxdpd_earliest_occur + eqw_max_pcb_mxdpd_latest_occur)/eqw_max_pcb_mxdpd_duration1))
      ) %>% 
      # transfer the dpd happen time into weight step 2
      mutate(
        eqw_max_bb_mxdpd_wgt = eqw_max_bb_mxdpd_cap/max(eqw_max_bb_mxdpd_cap, na.rm = TRUE),
        eqw_max_ccf_mxdpd_wgt = eqw_max_ccf_mxdpd_cap/max(eqw_max_ccf_mxdpd_cap, na.rm = TRUE),
        eqw_max_ip_mxdpd_wgt = eqw_max_ip_mxdpd_cap/max(eqw_max_ip_mxdpd_cap, na.rm = TRUE),
        eqw_max_ip_mxapd_wgt = eqw_max_ip_mxapd_cap/max(eqw_max_ip_mxapd_cap, na.rm = TRUE),
        eqw_max_pcb_mxdpd_wgt = eqw_max_pcb_mxdpd_cap/max(eqw_max_pcb_mxdpd_cap, na.rm = TRUE)
      ) %>% 
      # calculate weighted mxdpd related
      mutate(
        eqw_max_bb_mxdpd2=eqw_max_bb_mxdpd1*eqw_max_bb_mxdpd_wgt,
        eqw_max_ccf_mxdpd2=eqw_max_ccf_mxdpd1*eqw_max_ccf_mxdpd_wgt,
        eqw_max_ccf_mxdpd_serious2=eqw_max_ccf_mxdpd_serious1*eqw_max_ccf_mxdpd_wgt,
        eqw_max_ip_mxdpd2=eqw_max_ip_mxdpd1*eqw_max_ip_mxdpd_wgt,
        eqw_max_ip_mxapd2=eqw_max_ip_mxapd1*eqw_max_ip_mxapd_wgt,
        eqw_max_pcb_mxdpd2=eqw_max_pcb_mxdpd1*eqw_max_pcb_mxdpd_wgt,
        eqw_max_pcb_mxdpd_avg_comp_pct2=eqw_max_pcb_mxdpd_avg_comp_pct1*eqw_max_pcb_mxdpd_wgt,
        eqw_max_pcb_mxdpd_serious2=eqw_max_pcb_mxdpd_serious1*eqw_max_pcb_mxdpd_wgt
      ) %>% 
      # calculate weight for each dpd related
      mutate(
        eqw_max_bb_avg_dpd1_wgt =  eqw_max_bb_avg_dpd1/max(eqw_max_bb_avg_dpd1, na.rm = TRUE),
        eqw_max_bb_dpd_occur_pct1_wgt =  eqw_max_bb_dpd_occur_pct1/max(eqw_max_bb_dpd_occur_pct1, na.rm = TRUE),
        eqw_max_bb_mxdpd2_wgt =  eqw_max_bb_mxdpd2/max(eqw_max_bb_mxdpd2, na.rm = TRUE),
        eqw_max_ccf_avg_dpd_serious1_wgt =  eqw_max_ccf_avg_dpd_serious1/max(eqw_max_ccf_avg_dpd_serious1, na.rm = TRUE),
        eqw_max_ccf_dpd_occur_pct1_wgt =  eqw_max_ccf_dpd_occur_pct1/max(eqw_max_ccf_dpd_occur_pct1, na.rm = TRUE),
        eqw_max_ccf_mxdpd2_wgt =  eqw_max_ccf_mxdpd2/max(eqw_max_ccf_mxdpd2, na.rm = TRUE),
        eqw_max_ccf_mxdpd_serious2_wgt =  eqw_max_ccf_mxdpd_serious2/max(eqw_max_ccf_mxdpd_serious2, na.rm = TRUE),
        eqw_max_ip_avg_dpd1_wgt =  eqw_max_ip_avg_dpd1/max(eqw_max_ip_avg_dpd1, na.rm = TRUE),
        eqw_max_ip_dpd_occur_pct1_wgt =  eqw_max_ip_dpd_occur_pct1/max(eqw_max_ip_dpd_occur_pct1, na.rm = TRUE),
        eqw_max_ip_mxdpd2_wgt =  eqw_max_ip_mxdpd2/max(eqw_max_ip_mxdpd2, na.rm = TRUE),
        eqw_max_ip_avg_apd1_wgt = eqw_max_ip_avg_apd1/max(eqw_max_ip_avg_apd1, na.rm = TRUE),
        eqw_max_ip_avg_apd_as_pct1_wgt = eqw_max_ip_avg_apd_as_pct1/max(eqw_max_ip_avg_apd_as_pct1, na.rm = TRUE),
        eqw_max_ip_apd_occur_pct1_wgt = eqw_max_ip_apd_occur_pct1/max(eqw_max_ip_apd_occur_pct1, na.rm = TRUE),
        eqw_max_ip_mxapd2_wgt =  eqw_max_ip_mxapd2/max(eqw_max_ip_mxapd2, na.rm = TRUE),
        eqw_max_pcb_avg_dpd1_wgt =  eqw_max_pcb_avg_dpd1/max(eqw_max_pcb_avg_dpd1, na.rm = TRUE),
        eqw_max_pcb_avg_dpd_serious1_wgt =  eqw_max_pcb_avg_dpd_serious1/max(eqw_max_pcb_avg_dpd_serious1, na.rm = TRUE),
        eqw_max_pcb_dpd_occur_pct1_wgt =  eqw_max_pcb_dpd_occur_pct1/max(eqw_max_pcb_dpd_occur_pct1, na.rm = TRUE),
        eqw_max_pcb_mxdpd2_wgt =  eqw_max_pcb_mxdpd2/max(eqw_max_pcb_mxdpd2, na.rm = TRUE),
        eqw_max_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_max_pcb_mxdpd_avg_comp_pct2/max(eqw_max_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
        eqw_max_pcb_mxdpd_serious2_wgt =  eqw_max_pcb_mxdpd_serious2/max(eqw_max_pcb_mxdpd_serious2, na.rm = TRUE)
      ) %>% 
      # calculate penalty for each dpd related
      mutate(
        eqw_max_bb_avg_dpd1_pen = 10 * eqw_max_bb_avg_dpd1_wgt,
        eqw_max_bb_dpd_occur_pct1_pen = 10 * eqw_max_bb_dpd_occur_pct1_wgt,
        eqw_max_bb_mxdpd2_pen = 10 * eqw_max_bb_mxdpd2_wgt,
        eqw_max_ccf_avg_dpd_serious1_pen = 10 * eqw_max_ccf_avg_dpd_serious1_wgt,
        eqw_max_ccf_dpd_occur_pct1_pen = 10 * eqw_max_ccf_dpd_occur_pct1_wgt,
        eqw_max_ccf_mxdpd2_pen = 10 * eqw_max_ccf_mxdpd2_wgt,
        eqw_max_ccf_mxdpd_serious2_pen = 10 * eqw_max_ccf_mxdpd_serious2_wgt,
        eqw_max_ip_avg_dpd1_pen = 10 * eqw_max_ip_avg_dpd1_wgt,
        eqw_max_ip_dpd_occur_pct1_pen = 10 * eqw_max_ip_dpd_occur_pct1_wgt,
        eqw_max_ip_mxdpd2_pen = 10 * eqw_max_ip_mxdpd2_wgt,
        eqw_max_ip_avg_apd1_pen = 10 * eqw_max_ip_avg_apd1_wgt,
        eqw_max_ip_avg_apd_as_pct1_pen = 10 * eqw_max_ip_avg_apd_as_pct1_wgt,
        eqw_max_ip_apd_occur_pct1_pen = 10 * eqw_max_ip_apd_occur_pct1_wgt,
        eqw_max_ip_mxapd2_pen = 10 * eqw_max_ip_mxapd2_wgt,
        eqw_max_pcb_avg_dpd1_pen = 10 * eqw_max_pcb_avg_dpd1_wgt,
        eqw_max_pcb_avg_dpd_serious1_pen = 10 * eqw_max_pcb_avg_dpd_serious1_wgt,
        eqw_max_pcb_dpd_occur_pct1_pen = 10 * eqw_max_pcb_dpd_occur_pct1_wgt,
        eqw_max_pcb_mxdpd2_pen = 10 * eqw_max_pcb_mxdpd2_wgt,
        eqw_max_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_max_pcb_mxdpd_avg_comp_pct2_wgt,
        eqw_max_pcb_mxdpd_serious2_pen = 10 * eqw_max_pcb_mxdpd_serious2_wgt
      ) %>% 
      # remove na
      mutate(
        eqw_max_bb_avg_dpd1_pen = ifelse(is.na(eqw_max_bb_avg_dpd1_pen), 0, eqw_max_bb_avg_dpd1_pen),
        eqw_max_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_bb_dpd_occur_pct1_pen), 0, eqw_max_bb_dpd_occur_pct1_pen),
        eqw_max_bb_mxdpd2_pen = ifelse(is.na(eqw_max_bb_mxdpd2_pen), 0, eqw_max_bb_mxdpd2_pen),
        eqw_max_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_max_ccf_avg_dpd_serious1_pen), 0, eqw_max_ccf_avg_dpd_serious1_pen),
        eqw_max_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_ccf_dpd_occur_pct1_pen), 0, eqw_max_ccf_dpd_occur_pct1_pen),
        eqw_max_ccf_mxdpd2_pen = ifelse(is.na(eqw_max_ccf_mxdpd2_pen), 0, eqw_max_ccf_mxdpd2_pen),
        eqw_max_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_max_ccf_mxdpd_serious2_pen), 0, eqw_max_ccf_mxdpd_serious2_pen),
        eqw_max_ip_avg_dpd1_pen = ifelse(is.na(eqw_max_ip_avg_dpd1_pen), 0, eqw_max_ip_avg_dpd1_pen),
        eqw_max_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_ip_dpd_occur_pct1_pen), 0, eqw_max_ip_dpd_occur_pct1_pen),
        eqw_max_ip_mxdpd2_pen = ifelse(is.na(eqw_max_ip_mxdpd2_pen), 0, eqw_max_ip_mxdpd2_pen),
        
        eqw_max_ip_avg_apd1_pen = ifelse(is.na(eqw_max_ip_avg_apd1_pen), 0, eqw_max_ip_avg_apd1_pen),
        eqw_max_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_max_ip_avg_apd_as_pct1_pen), 0, eqw_max_ip_avg_apd_as_pct1_pen),
        eqw_max_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_max_ip_apd_occur_pct1_pen), 0, eqw_max_ip_apd_occur_pct1_pen),
        eqw_max_ip_mxapd2_pen = ifelse(is.na(eqw_max_ip_mxapd2_pen), 0, eqw_max_ip_mxapd2_pen),
        
        eqw_max_pcb_avg_dpd1_pen = ifelse(is.na(eqw_max_pcb_avg_dpd1_pen), 0, eqw_max_pcb_avg_dpd1_pen),
        eqw_max_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_max_pcb_avg_dpd_serious1_pen), 0, eqw_max_pcb_avg_dpd_serious1_pen),
        eqw_max_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_pcb_dpd_occur_pct1_pen), 0, eqw_max_pcb_dpd_occur_pct1_pen),
        eqw_max_pcb_mxdpd2_pen = ifelse(is.na(eqw_max_pcb_mxdpd2_pen), 0, eqw_max_pcb_mxdpd2_pen),
        eqw_max_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_max_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_max_pcb_mxdpd_avg_comp_pct2_pen),
        eqw_max_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_max_pcb_mxdpd_serious2_pen), 0, eqw_max_pcb_mxdpd_serious2_pen)
      ) %>% 
      mutate(
        penalty = eqw_max_bb_avg_dpd1_pen + eqw_max_bb_dpd_occur_pct1_pen + eqw_max_bb_mxdpd2_pen + 
          eqw_max_ccf_avg_dpd_serious1_pen + eqw_max_ccf_dpd_occur_pct1_pen + eqw_max_ccf_mxdpd2_pen + eqw_max_ccf_mxdpd_serious2_pen + 
          eqw_max_ip_avg_dpd1_pen + eqw_max_ip_dpd_occur_pct1_pen + eqw_max_ip_mxdpd2_pen + 
          eqw_max_ip_avg_apd_as_pct1_pen + eqw_max_ip_apd_occur_pct1_pen + eqw_max_ip_mxapd2_pen +
          eqw_max_pcb_avg_dpd1_pen + eqw_max_pcb_avg_dpd_serious1_pen + 
          eqw_max_pcb_dpd_occur_pct1_pen + eqw_max_pcb_mxdpd2_pen + eqw_max_pcb_mxdpd_avg_comp_pct2_pen +
          eqw_max_pcb_mxdpd_serious2_pen
      ) %>% 
      mutate(score_from_dpd_avg = 100 - penalty) %>% 
      mutate(
        eqw_min_bb_avg_dpd1 = pmax(eqw_min_bb_avg_dpd, 0),
        eqw_min_bb_dpd_occur_pct1 = pmax(eqw_min_bb_dpd_occur_pct, 0),
        eqw_min_bb_mxdpd1 = pmax(eqw_min_bb_mxdpd, 0),
        eqw_min_bb_mxdpd_duration1 = pmax(eqw_min_bb_mxdpd_duration, 0),
        eqw_min_ccf_avg_dpd_serious1 = pmax(eqw_min_ccf_avg_dpd_serious, 0),
        eqw_min_ccf_dpd_occur_pct1 = pmax(eqw_min_ccf_dpd_occur_pct, 0),
        eqw_min_ccf_mxdpd1 = pmax(eqw_min_ccf_mxdpd, 0),
        eqw_min_ccf_mxdpd_serious1 = pmax(eqw_min_ccf_mxdpd_serious, 0),
        eqw_min_ccf_mxdpd_duration1 = pmax(eqw_min_ccf_mxdpd_duration, 0),
        eqw_min_ip_avg_dpd1 = pmax(eqw_min_ip_avg_dpd, 0),
        eqw_min_ip_dpd_occur_pct1 = pmax(eqw_min_ip_dpd_occur_pct, 0),
        eqw_min_ip_mxdpd1 = pmax(eqw_min_ip_mxdpd, 0),
        eqw_min_ip_mxdpd_duration1 = pmax(eqw_min_ip_mxdpd_duration, 0),
        eqw_min_ip_avg_apd1 = pmax(eqw_min_ip_avg_apd, 0),
        eqw_min_ip_avg_apd_as_pct1 = pmax(eqw_min_ip_avg_apd_as_pct, 0),
        eqw_min_ip_apd_occur_pct1 = pmax(eqw_min_ip_apd_occur_pct, 0),
        eqw_min_ip_mxapd1 = pmax(eqw_min_ip_mxapd, 0),
        eqw_min_ip_mxapd_duration1 = pmax(eqw_min_ip_mxapd_duration, 0),
        eqw_min_pcb_avg_dpd1 = pmax(eqw_min_pcb_avg_dpd, 0),
        eqw_min_pcb_avg_dpd_serious1 = pmax(eqw_min_pcb_avg_dpd_serious, 0),
        eqw_min_pcb_dpd_occur_pct1 = pmax(eqw_min_pcb_dpd_occur_pct, 0),
        eqw_min_pcb_mxdpd1 = pmax(eqw_min_pcb_mxdpd, 0),
        eqw_min_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_min_pcb_mxdpd_avg_comp_pct, 0),
        eqw_min_pcb_mxdpd_serious1 = pmax(eqw_min_pcb_mxdpd_serious, 0),
        eqw_min_pcb_mxdpd_duration1 = pmax(eqw_min_pcb_mxdpd_duration, 0)
      ) %>%
      # transfer the dpd happen time into weight step 1
      # lower number means either no dpd or no data, thus better than positive number
      mutate(
        eqw_min_bb_mxdpd_cap = ifelse(eqw_min_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_bb_mxdpd_earliest_occur + eqw_min_bb_mxdpd_latest_occur)/eqw_min_bb_mxdpd_duration1)),
        eqw_min_ccf_mxdpd_cap = ifelse(eqw_min_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_ccf_mxdpd_earliest_occur + eqw_min_ccf_mxdpd_latest_occur)/eqw_min_ccf_mxdpd_duration1)),
        eqw_min_ip_mxdpd_cap = ifelse(eqw_min_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_ip_mxdpd_earliest_occur + eqw_min_ip_mxdpd_latest_occur)/eqw_min_ip_mxdpd_duration1)),
        eqw_min_ip_mxapd_cap = ifelse(eqw_min_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_min_ip_mxapd_earliest_occur + eqw_min_ip_mxapd_latest_occur)/eqw_min_ip_mxapd_duration1)),
        eqw_min_pcb_mxdpd_cap = ifelse(eqw_min_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_pcb_mxdpd_earliest_occur + eqw_min_pcb_mxdpd_latest_occur)/eqw_min_pcb_mxdpd_duration1))
      ) %>% 
      # transfer the dpd happen time into weight step 2
      mutate(
        eqw_min_bb_mxdpd_wgt = eqw_min_bb_mxdpd_cap/max(eqw_min_bb_mxdpd_cap, na.rm = TRUE),
        eqw_min_ccf_mxdpd_wgt = eqw_min_ccf_mxdpd_cap/max(eqw_min_ccf_mxdpd_cap, na.rm = TRUE),
        eqw_min_ip_mxdpd_wgt = eqw_min_ip_mxdpd_cap/max(eqw_min_ip_mxdpd_cap, na.rm = TRUE),
        eqw_min_ip_mxapd_wgt = eqw_min_ip_mxapd_cap/max(eqw_min_ip_mxapd_cap, na.rm = TRUE),
        eqw_min_pcb_mxdpd_wgt = eqw_min_pcb_mxdpd_cap/max(eqw_min_pcb_mxdpd_cap, na.rm = TRUE)
      ) %>% 
      # calculate weighted mxdpd related
      mutate(
        eqw_min_bb_mxdpd2=eqw_min_bb_mxdpd1*eqw_min_bb_mxdpd_wgt,
        eqw_min_ccf_mxdpd2=eqw_min_ccf_mxdpd1*eqw_min_ccf_mxdpd_wgt,
        eqw_min_ccf_mxdpd_serious2=eqw_min_ccf_mxdpd_serious1*eqw_min_ccf_mxdpd_wgt,
        eqw_min_ip_mxdpd2=eqw_min_ip_mxdpd1*eqw_min_ip_mxdpd_wgt,
        eqw_min_ip_mxapd2=eqw_min_ip_mxapd1*eqw_min_ip_mxapd_wgt,
        eqw_min_pcb_mxdpd2=eqw_min_pcb_mxdpd1*eqw_min_pcb_mxdpd_wgt,
        eqw_min_pcb_mxdpd_avg_comp_pct2=eqw_min_pcb_mxdpd_avg_comp_pct1*eqw_min_pcb_mxdpd_wgt,
        eqw_min_pcb_mxdpd_serious2=eqw_min_pcb_mxdpd_serious1*eqw_min_pcb_mxdpd_wgt
      ) %>% 
      # calculate weight for each dpd related
      mutate(
        eqw_min_bb_avg_dpd1_wgt =  eqw_min_bb_avg_dpd1/max(eqw_min_bb_avg_dpd1, na.rm = TRUE),
        eqw_min_bb_dpd_occur_pct1_wgt =  eqw_min_bb_dpd_occur_pct1/max(eqw_min_bb_dpd_occur_pct1, na.rm = TRUE),
        eqw_min_bb_mxdpd2_wgt =  eqw_min_bb_mxdpd2/max(eqw_min_bb_mxdpd2, na.rm = TRUE),
        eqw_min_ccf_avg_dpd_serious1_wgt =  eqw_min_ccf_avg_dpd_serious1/max(eqw_min_ccf_avg_dpd_serious1, na.rm = TRUE),
        eqw_min_ccf_dpd_occur_pct1_wgt =  eqw_min_ccf_dpd_occur_pct1/max(eqw_min_ccf_dpd_occur_pct1, na.rm = TRUE),
        eqw_min_ccf_mxdpd2_wgt =  eqw_min_ccf_mxdpd2/max(eqw_min_ccf_mxdpd2, na.rm = TRUE),
        eqw_min_ccf_mxdpd_serious2_wgt =  eqw_min_ccf_mxdpd_serious2/max(eqw_min_ccf_mxdpd_serious2, na.rm = TRUE),
        eqw_min_ip_avg_dpd1_wgt =  eqw_min_ip_avg_dpd1/max(eqw_min_ip_avg_dpd1, na.rm = TRUE),
        eqw_min_ip_dpd_occur_pct1_wgt =  eqw_min_ip_dpd_occur_pct1/max(eqw_min_ip_dpd_occur_pct1, na.rm = TRUE),
        eqw_min_ip_mxdpd2_wgt =  eqw_min_ip_mxdpd2/max(eqw_min_ip_mxdpd2, na.rm = TRUE),
        eqw_min_ip_avg_apd1_wgt = eqw_min_ip_avg_apd1/max(eqw_min_ip_avg_apd1, na.rm = TRUE),
        eqw_min_ip_avg_apd_as_pct1_wgt = eqw_min_ip_avg_apd_as_pct1/max(eqw_min_ip_avg_apd_as_pct1, na.rm = TRUE),
        eqw_min_ip_apd_occur_pct1_wgt = eqw_min_ip_apd_occur_pct1/max(eqw_min_ip_apd_occur_pct1, na.rm = TRUE),
        eqw_min_ip_mxapd2_wgt =  eqw_min_ip_mxapd2/max(eqw_min_ip_mxapd2, na.rm = TRUE),
        eqw_min_pcb_avg_dpd1_wgt =  eqw_min_pcb_avg_dpd1/max(eqw_min_pcb_avg_dpd1, na.rm = TRUE),
        eqw_min_pcb_avg_dpd_serious1_wgt =  eqw_min_pcb_avg_dpd_serious1/max(eqw_min_pcb_avg_dpd_serious1, na.rm = TRUE),
        eqw_min_pcb_dpd_occur_pct1_wgt =  eqw_min_pcb_dpd_occur_pct1/max(eqw_min_pcb_dpd_occur_pct1, na.rm = TRUE),
        eqw_min_pcb_mxdpd2_wgt =  eqw_min_pcb_mxdpd2/max(eqw_min_pcb_mxdpd2, na.rm = TRUE),
        eqw_min_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_min_pcb_mxdpd_avg_comp_pct2/max(eqw_min_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
        eqw_min_pcb_mxdpd_serious2_wgt =  eqw_min_pcb_mxdpd_serious2/max(eqw_min_pcb_mxdpd_serious2, na.rm = TRUE)
      ) %>% 
      # calculate penalty for each dpd related
      mutate(
        eqw_min_bb_avg_dpd1_pen = 10 * eqw_min_bb_avg_dpd1_wgt,
        eqw_min_bb_dpd_occur_pct1_pen = 10 * eqw_min_bb_dpd_occur_pct1_wgt,
        eqw_min_bb_mxdpd2_pen = 10 * eqw_min_bb_mxdpd2_wgt,
        eqw_min_ccf_avg_dpd_serious1_pen = 10 * eqw_min_ccf_avg_dpd_serious1_wgt,
        eqw_min_ccf_dpd_occur_pct1_pen = 10 * eqw_min_ccf_dpd_occur_pct1_wgt,
        eqw_min_ccf_mxdpd2_pen = 10 * eqw_min_ccf_mxdpd2_wgt,
        eqw_min_ccf_mxdpd_serious2_pen = 10 * eqw_min_ccf_mxdpd_serious2_wgt,
        eqw_min_ip_avg_dpd1_pen = 10 * eqw_min_ip_avg_dpd1_wgt,
        eqw_min_ip_dpd_occur_pct1_pen = 10 * eqw_min_ip_dpd_occur_pct1_wgt,
        eqw_min_ip_mxdpd2_pen = 10 * eqw_min_ip_mxdpd2_wgt,
        eqw_min_ip_avg_apd1_pen = 10 * eqw_min_ip_avg_apd1_wgt,
        eqw_min_ip_avg_apd_as_pct1_pen = 10 * eqw_min_ip_avg_apd_as_pct1_wgt,
        eqw_min_ip_apd_occur_pct1_pen = 10 * eqw_min_ip_apd_occur_pct1_wgt,
        eqw_min_ip_mxapd2_pen = 10 * eqw_min_ip_mxapd2_wgt,
        eqw_min_pcb_avg_dpd1_pen = 10 * eqw_min_pcb_avg_dpd1_wgt,
        eqw_min_pcb_avg_dpd_serious1_pen = 10 * eqw_min_pcb_avg_dpd_serious1_wgt,
        eqw_min_pcb_dpd_occur_pct1_pen = 10 * eqw_min_pcb_dpd_occur_pct1_wgt,
        eqw_min_pcb_mxdpd2_pen = 10 * eqw_min_pcb_mxdpd2_wgt,
        eqw_min_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_min_pcb_mxdpd_avg_comp_pct2_wgt,
        eqw_min_pcb_mxdpd_serious2_pen = 10 * eqw_min_pcb_mxdpd_serious2_wgt
      ) %>% 
      # remove na
      mutate(
        eqw_min_bb_avg_dpd1_pen = ifelse(is.na(eqw_min_bb_avg_dpd1_pen), 0, eqw_min_bb_avg_dpd1_pen),
        eqw_min_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_bb_dpd_occur_pct1_pen), 0, eqw_min_bb_dpd_occur_pct1_pen),
        eqw_min_bb_mxdpd2_pen = ifelse(is.na(eqw_min_bb_mxdpd2_pen), 0, eqw_min_bb_mxdpd2_pen),
        eqw_min_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_min_ccf_avg_dpd_serious1_pen), 0, eqw_min_ccf_avg_dpd_serious1_pen),
        eqw_min_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_ccf_dpd_occur_pct1_pen), 0, eqw_min_ccf_dpd_occur_pct1_pen),
        eqw_min_ccf_mxdpd2_pen = ifelse(is.na(eqw_min_ccf_mxdpd2_pen), 0, eqw_min_ccf_mxdpd2_pen),
        eqw_min_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_min_ccf_mxdpd_serious2_pen), 0, eqw_min_ccf_mxdpd_serious2_pen),
        eqw_min_ip_avg_dpd1_pen = ifelse(is.na(eqw_min_ip_avg_dpd1_pen), 0, eqw_min_ip_avg_dpd1_pen),
        eqw_min_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_ip_dpd_occur_pct1_pen), 0, eqw_min_ip_dpd_occur_pct1_pen),
        eqw_min_ip_mxdpd2_pen = ifelse(is.na(eqw_min_ip_mxdpd2_pen), 0, eqw_min_ip_mxdpd2_pen),
        
        eqw_min_ip_avg_apd1_pen = ifelse(is.na(eqw_min_ip_avg_apd1_pen), 0, eqw_min_ip_avg_apd1_pen),
        eqw_min_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_min_ip_avg_apd_as_pct1_pen), 0, eqw_min_ip_avg_apd_as_pct1_pen),
        eqw_min_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_min_ip_apd_occur_pct1_pen), 0, eqw_min_ip_apd_occur_pct1_pen),
        eqw_min_ip_mxapd2_pen = ifelse(is.na(eqw_min_ip_mxapd2_pen), 0, eqw_min_ip_mxapd2_pen),
        
        eqw_min_pcb_avg_dpd1_pen = ifelse(is.na(eqw_min_pcb_avg_dpd1_pen), 0, eqw_min_pcb_avg_dpd1_pen),
        eqw_min_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_min_pcb_avg_dpd_serious1_pen), 0, eqw_min_pcb_avg_dpd_serious1_pen),
        eqw_min_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_pcb_dpd_occur_pct1_pen), 0, eqw_min_pcb_dpd_occur_pct1_pen),
        eqw_min_pcb_mxdpd2_pen = ifelse(is.na(eqw_min_pcb_mxdpd2_pen), 0, eqw_min_pcb_mxdpd2_pen),
        eqw_min_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_min_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_min_pcb_mxdpd_avg_comp_pct2_pen),
        eqw_min_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_min_pcb_mxdpd_serious2_pen), 0, eqw_min_pcb_mxdpd_serious2_pen)
      ) %>% 
      mutate(
        penalty = eqw_min_bb_avg_dpd1_pen + eqw_min_bb_dpd_occur_pct1_pen + eqw_min_bb_mxdpd2_pen + 
          eqw_min_ccf_avg_dpd_serious1_pen + eqw_min_ccf_dpd_occur_pct1_pen + eqw_min_ccf_mxdpd2_pen + eqw_min_ccf_mxdpd_serious2_pen + 
          eqw_min_ip_avg_dpd1_pen + eqw_min_ip_dpd_occur_pct1_pen + eqw_min_ip_mxdpd2_pen + 
          eqw_min_ip_avg_apd_as_pct1_pen + eqw_min_ip_apd_occur_pct1_pen + eqw_min_ip_mxapd2_pen +
          eqw_min_pcb_avg_dpd1_pen + eqw_min_pcb_avg_dpd_serious1_pen + 
          eqw_min_pcb_dpd_occur_pct1_pen + eqw_min_pcb_mxdpd2_pen + eqw_min_pcb_mxdpd_avg_comp_pct2_pen +
          eqw_min_pcb_mxdpd_serious2_pen
      ) %>% 
      mutate(score_from_dpd_min = 100 - penalty)
    
    res <- dataset
    res$score_from_dpd_avg <- res_tmp$score_from_dpd_avg
    res$score_from_dpd_max <- res_tmp$score_from_dpd_max
    res$score_from_dpd_min <- res_tmp$score_from_dpd_min
    
  } else {
    res <- dataset
  }
  return(res)
}

FinalTouch <- function(dataset, rmv_fs){
  ##
  # Feature removal
  if(length(rmv_fs) == 0) prdctrs1_t <- dataset else prdctrs1_t <- dataset %>% select(-one_of(rmv_fs))
  
  ##
  # Scale and OHE
  prdctrs1_t_peek <- DataInspection(prdctrs1_t)
  num_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class != "character","feature"]
  chr_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class == "character","feature"]
  
  data_scaled_t <- DataScale(num_cols, prdctrs1_t, rep_na = TRUE, rep_na_with = 0)
  data_scaled_ohe_t <- OHE(chr_cols, data_scaled_t)
  prdctrs2_t_peek <- DataInspection(data_scaled_ohe_t)   # info only
  
  res <- list(
    peek1 = prdctrs1_t_peek,
    peek2 = prdctrs2_t_peek,
    coredata = data_scaled_ohe_t
  )
  
  return(res)
}

##
# training parameters
tuning_pars <- expand.grid(
  eta = c(0.01),
  gamma = c(0),
  max_depth = c(30),
  min_child_weight = c(1),
  subsample = c(0.8),
  colsample_bytree = c(0.8),
  nrounds = c(175),
  stopping_rounds = c(1000)
)

##
# global parameters
static_pars <- list(
  graph_output = FALSE,
  save_mod = TRUE,
  save_pred = FALSE,
  save_res = FALSE
)

##
# Manipulation
##

##
# Take sample
full_target <- full_target[, , drop = FALSE]
full_predictors <- full_predictors[, , drop = FALSE]

full_predictors <- AddFeatures(full_predictors, do = TRUE)

##
# Take validation set
allrows <- 1:nrow(full_target)
set.seed(123)
val_idx <- sample(allrows, floor(n_val_size*nrow(full_target)), replace = FALSE)
rem_idx <- allrows[!(allrows %in% val_idx)]

rem_target <- full_target[rem_idx, , drop = FALSE]
rem_predictors <- full_predictors[rem_idx,]

val_target <- full_target[val_idx, , drop = FALSE]
val_predictors <- full_predictors[val_idx,]

rm(full_target, full_predictors)
gc()

##
# Scale and OHE
prdctrs_train <- FinalTouch(rem_predictors[,], rmv_fs)
train_peek1 <- prdctrs_train$peek1
train_peek2 <- prdctrs_train$peek2

##
# Remove non-important features
rmv_fs_vi <- c(
  "eqw_min_papl_goods_cat_photo_cinema_equipment",
  "eqw_avg_papl_seller_indsty_jewelry",
  "OCCUPATION_TYPE_Private_service_staff",
  "eqw_min_papl_seller_indsty_industry",
  "eqw_max_papl_goods_cat_jewelry",
  "eqw_max_papl_type_suite_group_of_people",
  "eqw_avg_b_amt_credit_sum_overdue",
  "eqw_min_papl_loan_purpose_xna",
  "appl_org_type_Military",
  "eqw_min_papl_prod_comb_pos_mobile_without_interest",
  "eqw_max_papl_prod_type_x_sell",
  "eqw_min_papl_goods_cat_construction_materials",
  "eqw_max_papl_seller_indsty_xna",
  "appl_org_type_Agriculture",
  "eqw_max_papl_goods_cat_other",
  "eqw_min_papl_type_suite_other_b",
  "eqw_min_papl_goods_cat_clothing_and_accessories",
  "eqw_max_papl_portfolio_cards",
  "eqw_min_pcb_avg_dpd",
  "eqw_min_papl_yield_grp_low_action",
  "eqw_avg_papl_goods_cat_other",
  "eqw_min_ccf_mxdpd",
  "eqw_avg_papl_goods_cat_sport_and_leisure",
  "eqw_max_papl_goods_cat_sport_and_leisure",
  "eqw_avg_papl_goods_cat_medical_supplies",
  "eqw_max_papl_goods_cat_vehicles",
  "eqw_avg_papl_goods_cat_gardening",
  "appl_org_type_Services",
  "eqw_max_papl_goods_cat_homewares",
  "eqw_avg_papl_goods_cat_vehicles",
  "eqw_avg_papl_portfolio_cash",
  "eqw_min_papl_prod_type_walk_in",
  "eqw_avg_papl_prod_comb_pos_others_without_interest",
  "eqw_max_papl_goods_cat_medical_supplies",
  "eqw_max_papl_goods_cat_office_appliances",
  "eqw_max_ccf_mxdpd_serious",
  "eqw_min_papl_loan_purpose_xap",
  "eqw_min_papl_prod_comb_cash_street_high",
  "eqw_min_papl_channel_type_ap_cash_loan",
  "eqw_max_papl_seller_indsty_jewelry",
  "eqw_min_papl_prod_type_x_sell",
  "eqw_min_papl_loan_purpose_repairs",
  "eqw_min_papl_prod_comb_pos_other_with_interest",
  "appl_org_type_Restaurant",
  "OCCUPATION_TYPE_Waiters_barmen_staff",
  "eqw_max_papl_portfolio_pos",
  "eqw_max_papl_loan_purpose_xna",
  "eqw_avg_ccf_dpd_occur_pct",
  "eqw_max_papl_loan_purpose_xap",
  "eqw_max_papl_loan_purpose_medicine",
  "eqw_min_papl_prod_type_xna",
  "eqw_max_papl_prod_comb_pos_others_without_interest",
  "NAME_HOUSING_TYPE_Co_op_apartment",
  "appl_org_type_Housing",
  "eqw_min_ccf_mxdpd_serious",
  "OCCUPATION_TYPE_Secretaries",
  "appl_org_type_Police",
  "eqw_avg_pcb_avg_dpd_serious",
  "eqw_min_papl_channel_type_credit_and_cash_offices",
  "eqw_max_papl_loan_purpose_buying_a_used_car",
  "eqw_min_papl_portfolio_cash",
  "eqw_min_papl_seller_indsty_clothing",
  "eqw_avg_papl_prod_type_xna",
  "eqw_max_papl_channel_type_channel_of_corporate_sales",
  "appl_org_type_Realtor",
  "eqw_max_papl_goods_cat_medicine",
  "eqw_max_papl_loan_purpose_education",
  "eqw_avg_papl_goods_cat_medicine",
  "eqw_max_papl_portfolio_cash",
  "eqw_max_ccf_avg_dpd",
  "appl_org_type_Telecom",
  "eqw_min_papl_goods_cat_medicine",
  "eqw_min_papl_goods_cat_jewelry",
  "appl_org_type_Insurance",
  "eqw_max_papl_goods_cat_gardening",
  "OCCUPATION_TYPE_Realty_agents",
  "eqw_min_papl_goods_cat_xna",
  "appl_org_type_Legal_Services",
  "eqw_min_papl_seller_indsty_auto_technology",
  "appl_org_type_Electricity",
  "eqw_min_papl_pmt_type_non_cash_from_your_account",
  "eqw_min_papl_prod_comb_cash_street_middle",
  "eqw_avg_papl_goods_cat_office_appliances",
  "eqw_avg_papl_portfolio_pos",
  "appl_org_type_Hotel",
  "eqw_avg_ccf_mxdpd",
  "eqw_min_papl_goods_cat_tourism",
  "eqw_max_papl_loan_purpose_building_a_house_or_an_annex",
  "eqw_min_papl_goods_cat_homewares",
  "eqw_min_papl_prod_comb_pos_industry_without_interest",
  "eqw_min_papl_channel_type_contact_center",
  "eqw_avg_papl_loan_purpose_everyday_expenses",
  "appl_org_type_Advertising",
  "eqw_min_papl_goods_cat_auto_accessories",
  "eqw_min_papl_goods_cat_medical_supplies",
  "eqw_min_papl_prod_comb_cash_x_sell_middle",
  "eqw_avg_papl_type_suite_group_of_people",
  "eqw_min_papl_seller_indsty_xna",
  "eqw_min_papl_goods_cat_direct_sales",
  "eqw_avg_papl_loan_purpose_education",
  "eqw_max_papl_loan_purpose_everyday_expenses",
  "eqw_min_papl_loan_purpose_urgent_needs",
  "eqw_avg_papl_goods_cat_tourism",
  "eqw_min_pcb_avg_dpd_serious",
  "eqw_max_papl_goods_cat_direct_sales",
  "OCCUPATION_TYPE_IT_staff",
  "eqw_min_papl_portfolio_pos",
  "eqw_max_papl_loan_purpose_car_repairs",
  "eqw_avg_papl_loan_purpose_purchase_of_electronic_equipment",
  "eqw_avg_papl_loan_purpose_medicine",
  "eqw_min_papl_loan_purpose_other",
  "eqw_avg_papl_seller_indsty_mlm_partners",
  "eqw_max_papl_loan_purpose_purchase_of_electronic_equipment",
  "eqw_max_papl_prod_type_xna",
  "eqw_min_b_cnt_credit_prelong",
  "eqw_max_papl_client_type_xna",
  "eqw_max_papl_goods_cat_additional_service",
  "eqw_max_papl_loan_purpose_buying_a_new_car",
  "appl_org_type_Emergency",
  "eqw_max_papl_loan_purpose_journey",
  "eqw_avg_ccf_overdrawn_occur_pct",
  "eqw_avg_papl_yield_grp_xna",
  "eqw_min_papl_goods_cat_vehicles",
  "eqw_max_papl_yield_grp_xna",
  "eqw_max_papl_loan_purpose_buying_a_home",
  "eqw_avg_papl_loan_purpose_buying_a_used_car",
  "eqw_max_papl_loan_purpose_wedding_gift_holiday",
  "OCCUPATION_TYPE_HR_staff",
  "eqw_min_papl_prod_comb_cash_x_sell_high",
  "eqw_min_papl_prod_comb_cash_x_sell_low",
  "eqw_max_papl_pmt_type_cashless_from_the_account_of_the_employer",
  "eqw_avg_papl_goods_cat_direct_sales",
  "eqw_min_papl_goods_cat_sport_and_leisure",
  "eqw_max_papl_goods_cat_insurance",
  "eqw_min_papl_loan_purpose_medicine",
  "eqw_max_papl_loan_purpose_payments_on_other_loans",
  "eqw_avg_papl_loan_purpose_journey",
  "eqw_avg_papl_pmt_type_cashless_from_the_account_of_the_employer",
  "eqw_max_papl_seller_indsty_tourism",
  "eqw_avg_papl_channel_type_channel_of_corporate_sales",
  "eqw_min_papl_type_suite_other_a",
  "eqw_min_papl_loan_purpose_purchase_of_electronic_equipment",
  "eqw_max_papl_seller_indsty_mlm_partners",
  "eqw_min_papl_seller_indsty_tourism",
  "eqw_max_papl_channel_type_car_dealer",
  "eqw_max_papl_goods_cat_weapon",
  "eqw_avg_ccf_mxdpd_serious",
  "eqw_avg_bb_avg_dpd",
  "eqw_min_papl_portfolio_cards",
  "eqw_avg_papl_client_type_xna",
  "eqw_avg_papl_goods_cat_insurance",
  "eqw_max_papl_goods_cat_fitness",
  "eqw_max_papl_goods_cat_tourism",
  "eqw_min_ccf_avg_dpd",
  "eqw_max_papl_portfolio_cars",
  "eqw_max_papl_xna_loan_ref_pct",
  "eqw_avg_papl_loan_purpose_wedding_gift_holiday",
  "eqw_min_papl_type_suite_group_of_people",
  "eqw_min_papl_seller_indsty_jewelry",
  "eqw_avg_papl_loan_purpose_furniture",
  "eqw_min_papl_prod_comb_cash_street_low",
  "eqw_max_papl_loan_purpose_business_development",
  "eqw_avg_papl_loan_purpose_payments_on_other_loans",
  "eqw_max_b_bad_credit",
  "eqw_min_papl_loan_purpose_journey",
  "eqw_min_papl_loan_purpose_building_a_house_or_an_annex",
  "eqw_avg_papl_loan_purpose_buying_a_home",
  "eqw_min_papl_prod_comb_card_x_sell",
  "eqw_avg_papl_seller_indsty_tourism",
  "eqw_max_papl_loan_purpose_refusal_to_name_the_goal",
  "eqw_min_papl_loan_purpose_refusal_to_name_the_goal",
  "eqw_min_papl_loan_purpose_car_repairs",
  "eqw_min_papl_goods_cat_office_appliances",
  "eqw_min_papl_prod_comb_pos_others_without_interest",
  "eqw_min_papl_prod_comb_card_street",
  "eqw_min_papl_loan_purpose_everyday_expenses",
  "eqw_min_papl_goods_cat_other",
  "NAME_EDUCATION_TYPE_Academic_degree",
  "eqw_max_ccf_avg_dpd_serious",
  "eqw_min_b_credit_day_overdue",
  "eqw_min_papl_loan_purpose_buying_a_used_car",
  "eqw_min_papl_xna_loan_ref_pct",
  "eqw_max_papl_loan_purpose_gasification_water_supply",
  "eqw_avg_papl_loan_purpose_buying_a_holiday_home_land",
  "eqw_min_b_amt_credit_sum_overdue",
  "eqw_min_papl_yield_grp_xna",
  "eqw_avg_ccf_avg_dpd")
#prdctrs_train$coredata <- prdctrs_train$coredata %>% select(-one_of(rmv_fs_vi))

##
# SMOTE data so the data can be balanced
if(smote_flag){
  bd_tmp <- cbind.data.frame(rem_target, prdctrs_train$coredata, stringsAsFactors = FALSE)
  bd_fnl <- ROSE::ROSE(StrTarget ~., data = bd_tmp, 
                       N = floor(1.5 * sum(bd_tmp$StrTarget == unique(bd_tmp$StrTarget)[1])), 
                       p = 0.25,
                       seed = 1)$data
  
  prdctrs_train$coredata <- bd_fnl[,-1]
  rem_target <- bd_fnl[,1,drop=FALSE]
  rm(bd_tmp, bd_fnl)
  gc()
}

##
# Format data for the model
fmtd_data <- FormatData4Model(
  prdctrs = prdctrs_train$coredata,
  tgt = rem_target, 
  tgt_map = tgt_map,
  job = "bc",
  model = "xgbtree"
)

# save(fmtd_data, val_predictors, val_target, tuning_pars, static_pars, tgt_map, file = "fmtd_data.RData")
# load("fmtd_data.RData")

##
# Train
br <- GridSearchXgbtree2(
  proj = "HCDR",
  model_name = "xgbtree",
  dataset = fmtd_data$predictors,
  labels = fmtd_data$target,
  job = "bc",
  val_size = floor(length(fmtd_data$target)/20),
  cv_rep = 1,
  mdl_pars = tuning_pars,   # data.frame
  stc_pars = static_pars,    # list
  tgt_map = tgt_map
)

##
# Present result
score_board <- br$score_board
conf_matrix <- as.data.frame(br$valdn_results[[1]][[1]]$cf)

##
# Variable importance
raw_meat <- xgboost::xgb.importance(model = br$models[[1]][[1]])
features <- colnames(fmtd_data$predictors)
var_imp <- data.frame(
  variable = features[as.numeric(raw_meat$Feature)+1],
  importance = raw_meat$Gain,
  stringsAsFactors = FALSE
)

##
# Plot learning curve
mdl_lc <- FitPlot("xgboost tree", "bc", 
                  br$models[[1]][[1]]$evaluation_log, "iter", "train_error", "test_error")
mdl_lc
rm(fmtd_data)
gc()

##
# Run validation set
##
prdctrs_val <- FinalTouch(val_predictors[,], rmv_fs)

##
# Format data for the model
fmtd_vald <- FormatData4Model(
  prdctrs = prdctrs_val$coredata,
  tgt = val_target[,,drop = FALSE], 
  tgt_map = tgt_map,
  job = "bc",
  model = "xgbtree"
)

##
# Predict
val_pred <- predict(br$models[[1]][[1]], fmtd_vald$predictors)
rocr_pred <- ROCR::prediction(val_pred, fmtd_vald$target)
rocr_perf <- ROCR::performance(rocr_pred, measure = "auc")
rocr_perf@y.values

##
# Run test data
##
if(run_test){
  
  ##
  # Scale and OHE
  prdctrs_test <- FinalTouch(full_testdata, rmv_fs)
  test_peek1 <- prdctrs_test$peek1
  test_peek2 <- prdctrs_test$peek2
  
  ##
  # Load model
  # load(paste0(proj_dir, "Output/Model/", mdl_nm))
  pred_res <- predict(br$models[[1]][[1]], as.matrix(prdctrs_test$coredata))
  pred_res2 <- as.data.frame(pred_res)
  
  out <- data.frame(SK_ID_CURR = full_testdata$SK_ID_CURR,
                    TARGET = pred_res2[,1],
                    stringsAsFactors = FALSE)
  write.csv(out, paste0(proj_dir, "Submission/",
                        "run_result_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"),
                        ".csv"), 
            row.names = FALSE)
}