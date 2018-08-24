# ##
# # Manual training for xgbtree model
# ##
# 
# # ##
# # # Saving data
# # ##
# # full_testdata <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_test_predictors")
# # full_predictors <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_train_predictors01")
# # full_target <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "ml_train_target")
# # tgt_map <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "input02_target_map")
# # save(full_predictors, full_target, full_testdata, tgt_map, file = "hcdr_full03.RData")
# 
# ##
# # first things first, parameters
# if(R.Version()$os == "linux-gnu"){
#   mlh_dir <- paste0("/home/",Sys.info()["user"],"/projects/ShinyMLHome/")
# } else {
#   mlh_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/Data Science/ShinyMLHome/")
# }
# n_val_size <- 0.1
# dataset_nm <- c("hcdr_sample","hcdr_full03")[2]
# # rmv_fs <- c('SK_ID_CURR', 'NAME_TYPE_SUITE', 'HOUR_APPR_PROCESS_START', 'FLAG_MOBIL',
# #             'FLAG_CONT_MOBILE', 'FLAG_DOCUMENT_2', 'FLAG_DOCUMENT_4',
# #             'FLAG_DOCUMENT_7', 'FLAG_DOCUMENT_10',
# #             'FLAG_DOCUMENT_12', 'FLAG_DOCUMENT_14', 'FLAG_DOCUMENT_15',
# #             'FLAG_DOCUMENT_17', 'FLAG_DOCUMENT_19',
# #             'FLAG_DOCUMENT_20', 'FLAG_DOCUMENT_21', 'appl_process_weekday')
# 
# rmv_fs <- c('SK_ID_CURR')
# 
# run_test <- FALSE
# smote_flag <- FALSE
# 
# ##
# # Load required source files and data
# source(paste0(mlh_dir, "global.R"))
# setwd(proj_dir)
# load(paste0(proj_dir, dataset_nm, ".RData"))
# 
# AddFeatures <- function(dataset, do){
# 
#   if(do == TRUE){
# 
#     res_tmp <- dataset %>%
#       mutate(
#         eqw_avg_bb_avg_dpd1 = pmax(eqw_avg_bb_avg_dpd, 0),
#         eqw_avg_bb_dpd_occur_pct1 = pmax(eqw_avg_bb_dpd_occur_pct, 0),
#         eqw_avg_bb_mxdpd1 = pmax(eqw_avg_bb_mxdpd, 0),
#         eqw_avg_bb_mxdpd_duration1 = pmax(eqw_avg_bb_mxdpd_duration, 0),
#         eqw_avg_ccf_avg_dpd_serious1 = pmax(eqw_avg_ccf_avg_dpd_serious, 0),
#         eqw_avg_ccf_dpd_occur_pct1 = pmax(eqw_avg_ccf_dpd_occur_pct, 0),
#         eqw_avg_ccf_mxdpd1 = pmax(eqw_avg_ccf_mxdpd, 0),
#         eqw_avg_ccf_mxdpd_serious1 = pmax(eqw_avg_ccf_mxdpd_serious, 0),
#         eqw_avg_ccf_mxdpd_duration1 = pmax(eqw_avg_ccf_mxdpd_duration, 0),
#         eqw_avg_ip_avg_dpd1 = pmax(eqw_avg_ip_avg_dpd, 0),
#         eqw_avg_ip_dpd_occur_pct1 = pmax(eqw_avg_ip_dpd_occur_pct, 0),
#         eqw_avg_ip_mxdpd1 = pmax(eqw_avg_ip_mxdpd, 0),
#         eqw_avg_ip_mxdpd_duration1 = pmax(eqw_avg_ip_mxdpd_duration, 0),
#         eqw_avg_ip_avg_apd1 = pmax(eqw_avg_ip_avg_apd, 0),
#         eqw_avg_ip_avg_apd_as_pct1 = pmax(eqw_avg_ip_avg_apd_as_pct, 0),
#         eqw_avg_ip_apd_occur_pct1 = pmax(eqw_avg_ip_apd_occur_pct, 0),
#         eqw_avg_ip_mxapd1 = pmax(eqw_avg_ip_mxapd, 0),
#         eqw_avg_ip_mxapd_duration1 = pmax(eqw_avg_ip_mxapd_duration, 0),
#         eqw_avg_pcb_avg_dpd1 = pmax(eqw_avg_pcb_avg_dpd, 0),
#         eqw_avg_pcb_avg_dpd_serious1 = pmax(eqw_avg_pcb_avg_dpd_serious, 0),
#         eqw_avg_pcb_dpd_occur_pct1 = pmax(eqw_avg_pcb_dpd_occur_pct, 0),
#         eqw_avg_pcb_mxdpd1 = pmax(eqw_avg_pcb_mxdpd, 0),
#         eqw_avg_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_avg_pcb_mxdpd_avg_comp_pct, 0),
#         eqw_avg_pcb_mxdpd_serious1 = pmax(eqw_avg_pcb_mxdpd_serious, 0),
#         eqw_avg_pcb_mxdpd_duration1 = pmax(eqw_avg_pcb_mxdpd_duration, 0)
#       ) %>%
#       # transfer the dpd happen time into weight step 1
#       # lower number means either no dpd or no data, thus better than positive number
#       mutate(
#         eqw_avg_bb_mxdpd_cap = ifelse(eqw_avg_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_bb_mxdpd_earliest_occur + eqw_avg_bb_mxdpd_latest_occur)/eqw_avg_bb_mxdpd_duration1)),
#         eqw_avg_ccf_mxdpd_cap = ifelse(eqw_avg_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_ccf_mxdpd_earliest_occur + eqw_avg_ccf_mxdpd_latest_occur)/eqw_avg_ccf_mxdpd_duration1)),
#         eqw_avg_ip_mxdpd_cap = ifelse(eqw_avg_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_ip_mxdpd_earliest_occur + eqw_avg_ip_mxdpd_latest_occur)/eqw_avg_ip_mxdpd_duration1)),
#         eqw_avg_ip_mxapd_cap = ifelse(eqw_avg_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_avg_ip_mxapd_earliest_occur + eqw_avg_ip_mxapd_latest_occur)/eqw_avg_ip_mxapd_duration1)),
#         eqw_avg_pcb_mxdpd_cap = ifelse(eqw_avg_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_avg_pcb_mxdpd_earliest_occur + eqw_avg_pcb_mxdpd_latest_occur)/eqw_avg_pcb_mxdpd_duration1))
#       ) %>%
#       # transfer the dpd happen time into weight step 2
#       mutate(
#         eqw_avg_bb_mxdpd_wgt = eqw_avg_bb_mxdpd_cap/max(eqw_avg_bb_mxdpd_cap, na.rm = TRUE),
#         eqw_avg_ccf_mxdpd_wgt = eqw_avg_ccf_mxdpd_cap/max(eqw_avg_ccf_mxdpd_cap, na.rm = TRUE),
#         eqw_avg_ip_mxdpd_wgt = eqw_avg_ip_mxdpd_cap/max(eqw_avg_ip_mxdpd_cap, na.rm = TRUE),
#         eqw_avg_ip_mxapd_wgt = eqw_avg_ip_mxapd_cap/max(eqw_avg_ip_mxapd_cap, na.rm = TRUE),
#         eqw_avg_pcb_mxdpd_wgt = eqw_avg_pcb_mxdpd_cap/max(eqw_avg_pcb_mxdpd_cap, na.rm = TRUE)
#       ) %>%
#       # calculate weighted mxdpd related
#       mutate(
#         eqw_avg_bb_mxdpd2=eqw_avg_bb_mxdpd1*eqw_avg_bb_mxdpd_wgt,
#         eqw_avg_ccf_mxdpd2=eqw_avg_ccf_mxdpd1*eqw_avg_ccf_mxdpd_wgt,
#         eqw_avg_ccf_mxdpd_serious2=eqw_avg_ccf_mxdpd_serious1*eqw_avg_ccf_mxdpd_wgt,
#         eqw_avg_ip_mxdpd2=eqw_avg_ip_mxdpd1*eqw_avg_ip_mxdpd_wgt,
#         eqw_avg_ip_mxapd2=eqw_avg_ip_mxapd1*eqw_avg_ip_mxapd_wgt,
#         eqw_avg_pcb_mxdpd2=eqw_avg_pcb_mxdpd1*eqw_avg_pcb_mxdpd_wgt,
#         eqw_avg_pcb_mxdpd_avg_comp_pct2=eqw_avg_pcb_mxdpd_avg_comp_pct1*eqw_avg_pcb_mxdpd_wgt,
#         eqw_avg_pcb_mxdpd_serious2=eqw_avg_pcb_mxdpd_serious1*eqw_avg_pcb_mxdpd_wgt
#       ) %>%
#       # calculate weight for each dpd related
#       mutate(
#         eqw_avg_bb_avg_dpd1_wgt =  eqw_avg_bb_avg_dpd1/max(eqw_avg_bb_avg_dpd1, na.rm = TRUE),
#         eqw_avg_bb_dpd_occur_pct1_wgt =  eqw_avg_bb_dpd_occur_pct1/max(eqw_avg_bb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_avg_bb_mxdpd2_wgt =  eqw_avg_bb_mxdpd2/max(eqw_avg_bb_mxdpd2, na.rm = TRUE),
#         eqw_avg_ccf_avg_dpd_serious1_wgt =  eqw_avg_ccf_avg_dpd_serious1/max(eqw_avg_ccf_avg_dpd_serious1, na.rm = TRUE),
#         eqw_avg_ccf_dpd_occur_pct1_wgt =  eqw_avg_ccf_dpd_occur_pct1/max(eqw_avg_ccf_dpd_occur_pct1, na.rm = TRUE),
#         eqw_avg_ccf_mxdpd2_wgt =  eqw_avg_ccf_mxdpd2/max(eqw_avg_ccf_mxdpd2, na.rm = TRUE),
#         eqw_avg_ccf_mxdpd_serious2_wgt =  eqw_avg_ccf_mxdpd_serious2/max(eqw_avg_ccf_mxdpd_serious2, na.rm = TRUE),
#         eqw_avg_ip_avg_dpd1_wgt =  eqw_avg_ip_avg_dpd1/max(eqw_avg_ip_avg_dpd1, na.rm = TRUE),
#         eqw_avg_ip_dpd_occur_pct1_wgt =  eqw_avg_ip_dpd_occur_pct1/max(eqw_avg_ip_dpd_occur_pct1, na.rm = TRUE),
#         eqw_avg_ip_mxdpd2_wgt =  eqw_avg_ip_mxdpd2/max(eqw_avg_ip_mxdpd2, na.rm = TRUE),
#         eqw_avg_ip_avg_apd1_wgt = eqw_avg_ip_avg_apd1/max(eqw_avg_ip_avg_apd1, na.rm = TRUE),
#         eqw_avg_ip_avg_apd_as_pct1_wgt = eqw_avg_ip_avg_apd_as_pct1/max(eqw_avg_ip_avg_apd_as_pct1, na.rm = TRUE),
#         eqw_avg_ip_apd_occur_pct1_wgt = eqw_avg_ip_apd_occur_pct1/max(eqw_avg_ip_apd_occur_pct1, na.rm = TRUE),
#         eqw_avg_ip_mxapd2_wgt =  eqw_avg_ip_mxapd2/max(eqw_avg_ip_mxapd2, na.rm = TRUE),
#         eqw_avg_pcb_avg_dpd1_wgt =  eqw_avg_pcb_avg_dpd1/max(eqw_avg_pcb_avg_dpd1, na.rm = TRUE),
#         eqw_avg_pcb_avg_dpd_serious1_wgt =  eqw_avg_pcb_avg_dpd_serious1/max(eqw_avg_pcb_avg_dpd_serious1, na.rm = TRUE),
#         eqw_avg_pcb_dpd_occur_pct1_wgt =  eqw_avg_pcb_dpd_occur_pct1/max(eqw_avg_pcb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_avg_pcb_mxdpd2_wgt =  eqw_avg_pcb_mxdpd2/max(eqw_avg_pcb_mxdpd2, na.rm = TRUE),
#         eqw_avg_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_avg_pcb_mxdpd_avg_comp_pct2/max(eqw_avg_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
#         eqw_avg_pcb_mxdpd_serious2_wgt =  eqw_avg_pcb_mxdpd_serious2/max(eqw_avg_pcb_mxdpd_serious2, na.rm = TRUE)
#       ) %>%
#       # calculate penalty for each dpd related
#       mutate(
#         eqw_avg_bb_avg_dpd1_pen = 10 * eqw_avg_bb_avg_dpd1_wgt,
#         eqw_avg_bb_dpd_occur_pct1_pen = 10 * eqw_avg_bb_dpd_occur_pct1_wgt,
#         eqw_avg_bb_mxdpd2_pen = 10 * eqw_avg_bb_mxdpd2_wgt,
#         eqw_avg_ccf_avg_dpd_serious1_pen = 10 * eqw_avg_ccf_avg_dpd_serious1_wgt,
#         eqw_avg_ccf_dpd_occur_pct1_pen = 10 * eqw_avg_ccf_dpd_occur_pct1_wgt,
#         eqw_avg_ccf_mxdpd2_pen = 10 * eqw_avg_ccf_mxdpd2_wgt,
#         eqw_avg_ccf_mxdpd_serious2_pen = 10 * eqw_avg_ccf_mxdpd_serious2_wgt,
#         eqw_avg_ip_avg_dpd1_pen = 10 * eqw_avg_ip_avg_dpd1_wgt,
#         eqw_avg_ip_dpd_occur_pct1_pen = 10 * eqw_avg_ip_dpd_occur_pct1_wgt,
#         eqw_avg_ip_mxdpd2_pen = 10 * eqw_avg_ip_mxdpd2_wgt,
#         eqw_avg_ip_avg_apd1_pen = 10 * eqw_avg_ip_avg_apd1_wgt,
#         eqw_avg_ip_avg_apd_as_pct1_pen = 10 * eqw_avg_ip_avg_apd_as_pct1_wgt,
#         eqw_avg_ip_apd_occur_pct1_pen = 10 * eqw_avg_ip_apd_occur_pct1_wgt,
#         eqw_avg_ip_mxapd2_pen = 10 * eqw_avg_ip_mxapd2_wgt,
#         eqw_avg_pcb_avg_dpd1_pen = 10 * eqw_avg_pcb_avg_dpd1_wgt,
#         eqw_avg_pcb_avg_dpd_serious1_pen = 10 * eqw_avg_pcb_avg_dpd_serious1_wgt,
#         eqw_avg_pcb_dpd_occur_pct1_pen = 10 * eqw_avg_pcb_dpd_occur_pct1_wgt,
#         eqw_avg_pcb_mxdpd2_pen = 10 * eqw_avg_pcb_mxdpd2_wgt,
#         eqw_avg_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_avg_pcb_mxdpd_avg_comp_pct2_wgt,
#         eqw_avg_pcb_mxdpd_serious2_pen = 10 * eqw_avg_pcb_mxdpd_serious2_wgt
#       ) %>%
#       # remove na
#       mutate(
#         eqw_avg_bb_avg_dpd1_pen = ifelse(is.na(eqw_avg_bb_avg_dpd1_pen), 0, eqw_avg_bb_avg_dpd1_pen),
#         eqw_avg_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_bb_dpd_occur_pct1_pen), 0, eqw_avg_bb_dpd_occur_pct1_pen),
#         eqw_avg_bb_mxdpd2_pen = ifelse(is.na(eqw_avg_bb_mxdpd2_pen), 0, eqw_avg_bb_mxdpd2_pen),
#         eqw_avg_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_avg_ccf_avg_dpd_serious1_pen), 0, eqw_avg_ccf_avg_dpd_serious1_pen),
#         eqw_avg_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_ccf_dpd_occur_pct1_pen), 0, eqw_avg_ccf_dpd_occur_pct1_pen),
#         eqw_avg_ccf_mxdpd2_pen = ifelse(is.na(eqw_avg_ccf_mxdpd2_pen), 0, eqw_avg_ccf_mxdpd2_pen),
#         eqw_avg_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_avg_ccf_mxdpd_serious2_pen), 0, eqw_avg_ccf_mxdpd_serious2_pen),
#         eqw_avg_ip_avg_dpd1_pen = ifelse(is.na(eqw_avg_ip_avg_dpd1_pen), 0, eqw_avg_ip_avg_dpd1_pen),
#         eqw_avg_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_ip_dpd_occur_pct1_pen), 0, eqw_avg_ip_dpd_occur_pct1_pen),
#         eqw_avg_ip_mxdpd2_pen = ifelse(is.na(eqw_avg_ip_mxdpd2_pen), 0, eqw_avg_ip_mxdpd2_pen),
# 
#         eqw_avg_ip_avg_apd1_pen = ifelse(is.na(eqw_avg_ip_avg_apd1_pen), 0, eqw_avg_ip_avg_apd1_pen),
#         eqw_avg_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_avg_ip_avg_apd_as_pct1_pen), 0, eqw_avg_ip_avg_apd_as_pct1_pen),
#         eqw_avg_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_avg_ip_apd_occur_pct1_pen), 0, eqw_avg_ip_apd_occur_pct1_pen),
#         eqw_avg_ip_mxapd2_pen = ifelse(is.na(eqw_avg_ip_mxapd2_pen), 0, eqw_avg_ip_mxapd2_pen),
# 
#         eqw_avg_pcb_avg_dpd1_pen = ifelse(is.na(eqw_avg_pcb_avg_dpd1_pen), 0, eqw_avg_pcb_avg_dpd1_pen),
#         eqw_avg_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_avg_pcb_avg_dpd_serious1_pen), 0, eqw_avg_pcb_avg_dpd_serious1_pen),
#         eqw_avg_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_avg_pcb_dpd_occur_pct1_pen), 0, eqw_avg_pcb_dpd_occur_pct1_pen),
#         eqw_avg_pcb_mxdpd2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd2_pen), 0, eqw_avg_pcb_mxdpd2_pen),
#         eqw_avg_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_avg_pcb_mxdpd_avg_comp_pct2_pen),
#         eqw_avg_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_avg_pcb_mxdpd_serious2_pen), 0, eqw_avg_pcb_mxdpd_serious2_pen)
#       ) %>%
#       mutate(
#         penalty = eqw_avg_bb_avg_dpd1_pen + eqw_avg_bb_dpd_occur_pct1_pen + eqw_avg_bb_mxdpd2_pen +
#           eqw_avg_ccf_avg_dpd_serious1_pen + eqw_avg_ccf_dpd_occur_pct1_pen + eqw_avg_ccf_mxdpd2_pen + eqw_avg_ccf_mxdpd_serious2_pen +
#           eqw_avg_ip_avg_dpd1_pen + eqw_avg_ip_dpd_occur_pct1_pen + eqw_avg_ip_mxdpd2_pen +
#           eqw_avg_ip_avg_apd_as_pct1_pen + eqw_avg_ip_apd_occur_pct1_pen + eqw_avg_ip_mxapd2_pen +
#           eqw_avg_pcb_avg_dpd1_pen + eqw_avg_pcb_avg_dpd_serious1_pen +
#           eqw_avg_pcb_dpd_occur_pct1_pen + eqw_avg_pcb_mxdpd2_pen + eqw_avg_pcb_mxdpd_avg_comp_pct2_pen +
#           eqw_avg_pcb_mxdpd_serious2_pen
#       ) %>%
#       mutate(score_from_dpd_avg = 100 - penalty) %>%
#       mutate(
#         eqw_max_bb_avg_dpd1 = pmax(eqw_max_bb_avg_dpd, 0),
#         eqw_max_bb_dpd_occur_pct1 = pmax(eqw_max_bb_dpd_occur_pct, 0),
#         eqw_max_bb_mxdpd1 = pmax(eqw_max_bb_mxdpd, 0),
#         eqw_max_bb_mxdpd_duration1 = pmax(eqw_max_bb_mxdpd_duration, 0),
#         eqw_max_ccf_avg_dpd_serious1 = pmax(eqw_max_ccf_avg_dpd_serious, 0),
#         eqw_max_ccf_dpd_occur_pct1 = pmax(eqw_max_ccf_dpd_occur_pct, 0),
#         eqw_max_ccf_mxdpd1 = pmax(eqw_max_ccf_mxdpd, 0),
#         eqw_max_ccf_mxdpd_serious1 = pmax(eqw_max_ccf_mxdpd_serious, 0),
#         eqw_max_ccf_mxdpd_duration1 = pmax(eqw_max_ccf_mxdpd_duration, 0),
#         eqw_max_ip_avg_dpd1 = pmax(eqw_max_ip_avg_dpd, 0),
#         eqw_max_ip_dpd_occur_pct1 = pmax(eqw_max_ip_dpd_occur_pct, 0),
#         eqw_max_ip_mxdpd1 = pmax(eqw_max_ip_mxdpd, 0),
#         eqw_max_ip_mxdpd_duration1 = pmax(eqw_max_ip_mxdpd_duration, 0),
#         eqw_max_ip_avg_apd1 = pmax(eqw_max_ip_avg_apd, 0),
#         eqw_max_ip_avg_apd_as_pct1 = pmax(eqw_max_ip_avg_apd_as_pct, 0),
#         eqw_max_ip_apd_occur_pct1 = pmax(eqw_max_ip_apd_occur_pct, 0),
#         eqw_max_ip_mxapd1 = pmax(eqw_max_ip_mxapd, 0),
#         eqw_max_ip_mxapd_duration1 = pmax(eqw_max_ip_mxapd_duration, 0),
#         eqw_max_pcb_avg_dpd1 = pmax(eqw_max_pcb_avg_dpd, 0),
#         eqw_max_pcb_avg_dpd_serious1 = pmax(eqw_max_pcb_avg_dpd_serious, 0),
#         eqw_max_pcb_dpd_occur_pct1 = pmax(eqw_max_pcb_dpd_occur_pct, 0),
#         eqw_max_pcb_mxdpd1 = pmax(eqw_max_pcb_mxdpd, 0),
#         eqw_max_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_max_pcb_mxdpd_avg_comp_pct, 0),
#         eqw_max_pcb_mxdpd_serious1 = pmax(eqw_max_pcb_mxdpd_serious, 0),
#         eqw_max_pcb_mxdpd_duration1 = pmax(eqw_max_pcb_mxdpd_duration, 0)
#       ) %>%
#       # transfer the dpd happen time into weight step 1
#       # lower number means either no dpd or no data, thus better than positive number
#       mutate(
#         eqw_max_bb_mxdpd_cap = ifelse(eqw_max_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_bb_mxdpd_earliest_occur + eqw_max_bb_mxdpd_latest_occur)/eqw_max_bb_mxdpd_duration1)),
#         eqw_max_ccf_mxdpd_cap = ifelse(eqw_max_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_ccf_mxdpd_earliest_occur + eqw_max_ccf_mxdpd_latest_occur)/eqw_max_ccf_mxdpd_duration1)),
#         eqw_max_ip_mxdpd_cap = ifelse(eqw_max_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_ip_mxdpd_earliest_occur + eqw_max_ip_mxdpd_latest_occur)/eqw_max_ip_mxdpd_duration1)),
#         eqw_max_ip_mxapd_cap = ifelse(eqw_max_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_max_ip_mxapd_earliest_occur + eqw_max_ip_mxapd_latest_occur)/eqw_max_ip_mxapd_duration1)),
#         eqw_max_pcb_mxdpd_cap = ifelse(eqw_max_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_max_pcb_mxdpd_earliest_occur + eqw_max_pcb_mxdpd_latest_occur)/eqw_max_pcb_mxdpd_duration1))
#       ) %>%
#       # transfer the dpd happen time into weight step 2
#       mutate(
#         eqw_max_bb_mxdpd_wgt = eqw_max_bb_mxdpd_cap/max(eqw_max_bb_mxdpd_cap, na.rm = TRUE),
#         eqw_max_ccf_mxdpd_wgt = eqw_max_ccf_mxdpd_cap/max(eqw_max_ccf_mxdpd_cap, na.rm = TRUE),
#         eqw_max_ip_mxdpd_wgt = eqw_max_ip_mxdpd_cap/max(eqw_max_ip_mxdpd_cap, na.rm = TRUE),
#         eqw_max_ip_mxapd_wgt = eqw_max_ip_mxapd_cap/max(eqw_max_ip_mxapd_cap, na.rm = TRUE),
#         eqw_max_pcb_mxdpd_wgt = eqw_max_pcb_mxdpd_cap/max(eqw_max_pcb_mxdpd_cap, na.rm = TRUE)
#       ) %>%
#       # calculate weighted mxdpd related
#       mutate(
#         eqw_max_bb_mxdpd2=eqw_max_bb_mxdpd1*eqw_max_bb_mxdpd_wgt,
#         eqw_max_ccf_mxdpd2=eqw_max_ccf_mxdpd1*eqw_max_ccf_mxdpd_wgt,
#         eqw_max_ccf_mxdpd_serious2=eqw_max_ccf_mxdpd_serious1*eqw_max_ccf_mxdpd_wgt,
#         eqw_max_ip_mxdpd2=eqw_max_ip_mxdpd1*eqw_max_ip_mxdpd_wgt,
#         eqw_max_ip_mxapd2=eqw_max_ip_mxapd1*eqw_max_ip_mxapd_wgt,
#         eqw_max_pcb_mxdpd2=eqw_max_pcb_mxdpd1*eqw_max_pcb_mxdpd_wgt,
#         eqw_max_pcb_mxdpd_avg_comp_pct2=eqw_max_pcb_mxdpd_avg_comp_pct1*eqw_max_pcb_mxdpd_wgt,
#         eqw_max_pcb_mxdpd_serious2=eqw_max_pcb_mxdpd_serious1*eqw_max_pcb_mxdpd_wgt
#       ) %>%
#       # calculate weight for each dpd related
#       mutate(
#         eqw_max_bb_avg_dpd1_wgt =  eqw_max_bb_avg_dpd1/max(eqw_max_bb_avg_dpd1, na.rm = TRUE),
#         eqw_max_bb_dpd_occur_pct1_wgt =  eqw_max_bb_dpd_occur_pct1/max(eqw_max_bb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_max_bb_mxdpd2_wgt =  eqw_max_bb_mxdpd2/max(eqw_max_bb_mxdpd2, na.rm = TRUE),
#         eqw_max_ccf_avg_dpd_serious1_wgt =  eqw_max_ccf_avg_dpd_serious1/max(eqw_max_ccf_avg_dpd_serious1, na.rm = TRUE),
#         eqw_max_ccf_dpd_occur_pct1_wgt =  eqw_max_ccf_dpd_occur_pct1/max(eqw_max_ccf_dpd_occur_pct1, na.rm = TRUE),
#         eqw_max_ccf_mxdpd2_wgt =  eqw_max_ccf_mxdpd2/max(eqw_max_ccf_mxdpd2, na.rm = TRUE),
#         eqw_max_ccf_mxdpd_serious2_wgt =  eqw_max_ccf_mxdpd_serious2/max(eqw_max_ccf_mxdpd_serious2, na.rm = TRUE),
#         eqw_max_ip_avg_dpd1_wgt =  eqw_max_ip_avg_dpd1/max(eqw_max_ip_avg_dpd1, na.rm = TRUE),
#         eqw_max_ip_dpd_occur_pct1_wgt =  eqw_max_ip_dpd_occur_pct1/max(eqw_max_ip_dpd_occur_pct1, na.rm = TRUE),
#         eqw_max_ip_mxdpd2_wgt =  eqw_max_ip_mxdpd2/max(eqw_max_ip_mxdpd2, na.rm = TRUE),
#         eqw_max_ip_avg_apd1_wgt = eqw_max_ip_avg_apd1/max(eqw_max_ip_avg_apd1, na.rm = TRUE),
#         eqw_max_ip_avg_apd_as_pct1_wgt = eqw_max_ip_avg_apd_as_pct1/max(eqw_max_ip_avg_apd_as_pct1, na.rm = TRUE),
#         eqw_max_ip_apd_occur_pct1_wgt = eqw_max_ip_apd_occur_pct1/max(eqw_max_ip_apd_occur_pct1, na.rm = TRUE),
#         eqw_max_ip_mxapd2_wgt =  eqw_max_ip_mxapd2/max(eqw_max_ip_mxapd2, na.rm = TRUE),
#         eqw_max_pcb_avg_dpd1_wgt =  eqw_max_pcb_avg_dpd1/max(eqw_max_pcb_avg_dpd1, na.rm = TRUE),
#         eqw_max_pcb_avg_dpd_serious1_wgt =  eqw_max_pcb_avg_dpd_serious1/max(eqw_max_pcb_avg_dpd_serious1, na.rm = TRUE),
#         eqw_max_pcb_dpd_occur_pct1_wgt =  eqw_max_pcb_dpd_occur_pct1/max(eqw_max_pcb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_max_pcb_mxdpd2_wgt =  eqw_max_pcb_mxdpd2/max(eqw_max_pcb_mxdpd2, na.rm = TRUE),
#         eqw_max_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_max_pcb_mxdpd_avg_comp_pct2/max(eqw_max_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
#         eqw_max_pcb_mxdpd_serious2_wgt =  eqw_max_pcb_mxdpd_serious2/max(eqw_max_pcb_mxdpd_serious2, na.rm = TRUE)
#       ) %>%
#       # calculate penalty for each dpd related
#       mutate(
#         eqw_max_bb_avg_dpd1_pen = 10 * eqw_max_bb_avg_dpd1_wgt,
#         eqw_max_bb_dpd_occur_pct1_pen = 10 * eqw_max_bb_dpd_occur_pct1_wgt,
#         eqw_max_bb_mxdpd2_pen = 10 * eqw_max_bb_mxdpd2_wgt,
#         eqw_max_ccf_avg_dpd_serious1_pen = 10 * eqw_max_ccf_avg_dpd_serious1_wgt,
#         eqw_max_ccf_dpd_occur_pct1_pen = 10 * eqw_max_ccf_dpd_occur_pct1_wgt,
#         eqw_max_ccf_mxdpd2_pen = 10 * eqw_max_ccf_mxdpd2_wgt,
#         eqw_max_ccf_mxdpd_serious2_pen = 10 * eqw_max_ccf_mxdpd_serious2_wgt,
#         eqw_max_ip_avg_dpd1_pen = 10 * eqw_max_ip_avg_dpd1_wgt,
#         eqw_max_ip_dpd_occur_pct1_pen = 10 * eqw_max_ip_dpd_occur_pct1_wgt,
#         eqw_max_ip_mxdpd2_pen = 10 * eqw_max_ip_mxdpd2_wgt,
#         eqw_max_ip_avg_apd1_pen = 10 * eqw_max_ip_avg_apd1_wgt,
#         eqw_max_ip_avg_apd_as_pct1_pen = 10 * eqw_max_ip_avg_apd_as_pct1_wgt,
#         eqw_max_ip_apd_occur_pct1_pen = 10 * eqw_max_ip_apd_occur_pct1_wgt,
#         eqw_max_ip_mxapd2_pen = 10 * eqw_max_ip_mxapd2_wgt,
#         eqw_max_pcb_avg_dpd1_pen = 10 * eqw_max_pcb_avg_dpd1_wgt,
#         eqw_max_pcb_avg_dpd_serious1_pen = 10 * eqw_max_pcb_avg_dpd_serious1_wgt,
#         eqw_max_pcb_dpd_occur_pct1_pen = 10 * eqw_max_pcb_dpd_occur_pct1_wgt,
#         eqw_max_pcb_mxdpd2_pen = 10 * eqw_max_pcb_mxdpd2_wgt,
#         eqw_max_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_max_pcb_mxdpd_avg_comp_pct2_wgt,
#         eqw_max_pcb_mxdpd_serious2_pen = 10 * eqw_max_pcb_mxdpd_serious2_wgt
#       ) %>%
#       # remove na
#       mutate(
#         eqw_max_bb_avg_dpd1_pen = ifelse(is.na(eqw_max_bb_avg_dpd1_pen), 0, eqw_max_bb_avg_dpd1_pen),
#         eqw_max_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_bb_dpd_occur_pct1_pen), 0, eqw_max_bb_dpd_occur_pct1_pen),
#         eqw_max_bb_mxdpd2_pen = ifelse(is.na(eqw_max_bb_mxdpd2_pen), 0, eqw_max_bb_mxdpd2_pen),
#         eqw_max_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_max_ccf_avg_dpd_serious1_pen), 0, eqw_max_ccf_avg_dpd_serious1_pen),
#         eqw_max_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_ccf_dpd_occur_pct1_pen), 0, eqw_max_ccf_dpd_occur_pct1_pen),
#         eqw_max_ccf_mxdpd2_pen = ifelse(is.na(eqw_max_ccf_mxdpd2_pen), 0, eqw_max_ccf_mxdpd2_pen),
#         eqw_max_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_max_ccf_mxdpd_serious2_pen), 0, eqw_max_ccf_mxdpd_serious2_pen),
#         eqw_max_ip_avg_dpd1_pen = ifelse(is.na(eqw_max_ip_avg_dpd1_pen), 0, eqw_max_ip_avg_dpd1_pen),
#         eqw_max_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_ip_dpd_occur_pct1_pen), 0, eqw_max_ip_dpd_occur_pct1_pen),
#         eqw_max_ip_mxdpd2_pen = ifelse(is.na(eqw_max_ip_mxdpd2_pen), 0, eqw_max_ip_mxdpd2_pen),
# 
#         eqw_max_ip_avg_apd1_pen = ifelse(is.na(eqw_max_ip_avg_apd1_pen), 0, eqw_max_ip_avg_apd1_pen),
#         eqw_max_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_max_ip_avg_apd_as_pct1_pen), 0, eqw_max_ip_avg_apd_as_pct1_pen),
#         eqw_max_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_max_ip_apd_occur_pct1_pen), 0, eqw_max_ip_apd_occur_pct1_pen),
#         eqw_max_ip_mxapd2_pen = ifelse(is.na(eqw_max_ip_mxapd2_pen), 0, eqw_max_ip_mxapd2_pen),
# 
#         eqw_max_pcb_avg_dpd1_pen = ifelse(is.na(eqw_max_pcb_avg_dpd1_pen), 0, eqw_max_pcb_avg_dpd1_pen),
#         eqw_max_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_max_pcb_avg_dpd_serious1_pen), 0, eqw_max_pcb_avg_dpd_serious1_pen),
#         eqw_max_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_max_pcb_dpd_occur_pct1_pen), 0, eqw_max_pcb_dpd_occur_pct1_pen),
#         eqw_max_pcb_mxdpd2_pen = ifelse(is.na(eqw_max_pcb_mxdpd2_pen), 0, eqw_max_pcb_mxdpd2_pen),
#         eqw_max_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_max_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_max_pcb_mxdpd_avg_comp_pct2_pen),
#         eqw_max_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_max_pcb_mxdpd_serious2_pen), 0, eqw_max_pcb_mxdpd_serious2_pen)
#       ) %>%
#       mutate(
#         penalty = eqw_max_bb_avg_dpd1_pen + eqw_max_bb_dpd_occur_pct1_pen + eqw_max_bb_mxdpd2_pen +
#           eqw_max_ccf_avg_dpd_serious1_pen + eqw_max_ccf_dpd_occur_pct1_pen + eqw_max_ccf_mxdpd2_pen + eqw_max_ccf_mxdpd_serious2_pen +
#           eqw_max_ip_avg_dpd1_pen + eqw_max_ip_dpd_occur_pct1_pen + eqw_max_ip_mxdpd2_pen +
#           eqw_max_ip_avg_apd_as_pct1_pen + eqw_max_ip_apd_occur_pct1_pen + eqw_max_ip_mxapd2_pen +
#           eqw_max_pcb_avg_dpd1_pen + eqw_max_pcb_avg_dpd_serious1_pen +
#           eqw_max_pcb_dpd_occur_pct1_pen + eqw_max_pcb_mxdpd2_pen + eqw_max_pcb_mxdpd_avg_comp_pct2_pen +
#           eqw_max_pcb_mxdpd_serious2_pen
#       ) %>%
#       mutate(score_from_dpd_avg = 100 - penalty) %>%
#       mutate(
#         eqw_min_bb_avg_dpd1 = pmax(eqw_min_bb_avg_dpd, 0),
#         eqw_min_bb_dpd_occur_pct1 = pmax(eqw_min_bb_dpd_occur_pct, 0),
#         eqw_min_bb_mxdpd1 = pmax(eqw_min_bb_mxdpd, 0),
#         eqw_min_bb_mxdpd_duration1 = pmax(eqw_min_bb_mxdpd_duration, 0),
#         eqw_min_ccf_avg_dpd_serious1 = pmax(eqw_min_ccf_avg_dpd_serious, 0),
#         eqw_min_ccf_dpd_occur_pct1 = pmax(eqw_min_ccf_dpd_occur_pct, 0),
#         eqw_min_ccf_mxdpd1 = pmax(eqw_min_ccf_mxdpd, 0),
#         eqw_min_ccf_mxdpd_serious1 = pmax(eqw_min_ccf_mxdpd_serious, 0),
#         eqw_min_ccf_mxdpd_duration1 = pmax(eqw_min_ccf_mxdpd_duration, 0),
#         eqw_min_ip_avg_dpd1 = pmax(eqw_min_ip_avg_dpd, 0),
#         eqw_min_ip_dpd_occur_pct1 = pmax(eqw_min_ip_dpd_occur_pct, 0),
#         eqw_min_ip_mxdpd1 = pmax(eqw_min_ip_mxdpd, 0),
#         eqw_min_ip_mxdpd_duration1 = pmax(eqw_min_ip_mxdpd_duration, 0),
#         eqw_min_ip_avg_apd1 = pmax(eqw_min_ip_avg_apd, 0),
#         eqw_min_ip_avg_apd_as_pct1 = pmax(eqw_min_ip_avg_apd_as_pct, 0),
#         eqw_min_ip_apd_occur_pct1 = pmax(eqw_min_ip_apd_occur_pct, 0),
#         eqw_min_ip_mxapd1 = pmax(eqw_min_ip_mxapd, 0),
#         eqw_min_ip_mxapd_duration1 = pmax(eqw_min_ip_mxapd_duration, 0),
#         eqw_min_pcb_avg_dpd1 = pmax(eqw_min_pcb_avg_dpd, 0),
#         eqw_min_pcb_avg_dpd_serious1 = pmax(eqw_min_pcb_avg_dpd_serious, 0),
#         eqw_min_pcb_dpd_occur_pct1 = pmax(eqw_min_pcb_dpd_occur_pct, 0),
#         eqw_min_pcb_mxdpd1 = pmax(eqw_min_pcb_mxdpd, 0),
#         eqw_min_pcb_mxdpd_avg_comp_pct1 = pmax(eqw_min_pcb_mxdpd_avg_comp_pct, 0),
#         eqw_min_pcb_mxdpd_serious1 = pmax(eqw_min_pcb_mxdpd_serious, 0),
#         eqw_min_pcb_mxdpd_duration1 = pmax(eqw_min_pcb_mxdpd_duration, 0)
#       ) %>%
#       # transfer the dpd happen time into weight step 1
#       # lower number means either no dpd or no data, thus better than positive number
#       mutate(
#         eqw_min_bb_mxdpd_cap = ifelse(eqw_min_bb_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_bb_mxdpd_earliest_occur + eqw_min_bb_mxdpd_latest_occur)/eqw_min_bb_mxdpd_duration1)),
#         eqw_min_ccf_mxdpd_cap = ifelse(eqw_min_ccf_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_ccf_mxdpd_earliest_occur + eqw_min_ccf_mxdpd_latest_occur)/eqw_min_ccf_mxdpd_duration1)),
#         eqw_min_ip_mxdpd_cap = ifelse(eqw_min_ip_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_ip_mxdpd_earliest_occur + eqw_min_ip_mxdpd_latest_occur)/eqw_min_ip_mxdpd_duration1)),
#         eqw_min_ip_mxapd_cap = ifelse(eqw_min_ip_mxapd_duration1 == 0, 0, 1/abs((eqw_min_ip_mxapd_earliest_occur + eqw_min_ip_mxapd_latest_occur)/eqw_min_ip_mxapd_duration1)),
#         eqw_min_pcb_mxdpd_cap = ifelse(eqw_min_pcb_mxdpd_duration1 == 0, 0, 1/abs((eqw_min_pcb_mxdpd_earliest_occur + eqw_min_pcb_mxdpd_latest_occur)/eqw_min_pcb_mxdpd_duration1))
#       ) %>%
#       # transfer the dpd happen time into weight step 2
#       mutate(
#         eqw_min_bb_mxdpd_wgt = eqw_min_bb_mxdpd_cap/max(eqw_min_bb_mxdpd_cap, na.rm = TRUE),
#         eqw_min_ccf_mxdpd_wgt = eqw_min_ccf_mxdpd_cap/max(eqw_min_ccf_mxdpd_cap, na.rm = TRUE),
#         eqw_min_ip_mxdpd_wgt = eqw_min_ip_mxdpd_cap/max(eqw_min_ip_mxdpd_cap, na.rm = TRUE),
#         eqw_min_ip_mxapd_wgt = eqw_min_ip_mxapd_cap/max(eqw_min_ip_mxapd_cap, na.rm = TRUE),
#         eqw_min_pcb_mxdpd_wgt = eqw_min_pcb_mxdpd_cap/max(eqw_min_pcb_mxdpd_cap, na.rm = TRUE)
#       ) %>%
#       # calculate weighted mxdpd related
#       mutate(
#         eqw_min_bb_mxdpd2=eqw_min_bb_mxdpd1*eqw_min_bb_mxdpd_wgt,
#         eqw_min_ccf_mxdpd2=eqw_min_ccf_mxdpd1*eqw_min_ccf_mxdpd_wgt,
#         eqw_min_ccf_mxdpd_serious2=eqw_min_ccf_mxdpd_serious1*eqw_min_ccf_mxdpd_wgt,
#         eqw_min_ip_mxdpd2=eqw_min_ip_mxdpd1*eqw_min_ip_mxdpd_wgt,
#         eqw_min_ip_mxapd2=eqw_min_ip_mxapd1*eqw_min_ip_mxapd_wgt,
#         eqw_min_pcb_mxdpd2=eqw_min_pcb_mxdpd1*eqw_min_pcb_mxdpd_wgt,
#         eqw_min_pcb_mxdpd_avg_comp_pct2=eqw_min_pcb_mxdpd_avg_comp_pct1*eqw_min_pcb_mxdpd_wgt,
#         eqw_min_pcb_mxdpd_serious2=eqw_min_pcb_mxdpd_serious1*eqw_min_pcb_mxdpd_wgt
#       ) %>%
#       # calculate weight for each dpd related
#       mutate(
#         eqw_min_bb_avg_dpd1_wgt =  eqw_min_bb_avg_dpd1/max(eqw_min_bb_avg_dpd1, na.rm = TRUE),
#         eqw_min_bb_dpd_occur_pct1_wgt =  eqw_min_bb_dpd_occur_pct1/max(eqw_min_bb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_min_bb_mxdpd2_wgt =  eqw_min_bb_mxdpd2/max(eqw_min_bb_mxdpd2, na.rm = TRUE),
#         eqw_min_ccf_avg_dpd_serious1_wgt =  eqw_min_ccf_avg_dpd_serious1/max(eqw_min_ccf_avg_dpd_serious1, na.rm = TRUE),
#         eqw_min_ccf_dpd_occur_pct1_wgt =  eqw_min_ccf_dpd_occur_pct1/max(eqw_min_ccf_dpd_occur_pct1, na.rm = TRUE),
#         eqw_min_ccf_mxdpd2_wgt =  eqw_min_ccf_mxdpd2/max(eqw_min_ccf_mxdpd2, na.rm = TRUE),
#         eqw_min_ccf_mxdpd_serious2_wgt =  eqw_min_ccf_mxdpd_serious2/max(eqw_min_ccf_mxdpd_serious2, na.rm = TRUE),
#         eqw_min_ip_avg_dpd1_wgt =  eqw_min_ip_avg_dpd1/max(eqw_min_ip_avg_dpd1, na.rm = TRUE),
#         eqw_min_ip_dpd_occur_pct1_wgt =  eqw_min_ip_dpd_occur_pct1/max(eqw_min_ip_dpd_occur_pct1, na.rm = TRUE),
#         eqw_min_ip_mxdpd2_wgt =  eqw_min_ip_mxdpd2/max(eqw_min_ip_mxdpd2, na.rm = TRUE),
#         eqw_min_ip_avg_apd1_wgt = eqw_min_ip_avg_apd1/max(eqw_min_ip_avg_apd1, na.rm = TRUE),
#         eqw_min_ip_avg_apd_as_pct1_wgt = eqw_min_ip_avg_apd_as_pct1/max(eqw_min_ip_avg_apd_as_pct1, na.rm = TRUE),
#         eqw_min_ip_apd_occur_pct1_wgt = eqw_min_ip_apd_occur_pct1/max(eqw_min_ip_apd_occur_pct1, na.rm = TRUE),
#         eqw_min_ip_mxapd2_wgt =  eqw_min_ip_mxapd2/max(eqw_min_ip_mxapd2, na.rm = TRUE),
#         eqw_min_pcb_avg_dpd1_wgt =  eqw_min_pcb_avg_dpd1/max(eqw_min_pcb_avg_dpd1, na.rm = TRUE),
#         eqw_min_pcb_avg_dpd_serious1_wgt =  eqw_min_pcb_avg_dpd_serious1/max(eqw_min_pcb_avg_dpd_serious1, na.rm = TRUE),
#         eqw_min_pcb_dpd_occur_pct1_wgt =  eqw_min_pcb_dpd_occur_pct1/max(eqw_min_pcb_dpd_occur_pct1, na.rm = TRUE),
#         eqw_min_pcb_mxdpd2_wgt =  eqw_min_pcb_mxdpd2/max(eqw_min_pcb_mxdpd2, na.rm = TRUE),
#         eqw_min_pcb_mxdpd_avg_comp_pct2_wgt =  eqw_min_pcb_mxdpd_avg_comp_pct2/max(eqw_min_pcb_mxdpd_avg_comp_pct2, na.rm = TRUE),
#         eqw_min_pcb_mxdpd_serious2_wgt =  eqw_min_pcb_mxdpd_serious2/max(eqw_min_pcb_mxdpd_serious2, na.rm = TRUE)
#       ) %>%
#       # calculate penalty for each dpd related
#       mutate(
#         eqw_min_bb_avg_dpd1_pen = 10 * eqw_min_bb_avg_dpd1_wgt,
#         eqw_min_bb_dpd_occur_pct1_pen = 10 * eqw_min_bb_dpd_occur_pct1_wgt,
#         eqw_min_bb_mxdpd2_pen = 10 * eqw_min_bb_mxdpd2_wgt,
#         eqw_min_ccf_avg_dpd_serious1_pen = 10 * eqw_min_ccf_avg_dpd_serious1_wgt,
#         eqw_min_ccf_dpd_occur_pct1_pen = 10 * eqw_min_ccf_dpd_occur_pct1_wgt,
#         eqw_min_ccf_mxdpd2_pen = 10 * eqw_min_ccf_mxdpd2_wgt,
#         eqw_min_ccf_mxdpd_serious2_pen = 10 * eqw_min_ccf_mxdpd_serious2_wgt,
#         eqw_min_ip_avg_dpd1_pen = 10 * eqw_min_ip_avg_dpd1_wgt,
#         eqw_min_ip_dpd_occur_pct1_pen = 10 * eqw_min_ip_dpd_occur_pct1_wgt,
#         eqw_min_ip_mxdpd2_pen = 10 * eqw_min_ip_mxdpd2_wgt,
#         eqw_min_ip_avg_apd1_pen = 10 * eqw_min_ip_avg_apd1_wgt,
#         eqw_min_ip_avg_apd_as_pct1_pen = 10 * eqw_min_ip_avg_apd_as_pct1_wgt,
#         eqw_min_ip_apd_occur_pct1_pen = 10 * eqw_min_ip_apd_occur_pct1_wgt,
#         eqw_min_ip_mxapd2_pen = 10 * eqw_min_ip_mxapd2_wgt,
#         eqw_min_pcb_avg_dpd1_pen = 10 * eqw_min_pcb_avg_dpd1_wgt,
#         eqw_min_pcb_avg_dpd_serious1_pen = 10 * eqw_min_pcb_avg_dpd_serious1_wgt,
#         eqw_min_pcb_dpd_occur_pct1_pen = 10 * eqw_min_pcb_dpd_occur_pct1_wgt,
#         eqw_min_pcb_mxdpd2_pen = 10 * eqw_min_pcb_mxdpd2_wgt,
#         eqw_min_pcb_mxdpd_avg_comp_pct2_pen = 10 * eqw_min_pcb_mxdpd_avg_comp_pct2_wgt,
#         eqw_min_pcb_mxdpd_serious2_pen = 10 * eqw_min_pcb_mxdpd_serious2_wgt
#       ) %>%
#       # remove na
#       mutate(
#         eqw_min_bb_avg_dpd1_pen = ifelse(is.na(eqw_min_bb_avg_dpd1_pen), 0, eqw_min_bb_avg_dpd1_pen),
#         eqw_min_bb_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_bb_dpd_occur_pct1_pen), 0, eqw_min_bb_dpd_occur_pct1_pen),
#         eqw_min_bb_mxdpd2_pen = ifelse(is.na(eqw_min_bb_mxdpd2_pen), 0, eqw_min_bb_mxdpd2_pen),
#         eqw_min_ccf_avg_dpd_serious1_pen = ifelse(is.na(eqw_min_ccf_avg_dpd_serious1_pen), 0, eqw_min_ccf_avg_dpd_serious1_pen),
#         eqw_min_ccf_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_ccf_dpd_occur_pct1_pen), 0, eqw_min_ccf_dpd_occur_pct1_pen),
#         eqw_min_ccf_mxdpd2_pen = ifelse(is.na(eqw_min_ccf_mxdpd2_pen), 0, eqw_min_ccf_mxdpd2_pen),
#         eqw_min_ccf_mxdpd_serious2_pen = ifelse(is.na(eqw_min_ccf_mxdpd_serious2_pen), 0, eqw_min_ccf_mxdpd_serious2_pen),
#         eqw_min_ip_avg_dpd1_pen = ifelse(is.na(eqw_min_ip_avg_dpd1_pen), 0, eqw_min_ip_avg_dpd1_pen),
#         eqw_min_ip_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_ip_dpd_occur_pct1_pen), 0, eqw_min_ip_dpd_occur_pct1_pen),
#         eqw_min_ip_mxdpd2_pen = ifelse(is.na(eqw_min_ip_mxdpd2_pen), 0, eqw_min_ip_mxdpd2_pen),
# 
#         eqw_min_ip_avg_apd1_pen = ifelse(is.na(eqw_min_ip_avg_apd1_pen), 0, eqw_min_ip_avg_apd1_pen),
#         eqw_min_ip_avg_apd_as_pct1_pen = ifelse(is.na(eqw_min_ip_avg_apd_as_pct1_pen), 0, eqw_min_ip_avg_apd_as_pct1_pen),
#         eqw_min_ip_apd_occur_pct1_pen = ifelse(is.na(eqw_min_ip_apd_occur_pct1_pen), 0, eqw_min_ip_apd_occur_pct1_pen),
#         eqw_min_ip_mxapd2_pen = ifelse(is.na(eqw_min_ip_mxapd2_pen), 0, eqw_min_ip_mxapd2_pen),
# 
#         eqw_min_pcb_avg_dpd1_pen = ifelse(is.na(eqw_min_pcb_avg_dpd1_pen), 0, eqw_min_pcb_avg_dpd1_pen),
#         eqw_min_pcb_avg_dpd_serious1_pen = ifelse(is.na(eqw_min_pcb_avg_dpd_serious1_pen), 0, eqw_min_pcb_avg_dpd_serious1_pen),
#         eqw_min_pcb_dpd_occur_pct1_pen = ifelse(is.na(eqw_min_pcb_dpd_occur_pct1_pen), 0, eqw_min_pcb_dpd_occur_pct1_pen),
#         eqw_min_pcb_mxdpd2_pen = ifelse(is.na(eqw_min_pcb_mxdpd2_pen), 0, eqw_min_pcb_mxdpd2_pen),
#         eqw_min_pcb_mxdpd_avg_comp_pct2_pen = ifelse(is.na(eqw_min_pcb_mxdpd_avg_comp_pct2_pen), 0, eqw_min_pcb_mxdpd_avg_comp_pct2_pen),
#         eqw_min_pcb_mxdpd_serious2_pen = ifelse(is.na(eqw_min_pcb_mxdpd_serious2_pen), 0, eqw_min_pcb_mxdpd_serious2_pen)
#       ) %>%
#       mutate(
#         penalty = eqw_min_bb_avg_dpd1_pen + eqw_min_bb_dpd_occur_pct1_pen + eqw_min_bb_mxdpd2_pen +
#           eqw_min_ccf_avg_dpd_serious1_pen + eqw_min_ccf_dpd_occur_pct1_pen + eqw_min_ccf_mxdpd2_pen + eqw_min_ccf_mxdpd_serious2_pen +
#           eqw_min_ip_avg_dpd1_pen + eqw_min_ip_dpd_occur_pct1_pen + eqw_min_ip_mxdpd2_pen +
#           eqw_min_ip_avg_apd_as_pct1_pen + eqw_min_ip_apd_occur_pct1_pen + eqw_min_ip_mxapd2_pen +
#           eqw_min_pcb_avg_dpd1_pen + eqw_min_pcb_avg_dpd_serious1_pen +
#           eqw_min_pcb_dpd_occur_pct1_pen + eqw_min_pcb_mxdpd2_pen + eqw_min_pcb_mxdpd_avg_comp_pct2_pen +
#           eqw_min_pcb_mxdpd_serious2_pen
#       ) %>%
#       mutate(score_from_dpd_min = 100 - penalty)
# 
#     res <- dataset
#     res$score_from_dpd_avg <- res_tmp$score_from_dpd_avg
#     res$score_from_dpd_max <- res_tmp$score_from_dpd_max
#     res$score_from_dpd_min <- res_tmp$score_from_dpd_min
# 
#   } else {
#     res <- dataset
#   }
#   return(res)
# }
# 
# FinalTouch <- function(dataset, rmv_fs){
#   ##
#   # Feature removal
#   if(length(rmv_fs) == 0) prdctrs1_t <- dataset else prdctrs1_t <- dataset %>% select(-one_of(rmv_fs))
# 
#   ##
#   # Scale and OHE
#   prdctrs1_t_peek <- DataInspection(prdctrs1_t)
#   num_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class != "character","feature"]
#   chr_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class == "character","feature"]
# 
#   data_scaled_t <- DataScale(num_cols, prdctrs1_t, rep_na = TRUE, rep_na_with = 0)
#   data_scaled_ohe_t <- OHE(chr_cols, data_scaled_t)
#   prdctrs2_t_peek <- DataInspection(data_scaled_ohe_t)   # info only
# 
#   res <- list(
#     peek1 = prdctrs1_t_peek,
#     peek2 = prdctrs2_t_peek,
#     coredata = data_scaled_ohe_t
#   )
# 
#   return(res)
# }
# 
# ##
# # Manipulation
# ##
# 
# ##
# # Take sample
# full_target <- full_target[, , drop = FALSE]
# full_predictors <- full_predictors[, , drop = FALSE]
# 
# full_predictors <- AddFeatures(full_predictors, do = TRUE)
# 
# ##
# # Take validation set
# allrows <- 1:nrow(full_target)
# set.seed(123)
# val_idx <- sample(allrows, floor(n_val_size*nrow(full_target)), replace = FALSE)
# rem_idx <- allrows[!(allrows %in% val_idx)]
# 
# rem_target <- full_target[rem_idx, , drop = FALSE]
# rem_predictors <- full_predictors[rem_idx,]
# 
# val_target <- full_target[val_idx, , drop = FALSE]
# val_predictors <- full_predictors[val_idx,]
# 
# rm(full_target, full_predictors)
# gc()
# 
# ##
# # Scale and OHE
# prdctrs_train <- FinalTouch(rem_predictors[,], rmv_fs)
# train_peek1 <- prdctrs_train$peek1
# train_peek2 <- prdctrs_train$peek2
# 
# ##
# # SMOTE data so the data can be balanced
# if(smote_flag){
#   bd_tmp <- cbind.data.frame(rem_target, prdctrs_train$coredata, stringsAsFactors = FALSE)
#   bd_fnl <- ROSE::ROSE(StrTarget ~., data = bd_tmp,
#                        N = floor(1.5 * sum(bd_tmp$StrTarget == unique(bd_tmp$StrTarget)[1])),
#                        p = 0.25,
#                        seed = 1)$data
# 
#   prdctrs_train$coredata <- bd_fnl[,-1]
#   rem_target <- bd_fnl[,1,drop=FALSE]
#   rm(bd_tmp, bd_fnl)
#   gc()
# }
# 
# ##
# # Format data for the model
# fmtd_data <- FormatData4Model(
#   prdctrs = prdctrs_train$coredata,
#   tgt = rem_target,
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# 
# ##
# # Run validation set
# ##
# prdctrs_val <- FinalTouch(val_predictors[,], rmv_fs)
# 
# ##
# # Format data for the model
# fmtd_vald <- FormatData4Model(
#   prdctrs = prdctrs_val$coredata,
#   tgt = val_target[,,drop = FALSE], 
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# 
# ##
# # Scale and OHE
# prdctrs_test <- FinalTouch(full_testdata, rmv_fs)
# test_peek1 <- prdctrs_test$peek1
# test_peek2 <- prdctrs_test$peek2

# save(fmtd_data, fmtd_vald, tuning_pars, static_pars, tgt_map, file = "fmtd_data.RData")
setwd(proj_dir)
load("fmtd_data03.RData")
load("lightgbm_5000n_features.RData")
rm(prdctrs_test)
gc()

##
# training parameters
tuning_pars <- expand.grid(
  num_leaves = c(31),
  min_data_in_leaf = c(20),
  max_depth = c(-1),
  bagging_fraction = c(0.7),
  bagging_freq = c(0),
  feature_fraction = c(0.7),
  max_bin = c(255),
  learning_rate = c(0.01),
  num_iterations = c(5000),
  lambda_l1 = c(0),
  lambda_l2 = c(0),
  min_gain_to_split = c(0),
  early_stopping_round = c(0),
  num_threads = c(6)
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
# Select features
prdctrs_sltd <- fmtd_data$predictors[, top_feats_100]
tgts <- fmtd_data$target
rm(fmtd_data)
gc()

##
# Train
br <- GridSearchLgbm2(
  proj = "HCDR",
  model_name = "lightgbm",
  dataset = prdctrs_sltd,
  labels = tgts,
  job = "bc",
  val_size = floor(length(tgts)/20),
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
var_imp <- lightgbm::lgb.importance(br$models[[1]][[1]])
var_imp_plot <- lightgbm::lgb.plot.importance(var_imp, top_n = 10)

##
# Plot learning curve
eval_res <- data.frame(
  iter = 1:length(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
  train_logloss = unlist(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
  test_logloss = unlist(br$models[[1]][[1]]$record_evals$test$binary_logloss$eval)
)
mdl_lc <- FitPlot("lightgbm tree", "bc",
                  eval_res, "iter", "train_logloss", "test_logloss")
mdl_lc
rm(fmtd_data)
gc()

##
# Predict
val_prdctrs_sltd <- fmtd_vald$predictors[, top_feats_100]
val_tgts <- fmtd_vald$target
rm(fmtd_vald)
gc()

val_pred <- predict(br$models[[1]][[1]], val_prdctrs_sltd)
rocr_pred <- ROCR::prediction(val_pred, val_tgts)
rocr_perf <- ROCR::performance(rocr_pred, measure = "auc")
print(rocr_perf@y.values)

##
# Run test data
##
if(TRUE){
  
  rm(fmtd_data, fmtd_vald)
  load("fmtd_data03.RData")
  rm(fmtd_data, fmtd_vald)
  gc()
  
  test_prdctrs_sltd <- prdctrs_test$coredata[, top_feats_100]
  rm(prdctrs_test)
  gc()
  
  ##
  # Load model
  # load(paste0(proj_dir, "Output/Model/", mdl_nm))
  pred_res <- predict(br$models[[1]][[1]], as.matrix(prdctrs_test$coredata))
  pred_res2 <- as.data.frame(pred_res)
  
  out <- data.frame(SK_ID_CURR = test_id,
                    TARGET = pred_res2[,1],
                    stringsAsFactors = FALSE)
  write.csv(out, paste0(proj_dir, "Submission/",
                        "run_result_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"),
                        ".csv"), 
            row.names = FALSE)
}

# source("/home/tli/projects/ShinyMLHome/Manual/manual_lightgbm.R")
