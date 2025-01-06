
pacman::p_load(data.table, dplyr)

source("../helpers.R")

# variables to select
metrics <- c("pvalue", "NFI","PNFI","TLI","RFI","IFI","RNI","CFI","RMSEA")
conditions_null <- c("n_person_factor","type","n_time_factor","n_node_factor")
conditions_mean <- c("mean_trend_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_rt <- c("rewire_temporal_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_rc <- c("rewire_contemporaneous_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_nt <- c("rewire_temporal_factor","type","n_person_factor","n_time_factor","n_node_factor")
conditions_nc <- c("rewire_contemporaneous_factor","type","n_person_factor","n_time_factor","n_node_factor")

# create list to store data
dt_ls <- vector("list", 6)
names(dt_ls) <- c("dt_null","dt_mean", 
                  "dt_rt","dt_rc", 
                  "dt_nt","dt_nc")

# ----- null condition -----

# read data
simres_null <- results_reader("../Results/sim_1_null")$data
simres_null_0.8 <- results_reader("../revision/results/propPos_0.8/sim_1_null")$data
simres_null_nei.2 <- results_reader("../revision/results/nei_2/sim_1_null")$data

# select variables 
dt_null <- simres_null[, c(..conditions_null, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_null_0.8 <- simres_null_0.8[, c(..conditions_null, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_null_nei.2 <- simres_null_nei.2[, c(..conditions_null, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_null <- rbind(dt_null, dt_null_0.8, dt_null_nei.2)

# ----- sim 2 mean trends -----

# read data
sim_2a_mean_trend <- results_reader("../Results/sim_2a_mean_trend")$data
sim_2a_mean_trend_0.8 <- results_reader("../revision/results/propPos_0.8/sim_2a_mean_trend")$data
sim_2a_mean_trend_nei.2 <- results_reader("../revision/results/nei_2/sim_2a_mean_trend")$data

# select variables 
dt_mean <- sim_2a_mean_trend[, c(..conditions_mean, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_mean_0.8 <- sim_2a_mean_trend_0.8[, c(..conditions_mean, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_mean_nei.2 <- sim_2a_mean_trend_nei.2[, c(..conditions_mean, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_mean <- rbind(dt_mean, dt_mean_0.8, dt_mean_nei.2)

# ----- sim 3 mismatch temporal -----

# read data
sim_3a_rewire_temp <- results_reader("../Results/sim_3a_rewire_temp")$data
sim_3a_rewire_temp_0.8 <- results_reader("../revision/results/propPos_0.8/sim_3a_rewire_temp")$data
sim_3a_rewire_temp_nei.2 <- results_reader("../revision/results/nei_2/sim_3a_rewire_temp")$data

# select variables 
dt_rt <- sim_3a_rewire_temp[, c(..conditions_rt, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_rt_0.8 <- sim_3a_rewire_temp_0.8[, c(..conditions_rt, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_rt_nei.2 <- sim_3a_rewire_temp_nei.2[, c(..conditions_rt, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_rt <- rbind(dt_rt, dt_rt_0.8, dt_rt_nei.2)

# ----- sim 4 mismatch contemporaneous -----

# read data
sim_3b_rewire_cont <- results_reader("../Results/sim_3b_rewire_cont")$data
sim_3b_rewire_cont_0.8 <- results_reader("../revision/results/propPos_0.8/sim_3b_rewire_cont")$data
sim_3b_rewire_cont_nei.2 <- results_reader("../revision/results/nei_2/sim_3b_rewire_cont")$data

names(sim_3b_rewire_cont)[c(54, 51:53)]

# select variables 
dt_rc <- sim_3b_rewire_cont[, c(..conditions_rc, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_rc_0.8 <- sim_3b_rewire_cont_0.8[, c(..conditions_rc, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_rc_nei.2 <- sim_3b_rewire_cont_nei.2[, c(..conditions_rc, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_rc <- rbind(dt_rc, dt_rc_0.8, dt_rc_nei.2)

# ----- sim 5 nonstationary temporal -----

# read data
sim_4a_nonstationary_temp <- results_reader("../Results/sim_4a_nonstationary_temp")$data
sim_4a_nonstationary_temp_0.8 <- results_reader("../revision/results/propPos_0.8/sim_4a_nonstationary_temp")$data
sim_4a_nonstationary_temp_nei.2 <- results_reader("../revision/results/nei_2/sim_4a_nonstationary_temp")$data

# select variables 
dt_nt <- sim_4a_nonstationary_temp[, c(..conditions_nt, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_nt_0.8 <- sim_4a_nonstationary_temp_0.8[, c(..conditions_nt, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_nt_nei.2 <- sim_4a_nonstationary_temp_nei.2[, c(..conditions_nt, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_nt <- rbind(dt_nt, dt_nt_0.8, dt_nt_nei.2)

# ----- sim 6 nonstationary contemporaneous -----

# read data
sim_4b_nonstationary_cont <- results_reader("../Results/sim_4b_nonstationary_cont")$data
sim_4b_nonstationary_cont_0.8 <- results_reader("../revision/results/propPos_0.8/sim_4b_nonstationary_cont")$data
sim_4b_nonstationary_cont_nei.2 <- results_reader("../revision/results/nei_2/sim_4b_nonstationary_cont")$data

# select variables 
dt_nc <- sim_4b_nonstationary_cont[, c(..conditions_nc, ..metrics)][, condition := "nei = 1 & propPos = 0.5"]
dt_nc_0.8 <- sim_4b_nonstationary_cont_0.8[, c(..conditions_nc, ..metrics)][, condition := "nei = 1 & propPos = 0.8"]
dt_nc_nei.2 <- sim_4b_nonstationary_cont_nei.2[, c(..conditions_nc, ..metrics)][, condition := "nei = 2 & propPos = 0.5"]

# merge and store in list
dt_ls$dt_nc <- rbind(dt_nc, dt_nc_0.8, dt_nc_nei.2)

# save data list as RDS file
saveRDS(dt_ls, file = "simres.RDS")
