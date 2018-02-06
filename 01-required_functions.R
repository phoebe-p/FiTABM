## Contents
# 1. Historical scenarios & general - functions
# 2. Future scenarios - functions
# 3. Miscellaneous


##################################################################################
################# 1. Historical scenarios & general - functions ##################
##################################################################################
# 1.1 Set-up 
# 1.2 Agent/modelling functions
# 1.3 Cost calculation
# 1.4 Processing

#------------------------------- 1.1 Set-up -------------------------------------#

load_data <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar,
                      dep_caps = F, cap){
  # end_date: date up to which simulation will run
  # FiT_type: real_h, linear, perc_red
  if(missing(start_date)) start_date <- "1apr2010"
  if(missing(end_date)) end_date <- "1sep2016"
  if(missing(FiT_end_date)) FiT_end_date <- end_date
  if(missing(FiT_type)) {
    FiT_type <- "real_h"
  }
  if(missing(red_frac)) red_frac <- 0.03
  if(missing(init_fit)) init_fit <- 49.43 
  if(missing(final_fit)) final_fit <- 4.18
  if(missing(exp_tar)) exp_tar <- 4
  
  load_libraries()
  # DO NOT USE plyr LIBRARY 
  if (FiT_type == "real_h"){
    start_date <- "1jan2010"
    FiT_end_date <- "1oct2016"
    end_date <- "1oct2016"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
  }
  
  FiT_end_date <- dmy(FiT_end_date)
  end_date <- dmy(end_date)
  start_date <- dmy(start_date)
  
  if(!is.Date(end_date)) stop("Your end date is not a valid date")
  if(!(FiT_type == "real_h" | FiT_type == "perc_red" | FiT_type == "ann_perc_red" | FiT_type == "linear")) stop("FiT type not recognised")
  if(init_fit < 0 | final_fit <0) stop("FiTs have to be positive")
  
  
  elec_price_time <<- read_csv("Data/electricityprices.csv", col_names = F, col_types = cols())
  elec_price_time[7,] <<- c(2016, 17.33) # projection for 2016
  
  owner_occupiers <<- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = cols()) %>% mutate(X2 = X2*1000)
  
  #---------------------------------------------------------#
  # Load factor data
  
  LF <- read_csv("Data/LF_mean.csv", col_types = cols())
  
  # Filter out empty rows, group by region, find mean & std. dev. over all years, arrange
  # in alphabetical order.
  LF <<- LF %>% filter(!is.na(Region)) %>% group_by(Region) %>% 
    summarise(LF = mean(Weighted.mean), std_dev = sd(Weighted.mean)) %>% 
    arrange(Region) %>% mutate(Label = LETTERS[1:11])
  
  
  
  #---------------------------------------------------------#
  # Feed-in tariff data
  
  set_FiT(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar)
  
  #---------------------------------------------------------#
  # Deployment caps
  run_w_cap <<- F
  if (dep_caps == T) {
    set_dep_caps(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar)
    dep_cap_0 <<- dep_cap
    FiT_0 <<- FiT
    run_w_cap <<- T
  }
  #---------------------------------------------------------#
  
  
  # Population data
  
  population <- read_csv("Data/population_mid2012.csv", col_names = FALSE, col_types = cols()) %>% arrange(X1)
  
  region_weights <<- population$X2/sum(population$X2)
  
  rm(population)
  
  #---------------------------------------------------------#
  # PV cost data
  kW_price <<- read_csv('Data/PV_cost_data_est.csv', col_names = FALSE, col_types = cols()) %>% mutate(X1 = dmy(X1)) %>%
    filter(X1 >= start_date) 
  
  
  #---------------------------------------------------------#
  # Electricity use data
  
  means <- read_csv("Data/mean-electricity.csv", col_types = cols())
  medians <- read_csv("Data/median-electricity.csv", col_types = cols())
  
  mus <<- data.frame(matrix(ncol = 5, nrow = 10))
  sigmas <<- data.frame(matrix(ncol = 5, nrow = 10))
  
  for (i in 1:5){
    mean <- means[[i+1]]
    median <- medians[[i+1]]
    mus[[i]] <<- log(median) 
    sigmas[[i]] <<- sqrt(2*log(mean/median))
  }
  
  income_thresh <<- means$income
  
  rm(mean, median, i, means, medians)
  
 
  #---------------------------------------------------------#
  # Real deployment data
  if(exists("deployment")) {
    cat("\nDeployment data is already loaded - if you want to reload it, delete the 'deployment' variable\n")
  } else {
    ts <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    all_inst_cap <- process_inst_data() %>% 
      filter(technology_type == "Photovoltaic", installed_capacity <= 10, installationtype == "Domestic")
    current_cap <- vector(length = length(ts))
    avg_cap <- vector(length = length(ts))
    for (i in 1:length(ts)) {
      date_now <- ts[i] + months(1)
      installed_now <- filter(all_inst_cap, commissioned_date < date_now)
      current_cap[i] <- sum(installed_now$installed_capacity)
      avg_cap[i] <- current_cap[i]/nrow(installed_now)
    }
    
    
    deployment <<- data.frame(time_series = dmy("01feb2010") + months(0:(length(ts)-1)), 
                              real_cap = current_cap/1000, avg_cap = avg_cap)
    rm(all_inst_cap, installed_now, current_cap, date_now)
  }
  #---------------------------------------------------------#
  
}

load_libraries <- function(){
  library(tidyverse)
  library(stringr)
  library(reshape2)
  library(lubridate)
  library(magrittr)
}


process_inst_data <- function(){
  a <- read_csv("Data/all_inst_1.csv", skip = 2, col_types = cols())
  b <- read_csv("Data/all_inst_2.csv", skip = 2, col_types = cols())
  
  all_inst <- rbind(a, b)
  
  all_inst$InstallationType <- as.factor(all_inst$InstallationType)
  
  names(all_inst) %<>% str_replace_all(" \\(.*\\)", "") %>% str_replace_all(" ", "_") %>% str_to_lower 
  
  all_inst %<>% filter(technology_type == "Photovoltaic")
  
  all_inst %<>% select(technology_type, installed_capacity, commissioned_date, installationtype) 
  
  all_inst$commissioned_date %<>% dmy
  
  return(all_inst)
}

set_FiT <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar){
  if (end_date == FiT_end_date){
    FiT_zero <- NULL
  } else {
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
  }
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  if (FiT_type == "real_h"){
    FiT <<- rbind(read_csv("Data/FiT_levels.csv", col_types = cols()) %>% mutate(time_series = dmy(time_series)) %>% 
                    filter(time_series <= end_date), FiT_zero)
  }
  
  if (FiT_type == "linear"){
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = seq(init_fit, final_fit, length.out = length(time_series)),
                             FiT_large = seq(init_fit, final_fit, length.out = length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  
  if (FiT_type == "perc_red") {
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = geomSeries(init_fit, 1-red_frac, length(time_series)),
                             FiT_large = geomSeries(init_fit, 1-red_frac, length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "ann_perc_red") {
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    year_series <- year(seq(dmy("01jan2010"), end_date, by = '1 year'))
    FiT_yr <- geomSeries(init_fit, 1-red_frac, length(year_series))
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT <- sapply(FiT$time_series, function(x) FiT_yr[which(year_series == year(x))])
    FiT <- FiT %>% mutate(FiT_large = FiT, exp_tar = exp_tar)
    FiT <<- rbind(FiT, FiT_zero)
  }
  
  if(FiT_type == "dep_cap") {
    
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT[1] <- init_fit
    FiT <<- rbind(FiT, FiT_zero)
  }
}

geomSeries <- function(a, r, n){
  series <- vector(length = n)
  for (i in 1:n) series[i] = a*r^(i-1)
  series
}

set_dep_caps <- function(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar) {
  
  if (FiT_type == "real_f"){
    
    dep_cap <- read_csv("Data/real_dep_cap.csv", skip = 1, col_names = c("q_dates", "orig_cap", "FiT"), col_types = cols()) 
    dep_cap %<>% mutate(cap = orig_cap, inst_cap = NA) %>%
      mutate(q_dates = dmy(q_dates))
    FiT_list <- dep_cap$FiT
    dep_cap_n <- dep_cap %<>% select(q_dates, orig_cap, cap, inst_cap)
    
  } else if (FiT_type == "real_f_ext") {
    dep_cap <- read_csv("Data/real_dep_cap.csv", skip = 1, col_names = c("q_dates", "orig_cap", "FiT"), 
                        col_types = cols()) %>% mutate(q_dates = dmy(q_dates)) %>% select(q_dates, orig_cap)
    dep_cap_ext <- data.frame(q_dates = seq(dmy("1apr2019"), dmy("1jan2021"), by = '3 month'),
                              orig_cap = 62.1:69.1)
    dep_cap <- rbind(dep_cap, dep_cap_ext)
    dep_cap_n <- dep_cap %>% mutate(cap = orig_cap, inst_cap = NA) 
    FiT_list <- c(seq(4.18, by = -0.07, length.out = 18), 0)
  } else {  
    dep_cap_n <- data.frame(q_dates = seq(start_date, FiT_end_date, by = '3 months'), 
                            orig_cap = cap, cap = cap, inst_cap = NA)
    FiT_list <- FiT$FiT[FiT$time_series %in% dep_cap_n$q_dates]
  }
  
  FiT_new <- rep(FiT_list, 3)
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  FiT_new <- FiT_new[order(match(FiT_new, FiT_list))][1:length(time_series)]
  FiT_n <- data.frame(time_series = time_series, FiT = FiT_new,
                      FiT_large = FiT_new, exp_tar = exp_tar)
  FiT_list_n <- c(FiT_list, 0)
  
  if(FiT_end_date != end_date) {
    dep_cap_zero <- data.frame(q_dates = seq(tail(dep_cap_n$q_dates, 1) + months(3), end_date, by = '3 months'),
                               orig_cap = 0, cap = 0, inst_cap = NA)
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
    dep_cap <<- rbind(dep_cap_n, dep_cap_zero)
    FiT_list <<- FiT_list
    FiT <<- rbind(FiT_n, FiT_zero)
  } else {
    dep_cap <<- dep_cap_n
    FiT <<- FiT_n
    FiT_list <<- FiT_list_n
    
  }
}


initialise_vars <- function() {
  avg_u <<- NULL
  cost <<- NULL
  tot_cost <<- NULL
  cost_priv <<- NULL
  tot_cost_priv <<- NULL
  LCOE_avg <<- NULL
  LCOE_data <<- NULL
  if (run_w_cap == TRUE) FiT_levels <<- NULL
}

#------------------------- 1.2 Agent/model functions ----------------------------#

Household_Agent <- function(a, b, c, d) {
  # a = Y for adopter, N for non-adopter (char)
  # b = income
  # c = household size
  # d = UK region
  structure(list(factor(a, levels= c("Y", "N")), b, as.integer(c), 
                 factor(d, levels= c(LETTERS[1:11])), 
                 NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                 NULL),  
            class = "Household", 
            names = c("status", "income", "size", "region", "LF", "consumption", 
                      "inst_cap", "network", "u_inc", "u_ec", "u_soc", "u_cap", "u_tot", "FiT", "exp_tar", "date"))
}

extract <- function(x, str) { # x is adopters or agents (a list of Household objects)
  if (length(x) > 0) unname(unlist(sapply(x, function (x) x[str])))
}

assign_income <- function() {
  rlnorm(1, 10.06654, 0.574439)
}

assign_size <- function() {
  sample(1:5, size = 1, prob = c(0.29, 0.35, 0.16, 0.13, 0.07))
}

assign_region <- function() {
  sample(LETTERS[1:11], size = 1, prob = region_weights)
}

assign_soc_network <- function(A, n_ag, n_l) {
  A$network <- sample(1:n_ag, n_l)
  return(A)
}

assign_LF <- function(A) {
  A$LF <- LF$LF[which(LF$Label == A$region)]/100
  return(A)
}

assign_elec_cons <- function(A) {
  if (A$income < 150000) {
    index <- min(which(A$income < income_thresh)) - 1
    mu <- mus[index, A$size]
    sigma <- sigmas[index, A$size]
  } else {
    mu <- mus[10, A$size]
    sigma <- sigmas[10, A$size]
  }
  A$consumption <- rlnorm(1, mu, sigma)
  return(A)
  
}

assign_u_inc <- function(A, mean_inc) {
  A$u_inc <- 1/(1+exp((mean_inc-A$income)*0.0002))
  return(A)
}

assign_inst_cap <- function(A) {
  if (A$status == "N") { # otherwise agents who have already adopted can change inst_cap!
    inst_cap <- 0.3*A$income/kW_price_current
    meet_demand <- A$consumption/(A$LF*24*365)
    
    if (meet_demand < inst_cap) inst_cap <- meet_demand
    
    A$inst_cap <- inst_cap
    if (inst_cap > 4) {   # would they be better off going for the lower investment?
      ret_large <- annual_return(A)
      large_cap <- A$inst_cap
      A$inst_cap <- 4
      ret_small <- annual_return(A)
      if (ret_small > ret_large) A$inst_cap <- 4
      else A$inst_cap <- large_cap
      
      if (A$inst_cap > 10) A$inst_cap <- 10
      
    }
  }
  
  return(A)
}


utilities <- function(A, w, ags) {
  # A is an object representing an agent
  pp <- simple_PP(A)
  A$u_ec <- (20-pp)/19
  A$u_soc <- soc_utility(A, ags)
  A$u_cap <- cap_utility(A)
  A$u_tot <- w[1]*A$u_inc + w[2]*A$u_soc + w[3]*A$u_ec +  w[4]*A$u_cap
  
  return(A)
}

decide <- function(A, threshold) {
  if (A$status == "N"){
    # A is an object representing an agent
    if (A$u_tot > threshold && A$status == "N") {
      A$status[1] <- "Y"
      if (A$inst_cap <= 4) A$FiT <- FiT_current_small
      else A$FiT <- FiT_current_large
      A$exp_tar <- exp_tar_current
      A$date <- current_date
    }
  }
  return(A)
}

simple_PP <- function(A) {
  
  n <- 20 # economic lifetime
  
  R <- annual_return(A)
  
  if (R > 0)  pp <- A$inst_cap*kW_price_current/R
  else pp <- 20
  
  if (pp > 20) pp <- 20
  return(pp)
}


annual_return <- function(A){
  prod <- output(A$inst_cap, A$LF)
  if (A$inst_cap <= 4){
    R_FiT <- prod*FiT_current_small # true for all, regardless of what's consumed/exported
    displaced <- export <- 0.5*prod
    R_exp <- export*exp_tar_current
    R_sav <- displaced*elec_price
    R <- R_FiT + R_sav + R_exp
  } else {
    R_FiT <- prod*FiT_current_large # true for all, regardless of what's consumed/exported
    displaced <- export <- 0.5*prod
    R_exp <- export*exp_tar_current
    R_sav <- displaced*elec_price
    R <- R_FiT + R_sav + R_exp
  }
}


output <- function(x, y) {
  # x = installed capacity
  # y = load factor
  P <- x*24*365*y
}

soc_utility <- function(A, ags) {
  neighbours <- ags[A$network]
  links <- length(A$network)
  no_adopters <- sum(map_int(neighbours, "status") == 1)
  u_soc <- 1/(1+exp(1.2*((links/4)-no_adopters)))
  
}

cap_utility <- function(A) {
  u_cap <- 1/(1+exp(-(0.2*A$income-A$inst_cap*kW_price_current)*0.0007))
}

#---------------------------- 1.3 Cost calculation ------------------------------#
subs_cost <- function(adpts, rn, number_of_agents) {
  if (length(adpts) > 0) {
    adopt_dates <- unname((sapply(adpts, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    
    # UK policy: subsidies were guaranteed for 25 years before 1/8/2012, 20 years thereafter
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, 
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee)
    
    
    # find the number of owner-occupiers corresponding to the adoption year 
    adopt_costs %<>% mutate(n_owners = sapply(adopt_date, which_owner_year))
    
    adopt_costs %<>% mutate(end_date = adopt_date + years(guarantee), 
                            export = output/2, # assuming no meter is installed
                            annual_cost = output*FiT + export*exp_tar,
                            annual_cost_scaled = annual_cost*n_owners/number_of_agents,
                            tot_cost = guarantee*annual_cost,
                            tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    adopt_costs %<>% arrange(adopt_date)
    
    tot_sub_cost <- sum(adopt_costs$tot_cost_scaled)
    
    time_series <- dmy("01jan2010") + months(1:370)
    # have found annual & total cost per installation; can now find annual cost
    annual_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(adopt_costs, adopt_date < time_series[i], end_date >= time_series[i])
    annual_cost[i] <- sum(existing_inst$annual_cost_scaled)
    }
    
  }
  else { # no one has adopted
    time_series <- dmy("01jan2010") + months(1:370)
    annual_cost = rep(0, length(time_series))
    tot_sub_cost = 0
  }
  a <- data.frame(time_series = time_series, annual_cost = annual_cost,
                  run_number = rep(rn, length(time_series)))
  cost_results <- list(a, tot_sub_cost = tot_sub_cost)
}

priv_cost <- function(x, rn, number_of_agents) { # x = adopters
  if (length(x) > 0) {
    adopt_dates <- unname((sapply(x, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    inst_cap <- extract(x, "inst_cap")
    
    priv_costs <- data.frame(adopt_date = adopt_dates, 
                             inst_cap = inst_cap)
    
    priv_costs %<>% mutate(PV_cost = sapply(adopt_date, which_PV_cost), 
                           tot_cost = inst_cap*PV_cost, 
                           n_owners = sapply(adopt_date, which_owner_year),
                           tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    
    tot_priv_cost <- sum(priv_costs$tot_cost_scaled)
    
    time_series <- FiT$time_series
    cum_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(priv_costs, adopt_date < time_series[i])
    cum_cost[i] <- sum(existing_inst$tot_cost_scaled)
    }
  }
  else {   time_series <- FiT$time_series
  cum_cost = rep(0, length(time_series))
  tot_priv_cost = 0
  }
  cost_results <- list(data.frame(time_series = time_series, cum_cost = cum_cost, 
                                  run_number = rep(rn, length(time_series))), 
                       tot_priv_cost = tot_priv_cost)
}

calc_LCOE <- function(adpts, rn, number_of_agents) {
  
  if (length(adpts) > 0){
    r <<- 0.05
    
    adopt_dates <- adpts %>% sapply(function (x) x["date"]) %>% unname
    adopt_dates <- do.call("c", adopt_dates)
    
    PV_price <- adopt_dates %>% sapply(which_PV_cost)
    
    lifetime <- 25 # how long do the solar panels last?
    
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, inst_cap = extract(adpts, "inst_cap"),
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee,
                              PV_price = PV_price)
    
    adopt_costs %<>% mutate(export = output/2, annual_cost = output*FiT + export*exp_tar,
                            cap_cost = inst_cap*PV_price)
    
    tot_output <- sum(adopt_costs$output)
    
    
    adopt_costs %<>% mutate(LCOE_ind = LCOE(annual_cost, cap_cost, guarantee, output),
                            n_owners = sapply(adopt_date, which_owner_year),
                            output_scaled = output*n_owners/number_of_agents,
                            weight = output/tot_output)
    
    tot_output_scaled <- sum(adopt_costs$output_scaled)
    
    adopt_costs %<>% mutate(weight_scaled = output_scaled/tot_output_scaled, 
                            run_number = as.factor(rep(rn, nrow(adopt_costs))))
    
    adopt_costs %<>% select(adopt_date, LCOE_ind, weight_scaled, run_number, output_scaled)
    
    LCOE_weighted_scaled <- sum(adopt_costs$LCOE_ind*adopt_costs$weight_scaled)
  }
  else {
    LCOE_weighted_scaled <- NA
    adopt_costs <- data.frame(adopt_date = NA, LCOE_ind = NA, weight_scaled = NA,
                              run_number = rn, output_scaled = NA)
  }
  
  
  return(list(LCOE_weighted_scaled, adopt_costs))
}

LCOE <- function(annual_cost, cap_cost, guarantee, output) {
  cost <- 0
  prod_elec <- 0
  for (i in 1:25){
    disc_factor <- (1+r)^i 
    if (i == 1) {
      yr_cost <- annual_cost + cap_cost
    }
    
    else if (i > 1 && i <= guarantee) {
      yr_cost <- annual_cost
    }
    else if (i >1 && i > guarantee) {
      yr_cost <- 0
    }
    cost <- cost + yr_cost/disc_factor
    prod_elec <- prod_elec + output/disc_factor
    
  }
  LCOE <- 1000*cost/prod_elec # pounds per MWh
}

which_owner_year <- function(x) {
  owner_occupiers$X2[owner_occupiers$X1 == as.numeric(year(x))]
  
}

which_PV_cost <- function(x) {
  kW_price %>% filter(X1 == x) %>% select(X2) %>% unlist %>% unname
  
}


#------------------------------- 1.4 Processing ---------------------------------#

append_results <- function() {
  avg_u <<- rbind(avg_u, all_res_rn[[1]])
  cost <<- rbind(cost, all_res_rn[[2]])
  tot_cost <<- rbind(tot_cost, all_res_rn[[3]])
  cost_priv <<- rbind(cost_priv, all_res_rn[[4]])
  tot_cost_priv <<- rbind(tot_cost_priv, all_res_rn[[5]])
  LCOE_avg <<- rbind(LCOE_avg, all_res_rn[[6]])
  LCOE_data <<- rbind(LCOE_data, all_res_rn[[7]])
  if (run_w_cap == TRUE) FiT_levels <<- rbind(FiT_levels, all_res_rn[[8]])
}

calc_prod <- function(indiv_data, number_of_runs){
  cum_prod <- NULL
  current_prod_run <- vector(length = number_of_runs)
  for (i in 1:length(averages$time_series)) {
    
    for (j in 1:number_of_runs) {
      current_prod_run[j] <- indiv_data %>% 
        filter(run_number == j, adopt_date < averages$time_series[i]) %>% 
        summarise(sum(output_scaled))
      current_prod_run %<>% unlist
      
    }
    current_prod <- 
      data.frame(time_series = rep(averages$time_series[i], number_of_runs), 
                 run_number = as.factor(c(1:number_of_runs)), current_prod = current_prod_run)
    
    cum_prod <- rbind(cum_prod, current_prod)
  }
  colnames(cum_prod) <- c("time_series", "run_number", "current_prod")
  cum_prod_avg <- cum_prod %>% group_by(time_series) %>% 
    summarise(current_prod = mean(current_prod))
  res_list <- list(cum_prod, cum_prod_avg)
}

summarise_results <- function(avg_u, cost, cost_priv){
  number_of_runs <- max(as.numeric(avg_u$run_number))
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(u_inc = mean(mean_u_inc), u_ec = mean(mean_u_ec), u_soc = mean(mean_u_soc),
              u_cap = mean(mean_u_cap),
              u_tot = mean(mean_u_tot), avg_inst_cap = mean(avg_inst_cap, na.rm = TRUE),  
              sd_u_inc = sqrt(sum(sd_u_inc^2))/number_of_runs,
              sd_u_ec = sqrt(sum(sd_u_ec^2))/number_of_runs,
              sd_u_soc = sqrt(sum(sd_u_soc^2))/number_of_runs,
              sd_u_cap = sqrt(sum(sd_u_cap^2))/number_of_runs,
              sd_u_tot = sqrt(sum(sd_u_tot^2))/number_of_runs,
              tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              frac_of_adopters = mean(frac_of_adopters, na.rm = TRUE),
              inst_cap_diff = mean(inst_cap_diff, na.rm = TRUE))
  
  avg_cost <<- cost %>% group_by(time_series) %>% summarise(annual_cost = mean(annual_cost))
  
  avg_cost_priv <<- cost_priv %>% group_by(time_series) %>% summarise(cum_cost = mean(cum_cost))
}



load_plot_sim_data <- function(save_name, plot_u = T, plot_cost = T, plot_prod = T){
  load_use_cap <- F
  avg_u <<- read_rds(paste(save_name, "_avg_u.rds", sep = ""))
  cost <<- read_rds(paste(save_name, "_cost.rds", sep = ""))
  cost_priv <<- read_rds(paste(save_name, "_cost_priv.rds", sep = ""))
  LCOE_data <<- read_rds(paste(save_name, "_LCOE_data.rds", sep = ""))
  LCOE_avg <<- read_rds(paste(save_name, "_LCOE_avg.rds", sep = ""))
  FiT <<- read_rds(paste(save_name, "_FiT.rds", sep = ""))
  number_of_runs <- max(as.numeric(avg_u$run_number))
  if (file.exists(paste(save_name, "_FiT_levels.rds", sep = ""))){
    FiT_levels <<- read_rds(paste(save_name, "_FiT_levels.rds", sep = ""))
    load_use_cap <- T
  }
  
  # calculate averages of all runs
  
  try(past <- avg_u$inst_cap_diff)
  if (!is.null(past)) {
    summarise_results(avg_u, cost, cost_priv) 
    sum_abs_diff <- sum(abs(averages$inst_cap_diff))
  } else {
    future <- T
    summarise_results_f(avg_u, cost, cost_priv) 
  }
  # deviation from real data: sum of absolute values
  # of deviation from installed FiT capacity < 10 kW
  
  tot_subs_cost <- sum(avg_cost$annual_cost)/12
  tot_priv_cost <- tail(avg_cost_priv$cum_cost, 1)
  tot_overall_cost <- tot_subs_cost + tot_priv_cost
  #  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  #  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  if(plot_u == T){
    u_vars <- c("inc", "soc", "ec", "cap", "tot") # partial and total utilities
    yl <- c(expression(u[inc]), expression(u[soc]), expression(u[ec]), expression(u[cap]), expression(u[tot]))
    l <- 1
    for (app in u_vars) { # plot average over time of utility functions
      
      p <- ggplot() + theme_bw() +
        geom_line(data = avg_u,
                  aes(x = time_series, y = get(paste("mean_u_", app, sep = "")), 
                      group = run_number), alpha = 0.2) +
        geom_line(data = averages, aes(x = time_series, y = get(paste("u_", app, sep = ""))), size = 1) +
        geom_ribbon(data = averages, aes(x = time_series, 
                                         ymin = get(paste("u_", app, sep = "")) - get(paste("sd_u_", app, sep = "")),
                                         ymax = get(paste("u_", app, sep = "")) + get(paste("sd_u_", app, sep = ""))),
                    alpha = 0.15) +
        ylab(yl[l]) + xlab("Date") + theme(legend.position = "none") + coord_cartesian(expand = F) 
      print(p)
      l <- l+1
    }
  }
  
  print(ggplot() + theme_bw() + 
          geom_line(data = avg_u, 
                    aes(x = time_series, y = avg_inst_cap, group = run_number), alpha = 0.2) +
          geom_line(data = averages, aes(x = time_series, y = avg_inst_cap, color = "Modelled"), size = 1) +
          geom_line(data = deployment, aes(x = time_series, y = avg_cap, color = "Real"), size = 1) + ylim(0, 4) +
          ylab("Average installed capacity (kW)") + xlab("Date") +
          scale_colour_manual(name = "", values = c(Modelled = "black", Real = "blue")))
  
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment, aes(x = time_series, y = real_cap, color = "Real"), size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap, color = "Modelled"), size = 1) +
          ylab("Cumulative capacity (MW)") + xlab("Date") +
          scale_colour_manual(name = "", values = c(Modelled = "black", Real = "red")) +
          theme(legend.position = c(0.2, 0.75)) + scale_x_date(expand = c(0,0)) + 
          scale_y_continuous(expand = c(0,0), limits = c(0, max(avg_u$tot_inst_cap) + 100))
  )
  
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost/1e6, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost/1e6), color = "black", size = 1)+
            ylab("Annual cost (millions £)") + xlab("Date"))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost/1e6, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost/1e6), color = "black", size = 1)+
            ylab("Cumulative private cost (millions £)") + xlab("Date"))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number), alpha = 0.1)+
            ylab("LCOE (£/MWh)") + xlab("Date"))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod/1e6, group = run_number), alpha = 0.2) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod/1e6), size = 1) +
            ylab("Annual production (MWh/yr)") + xlab("Date"))
  }
  
  if (load_use_cap == T) {
    avg_FiT <- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  
  dep_sd <- sd(avg_u$tot_inst_cap[avg_u$time_series == max(avg_u$time_series)])
  subs_sd <- cost %>% group_by(run_number) %>% summarise(subs = sum(annual_cost)/12) %>% 
    select(subs) %>% unlist %>% sd
  priv_sd <- cost_priv %>% group_by(run_number) %>% summarise(priv = max(cum_cost)) %>% 
    select(priv) %>% unlist %>% sd
  ann_sd <- cost %>% group_by(run_number) %>% summarise(ann = max(annual_cost)) %>%
    select(ann) %>% unlist %>% sd 
  prod_sd <- cum_prod %>% group_by(run_number) %>% summarise(prod = sum(current_prod)/12) %>%
    select(prod) %>% unlist %>% sd
  
  
  cat("Final deployment at ", as.character(tail(averages$time_series, 1)), " (MW) = ", 
      tail(averages$tot_inst_cap, 1), " +/- ", dep_sd, "\n",
      "Total subsidy cost (billions £) = ", tot_subs_cost/1e9, " +/- ", subs_sd/1e9, "\n",
      "Total private cost (billion £) = ", tot_priv_cost/1e9, " +/- ", priv_sd/1e9, "\n",
      "Total cost (billions £) = ", tot_overall_cost/1e9, "\n",
      "Maximum annual cost (millions £) = ", max(avg_cost$annual_cost)/1e6, " +/- ", ann_sd/1e6, "\n",
      "Total production (GWh) = ", sum(cum_prod_avg$current_prod)/(12*1e6), " +/- ", prod_sd/1e6, "\n",
      "Weighted LCOE (£/MWh) = ", mean(LCOE_avg), "\n",
      sep = "")
  
  
}


##################################################################################
######################## 2. Future scenarios - functions #########################
##################################################################################
# 2.1 Set-up 
# 2.2 Agent/modelling functions
# 2.3 Cost calculation
# 2.4 Processing

#------------------------------- 2.1 Set-up -------------------------------------#


load_data_f <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar,
                        elec_trend, PV_trend,
                        dep_caps = F, cap){
  
  # end_date: date up to which simulation will run
  # FiT_type: real_h, linear, perc_red, ann_perc_red
  if(missing(start_date)) start_date <- "1oct2016"
  if(missing(end_date)) end_date <- "31dec2021"
  if(missing(FiT_end_date)) FiT_end_date <- end_date
  if(missing(FiT_type)) FiT_type <- "real_f"
  if(missing(red_frac)) red_frac <- 0.03
  if(missing(init_fit)) init_fit <- 5
  if(missing(final_fit)) final_fit <- 0
  if(missing(exp_tar)) exp_tar <- 4.91
  if(missing(elec_trend)) elec_trend <- "mid"
  if(missing(PV_trend)) PV_trend <- 0.07
  if(missing(cap)) cap <- 50
  
  load_libraries()
  # DO NOT USE plyr LIBRARY 
  
  if(FiT_type == "real_f"){
    start_date <- "1oct2016"
    FiT_end_date <- "1mar2019"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
    dep_caps <- T
  }
  
  if(FiT_type == "real_f_ext"){
    start_date <- "1oct2016"
    FiT_end_date <- "1mar2021"
    if (end_date < FiT_end_date) end_date <- FiT_end_date
    dep_caps <- T
  }
  
  FiT_end_date <- dmy(FiT_end_date)
  end_date <- dmy(end_date)
  start_date <- dmy(start_date)
  
  time_months <<- seq(start_date, end_date, by = '1 month')
  
  time_years <<- year(start_date):year(end_date)
  
  
  if(!is.Date(end_date)) stop("Your end date is not a valid date")
  if(!(FiT_type == "real_f" | FiT_type == "perc_red" | FiT_type == "ann_perc_red" | 
       FiT_type == "linear" | FiT_type == "dep_cap" | FiT_type == "real_f_ext")) stop("FiT type not recognised")
  if(init_fit < 0 | final_fit < 0) stop("FiTs have to be positive")
  if(!(elec_trend == "mid" | elec_trend == "low" | elec_trend == "high")) stop("Electricity price trend not recognised")
  
  
  #---------------------------------------------------------#
  
  elec_price_time <- read_csv("Data/electricityprices.csv", col_names = F, col_types = cols())
  elec_price_time[7,] <- c(2016, 17.33) # projection for 2016
  elec_price_time <<- future_elec_price(elec_price_time, elec_trend)
  
  owner_occupiers <- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = cols()) %>% mutate(X2 = X2*1000)
  
  owner_occupiers <<- data.frame(X1 = time_years, 
                                 X2 = owner_occupiers$X2[[7]])
  #---------------------------------------------------------#
  # Load factor data
  
  LF <- read_csv("Data/LF_mean.csv", col_types = cols())
  
  # Filter out empty rows, group by region, find mean & std. dev. over all years, arrange
  # in alphabetical order.
  LF <<- LF %>% filter(!is.na(Region)) %>% group_by(Region) %>% 
    summarise(LF = mean(Weighted.mean), std_dev = sd(Weighted.mean)) %>% 
    arrange(Region) %>% mutate(Label = LETTERS[1:11])
  
  
  #---------------------------------------------------------#
  # Feed-in tariff data
  
  set_FiT_f(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar)
  
  #---------------------------------------------------------#
  # Deployment caps
  run_w_cap <<- F
  if (dep_caps == T) {
    set_dep_caps(start_date, end_date, FiT_end_date, FiT_type, cap, exp_tar)
    dep_cap_0 <<- dep_cap
    FiT_0 <<- FiT
    run_w_cap <<- T
  }
  #---------------------------------------------------------#
  # Population data
  
  population <- read_csv("Data/population_mid2012.csv", col_names = FALSE, col_types = cols()) %>% arrange(X1)
  
  region_weights <<- population$X2/sum(population$X2)
  
  rm(population)
  
  #---------------------------------------------------------#
  # PV cost data
  init_PV <- read_csv('Data/PV_cost_data_est.csv', col_names = FALSE, col_types = cols()) %>% mutate(X1 = dmy(X1)) 
  
  kW_price <<- future_PV_price(init_PV, PV_trend, start_date, end_date)
  
  #---------------------------------------------------------#
  # Electricity use data
  
  means <- read_csv("Data/mean-electricity.csv", col_types = cols())
  medians <- read_csv("Data/median-electricity.csv", col_types = cols())
  
  mus <<- data.frame(matrix(ncol = 5, nrow = 10))
  sigmas <<- data.frame(matrix(ncol = 5, nrow = 10))
  
  for (i in 1:5){
    mean <- means[[i+1]]
    median <- medians[[i+1]]
    mus[[i]] <<- log(median) 
    sigmas[[i]] <<- sqrt(2*log(mean/median))
  }
  
  income_thresh <<- means$income
  
  rm(mean, median, i, means, medians)
  
  #---------------------------------------------------------#
  # Real deployment data
  if(exists("deployment")) {
    cat("\nDeployment data is already loaded - if you want to reload it, delete the 'deployment' variable\n")
  } else {
    
    ts <- seq(dmy("01jan2010"), dmy("1nov2016"), by = '1 month')
    
    all_inst_cap <- process_inst_data() %>% 
      filter(technology_type == "Photovoltaic", installed_capacity <= 10, installationtype == "Domestic")
    current_cap <- vector(length = length(ts))
    avg_cap <- vector(length = length(ts))
    for (i in 1:length(ts)) {
      date_now <- ts[i] + months(1)
      installed_now <- filter(all_inst_cap, commissioned_date < date_now)
      current_cap[i] <- sum(installed_now$installed_capacity)
      avg_cap[i] <- current_cap[i]/nrow(installed_now)
    }
    
    
    deployment <<- data.frame(time_series = dmy("01feb2010") + months(0:(length(ts)-1)), 
                              real_cap = current_cap/1000, avg_cap = avg_cap)
    rm(all_inst_cap, installed_now, current_cap, date_now)
  }
  #---------------------------------------------------------#
  
  
}

future_PV_price <- function(init_PV, x, start_date, end_date) {
  # x is annual percentage reduction
  # 4, 7, 10 for low, med, high cost reduction cases
  
  no_years <- length(year(start_date):year(end_date)) + 1
  year_cost <- vector(length = no_years)
  year_cost[1] <- init_PV %>% filter(X1 == start_date) %>% select(X2) %>% unlist %>% unname
  for (i in 1:(no_years-1)) {
    year_cost[i+1] <- year_cost[i] - x*year_cost[i]
  }
  
  monthly_cost <- approx(x = 1:no_years, y = year_cost,
                         method = "linear", n = (no_years-1)*12 + 1)
  monthly_cost <- data.frame(X1 = seq(start_date, length.out = length(monthly_cost$y), 
                                      by = '1 month'),
                             X2 = monthly_cost$y)
  
  monthly_cost %<>% filter(X1 >= start_date, X1 <= end_date)
  
}


future_elec_price <- function(elec_price_time, x){
  future_price_high <- vector(length = length(time_years))
  
  a <- lm(elec_price_time$X2[1:6] ~ elec_price_time$X1[1:6])
  for (i in 1:length(time_years)) {
    future_price_high[i] <- a$coefficients[[2]]*time_years[i] + a$coefficients[[1]]
  }
  future_price_low <- rep(mean(elec_price_time$X2[5:6]), length(time_years))
  future_price_mid <- (future_price_high+future_price_low)/2
  
  if (x == "high") {
    future_price <- future_price_high
  }
  
  else if (x == "mid") {
    future_price <- future_price_mid
  }
  
  else if (x == "low") {
    future_price <- future_price_low
    
  }
  elec_price <- data.frame(X1 = time_years, X2 = future_price)
  elec_price$X2[elec_price$X1 == 2016] <- elec_price_time$X2[elec_price_time$X1 == 2016]
  return(elec_price)
  
}


set_FiT_f <- function(start_date, end_date, FiT_end_date, FiT_type, red_frac, init_fit, final_fit, exp_tar){
  if (end_date == FiT_end_date){
    FiT_zero <- NULL
  } else {
    FiT_zero <- data.frame(time_series = seq(FiT_end_date + months(1), end_date, by = '1 month'), FiT = 0,
                           FiT_large = 0, exp_tar = 0)
  }
  time_series <- seq(start_date, FiT_end_date, by = '1 month')
  if (FiT_type == "linear"){
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = seq(init_fit, final_fit, length.out = length(time_series)),
                             FiT_large = seq(init_fit, final_fit, length.out = length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "perc_red") {
    
    FiT <<- rbind(data.frame(time_series = time_series, FiT = geomSeries(init_fit, 1-red_frac, length(time_series)),
                             FiT_large = geomSeries(init_fit, 1-red_frac, length(time_series)), exp_tar = exp_tar),
                  FiT_zero)
  }
  if (FiT_type == "ann_perc_red") {
    #  time_series <- seq(dmy("01jan2010"), end_date, by = '1 month')
    
    year_series <- year(seq(dmy("01jan2010"), end_date, by = '1 year'))
    FiT_yr <- geomSeries(init_fit, 1-red_frac, length(year_series))
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT <- sapply(FiT$time_series, function(x) FiT_yr[which(year_series == year(x))])
    FiT <- FiT %>% mutate(FiT_large = FiT, exp_tar = exp_tar)
    FiT <<- rbind(FiT, FiT_zero)
  }
  
  if(FiT_type == "dep_cap") {
    
    FiT <- data.frame(time_series = time_series, FiT = NA)
    FiT$FiT[1] <- init_fit
    FiT <<- rbind(FiT, FiT_zero)
  }
}

generate_populations_f <- function(n_agents, n_pop, dev, agent_name) {
  if(missing(n_agents)) n_agents <- 5000
  if(missing(n_pop)) n_pop <- 10
  if(missing(dev)) dev <- 25  # in MW - how far is the population's final deployment allowed to deviate
  # from real final deployment before it is rejected?
  if(missing(agent_name)) agent_name <- "agents"
  
  load_data(FiT_type = "real_h", start_date = "1jan2010", end_date = "1sep2016")
  
  batch_run_func_gen(number_of_agents = n_agents, n_des = n_pop, dev = dev, agent_name= agent_name)
}

batch_run_func_gen <- function(w, t, number_of_agents, n_des, dev, agent_name) {
  
  # Set threshold and weights, electricity price
  if(missing(w)) w <- c(0.27, 0.25, 0.05, 0.43) # Weights: income & social, economic, capital
  
  if(missing(t)) threshold <- 0.74 # Adoption threshold
  else threshold <- t
  
  if(missing(number_of_agents)) number_of_agents <- 5000
  
  
  
  try(if(signif(sum(w), digits = 6) != 1) stop("Your weights don't add up to 1!"))
  
  n_pops <- 0
  i <- 1
  avg_u <<- NULL
  
  while (n_pops < n_des) {
    
    all_res <- run_model_gen(number_of_agents, i, w, threshold, n_pops, dev, agent_name) # run the model once 
    n_pops <- all_res[[1]]
    cat(i, n_pops, "\n", sep = " ")
    
    i <- i+1
    avg_u <<- rbind(avg_u, all_res[[2]])
  }
  
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              inst_cap_diff = mean(inst_cap_diff, na.rm = TRUE))
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment, aes(x = time_series, y = real_cap), color = "blue", size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number, color = run_number))+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1))
  
  return()
  
}



run_model_gen <- function(number_of_agents, rn, w, threshold, n_in, dev, agent_name) {
  
  # Set up some parameters
  
  
  time_steps <- nrow(FiT) # number of months in time series
  
  agents <- rerun(number_of_agents, 
                  Household_Agent("N", assign_income(), assign_size(), assign_region()))
  
  n_links <- 10
  
  mean_income <- mean(extract(agents, "income"))
  agents %<>% map(assign_LF) %>% map(assign_elec_cons) %>% map(assign_u_inc, mean_inc = mean_income) %>% 
    map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)
  
  
  # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
  
  # assign further characteristics based on those previously assigned.
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      tot_inst_cap = vector(length = time_steps),
                      inst_cap_diff = vector(length = time_steps)
  )
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  for (i in 1:time_steps) {
    # set parameters for current time
    FiT_current_small <<- FiT$FiT[[i]]/100 # p to £
    FiT_current_large <<- FiT$FiT_large[[i]]/100
    exp_tar_current <<- FiT$exp_tar[[i]]/100 # p to £
    kW_price_current <<- kW_price$X2[i]
    current_date <<- FiT$time_series[i]
    elec_index <- which(sapply(elec_price_time$X1, function(x) grep(x, current_date)) == 1)
    elec_price <<- elec_price_time[[elec_index, 2]]/100
    n_owners <<- owner_occupiers[[elec_index, 2]]
    
    agents <- agents %>% map(assign_inst_cap) %>% map(utilities, w = w, ags = agents) %>% 
      map(decide, threshold = threshold)
    
    
    adopters <- agents[map(agents, "status") == 1]
    
    
    # Write data
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- 0
      avg_u$tot_inst_cap[i] <- 0
    }
    
    avg_u$inst_cap_diff[i] <- deployment$real_cap[i] - 
      avg_u$tot_inst_cap[i]
  }
  
  if (abs(tail(avg_u$inst_cap_diff, 1)) <= dev){
    write_rds(agents, paste(agent_name, "_", n_in + 1, ".rds", sep = ""))
    rm(agents)
    n_pops <- n_in + 1
    cat("Successful: run ", rn, "\n")
  }
  else n_pops <- n_in
  
  
  
  return(list(n_pops, avg_u))
  
  
}
#------------------------- 2.2 Agent/model functions ----------------------------#
# all the sane as for historical 

#---------------------------- 2.3 Cost calculation ------------------------------#

subs_cost_f <- function(adpts, rn, number_of_agents) {
  if (length(adpts) > 0) {
    adopt_dates <- unname((sapply(adpts, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    
    # UK policy: subsidies were guaranteed for 25 years before 1/8/2012, 20 years thereafter
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, 
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee)
    
    
    # find the number of owner-occupiers corresponding to the adoption year 
    adopt_costs %<>% mutate(n_owners = sapply(adopt_date, which_owner_year_f))
    
    adopt_costs %<>% mutate(end_date = adopt_date + years(guarantee), 
                            export = output/2, # assuming no meter is installed
                            annual_cost = output*FiT + export*exp_tar,
                            annual_cost_scaled = annual_cost*n_owners/number_of_agents,
                            tot_cost = guarantee*annual_cost,
                            tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    adopt_costs %<>% arrange(adopt_date)
    
    tot_sub_cost <- sum(adopt_costs$tot_cost_scaled)
    
    time_series <- dmy("01jan2010") + months(1:450)
    # have found annual & total cost per installation; can now find annual cost
    annual_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(adopt_costs, adopt_date < time_series[i], end_date >= time_series[i])
    annual_cost[i] <- sum(existing_inst$annual_cost_scaled)
    }
    
  }
  else { # no one has adopted
    time_series <- dmy("01jan2010") + months(1:450)
    annual_cost = rep(0, length(time_series))
    tot_sub_cost = 0
  }
  a <- data.frame(time_series = time_series, annual_cost = annual_cost,
                  run_number = rep(rn, length(time_series)))
  cost_results <- list(a, tot_sub_cost = tot_sub_cost)
}


priv_cost_f <- function(x, rn, number_of_agents) { # x = adopters
  if (length(x) > 0) {
    adopt_dates <- unname((sapply(x, function (x) x["date"])))
    adopt_dates <- do.call("c", adopt_dates)
    inst_cap <- extract(x, "inst_cap")
    
    priv_costs <- data.frame(adopt_date = adopt_dates, 
                             inst_cap = inst_cap)
    
    priv_costs %<>% mutate(PV_cost = sapply(adopt_date, which_PV_cost_f), 
                           tot_cost = inst_cap*PV_cost, 
                           n_owners = sapply(adopt_date, which_owner_year_f),
                           tot_cost_scaled = tot_cost*n_owners/number_of_agents)
    
    tot_priv_cost <- sum(priv_costs$tot_cost_scaled)
    
    time_series <- FiT$time_series
    cum_cost <- vector(length = length(time_series))
    for (i in 1:length(time_series))
    { existing_inst <- filter(priv_costs, adopt_date < time_series[i])
    cum_cost[i] <- sum(existing_inst$tot_cost_scaled)
    }
  }
  else {   time_series <- FiT$time_series
  cum_cost = rep(0, length(time_series))
  tot_priv_cost = 0
  }
  cost_results <- list(data.frame(time_series = time_series, cum_cost = cum_cost, 
                                  run_number = rep(rn, length(time_series))), 
                       tot_priv_cost = tot_priv_cost)
}

calc_LCOE_f <- function(adpts, rn, number_of_agents) {
  
  if (length(adpts) > 0){
    r <<- 0.05
    
    adopt_dates <- adpts %>% sapply(function (x) x["date"]) %>% unname
    adopt_dates <- do.call("c", adopt_dates)
    
    
    
    PV_price <- adopt_dates %>% sapply(which_PV_cost_f)
    
    lifetime <- 25 # how long do the solar panels last?
    
    after <- adopt_dates >= dmy("1aug2012")
    before <- adopt_dates < dmy("1aug2012")
    
    guarantee <- vector(length = length(adopt_dates))
    
    guarantee[after] <- 20
    guarantee[before] <- 25
    
    adopt_costs <- data.frame(adopt_date = adopt_dates, inst_cap = extract(adpts, "inst_cap"),
                              output = sapply(adpts, function (x) output(x$inst_cap, x$LF)), 
                              FiT = extract(adpts, "FiT"), exp_tar = extract(adpts, "exp_tar"), 
                              guarantee = guarantee,
                              PV_price = PV_price)
    
    adopt_costs %<>% mutate(export = output/2, annual_cost = output*FiT + export*exp_tar,
                            cap_cost = inst_cap*PV_price)
    
    tot_output <- sum(adopt_costs$output)
    
    
    adopt_costs %<>% mutate(LCOE_ind = LCOE(annual_cost, cap_cost, guarantee, output),
                            n_owners = sapply(adopt_date, which_owner_year_f),
                            output_scaled = output*n_owners/number_of_agents,
                            weight = output/tot_output)
    
    tot_output_scaled <- sum(adopt_costs$output_scaled)
    
    adopt_costs %<>% mutate(weight_scaled = output_scaled/tot_output_scaled, 
                            run_number = as.factor(rep(rn, nrow(adopt_costs))))
    
    adopt_costs %<>% select(adopt_date, LCOE_ind, weight_scaled, run_number, output_scaled)
    
    LCOE_weighted_scaled <- sum(adopt_costs$LCOE_ind*adopt_costs$weight_scaled)
  }
  else {
    LCOE_weighted_scaled <- NA
    adopt_costs <- data.frame(adopt_date = NA, LCOE_ind = NA, weight_scaled = NA,
                              run_number = rn, output_scaled = NA)
  }
  
  
  return(list(LCOE_weighted_scaled, adopt_costs))
}



which_PV_cost_f <- function(x) {
  
  kW_price_h <- read_csv('Data/PV_cost_data_est.csv', col_names = FALSE, col_types = "cd") %>% 
    mutate(X1 = dmy(X1)) %>% filter(X1 < kW_price$X1[1])
  
  kW_price_all <- rbind(kW_price, kW_price_h)
  PV_cost <- kW_price_all %>% filter(X1 == x) %>% select(X2) %>% unlist %>% unname
  PV_cost <- PV_cost[1]
  
  
}
which_owner_year_f <- function(x) {
  owner_occupier_h <- read_csv("Data/owner_occupiers.csv", col_names = F, col_types = "in") %>% 
    mutate(X2 = X2*1000)
  owner_occupier_all <- rbind(owner_occupier_h, owner_occupiers)
  owner_occupier_all$X2[owner_occupier_all$X1 == as.numeric(year(x))][1]
  
}


#------------------------------- 2.4 Processing ---------------------------------#

summarise_results_f <- function(avg_u, cost, cost_priv){
  number_of_runs <- max(as.numeric(avg_u$run_number))
  averages <<- avg_u %>% group_by(time_series) %>% 
    summarise(u_inc = mean(mean_u_inc), u_ec = mean(mean_u_ec), u_soc = mean(mean_u_soc),
              sd_u_inc = sqrt(sum(sd_u_inc^2))/number_of_runs,
              sd_u_ec = sqrt(sum(sd_u_ec^2))/number_of_runs,
              sd_u_soc = sqrt(sum(sd_u_soc^2))/number_of_runs,
              sd_u_cap = sqrt(sum(sd_u_cap^2))/number_of_runs,
              sd_u_tot = sqrt(sum(sd_u_tot^2))/number_of_runs,
              u_cap = mean(mean_u_cap),
              u_tot = mean(mean_u_tot), avg_inst_cap = mean(avg_inst_cap, na.rm = TRUE),  
              tot_inst_cap = mean(tot_inst_cap, na.rm = TRUE), 
              frac_of_adopters = mean(frac_of_adopters, na.rm = TRUE))
  
  avg_cost <<- cost %>% group_by(time_series) %>% summarise(annual_cost = mean(annual_cost))
  
  avg_cost_priv <<- cost_priv %>% group_by(time_series) %>% summarise(cum_cost = mean(cum_cost))
}


##################################################################################
################################ 3. Miscellaneous ################################
##################################################################################

consumer_cost <- function(avg_cost, cum_prod_avg, r, start_year, end_year){
  
  annual_cost <- avg_cost %>% mutate(year = year(time_series)) %>% group_by(year) %>% 
    summarise(ann_cost = sum(annual_cost/12)) %>% mutate(n = year - 2008, d = 1/(1+r)^n, d_cost = d*ann_cost) %>%
    filter(year < end_year) %>% select(year, ann_cost, d, d_cost)
  
  d_cost <- annual_cost %>% summarise(cost = sum(d_cost)) %>% unlist
  cat("Discounted FiT cost up to ", end_year, " = Â£", d_cost/1e9, " billion", sep = "", "\n")
  
  ann_prod <- cum_prod_avg %>% mutate(year = year(time_series)) %>% group_by(year) %>% 
    summarise(ann_prod = sum(current_prod/12)) %>% filter(year < end_year)
  
  if(max(ann_prod$year) < end_year) {
    ann_prod_n <- data.frame(year = (max(ann_prod$year)+1):(end_year - 1), ann_prod = tail(cum_prod_avg$current_prod, 1))
    ann_prod <- rbind(ann_prod, ann_prod_n)
  }
  
  ann_prod %<>% select(ann_prod)
  
  ann_prod %<>% mutate(value = 0.03*ann_prod)
  cost_to_cons <- cbind(ann_prod, annual_cost) %>% mutate(cost = (ann_cost - value)*d) 
  
  cat("Discounted cost to consumers up to ", end_year, " = Â£", sum(cost_to_cons$cost)/1e9, " billion", sep = "", "\n")
  
}