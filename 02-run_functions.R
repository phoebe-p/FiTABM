##################################################################################
################################ Historical (past) ###############################
##################################################################################

#--------------------------------- Batch runs -----------------------------------#

batch_run_func <- function(number_of_agents, 
                           number_of_runs, plot_u = T, plot_cost = T, plot_prod = T, save_name) {
  
  allowed_params <- read_tsv('Data/allowed_params_1000.txt', col_names = F)
  
  sample_for_run <- allowed_params[sample(1:nrow(allowed_params), number_of_runs, replace = TRUE), ]
  
  if(missing(number_of_agents)) number_of_agents <- 5000
  if(missing(number_of_runs)) number_of_runs <- 100
  
  initialise_vars() # create variables which will store output
  
  for (i1 in 1:number_of_runs) {
    w <- unlist(sample_for_run[i1, 1:4])
    threshold <- unlist(sample_for_run[i1, 5])
    if (run_w_cap == TRUE) { # Reset to original values for new run 
      FiT <<- FiT_0
      dep_cap <<- dep_cap_0
    }
    
    cat(i1, w, threshold, "\n")
    
    all_res_rn <<- run_model(number_of_agents, i1, w, threshold) # run the model once 
    
    append_results() # add results of current run to previous results
    
    
  }
  
  rm(all_res_rn, current_date, envir = .GlobalEnv)
  
  summarise_results(avg_u, cost, cost_priv) # calculate averages of all runs
  
  sum_abs_diff <- sum(abs(averages$inst_cap_diff)) # deviation from real data: sum of absolute values
  # of deviation from installed FiT capacity < 10 kW
  
  overall_tot_cost <- tot_cost_priv + tot_cost # total expenditure; used for calcul
  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  # Plotting
  
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
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost), color = "black", size = 1))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost), color = "black", size = 1))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number, color = run_number), alpha = 0.1))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod, group = run_number,
                                           color = run_number)) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod), size = 1))
  }
  
  if (run_w_cap == TRUE) {
    avg_FiT <<- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  print_vars <- paste("w = ", w[1], w[2], w[3], w[4], ", t =", threshold,", n_agents =", number_of_agents)
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment, aes(x = time_series, y = real_cap), color = "blue", size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1) +
          annotate("text", x = dmy("01jul2011"), y = 2000, label = print_vars))
  
  if(missing(save_name)){
    cat("\n", "Data not being saved!", "\n", sep = "")
  } else {
    write_rds(avg_u, paste(save_name, "_avg_u.rds", sep = ""))
    write_rds(cost, paste(save_name, "_cost.rds", sep = ""))
    write_rds(cost_priv, paste(save_name, "_cost_priv.rds", sep = ""))
    write_rds(LCOE_data, paste(save_name, "_LCOE_data.rds", sep = ""))
    write_rds(LCOE_avg, paste(save_name, "_LCOE_avg.rds", sep = ""))
    write_rds(FiT, paste(save_name, "_FiT.rds", sep = ""))
    if (run_w_cap == TRUE) write_rds(FiT_levels, paste(save_name, "_FiT_levels.rds", sep = ""))
  }
  
    to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT)
    
    if (run_w_cap == TRUE) to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT, FiT_levels)
  
  return(to_return)
  
}


#------------------------------- Individual runs --------------------------------#

run_model <- function(number_of_agents, rn, w, threshold) {
  
  # Set up some parameters
  
  
  time_steps <- nrow(FiT) # number of months in time series
  
  agents <- rerun(number_of_agents, 
                  Household_Agent("N", assign_income(), assign_size(), assign_region()))
  
  n_links <- 10
  
  mean_income <- mean(extract(agents, "income"))
  agents %<>% map(assign_LF) %>% map(assign_elec_cons) %>% map(assign_u_inc, mean_inc = mean_income) %>% 
    map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)
  
  adopters <- agents[map(agents, "status") == 1]
  
  if (length(adopters) > 0){
    n_owners <<- owner_occupiers[[1, 2]]
    init_cap <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
  }
  else{
    init_cap <- 0
  }
  
  # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
  
  # assign further characteristics based on those previously assigned.
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      mean_u_inc = vector(length = time_steps),
                      mean_u_ec = vector(length = time_steps),
                      mean_u_soc = vector(length = time_steps),
                      mean_u_cap = vector(length = time_steps),
                      mean_u_tot = vector(length = time_steps),
                      sd_u_inc = vector(length = time_steps),
                      sd_u_ec = vector(length = time_steps),
                      sd_u_soc = vector(length = time_steps),
                      sd_u_cap = vector(length = time_steps),
                      sd_u_tot = vector(length = time_steps),
                      frac_of_adopters = vector(length = time_steps),
                      avg_inst_cap = vector(length = time_steps),
                      tot_inst_cap = vector(length = time_steps),
                      inst_cap_diff = vector(length = time_steps)
  )
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  if(run_w_cap == TRUE) {
    quarter_done <- 0 # how many quarters' capacity have already been used up?
    exceeded <- FALSE # has the total available capacity been exceeded?
  }
  
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
    k <- extract(agents, "status") == "Y"
    avg_u$frac_of_adopters[i] <- length(k[k == TRUE])/number_of_agents
    avg_u$mean_u_ec[i] <- mean(extract(agents, "u_ec"))
    avg_u$mean_u_inc[i] <- mean(extract(agents, "u_inc"))
    avg_u$mean_u_soc[i] <- mean(extract(agents, "u_soc"))
    avg_u$mean_u_cap[i] <- mean(extract(agents, "u_cap"))
    avg_u$mean_u_tot[i] <- mean(extract(agents, "u_tot"))
    avg_u$sd_u_ec[i] <- sd(extract(agents, "u_ec"))
    avg_u$sd_u_inc[i] <- sd(extract(agents, "u_inc"))
    avg_u$sd_u_soc[i] <- sd(extract(agents, "u_soc"))
    avg_u$sd_u_cap[i] <- sd(extract(agents, "u_cap"))
    avg_u$sd_u_tot[i] <- sd(extract(agents, "u_tot"))
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- NA
      avg_u$tot_inst_cap[i] <- 0
    }
    
    avg_u$inst_cap_diff[i] <- deployment$real_cap[i] - 
      avg_u$tot_inst_cap[i]
    
    ##### Deployment cap code - only runs if using a deployment cap scenario
    if (run_w_cap == TRUE){
      
      
      if ((avg_u$tot_inst_cap[i] - init_cap) > sum(dep_cap$orig_cap) && exceeded == FALSE) { # total available capacity has been exceeded;
        # all further FiTs are zero
        FiT[(i+1):nrow(FiT), 2:4] <<- 0
        exceeded <- TRUE
      }
      
      if (exceeded == FALSE && i < nrow(FiT)) { # Not yet exceeded all the caps & not in the final time step
        
        which_q <- max(which(current_date >= dep_cap$q_dates)) # which quarter are we in?
        
        ref_cap <- avg_u$tot_inst_cap[avg_u$time_series == dep_cap$q_dates[which_q]]
        if(is_empty(ref_cap)) ref_cap <- init_cap
        
        current_quarter <- avg_u$tot_inst_cap[i] - ref_cap # how much capacity has been installed so far in the quarter?
        
        dep_cap$inst_cap[which_q] <<- current_quarter
        
        current_month <- avg_u$tot_inst_cap[i] - avg_u$tot_inst_cap[i-1] # capacity installed this month
        
        excess_cap <- dep_cap$cap[which_q] - current_quarter # how much capacity is left over in the current quarter?
        
        if (current_month > 0 && which_q < nrow(dep_cap) && quarter_done == which_q) {
          # some capacity has been installed this month, not in the final quarter, and we aren't in next q's capacity already
          remaining <- current_month # the remaining installed capacity which must be assigned to this or next month
          
          for (j in 1:(nrow(dep_cap)-which_q)) { # 
            
            if (dep_cap$cap[which_q + j] - remaining < 0) { # if we are exceeding the available capacity in a quarter
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) { # all the capacity has been allocated; break out of the j loop
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              
              break
            }
          }
        }
        
        if (excess_cap < 0 && which_q < nrow(dep_cap) && quarter_done != which_q) {
          # have exceeded the allocated capacity for this quarter
          remaining <- current_quarter - dep_cap$cap[which_q]
          
          quarter_done <- which_q
          for (j in 1:(nrow(dep_cap)-which_q)) {
            
            if (dep_cap$cap[which_q + j] - remaining < 0) {
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
              
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) {
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (current_date == dep_cap$q_dates[which_q] + months(2) && excess_cap > 0) {
          dep_cap$cap[which_q + 1] <<- dep_cap$cap[which_q + 1] + excess_cap
        }
        
      }
      
    }
    
    #### ^^^ End of deployment cap code
    
  }
  
  if (run_w_cap == TRUE && exceeded == TRUE) cat("Total available capacity was exceeded", "\n")
  
  FiT_outp <- cbind(FiT, run_number = rep(rn, nrow(FiT)))
  
  LCOE_data <- calc_LCOE(adopters, rn, number_of_agents)
  LCOE_avg <- LCOE_data[[1]]
  LCOE_data <- LCOE_data[[2]]
  cost_subs_res <- subs_cost(adopters, rn, number_of_agents)
  cost_priv_res <- priv_cost(adopters, rn, number_of_agents)
  tot_subs_cost <- cost_subs_res[[2]]
  ann_subs_cost <- cost_subs_res[[1]]
  
  tot_priv_cost <- cost_priv_res[[2]]
  cum_priv_cost <- cost_priv_res[[1]]
  all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                      tot_priv_cost, LCOE_avg, LCOE_data)
  if (run_w_cap == TRUE) {
    all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                        tot_priv_cost, LCOE_avg, LCOE_data, FiT_outp)
  }
  
  cat(length(adopters), "adopters in run", rn, "\n", sep = " ")
  
  
  return(all_results)
  
  
}


##################################################################################
############################## Projections (future) ##############################
##################################################################################

#--------------------------------- Batch runs -----------------------------------#

batch_run_func_f <- function(agent_name, 
                             number_of_runs, plot_u = T, plot_cost = T, plot_prod = T, save_name) {
  allowed_params <- read_tsv('Data/allowed_params_1000.txt', col_names = F)
  # Set threshold and weights, electricity price
  sample_for_run <- allowed_params[sample(1:nrow(allowed_params), number_of_runs, replace = TRUE), ]

  if(missing(agent_name)) agent_name <- "agents"
  number_of_agents <- length(read_rds(paste('Data/', agent_name, "_1.rds", sep = "")))
  
  if(missing(number_of_runs)) number_of_runs <- 100 
  
  
  initialise_vars() # create variables which will store output
  
  #  if (run_w_cap == TRUE) {
  #    FiT_0 <<- FiT
  #    dep_cap_0 <<- dep_cap
  #  }
  
  for (i1 in 1:number_of_runs) {
    
    if (run_w_cap == TRUE) {
      FiT <<- FiT_0
      dep_cap <<- dep_cap_0
    }
    w <- unlist(sample_for_run[i1, 1:4])
    threshold <- unlist(sample_for_run[i1, 5])
    print(i1)
    
    all_res_rn <<- run_model_f(agent_name, i1, w, threshold) # run the model once 
    
    append_results() # add results of current run to previous results
    
    
  }
  
  rm(all_res_rn, current_date, envir = .GlobalEnv)
  
  summarise_results_f(avg_u, cost, cost_priv) # calculate averages of all runs
  
  
  overall_tot_cost <- tot_cost_priv + tot_cost # total expenditure; used for calcul
  overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
  overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions
  
  prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
  cum_prod <<- prod_res[[1]]
  cum_prod_avg <<- prod_res[[2]]
  
  
  
  
  
  ############# Plotting ############
  
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
          geom_line(data = averages, aes(x = time_series, y = avg_inst_cap)) +
          geom_line(data = deployment, aes(x = time_series, y = avg_cap)))
  
  if (plot_cost == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cost, aes(x = time_series, y = annual_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost, aes(x = time_series, y = annual_cost), color = "black", size = 1))
    print(ggplot() + theme_bw() +
            geom_line(data = cost_priv, aes(x = time_series, y = cum_cost, group = run_number), alpha = 0.2)+
            geom_line(data = avg_cost_priv, aes(x = time_series, y = cum_cost), color = "black", size = 1))
    
    print(ggplot(LCOE_data) + theme_bw() + geom_point(aes(x=adopt_date, y = LCOE_ind, 
                                                          group = run_number, color = run_number), alpha = 0.1))
    
  }
  
  if (plot_prod == T){
    print(ggplot() + theme_bw() + 
            geom_line(data = cum_prod, aes(x = time_series, y = current_prod, group = run_number,
                                           color = run_number)) +
            geom_line(data = cum_prod_avg, aes(x=time_series, y = current_prod), size = 1))
  }
  
  if (run_w_cap == TRUE) {
    avg_FiT <- FiT_levels %>% group_by(time_series) %>% summarise(FiT = mean(FiT))
    print(ggplot() + geom_line(data = FiT_levels, aes(x = time_series, y = FiT, group = run_number,
                                                      color = run_number)) +
            geom_line(data = avg_FiT, aes(x=time_series, y = FiT), size = 1))
  }
  
  print_vars <- paste("w = ", w[1], w[2], w[3], w[4], ", t =", threshold,", n_agents =", number_of_agents)
  
  
  print(ggplot() + theme_bw() + 
          geom_line(data = deployment %>% filter(time_series <= averages$time_series[1]), 
                    aes(x = time_series, y = real_cap), color = "blue", size = 1) + 
          geom_line(data = avg_u, aes(x = time_series, 
                                      y = tot_inst_cap, group = run_number), alpha = 0.2)+
          geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1) +
          annotate("text", x = dmy("01jul2011"), y = 2000, label = print_vars))
  
  if(missing(save_name)){
    cat("\n", "Data not being saved!", "\n", sep = "")
  } else {
    write_rds(avg_u, paste(save_name, "_avg_u.rds", sep = ""))
    write_rds(cost, paste(save_name, "_cost.rds", sep = ""))
    write_rds(cost_priv, paste(save_name, "_cost_priv.rds", sep = ""))
    write_rds(LCOE_data, paste(save_name, "_LCOE_data.rds", sep = ""))
    write_rds(LCOE_avg, paste(save_name, "_LCOE_avg.rds", sep = ""))
    write_rds(FiT, paste(save_name, "_FiT.rds", sep = ""))
    if (run_w_cap == TRUE) write_rds(FiT_levels, paste(save_name, "_FiT_levels.rds", sep = ""))#
  }
  
  to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT)
  
  if (run_w_cap == TRUE) to_return <- list(avg_u, cost, cost_priv, LCOE_data, LCOE_avg, FiT, FiT_levels)
  
  return(to_return)
  
}

#------------------------------- Individual runs --------------------------------#

run_model_f <- function(agent_name, rn, w, threshold) {
  
  # Set up some parameters
  agent_index <- sample.int(10, 1)
  
  time_steps <- nrow(FiT) # number of months in time series
  
  # agents must be generated before run
  
  agents <- read_rds(paste('Data/', agent_name, "_", agent_index, ".rds", sep = ""))
  number_of_agents <- length(agents)
  
  # initial reference capacity:
  
  adopters <- agents[map(agents, "status") == 1]
  
  if (length(adopters) > 0){
    n_owners <<- owner_occupiers[[1, 2]]
    init_cap <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
  }
  else{
    init_cap <- 0
  }
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      mean_u_inc = vector(length = time_steps),
                      mean_u_ec = vector(length = time_steps),
                      mean_u_soc = vector(length = time_steps),
                      mean_u_cap = vector(length = time_steps),
                      mean_u_tot = vector(length = time_steps),
                      sd_u_inc = vector(length = time_steps),
                      sd_u_ec = vector(length = time_steps),
                      sd_u_soc = vector(length = time_steps),
                      sd_u_cap = vector(length = time_steps),
                      sd_u_tot = vector(length = time_steps),
                      frac_of_adopters = vector(length = time_steps),
                      avg_inst_cap = vector(length = time_steps),
                      tot_inst_cap = vector(length = time_steps)
  )
  
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  if(run_w_cap == TRUE) {
    quarter_done <- 0 # how many quarters' capacity have already been used up?
    exceeded <- FALSE # has the total available capacity been exceeded?
  }
  
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
    k <- extract(agents, "status") == "Y"
    avg_u$frac_of_adopters[i] <- length(k[k == TRUE])/number_of_agents
    avg_u$mean_u_ec[i] <- mean(extract(agents, "u_ec"))
    avg_u$mean_u_inc[i] <- mean(extract(agents, "u_inc"))
    avg_u$mean_u_soc[i] <- mean(extract(agents, "u_soc"))
    avg_u$mean_u_cap[i] <- mean(extract(agents, "u_cap"))
    avg_u$mean_u_tot[i] <- mean(extract(agents, "u_tot"))
    avg_u$sd_u_ec[i] <- sd(extract(agents, "u_ec"))
    avg_u$sd_u_inc[i] <- sd(extract(agents, "u_inc"))
    avg_u$sd_u_soc[i] <- sd(extract(agents, "u_soc"))
    avg_u$sd_u_cap[i] <- sd(extract(agents, "u_cap"))
    avg_u$sd_u_tot[i] <- sd(extract(agents, "u_tot"))
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- NA
      avg_u$tot_inst_cap[i] <- 0
    }
    
    ##### Deployment cap code - only runs if using a deployment cap scenario
    if (run_w_cap == TRUE){
      
      
      if ((avg_u$tot_inst_cap[i] - init_cap) > sum(dep_cap$orig_cap) && exceeded == FALSE) { # total available capacity has been exceeded;
        # all further FiTs are zero
        FiT[(i+1):nrow(FiT), 2:4] <<- 0
        exceeded <- TRUE
      }
      
      if (exceeded == FALSE && i < nrow(FiT)) { # Not yet exceeded all the caps & not in the final time step
        
        which_q <- max(which(current_date >= dep_cap$q_dates)) # which quarter are we in?
        
        ref_cap <- avg_u$tot_inst_cap[avg_u$time_series == dep_cap$q_dates[which_q]]
        if(is_empty(ref_cap)) ref_cap <- init_cap
        
        current_quarter <- avg_u$tot_inst_cap[i] - ref_cap # how much capacity has been installed so far in the quarter?
        
        dep_cap$inst_cap[which_q] <<- current_quarter
        
        current_month <- avg_u$tot_inst_cap[i] - avg_u$tot_inst_cap[i-1] # capacity installed this month
        
        excess_cap <- dep_cap$cap[which_q] - current_quarter # how much capacity is left over in the current quarter?
        
        if (current_month > 0 && which_q < nrow(dep_cap) && quarter_done == which_q) {
          # some capacity has been installed this month, not in the final quarter, and we aren't in next q's capacity already
          remaining <- current_month # the remaining installed capacity which must be assigned to this or next month
          
          for (j in 1:(nrow(dep_cap)-which_q)) { # 
            
            if (dep_cap$cap[which_q + j] - remaining < 0) { # if we are exceeding the available capacity in a quarter
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) { # all the capacity has been allocated; break out of the j loop
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (excess_cap < 0 && which_q < nrow(dep_cap) && quarter_done != which_q) {
          # have exceeded the allocated capacity for this quarter
          remaining <- current_quarter - dep_cap$cap[which_q]
          
          quarter_done <- which_q
          for (j in 1:(nrow(dep_cap)-which_q)) {
            
            if (dep_cap$cap[which_q + j] - remaining < 0) {
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
              
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) {
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (current_date == dep_cap$q_dates[which_q] + months(2) && excess_cap > 0) {
          dep_cap$cap[which_q + 1] <<- dep_cap$cap[which_q + 1] + excess_cap
        }
        
      }
      
    }
    
    #### ^^^ End of deployment cap code
    
  }
  
  if (run_w_cap == TRUE && exceeded == TRUE) cat("Total available capacity was exceeded", "\n")
  
  FiT_outp <- cbind(FiT, run_number = rep(rn, nrow(FiT)))
  
  LCOE_data <- calc_LCOE_f(adopters, rn, number_of_agents)
  LCOE_avg <- LCOE_data[[1]]
  LCOE_data <- LCOE_data[[2]]
  cost_subs_res <- subs_cost_f(adopters, rn, number_of_agents)
  cost_priv_res <- priv_cost_f(adopters, rn, number_of_agents)
  tot_subs_cost <- cost_subs_res[[2]]
  ann_subs_cost <- cost_subs_res[[1]]
  
  tot_priv_cost <- cost_priv_res[[2]]
  cum_priv_cost <- cost_priv_res[[1]]
  all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                      tot_priv_cost, LCOE_avg, LCOE_data)
  if (run_w_cap == TRUE) {
    all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                        tot_priv_cost, LCOE_avg, LCOE_data, FiT_outp)
  }
  
  cat(length(adopters), "adopters in run", rn, "\n", sep = " ")
  
  
  return(all_results)
  
  
}