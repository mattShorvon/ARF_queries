
# A faster version that doesn't use query_solution(). 

library(data.table)
library(doParallel)  
no_cores <- detectCores() - 2  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)  

MAP_for_shapley15 <- function(coalition, psi) {
  # Find leaves that satisfy constraints from the input coalition:
  coal <- coalition[!is.na(value),]
  leafIDs = as.list(rep(NA, nrow(coal)))
  leaves = numeric()
  for (i in sequence(nrow(coal))) {
    if (coal[i, type] == "cnt") {
      leafIDs[[i]] <- psi$cnt[variable == coal[i, variable]][coal[i, value] > min & coal[i, value] < max, f_idx]
    }
    else {
      leafIDs[[i]] <- psi$cat[variable == coal[i, variable]][coal[i, value] == value & prob > 0, f_idx]
    }
  }
  leaves <- Reduce(intersect, leafIDs)
  if (nrow(coal[type == "cnt"]) > 0) {
    leaves_info_cnt <- merge(psi[["cnt"]][f_idx %in% leaves & variable %in% coalition[, unique(variable)], .(variable, mu, f_idx)], 
                             psi$forest[f_idx %in% leaves, .(cvg, f_idx)], 
                             by = 'f_idx', sort = F) # set sort to false, you don't need to sort by fidx, by call in the merge!
  }
  if (nrow(coal[type=="cat"]) > 0) {
    leaves_info_cat <- merge(psi[["cat"]][f_idx %in% leaves & variable %in% coalition[, unique(variable)], .(variable, val, prob, f_idx)], 
                             psi$forest[f_idx %in% leaves, .(cvg, f_idx)], 
                             by = 'f_idx', sort = F)
  }
  for (i in 1:nrow(coalition)) { 
    if (!is.na(coalition[i, value])) {
      next
    }
    if (coalition[i, variable] %in% psi[["cnt"]][, unique(variable)]) {
      var = coalition[i, variable]
      coalition[i, value := weighted.mean(leaves_info_cnt[variable == var,]$mu, 
                                          leaves_info_cnt[variable == var,]$cvg)]
    }
    else {
      values <- data.table(val = character(), avg_prob = numeric())
      var = coalition[i, variable]
      sum <- 0
      k <- 0
      for (value in unique(leaves_info_cat[variable == var, val], by = "val")) {
        k <- k + 1
        values <- rbind(values, list(value,weighted.mean(leaves_info_cat[variable == var & val == value,]$prob, 
                                                         leaves_info_cat[variable == var & val == value,]$cvg)))
      }
      print(values)
      max <- values[, .SD[which.max(avg_prob)]]
      coalition[[i]] <- max$val   # this coerces the coalition vector into character type tho. 
    }
  }
  # print("MAP imputation: ")
  # print(Sys.time() - start_time)
  print(coalition)
  return(coalition) 
}

# functions for use with shapr (Aas et al.): 

prepare_data.arf <- function(x = explainer, index_features = NULL, psi = psi , ...) {
  # start_t = Sys.time()
  dt <- data.table(x$x_test) 
  dt[,id := .I]
  dt <- dt[rep(seq_len(nrow(x$x_test)), each = 2^ncol(x$x_test))]
  dt <- as.matrix(dt)
  S <- matrix(rep(t(x$S),nrow(x$x_test)),ncol=ncol(x$S),byrow=TRUE)
  coal <- matrix(NA, nrow = nrow(S), ncol = ncol(S))
  coal[,][S == 1] <- dt[,1:(ncol(dt)-1)][S == 1]
  dt <- as.data.table(coal)
  colnames(dt) <- x$feature_list$labels
  dt[, id := rep(seq_along(1:nrow(x$x_test)), each = 2^ncol(x$x_test))]
  dt[,id_combination := rep(seq_along(1:2^ncol(x$x_test)),nrow(x$x_test))]
  dt <- melt(dt, id.vars = c("id", "id_combination"))
  dt <- dt[order(id, id_combination)]
  var_names_cnt = psi[["cnt"]][, unique(variable)]
  is_cnt <- x$feature_list$labels %in% var_names_cnt
  for (i in 1:length(x$feature_list$labels)) {
    if (is_cnt[[i]]) {
      dt[variable == x$feature_list$labels[[i]], type := "cnt"]
    } 
    else {
      dt[variable == x$feature_list$labels[[i]], type := "cat"]
    }
  }
  # print(Sys.time() - start_t) # around 0.035 seconds on old laptop
  dt[, row_id := .GRP, by = c("id", "id_combination")]
  to_eval <- dt[id_combination != 1 & id_combination != 2^ncol(x$x_test), unique(row_id)] 
  dt[row_id %in% to_eval,] <- foreach(i = 1:length(to_eval), .combine = rbind, .packages = "data.table", .export = "MAP_for_shapley15") %dopar% {
    MAP_for_shapley15(dt[row_id == to_eval[i],], psi) # it seems to think I'm alr exporting .export = c("psi", "MAP_for_shapley15"), so took out
  }
  dt[, type := NULL]
  dt <- dcast(dt, row_id + id + id_combination ~ variable, value.var = "value")
  dt[, w := 1]
  dt[, row_id := NULL]
  return(dt)
}

explain.arf <- function(x, explainer, approach, psi, prediction_zero, n_batches = 1) {
  
  # Don't think I'm doing anything that needs a seed to be set?
  
  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  
  r <- prepare_and_predict(explainer, n_batches, prediction_zero, psi)
}