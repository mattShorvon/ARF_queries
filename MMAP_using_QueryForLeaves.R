
# takes a coalition that you want to calculate the value of, and turns it into
# a form that query_solution() can use. 
coalition_to_constraints <- function(coalition, var_names) {
  columns = c("connect1","constraints","constNo","variable","operator","value")
  query_table = data.frame(matrix(nrow = 0, ncol = length(columns)))
  i <- 0
  num_na <- 0
  col_1 = "AND"
  col_2 = paste(as.character(1:length(coalition)), collapse = ',')
  col_5 = "=="
  for (value in coalition) {
    i <- i + 1
    if (is.na(value)){
      num_na <- num_na + 1
      next
    }
    col_3 = i
    col_4 = var_names[i]
    col_6 = value
    query_table[nrow(query_table) + 1,] <- list(col_1, col_2, col_3, col_4, col_5,
                                                col_6)
  }
  if (num_na == length(coalition)) { # the case where the coalition is completely unfilled
    i <- 0
    col_5 = "<"
    for (value in coalition) {
      i <- i + 1
      col_3 = i
      col_4 = var_names[i]
      col_6 = Inf
      query_table[nrow(query_table) + 1,] <- list(col_1, col_2, col_3, col_4, col_5,
                                                  col_6)
    }
  }
  colnames(query_table) = columns
  return(query_table)
}

# SEEM TO REMEMBER THERE MIGHT HAVE BEEN A MISTAKE SOMEWHERE IN THIS. Need to check. 
# Compare to output of MAP_for_shapley15(). 
MAP_for_shapley <- function(coalition, var_names, params) {
  # Create constraints from the input coalition, find the leaves that satisfy 
  # these constraints.
  query_table <- coalition_to_constraints(coalition, var_names)
  query_table$pathString <- paste("query_tree",
                                  query_table$connect1,
                                  query_table$constraints,
                                  query_table$constrainNo,
                                  sep = "/")
  query <- as.Node(query_table)
  leaves <- query_solution(query, params)
  
  # Use list of leaves to find corresponding leaf info from params table
  leaves_info_cnt <- params[["cnt"]][f_idx %in% leaves, .(variable, mu), f_idx]
  leaves_cnt_cvg <- params$forest[f_idx %in% leaves, cvg, f_idx]
  leaves_info_cnt <- merge(leaves_info_cnt, leaves_cnt_cvg, by = 'f_idx')
  leaves_info_cat <- params[["cat"]][f_idx %in% leaves, .(variable, val, prob), f_idx]
  leaves_cat_cvg <- params$forest[f_idx %in% leaves, cvg, f_idx]
  leaves_info_cat <- merge(leaves_info_cat, leaves_cat_cvg, by = 'f_idx')
  
  # Find MAPs for the missing features:
  for (i in 1:length(coalition)) { # (i in 1:length(coalition)) put in after testing
    if (!is.na(coalition[i])) {
      next
    }
    if (var_names[i] %in% params[["cnt"]]$variable) {
      var = var_names[i]
      coalition[i] <- weighted.mean(leaves_info_cnt[variable == var,]$mu, 
                                    leaves_info_cnt[variable == var,]$cvg)
    }
    else {
      values <- data.table(val = character(), avg_prob = numeric())
      var = var_names[i]
      sum <- 0
      k <- 0
      for (value in unique(leaves_info_cat[variable == var, val], by = "val")) {
        k <- k + 1
        values <- rbind(values, list(value,weighted.mean(leaves_info_cat[variable == var & val == value,]$prob, 
                                                         leaves_info_cat[variable == var & val == value,]$cvg)))
      }
      print(values)
      max <- values[, .SD[which.max(avg_prob)]]
      coalition[i] <- max$val   # this coerces the coalition vector into character type tho. 
    }
  }
  return(coalition)
}
