
########################## query_solution() final version ######################

# Function that allows you to query a trained ARF for leaves that contain data
# satisfying constraints that you're interested in. Supports the use of more 
# complicated constraint statements that involve conjunctives/disjunctives over
# several constraints. The whole constraint statement is represented as a tree, 
# with the individual constraints at the leaf nodes and connectives between them
# at the inner nodes. The leafIDs satisfying the individual constraints are found
# and stored as attribtutes of the leaf nodes, then a forward pass of the tree
# evaluates the connective statements between these leafID lists until the root 
# node is reached. 

library(data.tree)

# input: query - a query in the form of a series of conjunctives and disjunctives 
#                over constraints on variables you are interested in. (example given underneath)
#        params - the psi table from a trained ARF
#
# output: a list of leafIDs of leaves from the ARF that contain data that adheres to the 
#         constraints in the input. 
query_solution <- function(query, params) {
  solve_constraint <- function(var, op, value, params) {
    if (is.numeric(value)) {
      psi <- params$cnt[variable == var]
      if (op == '==') {
        out <- psi[value > min & value < max, f_idx]
      } else if (op == '!=')  {
        out <- psi[(min > value | max < value), f_idx]
      } else if (op == '<') {
        out <- psi[max < value, f_idx]
      } else if (op == '<=') {
        out <- psi[max <= value, f_idx]
      } else if (op == '>') {
        out <- psi[min > value, f_idx]
      } else if (op == '>=') {
        out <- psi[min >= value, f_idx]
      }
    } else {
      psi <- params$cat[variable == var]
      if (op == '==') {
        out <- psi[val == value & prob > 0, f_idx]
      } else if (op == '!=') {
        tmp <- psi[, value %in% val, by = f_idx]
        out <- tmp[V1 == FALSE, f_idx]
      }
    }
    return(out)
  }
  
  get_leaves <- function(node) {
    return(solve_constraint(node$variable, node$operator, node$value, params))
  }
  
  query$Do(function(node)
    node$leafIds <- get_leaves(node), filterFun = isLeaf)
  # this gets the leaves from the params table that satisfy the constraints in the
  # leaves of the query data tree. Stores these leafs as a node attribute. After this
  # is run, solve_query can be run.
  
  solve_query <- function(node) {
    if (isRoot(node$parent)) {
      connective <- node$name
    } else {
      connective <- node$parent$name
    }
    children <- node$children
    children_leafIds <- list()
    solution <- numeric()
    for (item in node$children) {
      if (length(solution) == 0) {
        solution <- c(solution, item$leafIds)
        next
      }
      if (isRoot(node)) {
        next
      }
      if (connective == "NA") {
        solution <- union(solution, item$leafIds)
      }
      if (connective == "OR") {
        solution <- union(solution, item$leafIds)
      }
      if (connective == "AND") {
        solution <- intersect(solution, item$leafIds)
      }
    }
    node$leafIds <- solution
  }
  query$Do(solve_query, filterFun = isNotLeaf, traversal = "post-order")
  final_solution <- query$leafIds
  return(final_solution)
}

# making an example tree of a CNF query:
query_table = data.frame(connect1 = c("OR","OR","OR","OR","OR"), 
                         connect2 = c("NA","OR","OR","OR","OR"),
                         connect3 = c("NA","AND","AND","AND","AND"),
                         constraints = c("5","1,2","1,2","3,4","3,4"),
                         constrainNo = c(5, 1,2,3,4),
                         variable = c("Petal.Width", "Petal.Width", 
                                      "Sepal.Length","Petal.Length","Sepal.Width"),
                         operator = c(">","==","<",">","=="),
                         value = c(1,5,7,8,2))

query_table$pathString <- paste("query_tree",
                                query_table$connect1,
                                query_table$connect2,
                                query_table$connect3,
                                query_table$constraints,
                                query_table$constrainNo,
                                sep = "/")

query <- as.Node(query_table)
print(query, "variable","operator","value")

arf <- adversarial_rf(iris)
psi <- forde(arf, iris)
print(query_solution(query, psi))


############################ Step by step: #####################################

# useful if you want to see how this works

arf <- adversarial_rf(iris)
psi <- forde(arf, iris)
library(data.tree)

# making an example tree of a CNF query:
query_table = data.frame(connect1 = c("OR","OR","OR","OR","OR"), 
                         connect2 = c("NA","OR","OR","OR","OR"),
                         connect3 = c("NA","AND","AND","AND","AND"),
                         constraints = c("5","1,2","1,2","3,4","3,4"),
                         constrainNo = c(5, 1,2,3,4),
                         variable = c("Petal.Width", "Petal.Width", 
                                      "Sepal.Length","Petal.Length","Sepal.Width"),
                         operator = c(">","==","<",">","=="),
                         value = c(1,5,7,8,2))
# This is the equivalent of ((1 & 2) | (3 & 4)) | 5

query_table$pathString <- paste("query_tree",
                                query_table$connect1,
                                query_table$connect2,
                                query_table$connect3,
                                query_table$constraints,
                                query_table$constrainNo,
                                sep = "/")

query <- as.Node(query_table)
print(query, "variable","operator","value")

constraints <- query_table[,c("variable", "operator", "value")]

var <- constraints$variable[3]
op <- constraints$operator[3]
value <- constraints$value[3]

solve_constraint <- function(var, op, value, params) {
  if (is.numeric(value)) {
    psi <- params$cnt[variable == var]
    if (op == '==') {
      out <- psi[value > min & value < max, f_idx]
    } else if (op == '!=')  {
      out <- psi[(min > value | max < value), f_idx]
    } else if (op == '<') {
      out <- psi[max < value, f_idx]
    } else if (op == '<=') {
      out <- psi[max <= value, f_idx]
    } else if (op == '>') {
      out <- psi[min > value, f_idx]
    } else if (op == '>=') {
      out <- psi[min >= value, f_idx]
    }
  } else {
    psi <- params$cat[variable == var]
    if (op == '==') {
      out <- psi[val == value & prob > 0, f_idx]
    } else if (op == '!=') {
      tmp <- psi[, value %in% val, by = f_idx]
      out <- tmp[V1 == FALSE, f_idx]
    }
  }
  return(out)
}
print(solve_constraint(var,op,value,psi))

get_leaves <- function(node) {
  return(solve_constraint(node$variable, node$operator, node$value, psi))
}
constraint_solutions <- query$Get(get_leaves, filterFun = isLeaf) 
# this gets the leaves from the params table that satisfy the constraints in the 
# leaves of the query data tree. Stored in a list. Corresponding leaves for a 
# given constraint are accessed by typing constraint_solutions$"<constraint number>"
# you must use the quotation marks!

query$Do(function(node) node$leafIds <- get_leaves(node), filterFun = isLeaf)
# this gets the leaves from the params table that satisfy the constraints in the 
# leaves of the query data tree. Stores these leafs as a node attribute. After this
# is run, solve_query can be run.

solve_query <- function(node) {
  if (isRoot(node$parent)) {
    connective <- node$name
  } else {
    connective <- node$parent$name
  }
  children <- node$children
  children_leafIds <- list()
  solution <- numeric()
  for (item in node$children) {
    if (length(item$leafIds) != 0) {
      children_leafIds <- append(children_leafIds, list(item$leafIds))
    }
    if (length(solution) == 0) {
      solution <- c(solution, item$leafIds)
      next
    }
    if (isRoot(node)) {
      next
    }
    if (connective == "NA") {
      solution <- union(solution, item$leafIds)
    }
    if (connective == "OR") {
      solution <- union(solution, item$leafIds)
    }
    if (connective == "AND") {
      solution <- intersect(solution, item$leafIds)
    }
  }
  node$leafIds <- solution
}
query$Do(solve_query, filterFun = isNotLeaf, traversal = "post-order")
final_solution <- query$leafIds
