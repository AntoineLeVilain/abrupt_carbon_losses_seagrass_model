#hd_deter_ma

setwd('/sensitivity/3_f') # set your working directory


# Requirements
package_list <- c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "doParallel", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv")

for (package in package_list){
  
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  
  library(package, character.only = TRUE)
  
}

# Second solver in case the first one does not work
multisolve2 <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Initialize an empty dataframe to store equilibria
  equilibrium <- data.frame(matrix(0, ncol = length(model(0)), nrow = 1))
  colnames(equilibrium) <- names(model(0))
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Iterate over each combination
  for (i in 1:dim(combinations)[1]) {
    
    solutions_found <- FALSE # Flag to check if solution was found without error
    
    # Try to find the root using multiroot
    tryCatch({
      
      solutions <- multiroot(f = model, start = as.numeric(combinations[i, ]), positive = TRUE, useFortran = TRUE)
      solutions_found <- TRUE
    },
    warning = function(w) {
      # Silently handle warnings
    },
    error = function(e) {
      # Silently handle errors
    })
    
    # If no solution found, skip to the next iteration
    if (!solutions_found) {
      next
    }
    
    # If first iteration, save the root directly
    if (i == 1) {
      equilibrium[1, ] <- solutions$root
    } else {
      # For subsequent iterations, check if the solution has been found before
      for (j in 1:dim(equilibrium)[2]) {
        if (j == 1) {
          current <- equilibrium %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        } else {
          current <- current %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        }
      }
      
      # If solution is not already in the list, add it to the equilibrium dataframe
      if (dim(current)[1] == 0) {
        equilibrium <- rbind(equilibrium, solutions$root)
      }
    }
    
  }
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(round(equilibrium, 2))
}

# Solver for the asymptotic dynamics
multisolve <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Solve for each combination
  equilibrium <- searchZeros(as.matrix(combinations), fn = model, control = list(xtol = 10e-10, ftol = 10e-10, allowSingular=TRUE))$x
  
  if (is.null(equilibrium)){
    
    equilibrium <- multisolve2(model, lower_limit, upper_limit, iter)
    
  }
  
  equilibrium <- round(as.data.frame(equilibrium), 2)
  equilibrium <- equilibrium %>%
    select_if(is.numeric) %>%
    filter(apply(., 1, function(x) all(x >= 0)))
  
  equilibrium <- distinct(equilibrium)
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(equilibrium)
  
}

# Model
model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) - ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

rmax <- 0.011
ms <- 0.0014
topt <- 298.85
tmax <- 307.05
nr <- 0.015
i0 <- 709
ir <- 296
sh <- 22.7
hp <- 0.023
a <- 0.07
mh <- 0.028
alpha <- 0.194
h <- 0.68
phida <- 0.024
phids <- 0.00024
phidb <- 0.00005
ea <- 58000
r <- 8.314
td <- 303.65
hea <- 12.53
hes <- 12.53
fb1 <- 0.0022
fb2 <- 1335
beta <- 0.292
gamma <- 0.137
delta <- 0.052
l <- 0.01
da <- 0.125/6

z <- 40
temp <- 298.85
hmax <- 0.05
pmax <- 0.5
ma <- 0.0014
i <- 0.003975034

# multisolve parameters

iv1_max <- 5000
iv2_max <- 5
iv3_max <- 5
iv4_max <- 5
iv5_max <- 0.1
upper_limit <- c(iv1_max, iv2_max, iv3_max, iv4_max, iv5_max)
lower_limit <- c(10, 0, 0, 0, 0)
iter <- 2

########################################################

N <- 5000
params <- c(
  "rmax", "topt", "tmax", "l", "nr", "i0", "ir", "hp", "a", "mh", "alpha", "h", "phida", "phids", "phidb", "ea", "hea", "hes", "fb1", "fb2", "beta", "gamma", "delta", "ms", "td", "da"
)
order <- "first"
R <- 1000
#true_R <- 50
type <- "norm"
conf <- "0.95"

mat <- sobol_matrices(N = N, params = params, order = order)

per <- 0.05

ranges <- list(
  rmax = c(0.011*(1-per), 0.011*(1+per)),
  topt = c(297.565, 300.135),
  tmax = c(305.355, 308.745),
  nr = c(0.015*(1-per), 0.015*(1+per)),
  i0 = c(709*(1-per), 709*(1+per)),
  ir = c(296*(1-per), 296*(1+per)),
  hp = c(0.023*(1-per), 0.023*(1+per)),
  a = c(0.07*(1-per), 0.07*(1+per)),
  mh = c(0.028*(1-per), 0.028*(1+per)),
  alpha = c(0.194*(1-per), 0.194*(1+per)),
  h = c(0.68*(1-per), 0.68*(1+per)),
  phida = c(0.024*(1-per), 0.024*(1+per)),
  phids = c(0.00024*(1-per), 0.00024*(1+per)),
  phidb = c(0.00005*(1-per),0.00005*(1+per)),
  ea = c(58000*(1-per), 58000*(1+per)),
  hea = c(12.53*(1-per), 12.53*(1+per)),
  hes = c(12.53*(1-per), 12.53*(1+per)),
  fb1 = c(0.0022*(1-per), 0.0022*(1+per)),
  fb2 = c(1335*(1-per), 1335*(1+per)),
  beta = c(0.292*(1-per), 0.292*(1+per)),
  gamma = c(0.137*(1-per), 0.137*(1+per)),
  delta = c(0.052*(1-per), 0.052*(1+per)),
  l = c(0.01*(1-per), 0.01*(1+per)),
  ms = c(0.0014*(1-per), 0.0014*(1+per)),
  td = c(302.125, 305.175),
  da = c(0.125/6*(1-per), 0.125/6*(1+per))
)

for (param in names(ranges)) {
  mat[, param] <- qunif(mat[, param], ranges[[param]][1], ranges[[param]][2])
}

n.cores <- makeCluster(9)
registerDoParallel(n.cores)

# Create a progress file
progress_file <- "progress_hd_deter_ma.txt"
writeLines("Starting computation", progress_file)

# Run the parallel computation storing results in a list
results_list <- foreach(r_index = 1:nrow(mat), .packages = c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv"), .multicombine = TRUE, .inorder = FALSE) %dopar% {
  
  # Write the current index to the progress file
  cat(sprintf("Processing index: %d\n", r_index), file = progress_file, append = TRUE)
  
  for (c_index in 1:length(params)) {
    
    assign(params[c_index], as.numeric(mat[r_index, c_index]))
    
  }
  
  eq <- multisolve(model, lower_limit, upper_limit, iter)
  eq <- eq %>% 
    filter(S == max(S)) %>%
    mutate(index = r_index)
  
  if (nrow(eq) > 1){
    
    eq <- eq[1, ]
    
  }
  
  eq
  
}

# Combine the results into a single dataframe
full.dt <- do.call(rbind, results_list)

rm(results_list)
rm(mat)

full.dt <- full.dt[order(full.dt$index), ]

time <- rep(10000, nrow(full.dt))
full.dt <- cbind(time, full.dt)

write.csv(full.dt, "hd_deter_ma.csv")

#SEAGRASS

indices.dt <- melt(data.table(full.dt), measure.vars = c("S"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_ma_S.RData")
saveRDS(ind.dummy, file="hd_deter_ma_S.dummy.RData")

#CARBON

indices.dt <- melt(data.table(full.dt), measure.vars = c("CB"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_ma_C.RData")
saveRDS(ind.dummy, file="hd_deter_ma_C.dummy.RData")

# end parallelisation
stopCluster(n.cores)

rm(full.dt)

##hd_deter_pmax

# Requirements
package_list <- c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "doParallel", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv")

for (package in package_list){
  
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  
  library(package, character.only = TRUE)
  
}

# Second solver in case the first one does not work
multisolve2 <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Initialize an empty dataframe to store equilibria
  equilibrium <- data.frame(matrix(0, ncol = length(model(0)), nrow = 1))
  colnames(equilibrium) <- names(model(0))
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Iterate over each combination
  for (i in 1:dim(combinations)[1]) {
    
    solutions_found <- FALSE # Flag to check if solution was found without error
    
    # Try to find the root using multiroot
    tryCatch({
      
      solutions <- multiroot(f = model, start = as.numeric(combinations[i, ]), positive = TRUE, useFortran = TRUE)
      solutions_found <- TRUE
    },
    warning = function(w) {
      # Silently handle warnings
    },
    error = function(e) {
      # Silently handle errors
    })
    
    # If no solution found, skip to the next iteration
    if (!solutions_found) {
      next
    }
    
    # If first iteration, save the root directly
    if (i == 1) {
      equilibrium[1, ] <- solutions$root
    } else {
      # For subsequent iterations, check if the solution has been found before
      for (j in 1:dim(equilibrium)[2]) {
        if (j == 1) {
          current <- equilibrium %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        } else {
          current <- current %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        }
      }
      
      # If solution is not already in the list, add it to the equilibrium dataframe
      if (dim(current)[1] == 0) {
        equilibrium <- rbind(equilibrium, solutions$root)
      }
    }
    
  }
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(round(equilibrium, 2))
}

# Solver for the asymptotic dynamics
multisolve <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Solve for each combination
  equilibrium <- searchZeros(as.matrix(combinations), fn = model, control = list(xtol = 10e-10, ftol = 10e-10, allowSingular=TRUE))$x
  
  if (is.null(equilibrium)){
    
    equilibrium <- multisolve2(model, lower_limit, upper_limit, iter)
    
  }
  
  equilibrium <- round(as.data.frame(equilibrium), 2)
  equilibrium <- equilibrium %>%
    select_if(is.numeric) %>%
    filter(apply(., 1, function(x) all(x >= 0)))
  
  equilibrium <- distinct(equilibrium)
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(equilibrium)
  
}

# Model
model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) - ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

rmax <- 0.011
ms <- 0.0014
topt <- 298.85
tmax <- 307.05
nr <- 0.015
i0 <- 709
ir <- 296
sh <- 22.7
hp <- 0.023
a <- 0.07
mh <- 0.028
alpha <- 0.194
h <- 0.68
phida <- 0.024
phids <- 0.00024
phidb <- 0.00005
ea <- 58000
r <- 8.314
td <- 303.65
hea <- 12.53
hes <- 12.53
fb1 <- 0.0022
fb2 <- 1335
beta <- 0.292
gamma <- 0.137
delta <- 0.052
l <- 0.01
da <- 0.125/6

z <- 40
temp <- 298.85
hmax <- 0.05
pmax <- 0.6
ma <- 0
i <- 0.003975034

# multisolve parameters

iv1_max <- 5000
iv2_max <- 5
iv3_max <- 5
iv4_max <- 5
iv5_max <- 0.1
upper_limit <- c(iv1_max, iv2_max, iv3_max, iv4_max, iv5_max)
lower_limit <- c(10, 0, 0, 0, 0)
iter <- 2

########################################################

N <- 5000
params <- c(
  "rmax", "topt", "tmax", "l", "nr", "i0", "ir", "hp", "a", "mh", "alpha", "h", "phida", "phids", "phidb", "ea", "hea", "hes", "fb1", "fb2", "beta", "gamma", "delta", "ms", "td", "da"
)
order <- "first"
R <- 1000
#true_R <- 50
type <- "norm"
conf <- "0.95"

mat <- sobol_matrices(N = N, params = params, order = order)

per <- 0.05

ranges <- list(
  rmax = c(0.011*(1-per), 0.011*(1+per)),
  topt = c(297.565, 300.135),
  tmax = c(305.355, 308.745),
  nr = c(0.015*(1-per), 0.015*(1+per)),
  i0 = c(709*(1-per), 709*(1+per)),
  ir = c(296*(1-per), 296*(1+per)),
  hp = c(0.023*(1-per), 0.023*(1+per)),
  a = c(0.07*(1-per), 0.07*(1+per)),
  mh = c(0.028*(1-per), 0.028*(1+per)),
  alpha = c(0.194*(1-per), 0.194*(1+per)),
  h = c(0.68*(1-per), 0.68*(1+per)),
  phida = c(0.024*(1-per), 0.024*(1+per)),
  phids = c(0.00024*(1-per), 0.00024*(1+per)),
  phidb = c(0.00005*(1-per),0.00005*(1+per)),
  ea = c(58000*(1-per), 58000*(1+per)),
  hea = c(12.53*(1-per), 12.53*(1+per)),
  hes = c(12.53*(1-per), 12.53*(1+per)),
  fb1 = c(0.0022*(1-per), 0.0022*(1+per)),
  fb2 = c(1335*(1-per), 1335*(1+per)),
  beta = c(0.292*(1-per), 0.292*(1+per)),
  gamma = c(0.137*(1-per), 0.137*(1+per)),
  delta = c(0.052*(1-per), 0.052*(1+per)),
  l = c(0.01*(1-per), 0.01*(1+per)),
  ms = c(0.0014*(1-per), 0.0014*(1+per)),
  td = c(302.125, 305.175),
  da = c(0.125/6*(1-per), 0.125/6*(1+per))
)

for (param in names(ranges)) {
  mat[, param] <- qunif(mat[, param], ranges[[param]][1], ranges[[param]][2])
}

n.cores <- makeCluster(9)
registerDoParallel(n.cores)

# Create a progress file
progress_file <- "progress_hd_deter_pmax.txt"
writeLines("Starting computation", progress_file)

# Run the parallel computation storing results in a list
results_list <- foreach(r_index = 1:nrow(mat), .packages = c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv"), .multicombine = TRUE, .inorder = FALSE) %dopar% {
  
  # Write the current index to the progress file
  cat(sprintf("Processing index: %d\n", r_index), file = progress_file, append = TRUE)
  
  for (c_index in 1:length(params)) {
    
    assign(params[c_index], as.numeric(mat[r_index, c_index]))
    
  }
  
  eq <- multisolve(model, lower_limit, upper_limit, iter)
  eq <- eq %>% 
    filter(S == max(S)) %>%
    mutate(index = r_index)
  
  if (nrow(eq) > 1){
    
    eq <- eq[1, ]
    
  }
  
  eq
  
}

# Combine the results into a single dataframe
full.dt <- do.call(rbind, results_list)

rm(results_list)
rm(mat)

full.dt <- full.dt[order(full.dt$index), ]

time <- rep(10000, nrow(full.dt))
full.dt <- cbind(time, full.dt)

write.csv(full.dt, "hd_deter_pmax.csv")

#SEAGRASS

indices.dt <- melt(data.table(full.dt), measure.vars = c("S"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_pmax_S.RData")
saveRDS(ind.dummy, file="hd_deter_pmax_S.dummy.RData")

#CARBON

indices.dt <- melt(data.table(full.dt), measure.vars = c("CB"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_pmax_C.RData")
saveRDS(ind.dummy, file="hd_deter_pmax_C.dummy.RData")

# end parallelisation
stopCluster(n.cores)

rm(full.dt)

#hd_deter_temp

# Requirements
package_list <- c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "doParallel", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv")

for (package in package_list){
  
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  
  library(package, character.only = TRUE)
  
}

# Second solver in case the first one does not work
multisolve2 <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Initialize an empty dataframe to store equilibria
  equilibrium <- data.frame(matrix(0, ncol = length(model(0)), nrow = 1))
  colnames(equilibrium) <- names(model(0))
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Iterate over each combination
  for (i in 1:dim(combinations)[1]) {
    
    solutions_found <- FALSE # Flag to check if solution was found without error
    
    # Try to find the root using multiroot
    tryCatch({
      
      solutions <- multiroot(f = model, start = as.numeric(combinations[i, ]), positive = TRUE, useFortran = TRUE)
      solutions_found <- TRUE
    },
    warning = function(w) {
      # Silently handle warnings
    },
    error = function(e) {
      # Silently handle errors
    })
    
    # If no solution found, skip to the next iteration
    if (!solutions_found) {
      next
    }
    
    # If first iteration, save the root directly
    if (i == 1) {
      equilibrium[1, ] <- solutions$root
    } else {
      # For subsequent iterations, check if the solution has been found before
      for (j in 1:dim(equilibrium)[2]) {
        if (j == 1) {
          current <- equilibrium %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        } else {
          current <- current %>%
            filter((solutions$root[j] * 0.99 - max(upper_limit) * 0.0001) <= (!!sym(colnames(equilibrium)[j]))) %>%
            filter((!!sym(colnames(equilibrium)[j])) <= (solutions$root[j] * 1.01 + max(upper_limit) * 0.0001))
        }
      }
      
      # If solution is not already in the list, add it to the equilibrium dataframe
      if (dim(current)[1] == 0) {
        equilibrium <- rbind(equilibrium, solutions$root)
      }
    }
    
  }
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(round(equilibrium, 2))
}

# Solver for the asymptotic dynamics
multisolve <- function(model, lower_limit, upper_limit, iter) {
  
  # model is the model whose equilibria you want to find (function INPUT = list of variables, OUTPUT = list of dynamics)
  # lower_limit is a list of all lower limits of all initial values for all dimensions (LIST)
  # upper_limit is a list of all upper limits of all initial values for all dimensions (LIST)
  # iter is the number of iteration for each dimension, total number of iterations is iter^dimensions (NUMERIC)
  
  # Generate sequences for each dimension based on the lower and upper limits
  lower_upper_list <- lapply(1:length(lower_limit), function(i) seq(lower_limit[i], upper_limit[i], length.out = iter))
  
  # Generate all possible combinations of the sequences
  combinations <- expand.grid(lower_upper_list)
  colnames(combinations) <- names(model(0))
  combinations <- rbind(rep(0, length(lower_limit)), combinations) # Add a row of zeros
  
  # Solve for each combination
  equilibrium <- searchZeros(as.matrix(combinations), fn = model, control = list(xtol = 10e-10, ftol = 10e-10, allowSingular=TRUE))$x
  
  if (is.null(equilibrium)){
    
    equilibrium <- multisolve2(model, lower_limit, upper_limit, iter)
    
  }
  
  equilibrium <- round(as.data.frame(equilibrium), 2)
  equilibrium <- equilibrium %>%
    select_if(is.numeric) %>%
    filter(apply(., 1, function(x) all(x >= 0)))
  
  equilibrium <- distinct(equilibrium)
  
  # Return the equilibrium solutions, rounded to two decimal places
  return(equilibrium)
  
}

# Model
model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) - ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

rmax <- 0.011
ms <- 0.0014
topt <- 298.85
tmax <- 307.05
nr <- 0.015
i0 <- 709
ir <- 296
sh <- 22.7
hp <- 0.023
a <- 0.07
mh <- 0.028
alpha <- 0.194
h <- 0.68
phida <- 0.024
phids <- 0.00024
phidb <- 0.00005
ea <- 58000
r <- 8.314
td <- 303.65
hea <- 12.53
hes <- 12.53
fb1 <- 0.0022
fb2 <- 1335
beta <- 0.292
gamma <- 0.137
delta <- 0.052
l <- 0.01
da <- 0.125/6

z <- 40
temp <- 303
hmax <- 0.05
pmax <- 0.5
ma <- 0
i <- 0.003975034

# multisolve parameters

iv1_max <- 5000
iv2_max <- 5
iv3_max <- 5
iv4_max <- 5
iv5_max <- 0.1
upper_limit <- c(iv1_max, iv2_max, iv3_max, iv4_max, iv5_max)
lower_limit <- c(10, 0, 0, 0, 0)
iter <- 2

########################################################

N <- 5000
params <- c(
  "rmax", "topt", "tmax", "l", "nr", "i0", "ir", "hp", "a", "mh", "alpha", "h", "phida", "phids", "phidb", "ea", "hea", "hes", "fb1", "fb2", "beta", "gamma", "delta", "ms", "td", "da"
)
order <- "first"
R <- 1000
#true_R <- 50
type <- "norm"
conf <- "0.95"

mat <- sobol_matrices(N = N, params = params, order = order)

per <- 0.05

ranges <- list(
  rmax = c(0.011*(1-per), 0.011*(1+per)),
  topt = c(297.565, 300.135),
  tmax = c(305.355, 308.745),
  nr = c(0.015*(1-per), 0.015*(1+per)),
  i0 = c(709*(1-per), 709*(1+per)),
  ir = c(296*(1-per), 296*(1+per)),
  hp = c(0.023*(1-per), 0.023*(1+per)),
  a = c(0.07*(1-per), 0.07*(1+per)),
  mh = c(0.028*(1-per), 0.028*(1+per)),
  alpha = c(0.194*(1-per), 0.194*(1+per)),
  h = c(0.68*(1-per), 0.68*(1+per)),
  phida = c(0.024*(1-per), 0.024*(1+per)),
  phids = c(0.00024*(1-per), 0.00024*(1+per)),
  phidb = c(0.00005*(1-per),0.00005*(1+per)),
  ea = c(58000*(1-per), 58000*(1+per)),
  hea = c(12.53*(1-per), 12.53*(1+per)),
  hes = c(12.53*(1-per), 12.53*(1+per)),
  fb1 = c(0.0022*(1-per), 0.0022*(1+per)),
  fb2 = c(1335*(1-per), 1335*(1+per)),
  beta = c(0.292*(1-per), 0.292*(1+per)),
  gamma = c(0.137*(1-per), 0.137*(1+per)),
  delta = c(0.052*(1-per), 0.052*(1+per)),
  l = c(0.01*(1-per), 0.01*(1+per)),
  ms = c(0.0014*(1-per), 0.0014*(1+per)),
  td = c(302.125, 305.175),
  da = c(0.125/6*(1-per), 0.125/6*(1+per))
)

for (param in names(ranges)) {
  mat[, param] <- qunif(mat[, param], ranges[[param]][1], ranges[[param]][2])
}

n.cores <- makeCluster(9)
registerDoParallel(n.cores)

# Create a progress file
progress_file <- "progress_hd_deter_temp.txt"
writeLines("Starting computation", progress_file)

# Run the parallel computation storing results in a list
results_list <- foreach(r_index = 1:nrow(mat), .packages = c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv"), .multicombine = TRUE, .inorder = FALSE) %dopar% {
  
  # Write the current index to the progress file
  cat(sprintf("Processing index: %d\n", r_index), file = progress_file, append = TRUE)
  
  for (c_index in 1:length(params)) {
    
    assign(params[c_index], as.numeric(mat[r_index, c_index]))
    
  }
  
  eq <- multisolve(model, lower_limit, upper_limit, iter)
  eq <- eq %>% 
    filter(S == max(S)) %>%
    mutate(index = r_index)
  
  if (nrow(eq) > 1){
    
    eq <- eq[1, ]
    
  }
  
  eq
  
}

# Combine the results into a single dataframe
full.dt <- do.call(rbind, results_list)

rm(results_list)
rm(mat)

full.dt <- full.dt[order(full.dt$index), ]

time <- rep(10000, nrow(full.dt))
full.dt <- cbind(time, full.dt)

write.csv(full.dt, "hd_deter_temp.csv")

#SEAGRASS

indices.dt <- melt(data.table(full.dt), measure.vars = c("S"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_temp_S.RData")
saveRDS(ind.dummy, file="hd_deter_temp_S.dummy.RData")

#CARBON

indices.dt <- melt(data.table(full.dt), measure.vars = c("CB"))

indices.dt <- indices.dt[, c(-2,-3,-4,-5)]

ind <- sobol_indices(Y = indices.dt$value, N = N, params = params, boot = TRUE, first = "jansen", R = R, parallel = "multicore", ncpus = 8)

cols <- colnames(ind$results)[1:5]
ind$results[, (cols):= round(.SD, 3), .SDcols = (cols)]

ind.dummy <- sobol_dummy(Y = indices.dt$value, N = N, params = params, boot = TRUE, R=R)

saveRDS(ind, file="hd_deter_temp_C.RData")
saveRDS(ind.dummy, file="hd_deter_temp_C.dummy.RData")

# end parallelisation
stopCluster(n.cores)

rm(full.dt)
