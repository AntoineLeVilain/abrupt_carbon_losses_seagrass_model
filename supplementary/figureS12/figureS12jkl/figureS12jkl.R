# requirements

package_list <- c("readxl", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

library(doMPI)

# multisolve functions

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
  equilibrium <- searchZeros(as.matrix(combinations), fn = model, control = list(xtol = 10e-12, ftol = 10e-12, allowSingular=TRUE))$x
  
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

# model

model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) - ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

# 2D bifurcation###########

# start parallelisation

cl <- startMPIcluster()
registerDoMPI(cl)

# bifurcation diagram

for (par2 in c("ma", "pmax", "temp")){
  
  # parameter reset

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
  
  z <- 20
  temp <- 298.85
  hmax <- 0.10
  pmax <- 0.5
  ma <- 0
  i <- 0.01
  
  # multisolve parameters
  
  iv1_max <- 5000
  iv2_max <- 5
  iv3_max <- 5
  iv4_max <- 5
  iv5_max <- 10000
  upper_limit <- c(iv1_max, iv2_max, iv3_max, iv4_max, iv5_max)
  lower_limit <- c(10, 0, 0, 0, 0)
  iter <- 2

  par2_ranges <- list(
    ma = seq(0, 0.006, length.out = 150),
    pmax = seq(0.5, 2.5, length.out = 150),
    temp = seq(298.85, 308.15, length.out = 150)
  )
  
  par2_range <- par2_ranges[[par2]]
  
  par1_ranges <- list(
    ma = seq(0.015, 0.1, length.out = 150),
    pmax = seq(0.015, 0.1, length.out = 150),
    temp = seq(0.015, 0.1, length.out = 150)
  )
  
  par1_range <- par1_ranges[[par2]]
  par1 <- "nr"

  # bifurcation diagram
  
  foreach(par1_index = par1_range, .packages=c("rootSolve", "dplyr", "rlang"))%dopar%{
    
    assign(par1, par1_index)
    
    assign(paste("liste", par1, sep = "_"), c()) # x axis values
    assign(paste("liste", par2, sep = "_"), c()) # y axis values
    liste_stability <- c() # state of the ecological dynamics at each parameter value
    
    assign(par2, 0) # needed for length and variable names of model
    
    for (index in 1:length(model(0))){ # list of variable values at equilibrium
      
      assign(paste("liste", names(model(0))[index], sep = "_"), c())
      
    }
    
    for (par2_index in par2_range){
      
      assign(par2, par2_index)
      
      equilibrium <- multisolve(model, lower_limit, upper_limit, iter)
      
      # we determine the system state from the number of equilibrium values or the sign of the eigenvalues
      
      if (dim(equilibrium)[1] == 3){ # bistable
        
        assign(paste("liste", par1, sep = "_"), c(get(paste("liste", par1, sep = "_")), get(par1)))
        assign(paste("liste", par2, sep = "_"), c(get(paste("liste", par2, sep = "_")), get(par2)))
        liste_stability <- c(liste_stability, "bistable")
        
        max_save <- equilibrium[which(equilibrium[, 1] == max(equilibrium[, 1])), ]
        
        for (index2 in 1:length(model(0))){
          
          if (dim(equilibrium)[2] > 1){
            
            assign(paste("liste", names(model(0))[index2], sep = "_"), c(get(paste("liste", names(model(0))[index2], sep = "_")), max_save[1, index2]))
            
          } else {
            
            assign(paste("liste", names(model(0))[index2], sep = "_"), c(get(paste("liste", names(model(0))[index2], sep = "_")), max_save[1]))
            
          }
          
        }
        
      } else if (dim(equilibrium)[1] == 2){ # seagrass
        
        bare_eq <- equilibrium[which(equilibrium[, 1] == min(equilibrium[, 1])), ]
        
        jac = jacobian(model, as.numeric(bare_eq))
        ev = eigen(jac)$values
        
        if (all(sapply(ev, function(x) x < 0))){
          
          assign(paste("liste", par1, sep = "_"), c(get(paste("liste", par1, sep = "_")), get(par1)))
          assign(paste("liste", par2, sep = "_"), c(get(paste("liste", par2, sep = "_")), get(par2)))
          liste_stability <- c(liste_stability, "bistable")
          
          max_save <- equilibrium[which(equilibrium[, 1] == max(equilibrium[, 1])), ]
          
          for (index2 in 1:length(model(0))){
            
            if (dim(equilibrium)[2] > 1){
              
              assign(paste("liste", names(model(0))[index2], sep = "_"), c(get(paste("liste", names(model(0))[index2], sep = "_")), max_save[1, index2]))
              
            } else {
              
              assign(paste("liste", names(model(0))[index2], sep = "_"), c(get(paste("liste", names(model(0))[index2], sep = "_")), max_save[1]))
              
            }
            
          }
          
        } else {
          
          assign(paste("liste", par1, sep = "_"), c(get(paste("liste", par1, sep = "_")), get(par1)))
          assign(paste("liste", par2, sep = "_"), c(get(paste("liste", par2, sep = "_")), get(par2)))
          liste_stability <- c(liste_stability, "seagrass")
          
          max_save <- equilibrium[which(equilibrium[, 1] == max(equilibrium[, 1])), ]
          
          for (index3 in 1:length(model(0))){
            
            if (dim(equilibrium)[2] > 1){
              
              assign(paste("liste", names(model(0))[index3], sep = "_"), c(get(paste("liste", names(model(0))[index3], sep = "_")), max_save[1, index3]))
              
            } else {
              
              assign(paste("liste", names(model(0))[index3], sep = "_"), c(get(paste("liste", names(model(0))[index3], sep = "_")), max_save[1]))
              
            }
            
          }
          
        }
        
      } else { # bare
        
        assign(paste("liste", par1, sep = "_"), c(get(paste("liste", par1, sep = "_")), get(par1)))
        assign(paste("liste", par2, sep = "_"), c(get(paste("liste", par2, sep = "_")), get(par2)))
        liste_stability <- c(liste_stability, "bare")
        
        for (index4 in 1:length(model(0))){
          
          if (dim(equilibrium)[2] > 1){
            
            assign(paste("liste", names(model(0))[index4], sep = "_"), c(get(paste("liste", names(model(0))[index4], sep = "_")), equilibrium[1, index4]))
            
          } else {
            
            assign(paste("liste", names(model(0))[index4], sep = "_"), c(get(paste("liste", names(model(0))[index4], sep = "_")), equilibrium[1, 1]))
            
          }
          
        }
        
      }
      
    }
    
    data <- data.frame(par2 = get(paste("liste", par2, sep = "_")), par1 = get(paste("liste", par1, sep = "_")), stability = liste_stability)
    list_names <- c(par2, par1, "stability")
    
    for (index5 in 1:length(model(0))){
      
      data <- cbind(data, get(paste("liste", names(model(0))[index5], sep = "_")))
      
    }
    
    for (index6 in 1:length(model(0))){
      
      list_names <- c(list_names, names(model(0)[index6]))
      
    }
    
    colnames(data) <- list_names
    
    write.csv(data, file = paste0(par1, par1_index, ".csv"), row.names = FALSE)
    
  }
  
  assign('data', read.csv(paste0(par1, par1_range[1], ".csv"), header = TRUE))
  unlink(paste0(par1, par1_range[1], ".csv"))
  
  for (par1_index in par1_range[-1]){ # we bind all dataframes and delete them
    
    assign(paste0(par1, par1_index), read.csv(paste0(par1, par1_index, ".csv"), header = TRUE))
    
    data <- rbindlist(list(data, get(paste0(par1, par1_index))))
    
    unlink(paste0(par1, par1_index, ".csv"))
    rm(list=(paste0(par1, par1_index)))
    
  }
  
  write.csv(data, paste0(par1, "_", par2, ".csv"))
  
  rm(data)
  
}

# end parallelisation
closeCluster(cl)
mpi.quit()
