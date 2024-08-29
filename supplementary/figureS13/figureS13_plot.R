# working directory

setwd('/supplementary/figureS13') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "dplyr", "scales")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

# model

model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]*nr*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + ma + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) - ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (1-ma*da)*x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + ma*da*x[4]*(phida+phids)/2*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]*nr*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

# parameter values

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

z <- 20
temp <- 298.85
hmax <- 0.05
pmax <- 0.5
ma <- 0
i <- 0.02

data <- read.csv("ma_sh.csv")

for (j in unique(data$sh)){
  
  for (par1 in c("ma", "pmax", "temp")){

    assign(par1, read.csv(paste0(par1, "_sh.csv"), header = TRUE))
    
    # Define your start and end colors
    color_start = "#FF0000"  # Red
    color_end = "#0000FF"    # Blue
    
    # Create a color ramp function
    color_func = colorRampPalette(c(color_start, color_end))
    
    # Generate the gradient colors
    gradient_colors = color_func(length(unique(get(par1)$sh)))
    
    assign(paste0(par1, j, "seagrass"), get(par1) %>%
             filter(sh == j) %>%
             filter(stability == "seagrass" | stability == "bistable"))
    
    assign(paste0(par1, j, "bistable"), get(par1) %>%
             filter(sh == j) %>%
             filter(stability == "bistable"))
    
    assign(paste0(par1, j, "bare"), get(par1) %>%
             filter(sh == j) %>%
             filter(stability == "bare" | stability == "bistable"))
    
    if (nrow(get(paste0(par1, j, "bistable"))) == 0){
      
      # Add first row of bare df to seagrass df
      bare_first_row <- slice(get(paste0(par1, j, "bare")), 1)
      assign(paste0(par1, j, "seagrass"), get(paste0(par1, j, "seagrass")) %>% add_row(bare_first_row))
      
      # Swap column values
      df <- get(paste0(par1, j, "seagrass"))
      df[nrow(df), paste0("S", "_seagrass")] <- df[nrow(df), paste0("S", "_bare")]
      df[nrow(df), paste0("CA", "_seagrass")] <- df[nrow(df), paste0("CA", "_bare")]
      df[nrow(df), paste0("CS", "_seagrass")] <- df[nrow(df), paste0("CS", "_bare")]
      df[nrow(df), paste0("CB", "_seagrass")] <- df[nrow(df), paste0("CB", "_bare")]
      df[nrow(df), paste0("N", "_seagrass")] <- df[nrow(df), paste0("N", "_bare")]
      assign(paste0(par1, j, "seagrass"), df)
      
    }
    
    for (index in c("S", "CB", "CA_CS")){
      
      if (index == "S"){
        
        assign(paste0("plot_", par1, "_", index, "_", j), ggplot() + 
                 geom_line(data = get(paste0(par1, j, "seagrass")), 
                           aes(x = get(par1), y = get(paste0(index, "_seagrass"))), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid") + 
                 xlab(par1) + ylab(index) + theme_classic() + theme(legend.position = "none"))
        
        assign(paste0("plot_", par1, "_", index, "_", j), get(paste0("plot_", par1, "_", index, "_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get(paste0(index, "_bistable"))), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "dashed"))
        
        assign(paste0("plot_", par1, "_", index, "_", j), get(paste0("plot_", par1, "_", index, "_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get(paste0(index, "_bare"))), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid"))
        
      } else if (index == "CB") {
        
        assign(paste0("plot_", par1, "_C_", j), ggplot() + 
                 geom_line(data = get(paste0(par1, j, "seagrass")), 
                           aes(x = get(par1), y = get("CB_seagrass")), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid") + 
                 xlab(par1) + ylab(index) + theme_classic() + theme(legend.position = "none"))
        
        assign(paste0("plot_", par1, "_C_", j), get(paste0("plot_", par1, "_C_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get("CB_bistable")), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "dashed"))
        
        assign(paste0("plot_", par1, "_C_", j), get(paste0("plot_", par1, "_C_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get("CB_bare")), 
                           color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid"))
        
      } else {
        
        assign(paste0("plot_", par1, "_CA_CS_", j), ggplot() + 
                 geom_line(data = get(paste0(par1, j, "seagrass")), 
                           aes(x = get(par1), y = get("CA_seagrass")), 
                           color = "orange", size = 1, linetype = "solid") + 
                 xlab(par1) + ylab(index) + theme_classic() + theme(legend.position = "none"))
        
        assign(paste0("plot_", par1, "_CA_CS_", j), get(paste0("plot_", par1, "_CA_CS_", j)) + 
                 geom_line(data = get(paste0(par1, j, "seagrass")), 
                           aes(x = get(par1), y = get("CS_seagrass")), 
                           color = "green", size = 1, linetype = "solid") + 
                 xlab(par1) + ylab(index) + theme_classic() + theme(legend.position = "none"))
        
        assign(paste0("plot_", par1, "_CA_CS_", j), get(paste0("plot_", par1, "_CA_CS_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get("CA_bistable")), 
                           color = "orange", size = 1, linetype = "dashed"))
        
        assign(paste0("plot_", par1, "_CA_CS_", j), get(paste0("plot_", par1, "_CA_CS_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get("CS_bistable")), 
                           color = "green", size = 1, linetype = "dashed"))
        
        assign(paste0("plot_", par1, "_CA_CS_", j), get(paste0("plot_", par1, "_CA_CS_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get("CA_bare")), 
                           color = "orange", size = 1, linetype = "solid"))
        
        assign(paste0("plot_", par1, "_CA_CS_", j), get(paste0("plot_", par1, "_CA_CS_", j)) + 
                 geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get("CS_bare")), 
                           color = "green", size = 1, linetype = "solid"))
        
      }
      
    }
      
  }
    
}


plot_ma_S <- ggplot() + geom_line(data = `ma1e+06seagrass`, 
                                   aes(x = ma, y = S_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid") + xlab("ma") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_ma_S <- plot_ma_S + geom_line(data = `ma1e+06bare`, 
                                   aes(x = ma, y = S_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_ma_S.png", 
       plot_ma_S, 
       width = 5, height = 3)

plot_pmax_S <- ggplot() + geom_line(data = `pmax1e+06seagrass`, 
                                   aes(x = pmax, y = S_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid") + xlab("pmax") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_pmax_S <- plot_pmax_S + geom_line(data = `pmax1e+06bare`, 
                                   aes(x = pmax, y = S_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_pmax_S.png", 
       plot_pmax_S, 
       width = 5, height = 3)

plot_temp_S <- ggplot() + geom_line(data = `temp1e+06seagrass`, 
                                       aes(x = temp, y = S_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid") + xlab("temp") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_temp_S <- plot_temp_S + geom_line(data = `temp1e+06bare`, 
                                       aes(x = temp, y = S_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_temp_S.png", 
       plot_temp_S, 
       width = 5, height = 3)

plot_ma_C <- ggplot() + geom_line(data = `ma1e+06seagrass`, 
                                   aes(x = ma, y = CB_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid") + xlab("ma") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_ma_C <- plot_ma_C + geom_line(data = `ma1e+06bare`, 
                                   aes(x = ma, y = CB_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_ma_CB.png", 
       plot_ma_C, 
       width = 5, height = 3)

plot_pmax_C <- ggplot() + geom_line(data = `pmax1e+06seagrass`, 
                                       aes(x = pmax, y = CB_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")  + xlab("pmax") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_pmax_C <- plot_pmax_C + geom_line(data = `pmax1e+06bare`, 
                                       aes(x = pmax, y = CB_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_pmax_CB.png", 
       plot_pmax_C, 
       width = 5, height = 3)

plot_temp_C <- ggplot() + geom_line(data = `temp1e+06seagrass`, 
                                       aes(x = temp, y = CB_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid") + xlab("temp") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_temp_C <- plot_temp_C + geom_line(data = `temp1e+06bare`, 
                                       aes(x = temp, y = CB_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_temp_CB.png", 
       plot_temp_C, 
       width = 5, height = 3)
