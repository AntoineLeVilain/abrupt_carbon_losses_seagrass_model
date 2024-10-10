# working directory

setwd('figure3') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "dplyr", "scales")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

data <- read.csv("ma_sh.csv")

# plot

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

plot_ma_S <- ggplot() + geom_line(data = ma22.7seagrass, 
                                aes(x = ma, y = S_seagrass), 
                                color = gradient_colors[1], size = 1, linetype = "solid") + xlab("ma") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_ma_S <- plot_ma_S + geom_line(data = ma22.7bistable, 
                                  aes(x = ma, y = S_bistable), 
                                  color = gradient_colors[1], size = 1, linetype = "dashed")

plot_ma_S <- plot_ma_S + geom_line(data = ma22.7bare, 
                                   aes(x = ma, y = S_bare), 
                                   color = gradient_colors[1], size = 1, linetype = "solid")

plot_ma_S <- plot_ma_S + geom_line(data = `ma1e+06seagrass`, 
                                   aes(x = ma, y = S_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

plot_ma_S <- plot_ma_S + geom_line(data = `ma1e+06bare`, 
                                   aes(x = ma, y = S_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_ma_S.png", 
       plot_ma_S, 
       width = 5, height = 3)

plot_pmax_S <- ggplot() + geom_line(data = pmax22.7seagrass, 
                                  aes(x = pmax, y = S_seagrass), 
                                  color = gradient_colors[1], size = 1, linetype = "solid") + xlab("pmax") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_pmax_S <- plot_pmax_S + geom_line(data = pmax22.7bistable, 
                                   aes(x = pmax, y = S_bistable), 
                                   color = gradient_colors[1], size = 1, linetype = "dashed")

plot_pmax_S <- plot_pmax_S + geom_line(data = pmax22.7bare, 
                                   aes(x = pmax, y = S_bare), 
                                   color = gradient_colors[1], size = 1, linetype = "solid")

plot_pmax_S <- plot_pmax_S + geom_line(data = `pmax1e+06seagrass`, 
                                   aes(x = pmax, y = S_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

plot_pmax_S <- plot_pmax_S + geom_line(data = `pmax1e+06bare`, 
                                   aes(x = pmax, y = S_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_pmax_S.png", 
       plot_pmax_S, 
       width = 5, height = 3)

plot_temp_S <- ggplot() + geom_line(data = temp22.7seagrass, 
                                    aes(x = temp, y = S_seagrass), 
                                    color = gradient_colors[1], size = 1, linetype = "solid") + xlab("temp") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_temp_S <- plot_temp_S + geom_line(data = temp22.7bistable, 
                                       aes(x = temp, y = S_bistable), 
                                       color = gradient_colors[1], size = 1, linetype = "dashed")

plot_temp_S <- plot_temp_S + geom_line(data = temp22.7bare, 
                                       aes(x = temp, y = S_bare), 
                                       color = gradient_colors[1], size = 1, linetype = "solid")

plot_temp_S <- plot_temp_S + geom_line(data = `temp1e+06seagrass`, 
                                       aes(x = temp, y = S_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

plot_temp_S <- plot_temp_S + geom_line(data = `temp1e+06bare`, 
                                       aes(x = temp, y = S_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_temp_S.png", 
       plot_temp_S, 
       width = 5, height = 3)

plot_ma_C <- ggplot() + geom_line(data = ma22.7seagrass, 
                                  aes(x = ma, y = CB_seagrass), 
                                  color = gradient_colors[1], size = 1, linetype = "solid") + xlab("ma") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_ma_C <- plot_ma_C + geom_line(data = ma22.7bistable, 
                                   aes(x = ma, y = CB_bistable), 
                                   color = gradient_colors[1], size = 1, linetype = "dashed")

plot_ma_C <- plot_ma_C + geom_line(data = ma22.7bare, 
                                   aes(x = ma, y = CB_bare), 
                                   color = gradient_colors[1], size = 1, linetype = "solid")

plot_ma_C <- plot_ma_C + geom_line(data = `ma1e+06seagrass`, 
                                   aes(x = ma, y = CB_seagrass), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

plot_ma_C <- plot_ma_C + geom_line(data = `ma1e+06bare`, 
                                   aes(x = ma, y = CB_bare), 
                                   color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_ma_CB.png", 
       plot_ma_C, 
       width = 5, height = 3)

plot_pmax_C <- ggplot() + geom_line(data = pmax22.7seagrass, 
                                    aes(x = pmax, y = CB_seagrass), 
                                    color = gradient_colors[1], size = 1, linetype = "solid") + xlab("pmax") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_pmax_C <- plot_pmax_C + geom_line(data = pmax22.7bistable, 
                                       aes(x = pmax, y = CB_bistable), 
                                       color = gradient_colors[1], size = 1, linetype = "dashed")

plot_pmax_C <- plot_pmax_C + geom_line(data = pmax22.7bare, 
                                       aes(x = pmax, y = CB_bare), 
                                       color = gradient_colors[1], size = 1, linetype = "solid")

plot_pmax_C <- plot_pmax_C + geom_line(data = `pmax1e+06seagrass`, 
                                       aes(x = pmax, y = CB_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

plot_pmax_C <- plot_pmax_C + geom_line(data = `pmax1e+06bare`, 
                                       aes(x = pmax, y = CB_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_pmax_CB.png", 
       plot_pmax_C, 
       width = 5, height = 3)

plot_temp_C <- ggplot() + geom_line(data = temp22.7seagrass, 
                                    aes(x = temp, y = CB_seagrass), 
                                    color = gradient_colors[1], size = 1, linetype = "solid") + xlab("temp") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_temp_C <- plot_temp_C + geom_line(data = temp22.7bistable, 
                                       aes(x = temp, y = CB_bistable), 
                                       color = gradient_colors[1], size = 1, linetype = "dashed")

plot_temp_C <- plot_temp_C + geom_line(data = temp22.7bare, 
                                       aes(x = temp, y = CB_bare), 
                                       color = gradient_colors[1], size = 1, linetype = "solid")

plot_temp_C <- plot_temp_C + geom_line(data = `temp1e+06seagrass`, 
                                       aes(x = temp, y = CB_seagrass), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

plot_temp_C <- plot_temp_C + geom_line(data = `temp1e+06bare`, 
                                       aes(x = temp, y = CB_bare), 
                                       color = gradient_colors[2], size = 1, linetype = "solid")

ggsave("plot_temp_CB.png", 
       plot_temp_C, 
       width = 5, height = 3)



plot_ca_cs_ma <-  ggplot(ma22.7seagrass, aes(x = ma)) + 
  geom_line(aes(y = CS_seagrass), color = "#7F7F00", size = 1, linetype = "solid") +
  geom_line(aes(y = CA_seagrass*20), color = "#FF5200", size = 1, linetype = "solid") +
  scale_y_continuous(name = "CS", sec.axis = sec_axis(~./20, name="CA")) +
  xlab("ma") + theme_classic() + theme(legend.position = "none")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = ma22.7bistable, 
                                           aes(x = ma, y = CS_bistable), 
                                           color = "#7F7F00", size = 1, linetype = "dashed")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = ma22.7bistable, 
                                           aes(x = ma, y = CA_bistable*20), 
                                           color = "#FF5200", size = 1, linetype = "dashed")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = ma22.7bare, 
                                           aes(x = ma, y = CS_bare), 
                                           color = "#7F7F00", size = 1, linetype = "solid")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = ma22.7bare, 
                                           aes(x = ma, y = CA_bare*20), 
                                           color = "#FF5200", size = 1, linetype = "solid")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06seagrass`, 
                                           aes(x = ma, y = CA_seagrass*20), 
                                           color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06bistable`, 
                                           aes(x = ma, y = CA_bistable*20), 
                                           color = "#7F527F", size = 1, linetype = "dashed")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06bare`, 
                                           aes(x = ma, y = CA_bare*20), 
                                           color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06seagrass`, 
                                           aes(x = ma, y = CS_seagrass), 
                                           color = "#007F7F", size = 1, linetype = "solid")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06bistable`, 
                                           aes(x = ma, y = CS_bistable), 
                                           color = "#007F7F", size = 1, linetype = "dashed")

plot_ca_cs_ma <- plot_ca_cs_ma + geom_line(data = `ma1e+06bare`, 
                                           aes(x = ma, y = CS_bare), 
                                           color = "#007F7F", size = 1, linetype = "solid")

ggsave("plot_ca_cs_ma.png", 
       plot_ca_cs_ma, 
       width = 5, height = 3)

plot_ca_cs_pmax <-  ggplot(pmax22.7seagrass, aes(x = pmax)) + 
  geom_line(aes(y = CS_seagrass), color = "#7F7F00", size = 1, linetype = "solid") +
  geom_line(aes(y = CA_seagrass*2), color = "#FF5200", size = 1, linetype = "solid") +
  scale_y_continuous(name = "CS", sec.axis = sec_axis(~./2, name="CA")) +
  xlab("pmax") + theme_classic() + theme(legend.position = "none")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = pmax22.7bistable, 
                                           aes(x = pmax, y = CS_bistable), 
                                           color = "#7F7F00", size = 1, linetype = "dashed")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = pmax22.7bistable, 
                                           aes(x = pmax, y = CA_bistable*2), 
                                           color = "#FF5200", size = 1, linetype = "dashed")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = pmax22.7bare, 
                                           aes(x = pmax, y = CS_bare), 
                                           color = "#7F7F00", size = 1, linetype = "solid")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = pmax22.7bare, 
                                           aes(x = pmax, y = CA_bare*2), 
                                           color = "#FF5200", size = 1, linetype = "solid")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06seagrass`, 
                                           aes(x = pmax, y = CA_seagrass*2), 
                                           color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06bistable`, 
                                           aes(x = pmax, y = CA_bistable*2), 
                                           color = "#7F527F", size = 1, linetype = "dashed")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06bare`, 
                                           aes(x = pmax, y = CA_bare*2), 
                                           color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06seagrass`, 
                                           aes(x = pmax, y = CS_seagrass), 
                                           color = "#007F7F", size = 1, linetype = "solid")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06bistable`, 
                                           aes(x = pmax, y = CS_bistable), 
                                           color = "#007F7F", size = 1, linetype = "dashed")

plot_ca_cs_pmax <- plot_ca_cs_pmax + geom_line(data = `pmax1e+06bare`, 
                                           aes(x = pmax, y = CS_bare), 
                                           color = "#007F7F", size = 1, linetype = "solid")

ggsave("plot_ca_cs_pmax.png", 
       plot_ca_cs_pmax, 
       width = 5, height = 3)

plot_ca_cs_temp <-  ggplot(temp22.7seagrass, aes(x = temp)) + 
  geom_line(aes(y = CS_seagrass), color = "#7F7F00", size = 1, linetype = "solid") +
  geom_line(aes(y = CA_seagrass*20), color = "#FF5200", size = 1, linetype = "solid") +
  scale_y_continuous(name = "CS", sec.axis = sec_axis(~./20, name="CA")) +
  xlab("temp") + theme_classic() + theme(legend.position = "none")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = temp22.7bistable, 
                                               aes(x = temp, y = CS_bistable), 
                                               color = "#7F7F00", size = 1, linetype = "dashed")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = temp22.7bistable, 
                                               aes(x = temp, y = CA_bistable*20), 
                                               color = "#FF5200", size = 1, linetype = "dashed")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = temp22.7bare, 
                                               aes(x = temp, y = CS_bare), 
                                               color = "#7F7F00", size = 1, linetype = "solid")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = temp22.7bare, 
                                               aes(x = temp, y = CA_bare*20), 
                                               color = "#FF5200", size = 1, linetype = "solid")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06seagrass`, 
                                               aes(x = temp, y = CA_seagrass*20), 
                                               color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06bistable`, 
                                               aes(x = temp, y = CA_bistable*20), 
                                               color = "#7F527F", size = 1, linetype = "dashed")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06bare`, 
                                               aes(x = temp, y = CA_bare*20), 
                                               color = "#7F527F", size = 1, linetype = "solid")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06seagrass`, 
                                               aes(x = temp, y = CS_seagrass), 
                                               color = "#007F7F", size = 1, linetype = "solid")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06bistable`, 
                                               aes(x = temp, y = CS_bistable), 
                                               color = "#007F7F", size = 1, linetype = "dashed")

plot_ca_cs_temp <- plot_ca_cs_temp + geom_line(data = `temp1e+06bare`, 
                                               aes(x = temp, y = CS_bare), 
                                               color = "#007F7F", size = 1, linetype = "solid")

ggsave("plot_ca_cs_temp.png", 
       plot_ca_cs_temp, 
       width = 5, height = 3)
