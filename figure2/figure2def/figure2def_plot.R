# working directory

setwd('figure2/figure2def') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "gridExtra", "dplyr", "scales")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

# plot

for (par1 in c("ma", "pmax", "temp")){
  
  assign(par1, read.csv(paste0(par1, "_sh.csv"), header = TRUE))
  
  # Define your start and end colors
  color_start = "#FF0000"  # Red
  color_end = "#0000FF"    # Blue
  
  # Create a color ramp function
  color_func = colorRampPalette(c(color_start, color_end))
  
  # Generate the gradient colors
  gradient_colors = color_func(length(unique(get(par1)$sh)))
  
  for (j in unique(get(par1)$sh)){
    
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
      
    }
    
    if (nrow(get(paste0(par1, j, "bistable"))) == 0){
      
      # Swap column values
      df <- get(paste0(par1, j, "seagrass"))
      df[nrow(df), "S_seagrass"] <- df[nrow(df), "S_bare"]
      assign(paste0(par1, j, "seagrass"), df)
      
    }
    
    if (j == unique(get(par1)$sh)[1]){
      
      assign(paste0("plot_", par1, "_S"), ggplot() + 
               geom_line(data = get(paste0(par1, j, "seagrass")), 
                         aes(x = get(par1), y = get("S_seagrass")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid") + 
               xlab(par1) + ylab("S") + theme_classic() + theme(legend.position = "none"))
      
      assign(paste0("plot_", par1, "_S"), get(paste0("plot_", par1, "_S")) + 
               geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get("S_bistable")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "dashed"))
      
      assign(paste0("plot_", par1, "_S"), get(paste0("plot_", par1, "_S")) + 
               geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get("S_bare")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid"))
      
    } else {
      
      assign(paste0("plot_", par1, "_S"), get(paste0("plot_", par1, "_S")) + 
               geom_line(data = get(paste0(par1, j, "seagrass")), 
                         aes(x = get(par1), y = get("S_seagrass")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid") + 
               xlab(par1) + ylab("S") + theme_classic() + theme(legend.position = "none"))
      
      assign(paste0("plot_", par1, "_S"), get(paste0("plot_", par1, "_S")) + 
               geom_line(data = get(paste0(par1, j, "bistable")), aes(x = get(par1), y = get("S_bistable")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "dashed"))
      
      assign(paste0("plot_", par1, "_S"), get(paste0("plot_", par1, "_S")) + 
               geom_line(data = get(paste0(par1, j, "bare")), aes(x = get(par1), y = get("S_bare")), 
                         color = gradient_colors[which(unique(get(par1)$sh) == j)], size = 1, linetype = "solid"))
      
    }
    
  }
  
  ggsave(paste0("plot_", par1, "_S", ".png"), 
         get(paste0("plot_", par1, "_S")), 
         width = 5, height = 3)
  
}
