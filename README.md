Code for running the bifurcation diagrams and the sensitivity analysis of seagrass-sediment model
==============

***Antoine Le Vilain***

<ins>Contact</ins>: antoine.levilain18@gmail.com

This repository contains the code used to conduct the figures of: Abrupt rather than gradual losses of soil organic carbon following disturbance of modelled seagrass ecosystems. Each figure from the related study has its own folder, which includes the necessary scripts to rerun simulations, the output of those simulations, and the code to plot the results. By navigating to any figure’s folder, you can reproduce the simulations and visualise the results. The repository is organised to facilitate reproducibility and further exploration of the ecosystem model and its behavior under various scenarios.

We performed our analysis using R version 3.6.3.

Do not forget to add your working directory if you want to save the figures.

## Sensitivity analysis (Figure 4, Figure S11, Figure S15 & Figure S16)

The “sensitivity” folder contains subfolders with the scripts required to run the global sensitivity analysis using the Sobol method for each scenario/case, along with the resulting outputs. In this analysis, higher numerical values in folder names indicate a more deteriorated meadow, meaning it’s closer to the point of collapse. The analysis was conducted across different scenarios for different cases: “f” denotes the feedback case, while “no_f” represents the no feedback case. To recreate the figures, you can plot the pie charts for each scenario/case by running the sensitivity_plot.R script after setting the working directory to the appropriate subfolder.



