# FCR-GLM-methane
GLM-AED-CH4 model code, FCR model setup, calibration and uncertainty analysis.

## GLM-AED-CH4
Model code of the GLM-AED methane model. 

## CH4_model_calib
This folder inlcudes the calibration setup of the methane model using PEST. Runnig the workflow.py script in 'CH4 model calib' will start the calibration process with 10 parallel workers.

## CH4_UA
This folder includes the uncertainty analsyis setup that was ran to assess the predictive uncertainty of the methane model. Running the workflow.py located in 'CH4_UA' script will start the pestpp-ies process with 10 parallel workers. 

## FCR_model_setup
This folder includes the setup of the methane model based on the best parameter set attained through calibration. 

## Plots
This folder includes all the figures created by the R-scripts. Their numbering matches the figure captions in the manuscript including the figures in the Supporting information.

## R-scripts
This folder includes all the R scripts for creating the figures, processing the results of the uncertainty analysis and calculating the noise in the observed data.

