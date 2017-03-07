# FiTABM
An agent-based model, implemented in R, for modelling the effect of feed-in tariffs on installation of solar panels by households in Great Britain

## Getting started
If you don't have R installed, do that: https://cran.rstudio.com/

To make your life easier, also download RStudio: https://www.rstudio.com/products/rstudio/download/

Although FiTABM was written in RStudio, there is no reason it won't work in a different IDE/from the command line.

Once you have R/RStudio and the right packages:

1.  Download the FiTABM repository from https://github.com/phoebe-p/FiTABM (Clone or download -> Download ZIP), or create a clone if you prefer

2.  Extract the zip file into your preferred location (best to put the files in their own folder)

3.  If you have RStudio, open FiTABM.Rproj in RStudio; you'll automatically be in the correct working directly (where all the data files you need are). Otherwise, navigate to the folder where the data files are (where you extracted the .zip to).

4.  The functions which form the basis of the program rely on the following R packages:
    
    readr, dplyr, purrr, ggplot2, stringr, reshape2, lubridate, magrittr
    
    You can install these yourself, e.g by running the following line of R code:
    
    install.packages(c("readr", "dplyr", "purrr", "ggplot2", "stringr", "reshape2", "lubridate", "magrittr"))

5.  To actually load the functions into your global environment, run all the contents of 01-required_functions.R and 02-run_functions.R (Ctrl/Cmd + A, Ctrl/Cmd + Enter in RStudio). All the functions you need are now in your Global Environment.

6.  Now you can start running simulations!

## File list

1. FiTABM.Rproj -- RStudio project
2. 01-required_functions.R -- contains most of the necessary functions, except those for running in batches & actually executing the model.
3. 02-run_functions.R -- contains the functions for individual and batch runs; all functions called by the individual run function are defined in 01-required_functions.R
4. all_inst_1.csv and all_inst_2.csv -- data on individual PV installations in the UK. Big files!
5. electricityprices.csv -- estimated electricity prices in the UK (annual, 2010-2016)
6. FiT_levels.csv -- Real feed-in tariff (FiT) levels in Great Britain (monthly, 2010-2016)
7. Incomelist.csv -- Income percentiles for the UK population
8. LF_mean.csv -- Solar PV load factors per GB region
9. mean-electricity.csv -- Mean electricity consumption, depending on household size and income
10. median-electricity.csv -- Median electricity consumption, depending on household size and income
11. owner_occupiers.csv -- number of households which occupy a home they own, in Great Britain (annual, 2010-2016)
12. population_mid2012.csv -- population estimates per GB region, from 2012
13. PV_cost_data_est.csv -- PV cost data used in the model (monthly, 2010-2016)
14. real_dep_cap.csv -- Deployment caps implement on FiT scheme from October 2016
15. README.md -- this file.

  
    

