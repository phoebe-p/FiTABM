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
    readr
    dplyr
    purrr
    ggplot2
    stringr
    reshape2
    lubridate
    magrittr
    You can install these yourself, .e.g by running the following line of R code:
    install.packages(c("readr", "dplyr", "purrr", "ggplot2", "stringr", "reshape2", "lubridate", "magrittr"))

5.  To actually load the functions into your environment, run all the contents of 01-required_functions.R and 02-run_functions.R. All the functions you need are now in your Global Environment.
  
    

## File list:
