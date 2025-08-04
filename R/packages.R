# Loads necessary packages
## ipak function: install and load multiple R packages.
## check to see if packages are installed. Install them if they are not, then load them into the R session. (source: https://gist.github.com/stevenworthington/3178163)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos = "https://cran.uni-muenster.de/")
  sapply(pkg, require, character.only = TRUE)
}

# Loads packages
packages <- c(
  # "wpp2022", 
  "FactoMineR",
  "factoextra",
  "targets",
  "visNetwork",
  "tidyverse",
  "gt"
)

# Custom package (source: https://github.com/PPgp/wpp2022/tree/main)
library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2022")

# 
# # # Loads packages silently
# suppressPackageStartupMessages(
#   invisible(
#     lapply(packages, library, character.only = TRUE)
#   )
# )

# Loads packages
invisible(ipak(packages))

# Removes intermediary objects from the environment
rm(ipak)