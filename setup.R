# Install necessary packages
install.packages("usethis")
install.packages("Rcpp")
install.packages("devtools")

# Create a new package
usethis::create_package("~/DataStructures")

# Change working directory to the new package
setwd("~/DataStructures")

# Set up Rcpp
usethis::use_rcpp()

devtools::document()
devtools::install()

library(DataStructures)
timesTwo(3)  # Should return 10
