# Install necessary packages
install.packages("usethis")
install.packages("Rcpp")
install.packages("devtools")

# Create a new package
usethis::create_package("~/DataStructures")

# Change working directory to the new package
setwd("~/XDS")

# Set up Rcpp
usethis::use_rcpp()

devtools::document()
devtools::install(reload = FALSE)

library(DataStructures)
DataStructures::createNode(4, "Root Data")
DataStructures::printNode(node)
DataStructures::freeNode(node)
timesTwo(3)  # Should return 10
