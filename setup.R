# Install necessary packages
install.packages("usethis")
install.packages("Rcpp")
install.packages("devtools")

# Create a new package
usethis::create_package("~/XDS")

# Change working directory to the new package
setwd("~/XDS")

# Set up Rcpp
usethis::use_rcpp()

devtools::build()
devtools::document()
devtools::install(reload = FALSE)

Rcpp::sourceCpp("src/PriorityQueue.cpp")

q <- new(PriorityQueueWrapper)

x <- createQueueElement(0.33, list('ship'=2L, 'type'=2L))
y <- createQueueElement(0.94, list('ship'=3L, 'type'=1L))
z <- createQueueElement(0.11, list('ship'=1L, 'type'=2L))

q$push(x)
q$push(y)
q$push(z)

q$top()
o <- q$pop()
getQueueElementInfo(o)

gc(reset = T); gc()
