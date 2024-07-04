rm(list = ls())

setwd("~/XDS")

devtools::build()
devtools::document() # This fails
devtools::install(reload = FALSE)

Rcpp::sourceCpp("src/PriorityQueue.cpp") # This works

# After running the sourceCpp command, the following code works
x <- createQueueElement(0.33, list('ship'=2L, 'type'=2L))
y <- createQueueElement(0.94, list('ship'=3L, 'type'=1L))
z <- createQueueElement(0.11, list('ship'=1L, 'type'=2L))

getQueueElementInfo(x)
getQueueElementInfo(y)
getQueueElementInfo(z)

gc(reset = T); gc()
