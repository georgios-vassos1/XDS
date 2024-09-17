library(XDS)

devtools::clean_dll("~/XDS")
devtools::build("~/XDS")
devtools::document("~/XDS")
devtools::install("~/XDS", reload = F)

rPR <- function(env, ...) {
  item  <- XDS::getQueueElementInfo(env$event_list$pop())
  sid   <- item$additionalInfo$sid
  catid <- item$additionalInfo$catid

  propensity <- lapply(env$products, function(article) list(id = article$id, score = env$products[[article$id]]$beta * env$products[[article$id]]$q0))

  idx <- lapply(propensity, function(prop) {
    # Bernoulli trial for each product based on its propensity
    rbinom(1, 1, prop$score)
  }) |> unname() |> as.logical() |> which()

  request <- env$products[idx]
  request <- lapply(request, function(item) {
    item$products <- min(env$products[[item$id]]$beta * env$sim_time, env$products[[item$id]]$q0) |> round()
    return(item)
  })
  # Generate a request key and hash it
  request_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  request_hash <- digest::digest(request_key, algo = "sha256")
  # Store the request in the request table
  env$request_table[[request_hash]] <- request
  # Push to approval queue or schedule handling by adding the composite waiting time of approval
  sim$approval_queue$enqueue(request_key)

  # Update event list
  # Schedule the next (ship, category)-specific request event
  request_at <- env$sim_time + rexp(1L, 0.2)
  XDS::createQueueElement(request_at, 1L, list(
    "sid"   = sid,
    "catid" = catid,
    "reqid" = request_key,
    "request_at" = request_at
    )) |>
    env$event_list$push()
  # Schedule the approval event
  approval_at <- env$sim_time + rexp(1L, 0.2)
  XDS::createQueueElement(approval_at, 2L, list(
    "reqid" = request_key,
    "approval_at" = approval_at
    )) |>
    env$event_list$push()
}

initiate <- function(env, tau = 365L, ...) {
  args  <- list(...)
  env$n <- args[["n_ships"]]
  env$m <- args[["n_categories"]]
  env$tau <- tau

  with(env, {
    products <- list(
      list(id = 1L, name = "Diesel Fuel", "q0" = 10L, "beta" = 2.0),
      list(id = 2L, name = "Engine Oil",  "q0" = 10L, "beta" = 2.0),
      list(id = 3L, name = "Lubricant",   "q0" = 10L, "beta" = 1.0),
      list(id = 4L, name = "Fresh Water", "q0" = 10L, "beta" = 4.0),
      list(id = 5L, name = "Cleaning Supplies", "q0" = 10L, "beta" = 1.0)
    )

    sim_time   <- 0.0
    event_list <- new(XDS::PriorityQueueWrapper)

    request_key <- paste(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")

    request_at <- rexp(1L, 0.2)
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = "s016", 
      "catid" = "c86k", 
      "reqid" = request_key, 
      "request_at" = request_at
      )) |>
      event_list$push()
    XDS::createQueueElement(tau, 4L, list("message" = "Termination")) |>
      event_list$push()
  })
}

timing <- function(env, ...) {
  event <- env$event_list$top()
  env$next_event_type <- event$eventType
  env$sim_time        <- event$priority
}

sim <- new.env()
sim$request_table <- new.env(hash = TRUE)
sim$approval_queue <- new(XDS::QueueWrapper)
# sim$handling_queue <- new(XDS::QueueWrapper)
# sim$request_table[[digest(sim$event_list$top()$additionalInfo$reqid, algo = "sha256")]]

initiate(sim, tau = 365L)
while (1) {
  # Determine the next event
  timing(sim)
  # Invoke the appropriate event routine
  j <- sim$next_event_type
  switch (j,
          rPR(sim),
          decision_point(sim, B, D, Y, bandit, betas=betas, Sigma=diag(10L,sim$n.y), rls.lambda=0.98),
          supplier_evaluation(sim),
          break,
  )
}
