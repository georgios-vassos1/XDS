library(XDS)
library(data.table)
library(digest)

# devtools::clean_dll("~/XDS")
# devtools::build("~/XDS")
# devtools::document("~/XDS")
# devtools::install("~/XDS", reload = F)

# Request event
request_event <- function(env, ...) {
  print("Request event")
  item  <- XDS::getQueueElementInfo(env$event_list$pop())
  sid   <- item$additionalInfo$sid
  catid <- item$additionalInfo$catid
  # Time since last request relative to (i,j,\ell)
  i <- env$shipdx[[sid]]
  j <- env$catdx[[catid]]
  wijL <- env$sim_time - env$last_prd_at[((i - 1L) * env$m + (j - 1L)) * env$n_products + seq(env$n_products)]
  # Internal stock
  inventories <- sapply(
    env$prod_idx, function(ldx) {
      env$product_params[[ldx]]$q0 - env$product_params[[ldx]]$beta * wijL[ldx]
    })
  env$inventory[(seq(env$n_products) - 1L) * env$tau + floor(env$sim_time)] <- inventories
  # Calculate the propensity of each product
  propensity <- lapply(
    env$prod_idx, function(ldx) {
      list(id = ldx, score = min(env$product_params[[ldx]]$beta * wijL[ldx] / env$product_params[[ldx]]$q0, 1.0))
    })
  # Select the products to request
  order_idx <- lapply(propensity, function(prop) {
    # Bernoulli trial for each product based on its propensity
    rbinom(1L, 1L, prop$score)
  }) |> unname() |> as.logical() |> which()
  # Generate a request key and hash it
  request_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  request_hash <- digest::digest(request_key, algo = "sha256")
  # If no products are selected, schedule the next request event
  if (!length(order_idx)) {
    # Schedule the next (ship, category)-specific request event
    request_at <- env$sim_time + rexp(1L, 0.1)
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = sid,
      "catid" = catid,
      "reqid" = request_key
    )) |>
      env$event_list$push()
  } else {
    env$requests_count <- rdx <- env$requests_count + 1L
    env$requests_at[rdx] <- env$sim_time
    env$product_count[order_idx] <- pdx <- env$product_count[order_idx] + 1L
    env$demand_at[(order_idx - 1L) * env$tau + pdx] <- env$sim_time
    # Generate the request
    request <- vector(mode = "list", length = length(order_idx))
    for (k in seq_along(request)) {
      request[[k]]$idx  <- ldx <- order_idx[k]
      request[[k]]$id   <- env$prod_ids[order_idx[k]]
      request[[k]]$name <- env$prod_names[order_idx[k]]
      request[[k]]$q    <- min(env$product_params[[ldx]]$beta * wijL[ldx], env$product_params[[ldx]]$q0) |> round()
      env$last_prd_at[((i - 1L) * env$m + (j - 1L)) * env$n_products + ldx] <- env$sim_time
    }
    names(request) <- order_idx
    # Store the request in the request table
    env$request_table[[request_hash]] <- request
    # Push to approval queue or schedule handling by adding the composite waiting time of approval
    # sim$approval_queue$enqueue(request_key)
    # Time since last request relative to (i,j)
    wij <- env$sim_time - env$last_req_at[(i - 1L) * env$m + j]
    # Update event list and data containers
    env$last_req_at[(i - 1L) * env$m + j] <- env$sim_time
    # Schedule the next (ship, category)-specific request event
    # TODO: Hazard grows with inversely with inventories of products not requested; inventories[-order_idx]
    request_at <- env$sim_time + rexp(1L, 0.1)
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = sid,
      "catid" = catid,
      "reqid" = request_key
      )) |>
      env$event_list$push()
    # Schedule the approval event
    approval_at <- env$sim_time + rexp(1L, 0.5)
    XDS::createQueueElement(approval_at, 2L, list(
      "reqid" = request_key
      )) |>
      env$event_list$push()
  }
}

# Approval event
approval_event <- function(env, ...) {
  print("Approval event")
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  # Remove the request from the approval queue
  # reqkey <- env$approval_queue$dequeue()
  # Add the request to the handling queue
  # env$handling_queue$enqueue(reqkey)
  t <- item$priority
  # Schedule the handling event
  handling_at    <- env$sim_time + rexp(1L, 0.2)
  env$handling_count <- hdx <- env$handling_count + 1L
  env$handling_at[hdx] <- handling_at
  XDS::createQueueElement(handling_at, 3L, list(
    "reqid" = reqkey
  )) |>
    env$event_list$push()
}

# Handling event
handling_event <- function(env, ...) {
  print("Handling event")
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  tdx    <- env$sim_time
  # Remove the request from the handling queue
  # reqkey  <- env$handling_queue$dequeue()
  # Retrieve the request from the request table
  reqhash <- digest::digest(reqkey, algo = "sha256")
  request <- env$request_table[[reqhash]]
  reqdt   <- data.table::rbindlist(request)
  # Create new column names dynamically
  # supp_valid_cols <- paste0("valid_to_suppdx_", seq_len(env$n_suppliers))
  reqxdt <- reqdt[, data.table::rbindlist(lapply(env$supp_idx, function(jdx) {
    # For each supplier, create new rows for each idx
    data.table::data.table(
      supp_idx  = jdx,
      supp_id   = env$supp_ids[jdx],
      supp_name = env$supp_names[jdx],
      prod_idx  = idx,
      prod_id   = id,
      prod_name = name,
      q    = q,
      strategic_rate = env$strategic_rates[(idx - 1L) * env$n_suppliers + jdx],
      # spot_rate = env$spot_supplier_baseline[jdx] + 
      #   env$spot_product_seasonality[(idx - 1L) * env$tau + tdx] + 
      #   env$spot_variation[((idx - 1L) * env$n_suppliers + (jdx - 1L)) * env$tau + tdx],
      valid = env$valid_to[(idx - 1L) * env$n_suppliers + jdx] > tdx # Check if strategic rate is valid
    )
  })), by = .(idx)]
  ## Process the request
  # ## Policy 1:
  # #  - order the items with an active eCat entry
  # #  - create RFQ event for the remaining items
  # if (any(!reqxdt$valid)) {
  #   # Schedule the RFQ event
  #   rfq_response_at <- env$sim_time + rexp(1L, 0.4)
  #   env$rfq_count <- rfqdx <- env$rfq_count + 1L
  #   env$rfq_at[rfqdx] <- rfq_response_at
  #   XDS::createQueueElement(rfq_response_at, 5L, list(
  #     "reqid"     = reqkey,
  #     "reqinfodt" = reqxdt[valid == FALSE],
  #     "rfq_response_at" = rfq_response_at
  #   )) |>
  #     env$event_list$push()
  # }
  # if (any(reqxdt$valid)) {
  #   # Schedule the order event
  #   order_at <- env$sim_time + rexp(1L, 10.0)
  #   env$ordering_count <- odx <- env$ordering_count + 1L
  #   env$ordering_at[odx] <- order_at
  #   XDS::createQueueElement(order_at, 6L, list(
  #     "reqid"     = reqkey,
  #     "reqinfodt" = reqxdt[valid == TRUE],
  #     "order_at"  = order_at
  #   )) |>
  #     env$event_list$push()
  # }
  ## Policy 2:
  #  - create RFQ event for all items
  rfq_response_at <- env$sim_time + rexp(1L, 0.4)
  XDS::createQueueElement(rfq_response_at, 5L, list(
    "reqid"     = reqkey,
    "reqinfodt" = reqxdt,
    "rfq_response_at" = rfq_response_at
  )) |>
    env$event_list$push()
}

# Function to calculate minimal cost allocation and return a data.table
optimal_allocation <- function(dt, extra_cost) {
  # Step 1: Find the minimum rate for each product (mixed supplier allocation)
  min_rates  <- dt[, .SD[which.min(rate)], by = prod_idx]
  mixed_cost <- sum(min_rates$rate) + (data.table::uniqueN(min_rates$supp_idx) - 1L) * extra_cost

  # Step 2: Calculate the cost of assigning the entire order to each supplier
  single_costs <- dt[, .(total_cost = sum(rate)), by = supp_idx]

  # Step 3: Find the supplier with the minimum total cost (single supplier assignment)
  single_supplier <- single_costs[which.min(total_cost)]

  # Step 4: Compare the costs and return the optimal allocation
  if (single_supplier$total_cost < mixed_cost) {
    # Assign all products to the best single supplier
    optimal_allocation <- dt[supp_idx == single_supplier$supp_idx]
  } else {
    # Use the mixed supplier allocation
    optimal_allocation <- min_rates
  }

  optimal_allocation
}

# RFQ event
rfq_event <- function(env, ...) {
  print("RFQ event")
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  reqxdt <- item$additionalInfo$reqinfodt
  tdx    <- env$sim_time
  # Add the spot rates
  reqxdt[, spot_rate := env$spot_supplier_baseline[supp_idx] + 
           env$spot_product_seasonality[(idx - 1L) * env$tau + tdx] + 
           env$spot_variation[((idx - 1L) * env$n_suppliers + (supp_idx - 1L)) * env$tau + tdx], by = .(idx, supp_idx)]
  # Generate a request key and hash it
  rfq_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  rfq_hash <- digest::digest(rfq_key, algo = "sha256")
  # Store the request in the rfq table
  env$rfq_table[[rfq_hash]] <- list(
    "rfqid"     = rfq_key,
    "reqid"     = reqkey,
    "prod_idx"  = reqxdt$idx,
    "supp_idx"  = reqxdt$supp_idx,
    "q"         = reqxdt$q,
    "spot_rate" = reqxdt$spot_rate,
    "rfq_response_at" = item$additionalInfo$rfq_response_at
  )
  # Schedule the handling event
  order_at <- tdx + rexp(1L, 10.0)
  env$ordering_count <- odx <- env$ordering_count + 1L
  env$ordering_at[odx] <- order_at
  XDS::createQueueElement(order_at, 6L, list(
    "reqid" = reqkey,
    "rfqid"     = rfq_key,
    "reqinfodt" = reqxdt,
    "order_at"  = order_at
  )) |>
    env$event_list$push()
}

# Order event
order_event <- function(env, extra_cost, ...) {
  print("Order event")
  tdx    <- env$sim_time
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  reqxdt <- item$additionalInfo$reqinfodt
  # Generate a request key and hash it
  po_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  po_hash <- digest::digest(po_key, algo = "sha256")
  # ## Policy 1
  # volume.contract.t <- volume.spot.t <- 0L
  # if (!(is.null(item$additionalInfo$rfqid))) {
  #   data.table::setnames(reqxdt, old = "spot_rate", new = "rate")
  #   podt <- optimal_allocation(reqxdt, extra_cost)
  #   # Volume allocation
  #   K <- podt$supp_idx |> unique()
  #   volume.contract.t <- podt[, .(v = sum(q)), by = .(supp_idx)]$v
  # } else {
  #   data.table::setnames(reqxdt, old = "strategic_rate", new = "rate")
  #   podt <- optimal_allocation(reqxdt, extra_cost)
  #   # Volume allocation
  #   K <- podt$supp_idx |> unique()
  #   volume.spot.t <- podt[, .(v = sum(q)), by = .(supp_idx)]$v
  # }
  # env$volume[(K - 1L) * env$tau + floor(tdx)] <- volume.contract.t + volume.spot.t
  # env$volume_commitment[K] <- env$volume_commitment[K] - volume.contract.t
  ## Policy 2
  #  Comprehensive table
  data.table::melt(
    reqxdt, 
    id.vars       = c(
      "idx", 
      "supp_idx", 
      "supp_id", 
      "supp_name", 
      "prod_idx", 
      "prod_id", 
      "prod_name", 
      "q", 
      "valid"
    ),
    measure.vars  = c(
      "strategic_rate", 
      "spot_rate"
    ), 
    variable.name = "rate_type",
    value.name    = "rate") |>
    optimal_allocation(extra_cost) -> podt
  # Volume allocation
  data.table::dcast(
      podt[, .(v = sum(q)), by = .(supp_idx, rate_type)], 
      supp_idx ~ rate_type, 
      value.var = "v", 
      fill = 0L
    ) -> voldt
  # Get supplier indices
  K <- voldt$supp_idx
  # Check for missing columns
  strategic_volumes <- ifelse(is.null(voldt$strategic_rate), 0L, voldt$strategic_rate)
  spot_volumes      <- ifelse(is.null(voldt$spot_rate), 0L, voldt$spot_rate)
  # Update volume commitment
  env$volume_commitment[K] <- env$volume_commitment[K] - strategic_volumes
  # Update volume allocation
  env$volume[(K - 1L) * env$tau + floor(tdx)] <- strategic_volumes + spot_volumes
  # Update cost metric
  env$cost[floor(tdx)] <- env$cost[floor(tdx)] + 
    podt[, .(cost = sum(q * rate))]$cost + extra_cost * (data.table::uniqueN(podt$supp_idx) - 1L)
  ## Store the request in the order table
  env$order_table[[po_hash]] <- list(
    "poid"      = po_key,
    "reqid"     = reqkey,
    "rfqid"     = item$additionalInfo$rfqid,
    "prod_idx"  = podt$idx,
    "supp_idx"  = podt$supp_idx,
    "rate"      = podt$rate,
    "order_at"  = item$additionalInfo$order_at
  )
}

# Initialize the simulation environment
initiate <- function(env, tau = 365L, ...) {
  args  <- list(...)
  env$n <- ifelse(is.null(args[["n_ships"]]), 1L, args[["n_ships"]])
  env$m <- ifelse(is.null(args[["n_categories"]]), 1L, args[["n_categories"]])
  env$tau <- tau

  env$ships  <- c("s016")
  env$cats   <- c("c86k")
  env$shipdx <- as.list(setNames(seq_along(env$ships), env$ships))
  env$catdx  <- as.list(setNames(seq_along(env$cats), env$cats))

  with(env, {
    # Product level data
    prod_idx   <- seq(5L)
    prod_ids   <- seq(5L)
    prod_names <- c("Diesel Fuel", "Engine Oil", "Lubricant", "Fresh Water", "Cleaning Supplies")
    # Inventory depletion model parameterization
    product_params <- list(
      list("q0" = 10L, "beta" = 0.1),
      list("q0" = 10L, "beta" = 0.1),
      list("q0" = 10L, "beta" = 0.1),
      list("q0" = 20L, "beta" = 1.0),
      list("q0" = 10L, "beta" = 0.5)
    )
    n_products  <- length(prod_idx)

    # Supplier level data
    supp_idx <- seq(4L)
    supp_ids <- seq(4L)
    supp_names <- c("Supplier A", "Supplier B", "Supplier C", "Supplier D")
    # Auxiliary
    n_suppliers <- length(supp_idx)
    # Volume commitment
    volume_commitment <- c(100.0, 100.0, 40.0, 40.0)
    volume <- numeric(n_suppliers * tau)
    # Spot rate components
    spot_supplier_baseline <- rep(10.0, n_suppliers)
    spot_product_seasonality <- c(
      c(4.0 * sin(45.0 + 2.0 * pi * seq(1L, tau) / tau)),
      c(4.0 * sin(45.0 + 2.0 * pi * seq(1L, tau) / tau)),
      c(4.0 * sin(30.0 + 2.0 * pi * seq(1L, tau) / tau)),
      c(1.0 * sin(30.0 + 2.0 * pi * seq(1L, tau) / tau)),
      c(1.0 * sin(30.0 + 2.0 * pi * seq(1L, tau) / tau))
    )
    # Spot variation per product and supplier
    spot_variation <- c(
      rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 2.0), rnorm(tau, 0.0, 2.0), # Product 1
      rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 2.0), rnorm(tau, 0.0, 2.0), # Product 2
      rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 1.0), # Product 3
      rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 1.0), # Product 4
      rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 1.0)  # Product 5
    )
    # Strategic rates
    strategic_rates <- numeric(n_products * n_suppliers)
    for (i in seq(n_products)) {
      for (j in seq(n_suppliers)) {
        strategic_rates[(i - 1L) * n_suppliers + j] <- unname(
          quantile(
            spot_supplier_baseline[j] + 
              spot_product_seasonality[(i - 1L) * tau + seq(tau)] + 
              spot_variation[((i - 1L) * n_suppliers + (j - 1L)) * tau + seq(tau)],
          ))[2L]
      }
    }
    valid_to <- sample(50L:365L, n_products * n_suppliers, replace = TRUE)

    # Initialize simulation clock
    sim_time   <- 0.0
    event_list <- new(XDS::PriorityQueueWrapper)

    # Auxiliary data containers
    last_prd_at <- numeric(n * m * n_products)
    last_req_at <- numeric(n * m)
    requests_count <- 0L
    handling_count <- 0L
    rfq_count <- 0L
    ordering_count <- 0L
    requests_at   <- numeric(tau)
    handling_at   <- numeric(tau)
    rfq_at        <- numeric(tau)
    ordering_at   <- numeric(tau)
    product_count <- numeric(n_products)
    demand_at     <- numeric(n_products * tau)
    inventory     <- numeric(n_products * tau)
    cost          <- numeric(tau)
    # Generate the first request for each (i,j) pair
    request_key <- paste(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
    request_at  <- rexp(1L, 0.2)
    # Schedule the first request event
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = ships[1L], 
      "catid" = cats[1L], 
      "reqid" = request_key, 
      "request_at" = request_at
      )) |>
      event_list$push()
    # Schedule the termination event
    XDS::createQueueElement(tau, 4L, list("message" = "Termination")) |>
      event_list$push()
  })
}

# Timing function
timing <- function(env, ...) {
  event <- env$event_list$top()
  env$next_event_type <- event$eventType
  env$sim_time        <- event$priority
}

# Simulation run
sim <- new.env()
sim$request_table <- new.env(hash = TRUE)
sim$rfq_table     <- new.env(hash = TRUE)
sim$order_table   <- new.env(hash = TRUE)
# sim$approval_queue <- new(XDS::QueueWrapper)
# sim$handling_queue <- new(XDS::QueueWrapper)
# sim$request_table[[digest(sim$event_list$top()$additionalInfo$reqid, algo = "sha256")]]

initiate(sim, tau = 365L)
while (1L) {
  # Determine the next event
  timing(sim)
  # Invoke the appropriate event routine
  j <- sim$next_event_type
  switch (j,
    request_event(sim),
    approval_event(sim),
    handling_event(sim),
    break,
    rfq_event(sim),
    order_event(sim, 10.0)
  )
}
N.t <- rep(0L, sim$tau)
N.t[round(sim$requests_at)] <- 1.0
plot(seq(sim$tau), cumsum(N.t), type = "s")

# Inventory plot
matplot(matrix(sim$inventory, ncol = sim$n_products) |> round(), lty = 3L, pch = 16L, type = 'o')

lapply((seq(sim$n_products) - 1L) * sim$tau, function(idx) {
  N.t <- rep(0L, sim$tau)
  N.t[floor(sim$demand_at[idx + seq(sim$tau)])] <- 1L
  return(cumsum(N.t))
}) -> N.t
matplot(seq(sim$tau), do.call(cbind, N.t), type = "s", lty = 1L)

metadt <- data.table::rbindlist(as.list(sim$order_table))
# metadt[, qdx := seq_len(.N), by = .(reqid)]
metadt[, q := sim$request_table[[digest::digest(reqid, algo = "sha256")]][[as.character(prod_idx)]]$q, by = .(reqid, prod_idx)]

metadt[, .(demand = sum(q)), by = .(prod_idx)]
metadt[, .(volume = sum(q)), by = .(supp_idx)]
apply(matrix(sim$volume, ncol = sim$n_suppliers), 2L, sum)

sim$strategic_rates[3L * sim$n_suppliers + seq(sim$n_suppliers)]
spot_rates <- numeric(sim$n_products * sim$n_suppliers * sim$tau)
for (i in seq(sim$n_products)) {
  for (j in seq(sim$n_suppliers)) {
    idx <- ((i - 1L) * sim$n_suppliers + (j - 1L)) * sim$tau + seq(sim$tau)
    spot_rates[idx] <- sim$spot_supplier_baseline[k] + 
      sim$spot_product_seasonality[(i - 1L) * sim$tau + seq(sim$tau)] + 
      sim$spot_variation[idx]
  }
}
X <- matrix(spot_rates, ncol = sim$n_products)
matplot(matrix(X[,3L], ncol = sim$n_suppliers), type = 'l', lty = 1L)

# Volume commitment
volume <- 100.0 - 0.33 * seq(365L)
price  <- 8.0 + 2.0 * sin((2.0 * pi / 365.0 * seq(365L)) + pi / 3.0)
supplier_stockout <- rbinom(365L, 1L, 0.25)
network_efect     <- rbinom(365L, 1L, 0.25)
lead_time <- 3.0 + 3.0 * supplier_stockout + network_efect - (1.0 - network_efect)

expit <- function(x) 1.0 / (1.0 + exp(-x))

X  <- cbind(volume, price, lead_time)
Xs <- scale(X)
Y  <- Xs %*% c(2.0, 2.0, 1.0) + rnorm(365L, 0.0, 1.0)
plot(expit(Y))

