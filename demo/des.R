library(XDS)
library(data.table)
library(digest)
library(ggplot2)

# devtools::clean_dll("~/XDS")
# devtools::build("~/XDS")
# devtools::document("~/XDS")
# devtools::install("~/XDS", reload = F)

Weibull_haz <- function(t, t.ev, shape, scale, tau) {
  (shape / scale) * (((t - t.ev) / scale) ^ (shape - 1.0)) * 
    exp( cos(2.0 * pi * t / tau) + cos((2.0 * pi * t / tau) + pi / 3.0))
}

rReqTime <- function(t.ev, tau, shape, scale, n_intervals = 50L) {
  # Generate a random time point from the Uniform distribution
  u <- runif(1L)
  # Define the range of the time points
  t_range <- c(t.ev, tau)
  # Create a grid of discrete points (to define the intervals)
  t_grid <- seq(from = t_range[1L], to = t_range[2L], length.out = n_intervals + 1L)
  # Create a vector of midpoints of each interval
  midpoints <- (t_grid[-1L] + t_grid[-length(t_grid)]) / 2.
  # Evaluate the function at the midpoints
  haz_pcw <- Weibull_haz(midpoints, t.ev = t.ev, shape = shape, scale = scale, tau = tau)
  # Compute the cumulative hazard
  Haz <- 0.0
  for (i in 1L:n_intervals) {
    if (-log(u) >= Haz) break
    Haz <- Haz + haz_pcw[i] * (t_grid[i + 1L] - t_grid[i])
  }
  # Return the event time
  t_grid[i] + ((- log(u) - Haz[i]) / haz_pcw[i])
}

# Request event
request_event <- function(env, ...) {
  # print("Request event")
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
    # env$requests_count <- rdx <- env$requests_count + 1L
    # env$requests_at[rdx] <- env$sim_time
    # Schedule the next (ship, category)-specific request event
    request_at <- rReqTime(env$sim_time, env$tau, env$tPR_params$shape, env$tPR_params$scale, env$tPR_params$n_intervals)
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = sid,
      "catid" = catid,
      "reqid" = request_key
    )) |>
      env$event_list$push()
  } else {
    env$requests_count   <- rdx <- env$requests_count + 1L
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
    # Schedule the next (ship, category)-specific request event
    # TODO: Hazard grows with inversely with inventories of products not requested; inventories[-order_idx]
    request_at <- rReqTime(env$sim_time, env$tau, env$tPR_params$shape, env$tPR_params$scale, env$tPR_params$n_intervals)
    # request_at <- env$sim_time + rexp(1L, 0.1)
    # Time since last request relative to (i,j)
    wij <- env$sim_time - env$last_req_at[(i - 1L) * env$m + j]
    # Update event list and data containers
    env$last_req_at[(i - 1L) * env$m + j] <- env$sim_time
    # Schedule the next (ship, category)-specific request event
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
  # print("Approval event")
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  # Remove the request from the approval queue
  # reqkey <- env$approval_queue$dequeue()
  # Add the request to the handling queue
  # env$handling_queue$enqueue(reqkey)
  t <- item$priority
  # Schedule the handling event
  handling_at <- env$sim_time + rexp(1L, 0.2)
  env$handling_count <- hdx <- env$handling_count + 1L
  env$handling_at[hdx] <- handling_at
  XDS::createQueueElement(handling_at, 3L, list(
    "reqid" = reqkey
  )) |>
    env$event_list$push()
}

# Handling event
handling_event <- function(env, ...) {
  # print("Handling event")
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
      q = q,
      strategic_rate = env$strategic_rates[(idx - 1L) * env$n_suppliers + jdx],
      # spot_rate = env$spot_supplier_baseline[jdx] + 
      #   env$spot_product_seasonality[(idx - 1L) * env$tau + tdx] + 
      #   env$spot_variation[((idx - 1L) * env$n_suppliers + (jdx - 1L)) * env$tau + tdx],
      valid = env$valid_to[(idx - 1L) * env$n_suppliers + jdx] > tdx # Check if strategic rate is valid
    )
  })), by = .(idx)]
  ## Process the request
  ## Policy 1:
  #  - order the items with an active eCat entry
  #  - create RFQ event for the remaining items
  check <- reqxdt[, .(valid = any(valid)), by = .(idx)]$valid
  if (any(!check)) {
    reqxdt[, mask := any(valid), by = .(idx)]
    # Schedule the RFQ event
    rfq_response_at <- env$sim_time + rexp(1L, 0.4)
    env$rfq_count <- rfqdx <- env$rfq_count + 1L
    env$rfq_at[rfqdx] <- rfq_response_at
    XDS::createQueueElement(rfq_response_at, 5L, list(
      "reqid"     = reqkey,
      "reqinfodt" = reqxdt[mask == FALSE],
      "rfq_response_at" = rfq_response_at
    )) |>
      env$event_list$push()
  }
  if (any(check)) {
    # Schedule the order event
    order_at <- env$sim_time + rexp(1L, 10.0)
    env$ordering_count <- odx <- env$ordering_count + 1L
    env$ordering_at[odx] <- order_at
    XDS::createQueueElement(order_at, 6L, list(
      "reqid"     = reqkey,
      "reqinfodt" = reqxdt[valid == TRUE],
      "order_at"  = order_at
    )) |>
      env$event_list$push()
  }
}

# Handling event
dynamic_handling_event <- function(env, ...) {
  # print("Handling event")
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
  ## Policy 2:
  #  - create RFQ event for all items
  rfq_response_at <- env$sim_time + rexp(1L, 0.4)
  env$rfq_count <- rfqdx <- env$rfq_count + 1L
  env$rfq_at[rfqdx] <- rfq_response_at
  XDS::createQueueElement(rfq_response_at, 5L, list(
    "reqid"     = reqkey,
    "reqinfodt" = reqxdt,
    "rfq_response_at" = rfq_response_at
  )) |>
    env$event_list$push()
}

# RFQ event
rfq_event <- function(env, competition_coef = 0.0, ...) {
  # print("RFQ event")
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  reqxdt <- item$additionalInfo$reqinfodt
  tdx    <- floor(env$sim_time)
  # Add the spot rates
  reqxdt[, spot_rate := env$spot_supplier_baseline[supp_idx] + competition_coef * q + # 0.01 * env$spot_volume[supp_idx] +
           env$spot_supplier_seasonality[((idx - 1L) * env$n_suppliers + (supp_idx - 1L)) * env$tau + tdx] + 
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
  order_at <- env$sim_time + rexp(1L, 10.0)
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

# Order event
order_event <- function(env, extra_cost, ...) {
  # print("Order event")
  tdx    <- env$sim_time
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  reqxdt <- item$additionalInfo$reqinfodt
  # Generate a request key and hash it
  po_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  po_hash <- digest::digest(po_key, algo = "sha256")
  ## Policy 1
  volume.contract.t <- volume.spot.t <- 0L
  if (!(is.null(item$additionalInfo$rfqid))) {
    data.table::setnames(reqxdt, old = "spot_rate", new = "rate")
    podt.opt <- optimal_allocation(reqxdt, extra_cost)
    # Volume allocation
    K <- podt.opt$supp_idx |> unique()
    volume.spot.t <- podt.opt[, .(v = sum(q)), by = .(supp_idx)]$v
  } else {
    ux <- runif(env$n_suppliers, min = -0.01, max = 0.01)
    reqxdt[, strategic_rate := strategic_rate + ux[reqxdt$supp_idx]]
    data.table::setnames(reqxdt, old = "strategic_rate", new = "rate")
    podt.opt <- optimal_allocation(reqxdt, extra_cost)
    # Volume allocation
    K <- podt.opt$supp_idx |> unique()
    volume.contract.t <- podt.opt[, .(v = sum(q)), by = .(supp_idx)]$v
  }
  env$spot_volume[K] <- env$spot_volume[K] + volume.spot.t
  env$volume[(K - 1L) * env$tau + floor(tdx)] <- volume.contract.t + volume.spot.t
  env$volume_commitment[K] <- env$volume_commitment[K] - volume.contract.t
  # Update cost metric
  env$cost[floor(tdx)] <- env$cost[floor(tdx)] + 
    podt.opt[, .(cost = sum(q * rate))]$cost + extra_cost * (data.table::uniqueN(podt.opt$supp_idx) - 1L)
  # env$cost.rnd[floor(tdx)] <- env$cost.rnd[floor(tdx)] + 
  #   podt.rnd[, .(cost = sum(q * rate))]$cost + extra_cost * (data.table::uniqueN(podt.rnd$supp_idx) - 1L)
  ## Store the request in the order table
  env$order_table[[po_hash]] <- list(
    "poid"      = po_key,
    "reqid"     = reqkey,
    "rfqid"     = item$additionalInfo$rfqid,
    "prod_idx"  = podt.opt$idx,
    "supp_idx"  = podt.opt$supp_idx,
    "rate"      = podt.opt$rate,
    "order_at"  = item$additionalInfo$order_at
  )
}

# Order event
dynamic_order_event <- function(env, extra_cost, ...) {
  # print("Order event")
  tdx    <- env$sim_time
  item   <- XDS::getQueueElementInfo(env$event_list$pop())
  reqkey <- item$additionalInfo$reqid
  reqxdt <- item$additionalInfo$reqinfodt
  # Generate a request key and hash it
  po_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  po_hash <- digest::digest(po_key, algo = "sha256")
  ## Policy 2
  #  Comprehensive table
  reqxdt[, strategic_rate := strategic_rate * reqxdt$valid + 1e6 * (1L - reqxdt$valid)]
  # reqxdt$strategic_rate <- reqxdt$strategic_rate * reqxdt$valid + 1e6 * (1L - reqxdt$valid)
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
    value.name    = "rate")[
      (rate_type == "spot_rate") | ((rate_type == "strategic_rate") & (valid == TRUE))
    ] -> podt
  # Optimal allocation
  podt.opt <- optimal_allocation(podt, extra_cost)
  # # Random allocation
  # podt.rnd <- podt[, .SD[sample(.N, 1)], by = idx]
  # Volume allocation
  data.table::dcast(
    podt.opt[, .(v = sum(q)), by = .(supp_idx, rate_type)], 
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
  env$spot_volume[K] <- env$spot_volume[K] + spot_volumes
  # Update cost metric
  env$cost[floor(tdx)] <- env$cost[floor(tdx)] + 
    podt.opt[, .(cost = sum(q * rate))]$cost + extra_cost * (data.table::uniqueN(podt.opt$supp_idx) - 1L)
  # env$cost.rnd[floor(tdx)] <- env$cost.rnd[floor(tdx)] + 
  #   podt.rnd[, .(cost = sum(q * rate))]$cost + extra_cost * (data.table::uniqueN(podt.rnd$supp_idx) - 1L)
  ## Store the request in the order table
  env$order_table[[po_hash]] <- list(
    "poid"      = po_key,
    "reqid"     = reqkey,
    "rfqid"     = item$additionalInfo$rfqid,
    "prod_idx"  = podt.opt$idx,
    "supp_idx"  = podt.opt$supp_idx,
    "rate"      = podt.opt$rate,
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
    # PR event time generator parameters
    tPR_params <- list(
      "shape" = 1.1, 
      "scale" = 15.0,
      "n_intervals" = 50L
    )
    # Product level data
    prod_idx   <- seq(3L)
    prod_ids   <- seq(3L)
    prod_names <- c("Diesel Fuel", "Engine Oil", "Lubricant", "Fresh Water", "Cleaning Supplies")
    # Inventory depletion model parameterization
    product_params <- list(
      list("q0" = 50L,  "beta" = 0.4),
      list("q0" = 100L, "beta" = 0.8),
      list("q0" = 70L,  "beta" = 0.5)
    )
    n_products  <- length(prod_idx)

    # Supplier level data
    supp_idx <- seq(3L)
    supp_ids <- seq(3L)
    supp_names <- c("Supplier A", "Supplier B", "Supplier C")
    # Auxiliary
    n_suppliers <- length(supp_idx)
    # Volume commitment
    initial_agreement <- volume_commitment <- c(75.0, 75.0, 150.0)
    volume <- numeric(n_suppliers * tau)
    spot_volume <- numeric(n_suppliers)
    # Spot rate components
    spot_supplier_baseline <- c(10.0, 10.0, 12.0)
    spot_supplier_seasonality <- c(
      c(2.0 * cos(-(pi / 2.) + 2.0 * pi * seq(tau) / tau)), 
      c(3.0 * cos( (pi / 2.) + 2.0 * pi * seq(tau) / tau)), 
      c(2.0 * cos( (pi / 1.) + 2.0 * pi * seq(tau) / tau)),
      c(2.0 * cos( (pi / 1.) + 2.0 * pi * seq(tau) / tau)), 
      c(3.0 * cos(-(pi / 3.) + 2.0 * pi * seq(tau) / tau)), 
      c(2.0 * cos( (pi / 2.) + 2.0 * pi * seq(tau) / tau)),
      c(2.0 * cos( (3.*pi / 4.) + 2.0 * pi * seq(tau) / tau)), 
      c(3.0 * cos( (pi / 6.) + 2.0 * pi * seq(tau) / tau)), 
      c(2.0 * cos( (2.*pi / 3.) + 2.0 * pi * seq(tau) / tau))
    )
    # Random variation
    spot_sd <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    # Sample instance
    spot_variation <- numeric(n_products * n_suppliers * tau)
    for (i in prod_idx) {
      for (j in supp_idx) {
        spot_variation[((i - 1L) * n_suppliers + (j - 1L)) * tau + seq(tau)] <- 
          rnorm(tau, 0.0, spot_sd[(i - 1L) * n_suppliers + j])
      }
    }
    # Strategic rates
    strategic_rates <- rep(c(11.0, 11.0, 12.0), n_products)
    # strategic_rates <- numeric(n_products * n_suppliers)
    # for (i in seq(n_products)) {
    #   for (i in seq(n_products)) {
    #     for (j in seq(n_suppliers)) {
    #       jdx <- ((i - 1L) * n_suppliers + (j - 1L)) * tau
    #
    #       strategic_rates[(i - 1L) * n_suppliers + j] <- unname(
    #         quantile(
    #           spot_supplier_baseline[j] + 
    #             spot_supplier_seasonality[jdx + seq(tau)] + 
    #             spot_variation[jdx + seq(tau)] * 0.0,
    #         ))[3L]
    #     }
    #   }
    # }
    # valid_to <- pmin(sample(c(180L, 365L), n_products * n_suppliers, replace = TRUE), 365L)
    valid_to <- rep(c(182L, 182L, 365L), n_products)

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
    # cost.rnd      <- numeric(tau)
    # Generate the first request for each (i,j) pair
    request_key <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
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

reset.env <- function(env) {
  with(env, {
    # Volume commitment
    volume_commitment <- initial_agreement
    volume <- numeric(n_suppliers * tau)
    spot_volume <- numeric(n_suppliers)
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
    request_key <- paste(sample(c(0:9, letters, LETTERS), 32L, replace = TRUE), collapse = "")
    request_at  <- rexp(1L, 0.2)
    # Random variation example
    spot_variation <- numeric(n_products * n_suppliers * tau)
    for (i in prod_idx) {
      for (j in supp_idx) {
        spot_variation[((i - 1L) * n_suppliers + (j - 1L)) * tau + seq(tau)] <- 
          rnorm(tau, 0.0, spot_sd[(i - 1L) * n_suppliers + j])
      }
    }
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

run_ops <- function(sim, policy, competition_coef = 0.0, extra_cost = 10.0, ...) {
  args <- list(...)
  # Reset environment
  reset.env(sim)
  # Run the simulation
  while (1L) {
    # Determine the next event
    timing(sim)
    # Invoke the appropriate event routine
    j <- sim$next_event_type
    switch (j,
            request_event(sim),
            approval_event(sim),
            policy[["handling"]](sim),
            break,
            rfq_event(sim, competition_coef),
            policy[["order_allocation"]](sim, extra_cost)
    )
  }
  # Return the simulation results
  list(
    "cost"       = sum(sim$cost),
    "compliance" = (sim$initial_agreement - sim$volume_commitment) / sim$initial_agreement
  )
}

policy <- list(
  "handling"         = dynamic_handling_event,
  "order_allocation" = dynamic_order_event
)

# Initialize simulation storage containers
sim$request_table <- new.env(hash = TRUE)
sim$rfq_table     <- new.env(hash = TRUE)
sim$order_table   <- new.env(hash = TRUE)
# Run the simulation
run_ops(sim, policy, competition_coef = 0.1)

## Complementary output
sum(sim$volume)
sim$volume_commitment
sim$spot_volume
sum(sim$cost)

# sim$volume[is.na(sim$volume)] <- 0L
# matplot(matrix(sim$volume, ncol = sim$n_suppliers), type = 's', lty = 2L)
# sim$volume[sim$volume == 0L] <- NA
# matpoints(seq(sim$tau) + 0.5, matrix(sim$volume, ncol = sim$n_suppliers), pch = 16L)

N.t <- rep(0L, sim$tau)
N.t[round(sim$requests_at)] <- 1.0
plot(seq(sim$tau), cumsum(N.t), type = "s", ylab = "Accumulated Number of PRs", xlab = "Time")

# total_volume <- matrix(sim$volume, ncol = sim$n_suppliers) |> apply(1L, sum)
# total_volume[total_volume == 0] <- 1L
# plot(cumsum(sim$cost), type = 's', ylab = "Accumulated Cost", xlab = "Time") # , ylim = c(0L, 6500L))

## Compare policies
# Function to run a single trial
run_trial <- function(idx, sim, policy, competition_coef = 0.0, extra_cost = 10.0, ...) {
  sim$request_table <- new.env(hash = TRUE)
  sim$rfq_table     <- new.env(hash = TRUE)
  sim$order_table   <- new.env(hash = TRUE)

  # Reset environment
  reset.env(sim)
  # Run the simulation
  while (1L) {
    # Determine the next event
    timing(sim)
    # Invoke the appropriate event routine
    j <- sim$next_event_type
    switch (j,
            request_event(sim),
            approval_event(sim),
            policy[["handling"]](sim),
            break,
            rfq_event(sim, competition_coef),
            policy[["order_allocation"]](sim, extra_cost)
    )
  }
  
  list(
    "cost"       = cumsum(sim$cost),
    "compliance" = (sim$initial_agreement - sim$volume_commitment) / sim$initial_agreement
  )
}

## Parallelize computations
policy <- list(
  list(
    "handling"         = handling_event,
    "order_allocation" = order_event
  ),
  list(
    "handling"         = dynamic_handling_event,
    "order_allocation" = dynamic_order_event
  )
)

cost <- list(
  "Standard" = NULL,
  "Dynamic"  = NULL
)

compliance <- list(
  "Standard" = NULL,
  "Dynamic"  = NULL
)

function_names <- names(which(sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))))

cl <- parallel::makeCluster(parallel::detectCores() - 2L)
parallel::clusterExport(cl, c("sim", "policy", function_names))

trials <- 10000L # Number of trials

pdx <- 2L # Policy index

timex <- Sys.time()
results <- parallel::parLapply(cl, seq_len(trials), run_trial, sim = sim, policy = policy[[pdx]], competition_coef = 0.2)
timex <- Sys.time() - timex

cost[[pdx]]       <- vapply(results, function(x) x$cost[sim$tau], numeric(1L))
compliance[[pdx]] <- vapply(results, function(x) x$compliance, numeric(3)) |> t()

parallel::stopCluster(cl)

# hist(cost[[pdx]], freq = F, breaks = 50L, main = "Cost Distribution")
# lines(density(cost[[pdx]], kernel = "optcosine", bw = "SJ"), col = "steelblue", lwd = 2)

dt <- data.table::as.data.table(do.call(cbind, cost)) |>
  data.table::melt.data.table(measure.vars = c("Standard", "Dynamic"), variable.name = "Policy", value.name = "Cost")
# dt[Policy == "Dynamic"]$Cost[(cost[[2L]] > max(cost[[1L]]))] <- NA

# Create the plot
ggplot(dt, aes(x = Cost, color = Policy)) +
  geom_histogram(aes(y = after_stat(density), fill = Policy), 
                 bins = 60L, color = "white", 
                 position = "identity", alpha = 0.5) +  # Histogram
  geom_density(aes(color = Policy), kernel = "cosine", bw = "sj", linewidth = 1.0) +  # Density line
  # scale_y_continuous(labels = scales::scientific_format()) + 
  labs(title = "Strong Competition", x = "Cost", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12L, face = "bold"),
    legend.text  = element_text(size = 12L),
    plot.title   = element_text(size = 16L, face = "italic"),
    axis.title.x = element_text(size = 14L, face = "bold"),
    axis.title.y = element_text(size = 14L, face = "bold"),
    axis.text.y = element_text(angle = 0.0, hjust = 1),
    axis.text.x = element_text(angle = 0.0, hjust = 1),
    axis.text  = element_text(size = 14L),
    strip.text = element_text(size = 14L)
  )

ComplianceMatMild <- list(
  "Standard" = apply(compliance[[1L]], 2L, mean) + t(qnorm(0.975) * c(-1.0, 0.0, 1.0) %*% t(apply(compliance[[1L]], 2L, sd) / sqrt(trials))),
  "Dynamic"  = apply(compliance[[2L]], 2L, mean) + t(qnorm(0.975) * c(-1.0, 0.0, 1.0) %*% t(apply(compliance[[2L]], 2L, sd) / sqrt(trials)))
)

# cost       <- matrix(0L, nrow = trials, ncol = sim$tau)
# compliance <- matrix(0L, nrow = trials, ncol = sim$n_suppliers)
# # Run trials in parallel
# for (idx in seq(trials)) {
#   if (idx %% 100L == 0L) {
#     print(idx)
#   }
# 
#   run_trial(NA, sim, policy)
# 
#   cost[idx,]       <- cumsum(sim$cost)
#   compliance[idx,] <- (sim$initial_agreement - sim$volume_commitment) / sim$initial_agreement
# }
# matplot(seq(sim$tau), t(cost), type = "s", col = "gray")

## Check spot rates
# Simulate spot rates
generate_spot_rates <- function(sim, ...) {
  spot_rates <- numeric(sim$tau * sim$n_products * sim$n_suppliers)
  for (i in seq(sim$n_products)) {
    idx <- ((seq(sim$tau) - 1L) * sim$n_products + (i - 1L)) * sim$n_suppliers
    for (j in seq(sim$n_suppliers)) {
      jdx <- ((i - 1L) * sim$n_suppliers + (j - 1L)) * sim$tau
      spot_rates[idx + j] <- sim$spot_supplier_baseline[j] + 
        sim$spot_supplier_seasonality[jdx + seq(sim$tau)] +
        sim$spot_variation[jdx + seq(sim$tau)] * 0.0
    }
  }
  spot_rates
}

spot_rates <- generate_spot_rates(sim)

# Get exploration data collected via RFQs
rfqdt <- data.table::rbindlist(as.list(sim$rfq_table))[
  order(rfq_response_at), 
  .(Time = floor(rfq_response_at), 
    Product  = paste0("Product ", prod_idx), 
    Supplier = sim$supp_names[supp_idx], 
    ObservedSpotRate = spot_rate, 
    q = q)
]

# Combine spot rates with the sample of observed rates
dt_spot <- data.table::data.table(
  Time     = rep(seq(sim$tau), each = sim$n_products * sim$n_suppliers),
  Product  = factor(rep(paste0("Product ", seq(sim$n_products)), each = sim$n_suppliers, times = sim$tau)),
  Supplier = factor(rep(sim$supp_names, times = sim$n_products * sim$tau)),
  SpotRate = spot_rates,
  LowerBound = spot_rates + qnorm(0.025, mean = 0, sd = sim$spot_sd) * sim$spot_sd,
  UpperBound = spot_rates + qnorm(0.975, mean = 0, sd = sim$spot_sd) * sim$spot_sd
) |>
  data.table::merge.data.table(rfqdt, by = c("Time", "Product", "Supplier"), all = TRUE)

# Plot spot market conditions
dt_spot |> 
  ggplot(aes(x = Time, y = ObservedSpotRate, color = Supplier)) +
  facet_wrap(~Product, nrow = 2L) +  # Horizontally stack the plots
  geom_point(pch = 4L, size = 3L) +
  # geom_line(aes(y = SpotRate), linetype = 2L) +
  geom_ribbon(
    aes(ymin = LowerBound, ymax = UpperBound, fill = Supplier), 
    alpha = 0.2, color = NA) +
  lims(x = c(0L, 365L), y = c(0L, 20L)) +
  labs(x = "Time", y = "Spot Rates") +
  theme_minimal() +
  theme(
    legend.position = "top",                # Move legend to the top
    legend.title = element_blank(), # Increase legend title font size
    legend.text  = element_text(size = 12L),  # Increase legend items font size
    axis.title   = element_text(size = 14L),   # Increase axis titles font size
    axis.text    = element_text(size = 12L),    # Increase axis text font size
    strip.text   = element_text(size = 14L, face = "bold")    # Increase facet labels font size
  )

# Inventory plot
sim$inventory[sim$inventory == 0L] <- NA
matplot(matrix(sim$inventory, ncol = sim$n_products) |> round(), lty = 3L, pch = 16L, type = 'p')

lapply((seq(sim$n_products) - 1L) * sim$tau, function(idx) {
  N.t <- rep(0L, sim$tau)
  N.t[floor(sim$demand_at[idx + seq(sim$tau)])] <- 1L
  return(cumsum(N.t))
}) -> N.t
matplot(seq(sim$tau), do.call(cbind, N.t), type = "s", lty = 1L)

metadt <- data.table::rbindlist(as.list(sim$order_table))
# metadt[, qdx := seq_len(.N), by = .(reqid)]
metadt[, q := sim$request_table[[digest::digest(reqid, algo = "sha256")]][[
  as.character(prod_idx)]]$q, by = .(reqid, prod_idx)]

metadt[, .(demand = sum(q)), by = .(prod_idx)]
metadt[, .(volume = sum(q)), by = .(supp_idx)]
apply(matrix(sim$volume, ncol = sim$n_suppliers), 2L, sum)

sim$strategic_rates[2L * sim$n_suppliers + seq(sim$n_suppliers)]
spot_rates <- numeric(sim$tau * sim$n_products * sim$n_suppliers)
for (i in seq(sim$n_products)) {
  idx <- ((seq(sim$tau) - 1L) * sim$n_products + (i - 1L)) * sim$n_suppliers
  for (j in seq(sim$n_suppliers)) {
    jdx <- ((i - 1L) * sim$n_suppliers + (j - 1L)) * sim$tau
    spot_rates[idx + j] <- sim$spot_supplier_baseline[j] + 
      sim$spot_supplier_seasonality[jdx + seq(sim$tau)] + 
      sim$spot_variation[jdx + seq(sim$tau)]
  }
}
X <- matrix(spot_rates, ncol = sim$n_products)
matplot(matrix(X[,2L], ncol = sim$n_suppliers), type = 'l', lty = 1L)

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
