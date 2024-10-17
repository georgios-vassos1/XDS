library(XDS)
library(data.table)
library(digest)

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
  t.ev <- t_grid[i] + ((- log(u) - Haz[i]) / haz_pcw[i])
  # Return the event time and cumulative hazard
  list(
    "t.ev" = t.ev,
    "Haz"  = Haz + haz_pcw[i] * (t.ev - t_grid[i])
  )
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
  items_idx <- lapply(propensity, function(prop) {
    # Bernoulli trial for each product based on its propensity
    rbinom(1L, 1L, prop$score)
  }) |> unname() |> as.logical() |> which()
  # Generate a request key and hash it
  request_key  <- paste(sample(c(0L:9L, letters, LETTERS), 32L, replace = TRUE), collapse = "")
  request_hash <- digest::digest(request_key, algo = "sha256")
  # If no products are selected, schedule the next request event
  if (!length(items_idx)) {
    env$requests_count   <- rdx <- env$requests_count + 1L
    env$requests_at[rdx] <- env$sim_time
    # Schedule the next (ship, category)-specific request event
    xev <- rReqTime(env$sim_time, env$tau, env$tPR_params$shape, env$tPR_params$scale, env$tPR_params$n_intervals)
    request_at <- xev$t.ev
    env$haz_at[(i - 1L) * env$m * env$tau + (j - 1L) * env$tau + floor(env$sim_time)] <- xev$Haz
    XDS::createQueueElement(request_at, 1L, list(
      "sid"   = sid,
      "catid" = catid,
      "reqid" = request_key
    )) |>
      env$event_list$push()
  } else {
    env$requests_count   <- rdx <- env$requests_count + 1L
    env$requests_at[rdx] <- env$sim_time
    env$product_count[items_idx] <- pdx <- env$product_count[items_idx] + 1L
    env$demand_at[(items_idx - 1L) * env$tau + pdx] <- env$sim_time
    # Generate the request
    request <- vector(mode = "list", length = length(items_idx))
    for (k in seq_along(request)) {
      request[[k]]$idx  <- ldx <- items_idx[k]
      request[[k]]$id   <- env$prod_ids[items_idx[k]]
      request[[k]]$name <- env$prod_names[items_idx[k]]
      request[[k]]$q    <- min(env$product_params[[ldx]]$beta * wijL[ldx], env$product_params[[ldx]]$q0) |> round()
      env$last_prd_at[((i - 1L) * env$m + (j - 1L)) * env$n_products + ldx] <- env$sim_time
    }
    names(request) <- items_idx
    # Store the request in the request table
    env$request_table[[request_hash]] <- request
    # Push to approval queue or schedule handling by adding the composite waiting time of approval
    # sim$approval_queue$enqueue(request_key)
    # Schedule the next (ship, category)-specific request event
    # TODO: Hazard grows with inversely with inventories of products not requested; inventories[-items_idx]
    xev <- rReqTime(env$sim_time, env$tau, env$tPR_params$shape, env$tPR_params$scale, env$tPR_params$n_intervals)
    request_at <- xev$t.ev
    env$haz_at[(i - 1L) * env$m * env$tau + (j - 1L) * env$tau + floor(env$sim_time)] <- xev$Haz
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
      "scale" = 10.0,
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
    # Initialize simulation clock
    sim_time   <- 0.0
    event_list <- new(XDS::PriorityQueueWrapper)
    # Auxiliary data containers
    last_prd_at <- numeric(n * m * n_products)
    last_req_at <- numeric(n * m)
    haz_at <- numeric(n * m * tau)
    requests_count <- 0L
    requests_at   <- numeric(tau)
    handling_at   <- numeric(tau)
    product_count <- numeric(n_products)
    demand_at     <- numeric(n_products * tau)
    inventory     <- numeric(n_products * tau)
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
    XDS::createQueueElement(tau, 3L, list("message" = "Termination")) |>
      event_list$push()
  })
}

reset.env <- function(env) {
  with(env, {
    # Initialize simulation clock
    sim_time   <- 0.0
    event_list <- new(XDS::PriorityQueueWrapper)
    # Auxiliary data containers
    last_prd_at <- numeric(n * m * n_products)
    last_req_at <- numeric(n * m)
    requests_count <- 0L
    requests_at   <- numeric(tau)
    product_count <- numeric(n_products)
    demand_at     <- numeric(n_products * tau)
    inventory     <- numeric(n_products * tau)
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
    XDS::createQueueElement(tau, 3L, list("message" = "Termination")) |>
      event_list$push()
  })
}

# Timing function
timing <- function(env, ...) {
  event <- env$event_list$top()
  env$next_event_type <- event$eventType
  env$sim_time        <- event$priority
}

run <- function(...) {
  # Simulation run
  sim <- new.env()
  initiate(sim, tau = 365L)
  # reset.env(sim)
  while (1L) {
    # Determine the next event
    timing(sim)
    # Invoke the appropriate event routine
    j <- sim$next_event_type
    switch (j,
            request_event(sim),
            approval_event(sim),
            break,
    )
  }
  N <- rep(0L, sim$tau)
  N[floor(sim$requests_at)] <- 1.0
  N
}
# plot(seq(sim$tau), cumsum(N), type = "s")
N  <- replicate(10000L, run())
Nz <- apply(N, 2L, cumsum)
N.avg <- apply(Nz, 1L, mean) + apply(Nz, 1L, sd) %*% t(c(-1.0, 0.0, 1.0) * qnorm(0.975))

## Fancy plot
df <- data.frame(
  t = rep(seq(sim$tau), 1000L), 
  N = c(Nz[,sample(10000L, 1000L, replace = FALSE)]), 
  experiment = factor(rep(seq(1000L), each = sim$tau))
)
df.avg <- data.frame(t = seq(sim$tau), N.avg = N.avg[,2L])

main_plot <- ggplot() +
  geom_step(
    data = df, 
    aes(x = t, y = N, group = experiment, color = "Sample Path"), 
    linetype = "solid", alpha = 0.1) +
  geom_step(
    data = df.avg, 
    aes(x = t, y = N.avg, color = "Average Path"), 
    linetype = "dashed", size = 1.1) +
  labs(title = "", x = "t (Time)", y = "N(t)") +
  scale_color_manual(values = c("Sample Path" = "steelblue", "Average Path" = "darkblue")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.95),
    legend.title = element_blank(),
    legend.text  = element_text(size = 12L),
    plot.title   = element_blank(), # element_text(size = 16L, face = "italic"),
    axis.title.x = element_text(size = 14L, face = "bold"),
    axis.title.y = element_text(size = 14L, face = "bold"),
    axis.text.y  = element_text(angle = 0.0, hjust = 1),
    axis.text.x  = element_text(angle = 0.0, hjust = 1),
    axis.text    = element_text(size = 14L),
    strip.text   = element_text(size = 14L),
    plot.margin = unit(c(0.5, -0.2, 0.5, 0.5), "cm")
  )

complementary_plot <- ggplot(data.frame(N = Nz[365L,]), aes(x = N)) +
  geom_histogram(aes(x = N, y = ..density..), binwidth = 1.0, fill = "steelblue", color = "white", alpha = 0.7) +
  coord_flip() +
  xlim(range(df$N)) +
  labs(title = "", y = "Density", x = "") +
  theme_minimal() +
  theme(
    plot.title   = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x  = element_text(angle = 0.0, hjust = 1),
    axis.title.x = element_text(size = 14L, face = "bold"),
    axis.text    = element_text(size = 14L),
    strip.text   = element_text(size = 14L),
    plot.margin = unit(c(0.5, 0.5, 0.5, -0.2), "cm")
  )

grid.arrange(main_plot, complementary_plot, ncol = 2L, widths = c(3L, 1L))

# Standard plot
matplot(
  Nz,
  type = "s",
  ylab = "N(t)",
  xlab = "t (time)",
  col = "gray",
  lty = 1L)
matlines(
  N.avg[,2L],
  type = "s",
  col = "black",
  lwd = c(2L),
  lty = c(2L))
# matlines(
#   cumsum(sim$haz_at),
#   lty = 3L, 
#   lwd = 2L,
#   col = "red",
#   type = "s")
legend(
  "topleft",
  legend = c("Sample paths", "Average path"),
  col = c("gray", "black"),
  lty = c(1L, 2L),
  lwd = c(2L, 1L),
  bty = "n")


# Inventory plot
sim <- new.env()
initiate(sim, tau = 365L)
# reset.env(sim)
while (1L) {
  # Determine the next event
  timing(sim)
  # Invoke the appropriate event routine
  j <- sim$next_event_type
  switch (j,
          request_event(sim),
          approval_event(sim),
          break,
  )
}
N <- rep(0L, sim$tau)
N[floor(sim$requests_at)] <- 1.0
plot(cumsum(N), type = 's')

sim$inventory[sim$inventory == 0L] <- NA
# matplot(matrix(sim$inventory, ncol = sim$n_products) |> floor(), lty = 3L, pch = 16L, type = 'p')

lapply((seq(sim$n_products) - 1L) * sim$tau, function(idx) {
  N.t <- rep(0L, sim$tau)
  N.t[floor(sim$demand_at[idx + seq(sim$tau)])] <- 1L
  return(cumsum(N.t))
}) -> N.t
matplot(seq(sim$tau), do.call(cbind, N.t), type = "s", lty = 1L)

## Inventory simulation
tau <- sim$tau
n_goods <- sim$n_products
q_params <- sim$product_params

lapply((seq(sim$n_products) - 1L) * sim$tau, function(idx) {
  N <- rep(0L, sim$tau)
  N[floor(sim$demand_at[idx + seq(sim$tau)])] <- 1L
  N
}) |> (\(N) { do.call(cbind, N) })() -> N

q <- numeric(tau * n_goods)
t.ev <- numeric(n_goods)
for (t in seq(tau)) {
  tdx <- (t - 1L) * n_goods

  q[tdx + seq(n_goods)] <- sapply(
    seq_along(q_params), function(j) {
      q0   <- q_params[[j]]$q0
      beta <- q_params[[j]]$beta
      q0 - (beta * (t - t.ev[j]))
    })

  if (any(N[t,] > 0L)) {
    idx <- which(N[t,] > 0L)
    t.ev[idx] <- t
  }
}

par(mar = c(5., 4., 4., 2.) + 0.1) 
matplot(
  seq(tau),
  matrix(q, nrow = tau, byrow = TRUE), 
  type = 'l', 
  col = 1L:n_goods, 
  lty = 1L + seq(3L), 
  lwd = 2L,
  xlab = 'Time', 
  ylab = 'Inventory Stock')
# matpoints(
#   matrix(sim$inventory, ncol = sim$n_products) |> round(), 
#   lty = 3L, 
#   lwd = 2L,
#   pch = 4L, 
#   type = 'p')
legend(
  "topright",
  inset  = c(0.4, -0.0),
  legend = paste("Family", 1L:n_goods),
  col = 1L:n_goods,
  lty = 1L + seq(3L),
  lwd = 2L,
  xpd = TRUE,
  bty = "n"
)
