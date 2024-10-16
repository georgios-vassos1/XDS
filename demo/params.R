library(ggplot2)
########################################################################################################################
# Price parameters
########################################################################################################################
tau <- 365L
# Product level data
prod_idx   <- seq(3L)
prod_ids   <- seq(3L)
prod_names <- c("Diesel Fuel", "Engine Oil", "Cleaning Supplies")
# Inventory depletion model parameterization
n_products  <- length(prod_idx)

# Supplier level data
supp_idx <- seq(3L)
supp_ids <- seq(3L)
supp_names <- c("Supplier A", "Supplier B", "Supplier C")
# Auxiliary
n_suppliers <- length(supp_idx)
# Volume commitment
volume_commitment <- c(100.0, 100.0, 50.0)
volume <- numeric(n_suppliers * tau)
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

spot_variation <- numeric(n_products * n_suppliers * tau)
spot_sd <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
for (i in prod_idx) {
  for (j in supp_idx) {
    spot_variation[((i - 1L) * n_suppliers + (j - 1L)) * tau + seq(tau)] <- rnorm(tau, 0.0, spot_sd[(i - 1L) * n_suppliers + j])
  }
}
# # Spot variation per product and supplier
# spot_variation <- c(
#   rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 1.0), # Product 1
#   rnorm(tau, 0.0, 1.0), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 2.0), # Product 2
#   rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5), rnorm(tau, 0.0, 0.5)  # Product 3
# )

spot_rates <- numeric(tau * n_products * n_suppliers)
for (i in seq(n_products)) {
  idx <- ((seq(tau) - 1L) * n_products + (i - 1L)) * n_suppliers
  for (j in seq(n_suppliers)) {
    jdx <- ((i - 1L) * n_suppliers + (j - 1L)) * tau
    spot_rates[idx + j] <- spot_supplier_baseline[j] + 
      spot_supplier_seasonality[jdx + seq(tau)] + 
      spot_variation[jdx + seq(tau)] * 0.0
  }
}

df_spot <- data.frame(
  Time = rep(seq(tau), each = n_products * n_suppliers),
  Product  = factor(rep(paste0("Product ", seq(n_products)), each = n_suppliers, times = tau)),
  Supplier = factor(rep(supp_names, times = n_products * tau)),
  SpotRate = spot_rates,
  LowerBound = spot_rates + qnorm(0.025, mean = 0, sd = spot_sd) * spot_sd,
  UpperBound = spot_rates + qnorm(0.975, mean = 0, sd = spot_sd) * spot_sd
)

df_spot |> 
  ggplot(aes(x = Time, y = SpotRate, color = Supplier)) +
  facet_wrap(~Product, nrow = 2L) +  # Horizontally stack the plots
  geom_line(linewidth = 1.) +  # Line for spot rates
  geom_ribbon(
    aes(ymin = LowerBound, ymax = UpperBound, fill = Supplier), alpha = 0.2, color = NA) +
  lims(x = c(0L, 365L), y = c(0L, 20L)) +
  labs(x = "Time", y = "Spot Rates") +
  theme_minimal() +
  theme(
    legend.position = "top",                # Move legend to the top
    legend.title = element_blank(), # Increase legend title font size
    legend.text  = element_text(size = 12L),  # Increase legend items font size
    axis.title = element_text(size = 14L),   # Increase axis titles font size
    axis.text  = element_text(size = 12L),    # Increase axis text font size
    strip.text = element_text(size = 14L, face = "bold")    # Increase facet labels font size
  )


## Manual plot
par(mfrow = c(3L, 1L), mar = c(5., 4. + 0.4, 4., 2.))

# Iterate over i = 1, 2, 3
for (i in 1L:3L) {
  plot(
    NA, 
    type     = 'n', 
    cex.lab  = 1.75, 
    cex.axis = 1.75, 
    cex.main = 1.75,
    ylim = range(spot_rates) + c(-2.0, 2.0), 
    xlim = c(1L, tau), 
    xlab = 'Time', 
    ylab = 'Spot Rates', 
    main = paste('Product ', i))

  # Iterate over j = 1, 2, 3
  for (j in 1L:3L) {
    pdx <- ((seq(tau) - 1L) * n_products + (i - 1L)) * n_suppliers + j
    lines(
      spot_rates[pdx], 
      type = 'l', 
      col  = j, 
      lty  = 1L, 
      pch  = j, 
      lwd  = 2L)

    idx <- (i - 1) * n_suppliers + j
    lower_bound <- spot_rates[pdx] + qnorm(0.025, mean = 0, sd = spot_sd[idx]) * spot_sd[idx]
    upper_bound <- spot_rates[pdx] + qnorm(0.975, mean = 0, sd = spot_sd[idx]) * spot_sd[idx]

    # Shading the confidence interval
    polygon(
      c(seq(tau), rev(seq(tau))), 
      c(lower_bound, rev(upper_bound)), 
      col = rgb(0.1, 0.1, 0.1, alpha = 0.2),  # Semi-transparent gray color
      border = NA
    )
  }

  legend(
    "topright", 
    legend = paste('Supplier ', 1L:3L), 
    col = 1L:3L, 
    lty = 1L, 
    lwd = 2L, 
    horiz = TRUE, 
    cex = 1.5,
    bty = "n",
    xpd = TRUE, 
    inset = c(0, -0.15))
}
par(mfrow = c(1L, 1L))

# Strategic rates
strategic_rates <- numeric(n_products * n_suppliers)
for (i in seq(n_products)) {
  for (j in seq(n_suppliers)) {
    idx <- ((seq(tau) - 1L) * n_products + (i - 1L)) * n_suppliers
    jdx <- ((i - 1L) * n_suppliers + (j - 1L)) * tau

    strategic_rates[(i - 1L) * n_suppliers + j] <- unname(
      quantile(
        spot_rates[idx + j] <- spot_supplier_baseline[j] + 
          spot_supplier_seasonality[jdx + seq(tau)] + 
          spot_variation[jdx + seq(tau)] * 0.0,
      ))[3L]
  }
}
(valid_from <- sample(c(0L), n_products * n_suppliers, replace = TRUE))
#  (valid_to   <- pmin(valid_from + sample(c(180L, 365L), n_products * n_suppliers, replace = TRUE), 365L))
(valid_to   <- rep(c(182L, 182L, 365L), n_products))
strategic_rates


strategic_rates <- rep(c(11.0, 11.0, 12.0), n_products)
# Create a data frame
df_strategic <- data.frame(
  Product  = factor(rep(paste0("Product ", seq(n_products)), each  = n_suppliers)),
  Supplier = factor(rep(supp_names, times = n_products)),
  StrategicRate = strategic_rates + c(-0.1, 0.1, 0.0),
  ValidFrom = valid_from,
  ValidTo   = valid_to
)

df_strategic |>
  ggplot(aes(x = ValidFrom, xend = ValidTo, y = StrategicRate, yend = StrategicRate, color = Supplier)) +
  facet_wrap(~Product, scales = "free_y", nrow = 2L) +
  geom_segment(size = 1.5) +
  lims(x = c(0L, 365L), y = c(0L, 20L)) +
  labs(x = "Time", y = "Strategic Rate") +
  theme_minimal() +
  theme(
    legend.position = "top",                # Move legend to the top
    legend.title = element_blank(), # Increase legend title font size
    legend.text  = element_text(size = 12L),  # Increase legend items font size
    axis.title = element_text(size = 14L),   # Increase axis titles font size
    axis.text  = element_text(size = 12L),    # Increase axis text font size
    strip.text = element_text(size = 14L, face = "bold")    # Increase facet labels font size
  )

ggplot(df_spot, aes(x = Time, y = SpotRate, color = Supplier)) +
  facet_wrap(~Product, nrow = 2L) +  # Horizontally stack the plots
  geom_ribbon(
    aes(ymin = LowerBound, ymax = UpperBound, fill = Supplier), alpha = 0.2, color = NA) +
  geom_segment(
    data = df_strategic, 
    aes(x = ValidFrom, xend = ValidTo, y = StrategicRate, yend = StrategicRate, color = Supplier),
    linewidth = 1.0, alpha = 1.0) +
  lims(x = c(0L, 365L), y = c(0L, 20L)) +
  labs(x = "Time", y = "Market Rates") +
  theme_minimal() +
  theme(
    legend.position = "top",                # Move legend to the top
    legend.title = element_blank(), # Increase legend title font size
    legend.text  = element_text(size = 12L),  # Increase legend items font size
    axis.title = element_text(size = 14L),   # Increase axis titles font size
    axis.text  = element_text(size = 12L),    # Increase axis text font size
    strip.text = element_text(size = 14L, face = "bold")    # Increase facet labels font size
  )

########################################################################################################################
# Demand parameters
########################################################################################################################
Weibull_haz <- function(t, t.ev, ...) {
  args <- list(...)

  shape <- args$shape
  scale <- args$scale
  tau <- args$tau

  (shape / scale) * (((t - t.ev) / scale) ^ (shape - 1.0)) * 
    exp( cos(2.0 * pi * t / tau) + cos((2.0 * pi * t / tau) + pi/4.0))
}

Weibull_Haz <- function(t, t.ev, Haz.t.ev, ...) {
  args <- list(...)

  shape <- args$shape
  scale <- args$scale
  tau <- args$tau

  Haz.t.ev + integrate(function(x) Weibull_haz(x, t.ev = t.ev, shape = shape, scale = scale, tau = tau), t.ev, t)$value
}

rReqTime <- function(t.ev, n_intervals = 50L, ...) {
  args <- list(...)
  
  shape <- args$shape
  scale <- args$scale
  tau   <- args$tau

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
  # Compute the event time
  t.ev <- t_grid[i] + ((- log(u) - Haz[i]) / haz_pcw[i])

  # Return the event time and cumulative hazard
  list(
    "t.ev" = t.ev,
    "Haz"  = Haz + haz_pcw[i] * (t.ev - t_grid[i])
  )
}

## Test
shape <- 1.5
scale <- 10.0
tau   <- 365L

t.ev <- Haz.t.ev <- 0.0

span <- seq(0L, 365L, length.out = 1000L)
# Plot the hazard rate
plot(
  span, 
  Weibull_haz(span, t.ev = 0.0, shape = 1.5, scale = 10.0, tau = 365L),
  xlab = 'Time',
  ylab = 'Hazard rate',
  type = 'l')

# Plot the cumulative hazard
plot(
  span, 
  sapply(span, Weibull_Haz, t.ev = 0.0, Haz.t.ev = 0.0, shape = 1.5, scale = 10.0, tau = 365L),
  xlab = 'Time',
  ylab = 'Cumulative hazard',
  type = 'l')

# Single run simulation loop
N <- numeric(tau)
Haz <- c()
while (1L) {
  x <- rReqTime(t.ev, tau = tau, shape = shape, scale = scale)
  t.ev <- x$t.ev
  Haz <- c(Haz, x$Haz)
  if (t.ev >= tau) break
  Haz.t.ev <- x$Haz
  N[round(t.ev)] <- N[round(t.ev)] + 1L
}
plot(cumsum(N), type = 's')

q_params <- list(
  list("q0" = 10L, "beta" = 0.1),
  list("q0" = 20L, "beta" = 0.2),
  list("q0" = 10L, "beta" = 0.1),
  list("q0" = 15L, "beta" = 0.2),
  list("q0" = 10L, "beta" = 0.2)
)
n_goods <- length(q_params)

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

  if ((N[t] > 0L)) {
    idx <- sample(n_goods, sample(n_goods - 2L, 1L))
    t.ev[idx] <- t
  }
}

matplot(
  seq(tau),
  matrix(q, nrow = tau, byrow = TRUE), 
  type = 'l', 
  col = 1L:n_goods, 
  lty = 1L, 
  lwd = 2L,
  xlab = 'Time', 
  ylab = 'Inventory')

# Simulate the number of events
run <- function(tau, shape, scale) {
  t.ev <- 0.0
  N    <- numeric(tau)
  while (1L) {
    x <- rReqTime(t.ev, tau = tau, shape = shape, scale = scale)
    t.ev <- x$t.ev
    if (t.ev >= tau) break
    N[round(t.ev)] <- N[round(t.ev)] + 1L
  }
  cumsum(N)
}
Nz <- replicate(1000L, run(tau, shape, scale))
matplot(Nz, type = 's', col = 'gray', lty = 1L, xlab = 't (time)', ylab = 'N(t)')
N.avg <- apply(Nz, 1L, mean) + apply(Nz, 1L, sd) %*% t(c(-1.0, 0.0, 1.0) * qnorm(0.975))
lines(N.avg[,2L], type = 'l', lw = 2L, col = 'black', lty = 2L)
lines(N.avg[,1L], col = 'black', lwd = 2L, lty = 3L)
lines(N.avg[,3L], col = 'black', lwd = 2L, lty = 3L)
