# scripts/pre_train_agents.R
# Este script genera una memoria sintética pre-entrenada para agentes pedestres.
# No representa observaciones reales, salvo que el usuario reemplace los insumos.
# Su función es inicializar hotspots de fricción para demostración, pedagogía y experimentación.
# Script for pre-training the Pedestrian Agent's Collective Memory (Hotspots)
# by running 100 BVP simulations under various conditions.

# Define base directories (relative path setup with robust fallback)
if (dir.exists("puerto_umbral_zenodo_bundle/app")) {
  setwd("puerto_umbral_zenodo_bundle/app")
} else if (basename(getwd()) != "app" && dir.exists("app")) {
  setwd("app")
} else if (basename(getwd()) == "scripts") {
  setwd("../app")
}

# Source global.R to load libraries, data frames and functions
source("global.R", encoding = "UTF-8")

cat("=== STARTING PEATONAL AGENT PRE-TRAINING ===\n")
cat("Sourced global.R successfully. manzanas_df size:", nrow(manzanas_df), "\n")

# Initialize collective trauma memory matrix (30x30)
m_trauma <- matrix(0, 30, 30)

# We will run 1000 random geodesic simulations under different scenarios
set.seed(123)
n_runs <- 1000
failures <- 0

# Base grid vectors
xg <- seq(min(manzanas_df$x) - 100, max(manzanas_df$x) + 100, length.out = 30)
yg <- seq(min(manzanas_df$y) - 100, max(manzanas_df$y) + 100, length.out = 30)
dx <- mean(diff(xg))
dy <- mean(diff(yg))

profiles <- c("cuidado", "mayor", "nocturna", "joven")

for (i in 1:n_runs) {
  # Randomly pick origin and destination
  idx_orig <- sample(1:nrow(manzanas_df), 1)
  idx_dest <- sample(1:nrow(manzanas_df), 1)
  
  if (idx_orig == idx_dest) next
  
  orig <- manzanas_df[idx_orig, ]
  dest <- manzanas_df[idx_dest, ]
  
  # Pick a random profile and vital energy
  profile <- sample(profiles, 1)
  v0_vital <- sample(10:25, 1)
  
  # Generate a temporary active surface with a random distortion (delito or obstacle) to simulate real-world difficulty
  dist_df <- data.frame(
    id = "temp_dist",
    x = sample(seq(min(manzanas_df$x), max(manzanas_df$x), length.out=10), 1),
    y = sample(seq(min(manzanas_df$y), max(manzanas_df$y), length.out=10), 1),
    type = "delito",
    mag = "severa",
    stringsAsFactors = FALSE
  )
  
  # Compute active surface for the run
  Z_base <- matrix(runif(900, 20, 80), 30, 30) # Random background topography
  Z_active <- Z_base
  for (r in 1:30) {
    for (c in 1:30) {
      dist_sq <- (xg[c] - dist_df$x)^2 + (yg[r] - dist_df$y)^2
      Z_active[r, c] <- Z_active[r, c] + 250 * exp(-dist_sq / (2 * 350^2))
    }
  }
  
  # Compute correct manifold geometry and GammaPacked
  pre <- compute_manifold_geometry(
    Xg = xg, Yg = yg, Z_active = Z_active, Z_alt = matrix(0, 30, 30),
    dx = dx, dy = dy, lambda_val = 0.8, exp_mode_val = "base", g_urban_ratio_val = 8.0, use_lw = TRUE
  )
  
  # Solve BVP geodesic
  # First find best theta
  dx_dist <- dest$x - orig$x
  dy_dist <- dest$y - orig$y
  d_total <- sqrt(dx_dist^2 + dy_dist^2)
  tmax_val <- max(5, min(500, 1.5 * d_total / v0_vital))
  dt_val <- tmax_val / 100
  
  opt_theta_local <- function(th) {
    traj <- solve_geodesic(
      pre = pre, x0 = orig$x, y0 = orig$y, theta = th, v0 = v0_vital,
      dest_x = dest$x, dest_y = dest$y, use_V = TRUE,
      A_wall = 15000, A_dest = 10000,
      distorsiones = dist_df, profile = profile, A_slope = 5000,
      omega_damping = 0.4, Tmax = tmax_val, dt = dt_val,
      exp_mode = "base", k_conservation = 2.0, amp_santuario = 20000,
      sigma_capital = 1.0, capital_flow_direction = "rural_to_urban"
    )
    last_pt <- traj[nrow(traj), ]
    sqrt((last_pt$x - dest$x)^2 + (last_pt$y - dest$y)^2)
  }
  
  theta_grid <- seq(-pi, pi, length.out = 12)
  dists <- sapply(theta_grid, opt_theta_local)
  best_th <- theta_grid[which.min(dists)]
  
  traj <- tryCatch({
    solve_geodesic(
      pre = pre, x0 = orig$x, y0 = orig$y, theta = best_th, v0 = v0_vital,
      dest_x = dest$x, dest_y = dest$y, use_V = TRUE,
      A_wall = 15000, A_dest = 10000,
      distorsiones = dist_df, profile = profile, A_slope = 5000,
      omega_damping = 0.4, Tmax = tmax_val, dt = dt_val,
      exp_mode = "base", k_conservation = 2.0, amp_santuario = 20000,
      sigma_capital = 1.0, capital_flow_direction = "rural_to_urban"
    )
  }, error = function(e) NULL)
  
  if (is.null(traj) || nrow(traj) == 0) next
  
  last_pt <- traj[nrow(traj), ]
  dist_final <- sqrt((last_pt$x - dest$x)^2 + (last_pt$y - dest$y)^2)
  
  # Accumulate trauma if walk failed or faced high friction
  if (dist_final > 150) {
    failures <- failures + 1
    for (k in seq_len(nrow(traj))) {
      tx <- traj$x[k]
      ty <- traj$y[k]
      
      idx_x <- which.min(abs(xg - tx))
      idx_y <- which.min(abs(yg - ty))
      
      # Add Gaussian trauma bump
      for (r in max(1, idx_y-2):min(30, idx_y+2)) {
        for (c in max(1, idx_x-2):min(30, idx_x+2)) {
          dist_sq <- (xg[c] - tx)^2 + (yg[r] - ty)^2
          m_trauma[r, c] <- m_trauma[r, c] + 45 * exp(-dist_sq / (2 * 200^2))
        }
      }
    }
  }
}

# Cap values and add natural hotspots for urban barriers
m_trauma[m_trauma > 600] <- 600
m_trauma[10:18, 11:15] <- m_trauma[10:18, 11:15] + 90 # Canal Las Perdices
m_trauma[6:9, 20:24] <- m_trauma[6:9, 20:24] + 120 # Ladera Quebrada Macul
m_trauma[m_trauma > 600] <- 600

cat("Pre-training finished. Total failures processed:", failures, "/", n_runs, "\n")

# Save as JSON
dir.create("www/data", showWarnings = FALSE, recursive = TRUE)
json_data <- jsonlite::toJSON(m_trauma, pretty = TRUE)
writeLines(json_data, "www/data/pre_trained_memory.json")
cat("Successfully wrote pre_trained_memory.json to www/data/ (Knowledge base established).\n")
