
# NM

# From Kimi LLM, unknown source code named DPSO.


DPSO <- function(num_particles, dimensions, max_iter, fitness_function) {
  # Initialize particles and velocities
  particles <- matrix(sample(c(0, 1), num_particles * dimensions, 
     replace = TRUE), nrow = num_particles, ncol = dimensions)
  velocities <- matrix(0, nrow = num_particles, ncol = dimensions)
  

  # Initialize personal best positions and fitness
  pbest_positions <- particles
  pbest_fitness <- apply(particles, 1, fitness_function)
  
  # Initialize global best position and fitness
  gbest_index <- which.min(pbest_fitness)
  gbest_position <- pbest_positions[gbest_index, , drop = FALSE]
  gbest_fitness <- min(pbest_fitness)
  
  # Define the update velocity function
  update_velocity <- function(w, c1, c2) {
    for (i in 1:num_particles) {
      r1 <- runif(dimensions)
      r2 <- runif(dimensions)
      cognitive_velocity <- c1 * r1 * (pbest_positions[i, ] - particles[i, ])
      social_velocity <- c2 * r2 * (gbest_position - particles[i, ])
      velocities[i, ] <- 
         w * velocities[i, ] + cognitive_velocity + social_velocity
    }
  }

  # Define the update position function
  update_position <- function() {
    for (i in 1:num_particles) {
      for (j in 1:dimensions) {
        if (runif(1) < 1 / (1 + exp(-velocities[i, j]))) {
          particles[i, j] <- 1 - particles[i, j]
        }
      }
    }
  }
  
  optimize <- function(w, c1, c2) {
    for (iteration in 1:max_iter) {
      update_velocity(w, c1, c2)
      update_position()
      for (i in 1:num_particles) {
        fitness <- fitness_function(particles[i, ])
        if (fitness < pbest_fitness[i]) {
          pbest_fitness[i] <- fitness
          pbest_positions[i, ] <- particles[i, ]
          if (fitness < gbest_fitness) {
            gbest_fitness <<- fitness
            gbest_position <<- particles[i, , drop = FALSE]
          }
        }
      }
      cat("Iteration", iteration, "Best Fitness:", gbest_fitness, "\n")
    }
    return(list(best_position = gbest_position, best_fitness = gbest_fitness))
  }
  
  # Return the optimize function
  return(list(optimize = optimize))
}

# Example usage
# Example fitness function: minimize the number of ones in the solution

fitness_function <- function(solution) {
  return(sum(solution))
}

num_particles <- 30
dimensions <- 10
max_iter <- 100
w <- 0.5
c1 <- 1.5
c2 <- 1.5

# Create DPSO object
dps <- DPSO(num_particles, dimensions, max_iter, fitness_function)

# Run optimization
result <- dps$optimize(w, c1, c2)
cat("Best Position:", result$best_position, "\n")
cat("Best Fitness:", result$best_fitness, "\n")

