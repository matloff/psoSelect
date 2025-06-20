
# from Kimi LLM, base on 

# https://www.cs.montana.edu/users/john.sheppard/pubs/gecco-2016a.pdf

CPSO <- function(num_particles, dimensions, num_states, max_iter, fitness_function) {
  # Initialize particles and velocities
  particles <- lapply(1:num_particles, function(i) runif(dimensions * num_states))
  particles <- lapply(particles, function(p) matrix(p, nrow = dimensions, ncol = num_states))
  velocities <- lapply(1:num_particles, function(i) runif(dimensions * num_states))
  velocities <- lapply(velocities, function(v) matrix(v, nrow = dimensions, ncol = num_states))

 # Initialize personal best positions and fitness
  pbest_positions <- particles
  pbest_fitness <- sapply(particles, function(p) fitness_function(sample_solution(p)))
  
  # Initialize global best position and fitness
  gbest_index <- which.min(pbest_fitness)
  gbest_position <- pbest_positions[[gbest_index]]
  gbest_fitness <- min(pbest_fitness)
  
  # Define the update velocity function
  update_velocity <- function(w, c1, c2) {
    for (i in 1:num_particles) {
      r1 <- runif(dimensions * num_states)
      r1 <- matrix(r1, nrow = dimensions, ncol = num_states)
      r2 <- runif(dimensions * num_states)
      r2 <- matrix(r2, nrow = dimensions, ncol = 
num_states)
      cognitive_velocity <- c1 * r1 * (pbest_positions[[i]] - particles[[i]])
      social_velocity <- c2 * r2 * (gbest_position - particles[[i]])
      velocities[[i]] <- w * velocities[[i]] + cognitive_velocity + social_velocity
    }
  }
  
  # Define the update position function
  update_position <- function() {
    for (i in 1:num_particles) {
      particles[[i]] <- particles[[i]] + velocities[[i]]
      particles[[i]] <- apply(particles[[i]], 2, function(col) {
        col[col < 0] <- 0
        col[col > 1] <- 1
        col / sum(col)
      })
    }
  }
  
  # Define the sample solution function
  sample_solution <- function(particle) {
    sapply(1:dimensions, function(d) sample(1:num_states, 1, prob = particle[d, ]))
  }
  
  # Define the optimization function
  optimize <- function(w, c1, c2) {
    for (iteration in 1:max_iter) {
      update_velocity(w, c1, c2)
      update_position()
      for (i in 1:num_particles) {
        sample <- sample_solution(particles[[i]])
        fitness <- fitness_function(sample)
        if (fitness < pbest_fitness[i]) {
          pbest_fitness[i] <- fitness
          pbest_positions[[i]] <- particles[[i]]
          if (fitness < gbest_fitness) {
            gbest_fitness <<- fitness
            gbest_position <<- particles[[i]]
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
fitness_function <- function(solution) {
  # Example fitness function: minimize the sum of the solution
  return(sum(solution))
}

num_particles <- 30
dimensions <- 10
num_states <- 5
max_iter <- 100
w <- 0.5
c1 <- 1.5
c2 <- 1.5
# Create ICPSO object
icps <- ICPSO(num_particles, dimensions, num_states, max_iter, fitness_function)

# Run optimization
result <- icps$optimize(w, c1, c2)
cat("Best Position:", result$best_position, "\n")
cat("Best Fitness:", result$best_fitness, "\n")

