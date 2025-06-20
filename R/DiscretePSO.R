
# NM: from a ChatGPT query; all comments not flagged as 'NM" are from
# the LLM

# NM: example here optimizes a length-20 vector of 1s and 0s, trying to
# max the sum

# NM: starting position is generated randomly

# Key Ideas of Discrete PSO
# 
# Positions are binary vectors (0/1).
# 
# Velocities are real numbers, but used to compute probability of a bit being 1.
# 
# Update rule uses a sigmoid transformation to squash velocity into [0,1].
# 
# Particles update their bits based on a Bernoulli trial with this probability.

# Key Source
# 
# Kennedy, J. and Eberhart, R. (1997):
# A discrete binary version of the particle swarm algorithm,
# IEEE International Conference on Systems, Man, and Cybernetics, 1997.
# https://ieeexplore.ieee.org/document/625213
# 
# This is the foundational paper that introduced BPSO. It uses:
# 
# A velocity as a real-valued vector.
# 
# A sigmoid function to convert velocity into a probability.
# 
# A Bernoulli trial (i.e., coin flip) to update binary positions.


# Additional Influences
# 
# Many modern tutorials and practical guides (such as implementations in
# Python and MATLAB) follow this same core idea:
# 
# Keep real-valued velocities.
# 
# Use a sigmoid to generate probabilities.
# 
# Sample each bit of the new position accordingly.

sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

fitness_function <- function(position) {
  # Example: maximize the number of 1s
  sum(position)
}

initialize_particle <- function(dim) {
  position <- sample(0:1, dim, replace = TRUE)
  velocity <- runif(dim, min = -1, max = 1)
  best_position <- position
  best_fitness <- fitness_function(position)
  list(position = position,
       velocity = velocity,
       best_position = best_position,
       best_fitness = best_fitness)
}

update_velocity <- function(particle,global_best,w=0.5,c1=1.5,c2=1.5) {
  r1 <- runif(length(particle$position))
  r2 <- runif(length(particle$position))
  cognitive <- c1 * r1 * (particle$best_position - particle$position)
  social <- c2 * r2 * (global_best - particle$position)
  particle$velocity <- w * particle$velocity + cognitive + social
  particle
}

update_position <- function(particle) {
  prob <- sigmoid(particle$velocity)
  particle$position <- as.integer(runif(length(prob)) < prob)
  
  fitness <- fitness_function(particle$position)
  if (fitness > particle$best_fitness) {
    particle$best_fitness <- fitness
    particle$best_position <- particle$position
  }
  particle
}

discrete_pso <- function(n_particles = 30, dim = 20, max_iter = 100) {
  swarm <- lapply(1:n_particles, function(i) initialize_particle(dim))
  
  global_best_particle <- 
     swarm[[which.max(sapply(swarm, function(p) p$best_fitness))]]
  global_best <- global_best_particle$best_position
  global_best_fitness <- global_best_particle$best_fitness
  
  for (iter in 1:max_iter) {
    for (i in seq_along(swarm)) {
      swarm[[i]] <- update_velocity(swarm[[i]], global_best)
      swarm[[i]] <- update_position(swarm[[i]])
     
      if (swarm[[i]]$best_fitness > global_best_fitness) {
        global_best <- swarm[[i]]$best_position
        global_best_fitness <- swarm[[i]]$best_fitness
      }
    }
  }

  list(best_position = global_best, best_fitness = global_best_fitness)
}

result <- discrete_pso()
cat("Best solution:", result$best_position, "\n")
cat("Best fitness:", result$best_fitness, "\n")

