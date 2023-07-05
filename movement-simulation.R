## Modelling robot movement as a Markov chain ##

# Define states and transition probabilities
states <- c("A", "B", "C", "D")
transition_matrix <- matrix(c(0.2, 0.4, 0.3, 0.1,
                              0.3, 0.2, 0.4, 0.1,
                              0.1, 0.3, 0.2, 0.4,
                              0.4, 0.1, 0.3, 0.2), nrow = 4, byrow = TRUE,
                            dimnames = list(states, states))

# Function to simulate robot movement

simulateMovement <- function(initial_state, transition_matrix, num_steps) {
  current_state <- initial_state
  trajectory <- c(current_state)
  
  for (i in 1:num_steps) {
    current_state <- sample(states, size = 1, prob = transition_matrix[current_state, ])
    trajectory <- c(trajectory, current_state)
  }
  
  return(trajectory)
}

# Simulate robot movement for 10 steps starting from state "A"

trajectory <- simulateMovement("A", transition_matrix, num_steps = 10)
trajectory
