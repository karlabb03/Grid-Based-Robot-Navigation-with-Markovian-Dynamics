library(ggplot2)
library(reshape2)

# Define states and corresponding coordinates on 3x3 grid
states <- 1:9
coordinates <- data.frame(
  state = states,
  x = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  y = c(3, 3, 3, 2, 2, 2, 1, 1, 1)
)

# Define transition probabilities
transition_matrix <- matrix(c(0, 0.4, 0, 0.6, 0, 0, 0, 0, 0,
                              0.3, 0, 0.4, 0, 0.2, 0, 0, 0, 0,
                              0, 0.3, 0, 0, 0, 0.7, 0, 0, 0,
                              0.2, 0, 0, 0, 0.4, 0, 0.3, 0, 0,
                              0, 0.2, 0, 0.3, 0, 0.4, 0, 0.1, 0,
                              0, 0, 0.1, 0, 0.3, 0, 0, 0, 0.6,
                              0, 0, 0, 0.6, 0, 0, 0, 0.4, 0,
                              0, 0, 0, 0, 0.2, 0, 0.3, 0, 0.4,
                              0, 0, 0, 0, 0, 0.7, 0, 0.3, 0), 
                            nrow = 9, byrow = TRUE,
                            dimnames = list(states, states))

# Function to simulate robot movement (modified for grid)
simulateMovement1 <- function(initial_state, transition_matrix, num_steps) {
  current_state <- initial_state
  trajectory <- data.frame(x = coordinates$x[match(current_state, coordinates$state)],
                           y = coordinates$y[match(current_state, coordinates$state)])
  
  for (i in 1:num_steps) {
    current_state <- sample(states, size = 1, prob = transition_matrix[current_state, ])
    x <- coordinates$x[match(current_state, coordinates$state)]
    y <- coordinates$y[match(current_state, coordinates$state)]
    trajectory <- rbind(trajectory, data.frame(x = x, y = y))
  }
  
  return(trajectory)
}

# Simulate robot movement for 10 steps starting from state 1
trajectory <- simulateMovement1(1, transition_matrix, num_steps = 10)

# function to create line segments with arrowheads
createLineSegments <- function(trajectory) {
  segments <- data.frame(
    x_start = head(trajectory$x, -1),
    y_start = head(trajectory$y, -1),
    x_end = tail(trajectory$x, -1),
    y_end = tail(trajectory$y, -1)
  )
  return(segments)
}

segments <- createLineSegments(trajectory)

# plot
grid_plot <- ggplot(coordinates, aes(x, y)) +
  geom_text(aes(label = state), color = "black", size = 5) +
  geom_segment(data = segments, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red", linewidth = 1) +
  geom_point(data = coordinates[coordinates$state == initial_state,], aes(x, y),
             color = "blue", size = 3) +
  coord_equal() +
  theme_void()
grid_plot

## For any grid size ##

# Function to generate transition matrix
generateTransitionMatrix <- function(grid_size, movement_prob) {
  num_states <- grid_size*grid_size
  states <- 1:num_states
  transition_matrix <- matrix(0, nrow = num_states, ncol = num_states,
                              dimnames = list(states, states))
  
  for (i in 1:num_states) {
    row <- ((i - 1) %/% grid_size) + 1
    col <- ((i - 1) %% grid_size) + 1
    
    if (row > 1) { # can move up
      transition_matrix[i, (row - 2) * grid_size + col] <- movement_prob
    }
    if (row < grid_size) { # down
      transition_matrix[i, row * grid_size + col] <- movement_prob
    }
    if (col > 1) { # left
      transition_matrix[i, (row - 1) * grid_size + (col - 1)] <- movement_prob
    }
    if (col < grid_size) { # right
      transition_matrix[i, (row - 1) * grid_size + (col + 1)] <- movement_prob
    }
  }
  
  return(transition_matrix)
}

grid_size <- 6
movement_prob <- 0.25
transition_matrix <- generateTransitionMatrix(grid_size, movement_prob)

# Define states and corresponding coordinates on the grid
states <- 1:(grid_size*grid_size)
coordinates <- expand.grid(x = 1:grid_size, y = 1:grid_size)
coordinates$state <- states

initial_state <- 5
num_steps <- 30

trajectory <- simulateMovement1(initial_state, transition_matrix, num_steps)
segments <- createLineSegments(trajectory)

grid_plot <- ggplot(coordinates, aes(x, y)) +
  geom_text(aes(label = state), color = "black", size = 5) +
  geom_segment(data = segments, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red", linewidth = 1) +
  geom_point(data = coordinates[coordinates$state == initial_state,], aes(x, y),
             color = "blue", size = 3) +
  coord_equal() +
  theme_void()
grid_plot

## Heatmap ##

# Modified movement simulation function to record state sequences

simulateMovement2 <- function(initial_state, transition_matrix, num_steps) {
  current_state <- initial_state
  state_sequence <- c(current_state)
  
  for (i in 1:num_steps) {
    current_state <- sample(states, size = 1, prob = transition_matrix[current_state, ])
    state_sequence <- c(state_sequence, current_state)
  }
  
  return(state_sequence)
}

num_repetitions <- 1000
state_sequences <- replicate(num_repetitions, simulateMovement2(initial_state, transition_matrix, num_steps))
state_matrix <- matrix(unlist(state_sequences), nrow = num_repetitions, byrow = TRUE)
state_frequencies <- table(state_matrix) / num_repetitions
state_grid <- matrix(state_frequencies, nrow = grid_size, ncol = grid_size, byrow = TRUE)

# Plot heatmap of state frequencies
heatmap_plot <- ggplot(melt(state_grid), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(x = "X", y = "Y", fill = "Frequency") +
  theme_minimal()
heatmap_plot
