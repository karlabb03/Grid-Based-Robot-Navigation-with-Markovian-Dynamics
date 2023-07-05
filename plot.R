library(ggplot2)

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

# Function to simulate robot movement
simulateMovement <- function(initial_state, transition_matrix, num_steps) {
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

# Simulate robot movement for 10 steps starting from state 1
trajectory <- simulateMovement(1, transition_matrix, num_steps = 10)
segments <- createLineSegments(trajectory)

# plot
grid_plot <- ggplot(coordinates, aes(x, y)) +
  geom_text(aes(label = state), color = "black", size = 5) +
  geom_segment(data = segments, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0.5, 3.5), breaks = 1:3) +
  scale_y_continuous(limits = c(0.5, 3.5), breaks = 1:3) +
  coord_equal() +
  theme_void()
grid_plot