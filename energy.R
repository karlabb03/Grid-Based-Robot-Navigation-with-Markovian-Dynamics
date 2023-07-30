## movement simulation based on energy ##

library(ggplot2)
library(dplyr)

# Function to simulate robot movement using vectors (instead of transition matrix) and energy consumption
simulateMovement3 <- function(initial_state, energy) {
  current_state <- initial_state
  trajectory <- data.frame(x = coordinates$x[match(current_state, coordinates$state)],
                           y = coordinates$y[match(current_state, coordinates$state)])
  
  while (energy > 0) {
    direction <- sample(c("up", "down", "left", "right"), size = 1)
    row <- (current_state - 1) %/% grid_size + 1
    col <- (current_state - 1) %% grid_size + 1
    
    if (direction == "up" && row > 1) {
      row <- row - 1
    } else if (direction == "down" && row < grid_size) {
      row <- row + 1
    } else if (direction == "left" && col > 1) {
      col <- col - 1
    } else if (direction == "right" && col < grid_size) {
      col <- col + 1
    }
    
    current_state <- (row - 1) * grid_size + col
    trajectory <- rbind(trajectory, data.frame(x = col, y = row))
    
    energy <- energy - 1
  }
  
  return(trajectory)
}

grid_size <- 6

states <- 1:(grid_size * grid_size)
coordinates <- expand.grid(x = 1:grid_size, y = 1:grid_size)
coordinates$state <- states

initial_state <- 8
energy <- 30

trajectory <- simulateMovement3(initial_state, energy)
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