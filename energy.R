## movement simulation based on energy ##

library(ggplot2)
library(dplyr)

# Function to simulate robot movement using vectors (instead of transition matrix) and energy consumption
simulateMovementWithEnergy <- function(initial_state, initial_energy) {
  current_state <- initial_state
  current_energy <- initial_energy
  step_count <- 0 
  trajectory <- data.frame(x = coordinates$x[match(current_state, coordinates$state)],
                           y = coordinates$y[match(current_state, coordinates$state)])
  
  while (current_energy > 0) {
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
    
    destination_energy <- energy_values[row, col]
    
    if (destination_energy > current_energy) {
      break 
    }
    
    current_state <- (row - 1) * grid_size + col
    trajectory <- rbind(trajectory, data.frame(x = col, y = row))
    
    current_energy <- current_energy + destination_energy
    step_count <- step_count + 1
  }
  
  return(list(trajectory = trajectory, steps = step_count))
}

grid_size <- 6

states <- 1:(grid_size * grid_size)
coordinates <- expand.grid(x = 1:grid_size, y = 1:grid_size)
coordinates$state <- states

energy_values <- matrix(c(-1, -2, -3, -2, -1, 10,
                          -1, -1, -2, -2, -1, -1,
                          -1, 10, -1, -3, -2, -1,
                          -1, -1, -1, -1, -1, -1,
                          -1, -2, -1, 10, -1, -1,
                          -1, -1, -1, -1, -1, -2), nrow = grid_size, ncol = grid_size)

initial_state <- 8
initial_energy <- 50

result <- simulateMovementWithEnergy(initial_state, initial_energy)
trajectory <- result$trajectory
step_count <- result$steps

segments <- data.frame(
  x_start = head(trajectory$x, -1),
  y_start = head(trajectory$y, -1),
  x_end = tail(trajectory$x, -1),
  y_end = tail(trajectory$y, -1)
)

grid_plot <- ggplot(coordinates, aes(x, y)) +
  geom_text(aes(label = state), colour = "black", size = 5) +
  geom_segment(data = segments, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red", linewidth = 1) +
  geom_point(data = coordinates[coordinates$state == initial_state,], aes(x, y),
             colour = "blue", size = 3) +
  coord_equal() +
  theme_void()
grid_plot
cat("Number of steps taken:", step_count, "\n")

energy_heatmap <- ggplot(data = melt(energy_values), aes(x = Var2, y = Var1, fill = value, label = ifelse(value >= 0, sprintf("+%d", value), as.character(value)))) +
  geom_tile() +
  geom_text(colour = "black", size = 4, vjust = 1) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "Column", y = "Row", fill = "Energy Demand") +
  theme_minimal() +
  coord_fixed(ratio = 1)

print(energy_heatmap)