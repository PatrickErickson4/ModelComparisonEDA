library(ggplot2)
library(dplyr)

# Define the structure of the neural network
input_nodes <- 20
hidden_nodes <- 13
output_nodes <- 1

# Calculate vertical midpoint for centering layers
max_nodes <- max(input_nodes,
                 hidden_nodes,
                 output_nodes)
input_y <- seq(1,
               max_nodes,
               length.out = input_nodes)
hidden_y <- seq(1 + (max_nodes - hidden_nodes) / 2,
                hidden_nodes + (max_nodes - hidden_nodes) / 2,
                length.out = hidden_nodes)
output_y <- seq(1 + (max_nodes - output_nodes) / 2,
                output_nodes + (max_nodes - output_nodes) / 2,
                length.out = output_nodes)

# Create a data frame for nodes
nodes <- data.frame(
  layer = c(rep("Input", input_nodes), 
            rep("Hidden (ReLU Activation)", hidden_nodes), 
            rep("Output (Sigmoid)", output_nodes)),
  node_id = c(1:input_nodes, 1:hidden_nodes, 1:output_nodes),
  x = c(rep(1, input_nodes), rep(2, hidden_nodes), rep(3, output_nodes)),
  y = c(input_y, hidden_y, output_y)
)

# Create a data frame for connections
connections <- expand.grid(
  from = 1:input_nodes,
  to = (1:hidden_nodes) + input_nodes
) %>%
  mutate(x_from = 1, y_from = input_y[from], x_to = 2, y_to = hidden_y[to - input_nodes]) %>%
  bind_rows(expand.grid(
    from = (1:hidden_nodes) + input_nodes,
    to = (1:output_nodes) + input_nodes + hidden_nodes
  ) %>% mutate(x_from = 2, y_from = hidden_y[from - input_nodes], x_to = 3, y_to = output_y[to - input_nodes - hidden_nodes]))

# Plot the neural network
ggplot() +
  # Plot nodes
  geom_point(data = nodes, aes(x = x, y = y), size = 5, color = "blue") +
  geom_segment(data = connections,
               aes(x = x_from,
                   y = y_from,
                   xend = x_to,
                   yend = y_to),
               alpha = 0.3) +
  # Add labels for layers
  annotate("text", x = 1.15,
           y = max_nodes + 2,
           label = "\n20 inputs",
           size = 5,
           hjust = 0.5) +
  annotate("text",
           x = 2,
           y = max_nodes + 2,
           label = "\n13 ReLU Nodes",
           size = 5,
           hjust = 0.5) +
  annotate("text",
           x = 2.85,
           y = max_nodes + 2,
           label = "\nSigmoid Output\nLayer",
           size = 5,
           hjust = 0.5) +
  # Customize the plot
  theme_void() +
  labs(title = "Final Neural Network Architecture") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )