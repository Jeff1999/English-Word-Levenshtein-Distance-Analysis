# Install and load required packages
library(igraph)
library(stringdist)

# Read the wordlist from a text file containing most popular 10,000 English words
# The file "google-10000-english-no-swears.txt" should be in the working directory
wordlist <- readLines("google-10000-english-no-swears.txt")

# Create an adjacency matrix based on the Levenshtein distance (edit distance)
# 'outer' computes pairwise distances between all words using Levenshtein distance
m <- outer(wordlist, wordlist, stringdist, method = "lv")

# Only keep edges where the distance is exactly 1 (single-character transformations)
m[m != 1] <- 0

# Create an undirected graph from the adjacency matrix
wordweb <- graph_from_adjacency_matrix(m, mode = "undirected")

# Assign word names as vertex labels in the graph
V(wordweb)$name <- wordlist

# Calculate centrality metrics
# Betweenness centrality measures how often a vertex 
# appears on the shortest paths between other vertices
btw <- betweenness(wordweb)

# Degree centrality counts the number of connections (edges) each vertex has
deg <- degree(wordweb)

# Print Degree Centrality Results
cat("\n=== DEGREE CENTRALITY ===\n")
cat("\nTop 10 Highest Degree Words/Characters:\n")

# Find and display the top 10 vertices with the highest degree centrality
top_deg <- head(sort(deg, decreasing = TRUE), 10)
print(data.frame(
  word = names(top_deg), # Word names
  degree = top_deg       # Degree centrality values
), row.names = FALSE)

cat("\nLast 10 Zero Degree Words (Total Zero Degree Words:", sum(deg == 0), "):\n")

# Find and display the first 10 words with zero degree centrality
bottom_deg <- head(sort(deg), 10)
print(data.frame(
  word = names(bottom_deg), # Word names
  degree = bottom_deg       # Degree centrality values
), row.names = FALSE) 

# Print Betweenness Centrality Results
cat("\n=== BETWEENNESS CENTRALITY ===\n")
cat("\nTop 10 Highest Betweenness:\n")

# Find and display the top 10 vertices with the highest betweenness centrality
top_btw <- head(sort(btw, decreasing = TRUE), 10)
print(data.frame(
  word = names(top_btw), # Word names
  betweenness = round(top_btw, 2) # Betweenness centrality values rounded to 2 decimals
), row.names = FALSE)

cat("\nLast 10 Zero Betweenness Words (Total Zero Betweenness Words:", sum(btw == 0), "):\n")
# Find and display the first 10 words with zero betweenness centrality
bottom_btw <- head(sort(btw), 10)
print(data.frame(
  word = names(bottom_btw),
  betweenness = bottom_btw # Betweenness centrality values
), row.names = FALSE)

# Community detection using the Louvain algorithm
# NOTE: BECAUSE OF HOW THE LOUVIAN ALGORITHM OPERATES, IT IS NORMAL TO HAVE 
# SOME DEVIATION BETWEEN CODE EXECUTION AND COMMUNTIY STRUCTURES 
# (most analysis however are unaffected)

# Identify clusters of words based on their connections using the Louvain algorithm
communities <- cluster_louvain(wordweb)

# Get the sizes of each detected community (number of words in each cluster)
community_sizes <- sizes(communities)

# Calculate summary statistics for community sizes
cat("\n=== COMMUNITY SIZE STATISTICS ===\n")
cat("Number of Communities:", length(community_sizes), "\n") # Total number of communities
cat("Mean Community Size:", round(mean(community_sizes), 2), "\n")  # Average size of communities
cat("Median Community Size:", median(community_sizes), "\n") # Median size of communities
cat("Standard Deviation:", round(sd(community_sizes), 2), "\n") # Variation in community sizes
cat("Minimum Size:", min(community_sizes), "\n") # Smallest community size
cat("Maximum Size:", max(community_sizes), "\n") # Largest community size
cat("Total Words in Communities:", sum(community_sizes), "\n") # Total number of words across all communities

full_density <- edge_density(wordweb) # Calculate the density of the entire network
cat("Full Network Density:", round(full_density, 6)) # Print the density

# Open a new plot window for community size visualization
dev.new()
# Plot community sizes as a bar chart
plot(sort(community_sizes, decreasing=TRUE), 
     type="h",  # Use vertical lines to represent sizes
     main="Community Sizes in Word Network", # Title of the plot
     xlab="Community (ordered by size)", # X-axis label
     ylab="Size (number of words)", # Y-axis label
     col="blue") 

# Open a second plot window for network visualization
dev.new()
# Visualize the word network with nodes colored by their community membership
plot(wordweb, 
     vertex.label = NA, # Hide vertex labels to reduce clutter
     vertex.size = 1, # Set small vertex size for clarity
     vertex.color = membership(communities), # Color nodes based on their community
     main = "Word Network Communities") # Title of the plot


# Function to determine the type of edit between two words
get_edit_type <- function(word1, word2) {
  len_diff <- nchar(word1) - nchar(word2) # Calculate length difference between the two words
  if (len_diff == 1) {
    return("deletion")  # word1 is longer, so a letter was deleted to get word2
  } else if (len_diff == -1) {
    return("addition")  # word1 is shorter, so a letter was added to get word2
  } else if (len_diff == 0) {
    return("substitution") # Lengths are the same, so one letter was substituted
  } else {
    return(NA) # Return NA for unexpected cases
  }
}

# Extract the edge list (pairs of connected words) from the word network
edges <- as_edgelist(wordweb)

# Determine the type of edit for each edge in the network
edge_types <- sapply(1:nrow(edges), function(i) {
  get_edit_type(edges[i,1], edges[i,2])  # Apply the get_edit_type function to each edge
})

# Filter edges by their type of transformation
substitution_edges <- edges[edge_types == "substitution",] # Edges for substitutions
addition_edges <- edges[edge_types == "addition",] # Edges for additions
deletion_edges <- edges[edge_types == "deletion",] # Edges for deletions

# Create separate graphs for each type of transformation
substitution_graph <- graph_from_edgelist(matrix(substitution_edges, ncol=2), directed=FALSE)
addition_graph <- graph_from_edgelist(matrix(addition_edges, ncol=2), directed=FALSE)
deletion_graph <- graph_from_edgelist(matrix(deletion_edges, ncol=2), directed=FALSE)

# Assign vertex names to ensure nodes are properly labeled in each subgraph
V(substitution_graph)$name <- V(substitution_graph)
V(addition_graph)$name <- V(addition_graph)
V(deletion_graph)$name <- V(deletion_graph)

# Detect communities within each subgraph using the Louvain algorithm
substitution_communities <- cluster_louvain(substitution_graph) # Communities in substitution graph
addition_communities <- cluster_louvain(addition_graph) # Communities in addition graph
deletion_communities <- cluster_louvain(deletion_graph) # Communities in deletion graph

# Print statistics for each edit type
cat("\n=== EDIT TYPE COMMUNITY STATISTICS ===\n")

# Function to calculate and display statistics about communities
print_community_stats <- function(communities, type_name) {
  sizes <- sizes(communities) # Get sizes of communities
  
  # Print summary statistics for the given community type
  cat(sprintf("\n%s Communities:\n", type_name)) 
  cat("Number of Communities:", length(sizes), "\n") # Total number of communities
  cat("Mean Community Size:", round(mean(sizes), 2), "\n") # Average size of communities
  cat("Median Community Size:", median(sizes), "\n") # Median size of communities
  cat("Standard Deviation:", round(sd(sizes), 2), "\n") # Variation in community sizes
  cat("Minimum Size:", min(sizes), "\n") # Smallest community size
  cat("Maximum Size:", max(sizes), "\n") # Largest community size
  cat("Total Words:", sum(sizes), "\n") # Total number of words in the communities
  return(sizes) 
}

# Calculate and store statistics for each edit type
sub_sizes <- print_community_stats(substitution_communities, "Substitution")
add_sizes <- print_community_stats(addition_communities, "Addition")
del_sizes <- print_community_stats(deletion_communities, "Deletion")

# Visualize community sizes for each edit type
# Substitution community size plot
dev.new() # Open a new plot window
plot(sort(sub_sizes, decreasing=TRUE), type="h",
     main="Substitution Community Sizes", # Title of the plot
     xlab="Community (ordered by size)", # X-axis label
     ylab="Size (number of words)", # Y-axis label
     col="red", # Bar color
     lwd=2) # Line width
grid() # Add a grid for better readability

# Addition Communities Plot
dev.new() # Open a new plot window
plot(sort(add_sizes, decreasing=TRUE), type="h",
     main="Addition Community Sizes", 
     xlab="Community (ordered by size)", 
     ylab="Size (number of words)", 
     col="green",
     lwd=2)
grid()

# Deletion community size plot
dev.new() # Open a new plot window
plot(sort(del_sizes, decreasing=TRUE), type="h",
     main="Deletion Community Sizes", 
     xlab="Community (ordered by size)", 
     ylab="Size (number of words)", 
     col="blue",
     lwd=2)
grid()

# Visualize networks for each edit type with nodes colored by community membership
# Substitution network visualization
dev.new() # Open a new plot window
plot(substitution_graph, 
     vertex.label = NA, # Hide vertex labels to reduce clutter
     vertex.size = 1, # Set small vertex size
     vertex.color = membership(substitution_communities), # Color nodes by community
     main = "Substitution Word Network Communities", # Title of the plot
     layout = layout_with_fr(substitution_graph)) # Use a force-directed layout

# Addition network visualization
dev.new()
plot(addition_graph, 
     vertex.label = NA, 
     vertex.size = 1,
     vertex.color = membership(addition_communities),
     main = "Addition Word Network Communities",
     layout = layout_with_fr(addition_graph))

# Deletion network visualization
dev.new()
plot(deletion_graph, 
     vertex.label = NA, 
     vertex.size = 1,
     vertex.color = membership(deletion_communities),
     main = "Deletion Word Network Communities",
     layout = layout_with_fr(deletion_graph))


# Calculate clustering coefficients for full network
cat("\n=== CLUSTERING COEFFICIENT ANALYSIS ===\n")

# Calculate global clustering coefficient (overall connectedness in the network)
global_cc <- transitivity(wordweb, type="global")

# Calculate the average local clustering coefficient (average connectedness of individual nodes)
avg_local_cc <- transitivity(wordweb, type="average")

# Print summary results for clustering coefficients of the full network
cat("\nFull Network Clustering Coefficients:")
cat("\nGlobal Clustering Coefficient:", round(global_cc, 4)) # Overall measure of clustering in the network
cat("\nAverage Local Clustering Coefficient:", round(avg_local_cc, 4)) # Average clustering at individual nodes

# Calculate local clustering coefficients for each vertex
local_cc <- transitivity(wordweb, type="local") # Individual node clustering coefficients

# Print statistics about clustering coefficients
cat("\nNumber of vertices:", vcount(wordweb)) # Total number of vertices in the network
cat("\nNumber of NA clustering coefficients:", sum(is.na(local_cc))) # Number of nodes with undefined clustering coefficients
cat("\nNumber of clustering coefficients = 1:", sum(local_cc == 1, na.rm=TRUE)) # Number of fully clustered nodes

# Get the top 10 words with the highest local clustering coefficients
valid_cc <- local_cc[!is.na(local_cc)] # Exclude NA values from clustering coefficients
names(valid_cc) <- V(wordweb)$name[!is.na(local_cc)] # Assign names to the valid coefficients
top_indices <- head(order(valid_cc, decreasing=TRUE), 10) # Indices of the top 10 highest coefficients
top_clustered <- data.frame(
  word = names(valid_cc)[top_indices], # Word names
  clustering_coeff = round(valid_cc[top_indices], 4), # Clustering coefficients rounded to 4 decimal places
  degree = degree(wordweb)[names(valid_cc)[top_indices]]  # Corresponding degree centrality for context
)

# Print the top 10 most clustered words along with their degree centrality
cat("\n\nTop 10 Most Clustered Words (with degree):\n")
print(top_clustered)


# Analyze the neighborhoods of a few top clustered words
cat("\nNeighborhood Analysis of Top Clustered Words:\n")
for(i in 1:min(3, nrow(top_clustered))) { # Analyze up to the first 3 top clustered words
  word <- as.character(top_clustered$word[i]) # Current word being analyzed
  neighbors <- neighbors(wordweb, V(wordweb)[word])  # Get neighbors of the word
  cat("\nWord:", word) # Print the word
  cat("\nDegree:", length(neighbors)) # Print the number of neighbors (degree)
  cat("\nNeighbors:", V(wordweb)$name[neighbors], "\n") # Print the names of the neighbors
}

# Plot the distribution of local clustering coefficients
dev.new() # Open a new plot window
hist(local_cc[!is.na(local_cc)], 
     main="Distribution of Local Clustering Coefficients\n(Full Network)", # Title of the plot
     xlab="Clustering Coefficient",
     ylab="Frequency",
     breaks=50,  # Increased number of breaks for finer detail
     col="lightblue",
     border="white") # Remove border color for a cleaner look
grid() # Add grid lines to improve readability


# Calculate shortest and longest paths
cat("\n=== PATH ANALYSIS ===\n")

# Calculate pairwise distances between all nodes in the network
# This creates a matrix where each entry represents the shortest path distance between two nodes
distances <- distances(wordweb)

# Find the maximum finite distance, representing the longest path between two connected words
max_dist <- max(distances[is.finite(distances)]) # Exclude infinite distances (disconnected nodes)

# Find the minimum non-zero distance, representing the shortest path between distinct nodes
min_dist <- min(distances[distances > 0]) # Exclude self-loops (distance = 0)

# Count the number of direct connections (edges where path length is exactly 1)
num_direct <- sum(distances == 1)

# Count the number of self-loops (distance = 0, node to itself)
num_self <- sum(distances == 0)

# Identify example pairs for the longest path
# Get indices of nodes with the maximum path distance
longest_pair_indices <- which(distances == max_dist, arr.ind = TRUE)[1,]
longest_path_start <- V(wordweb)$name[longest_pair_indices[1]] # Starting word for the longest path
longest_path_end <- V(wordweb)$name[longest_pair_indices[2]] # Ending word for the longest path

# Retrieve the sequence of nodes forming the longest path
longest_path <- get.shortest.paths(wordweb, 
                                   from = longest_pair_indices[1], 
                                   to = longest_pair_indices[2])$vpath[[1]]

# Convert the node indices to word names for readability
longest_path_words <- V(wordweb)$name[longest_path]

# Identify example pairs for the minimum path length (1, direct connection)
min_pair_indices <- which(distances == 1, arr.ind = TRUE)[1,]
min_path_start <- V(wordweb)$name[min_pair_indices[1]] # Starting word for a direct connection
min_path_end <- V(wordweb)$name[min_pair_indices[2]] # Ending word for a direct connection

# Print general path analysis results
cat("\nPath Analysis Results:")
cat("\nTotal Network Size:", vcount(wordweb), "words") # Total number of nodes in the network
cat("\nNumber of direct connections (path length 1):", num_direct) # Number of direct edges
cat("\nMinimum Path Length (excluding self):", min_dist)  # Shortest path length between distinct nodes
cat("\nMaximum Path Length:", max_dist) # Longest path length in the connected components

# Print example of a direct connection (shortest path)
cat("\n\nShortest Path Example (length 1):")
cat("\nStart Word:", min_path_start)
cat("\nEnd Word:", min_path_end)

# Print example of the longest path
cat("\n\nLongest Path Example:")
cat("\nStart Word:", longest_path_start)
cat("\nEnd Word:", longest_path_end)
cat("\nPath:", paste(longest_path_words, collapse=" -> ")) # Display the sequence of words in the path

# Analyze finite path lengths (exclude infinite distances and self-loops)
path_lengths <- distances[is.finite(distances) & distances > 0]

# Print statistical analysis of path lengths
cat("\n\nPath Length Statistics (for connected words):")
cat("\nMean Path Length:", round(mean(path_lengths), 2)) # Average path length
cat("\nMedian Path Length:", median(path_lengths)) # Median path length
cat("\nStandard Deviation:", round(sd(path_lengths), 2)) # Variation in path lengths


# Plot a histogram of path lengths
dev.new() # Open a new plot window
hist(path_lengths,
     main="Distribution of Path Lengths in Word Network", # Title of the histogram
     xlab="Path Length", # X-axis label
     ylab="Frequency", # Y-axis label
     col="lightblue",
     border="white",
     breaks=seq(min(path_lengths)-0.5, max(path_lengths)+0.5, by=1)) # Create bins for each path length
grid() # Add grid lines for readability

# Print examples of paths with different lengths
cat("\n\nExample Paths of Different Lengths:")
for(len in unique(sort(path_lengths))[1:min(5, length(unique(path_lengths)))]) { # Limit to first 5 unique lengths
  # Find a pair of nodes with the given path length
  pair_indices <- which(distances == len, arr.ind = TRUE)[1,]
  start_word <- V(wordweb)$name[pair_indices[1]] # Starting word for the path
  end_word <- V(wordweb)$name[pair_indices[2]] # Ending word for the path
  path <- get.shortest.paths(wordweb,
                             from = pair_indices[1],
                             to = pair_indices[2])$vpath[[1]] # Retrieve the sequence of nodes in the path
  path_words <- V(wordweb)$name[path] # Convert the node indices to word names
  cat(sprintf("\nLength %d: %s -> %s", len, start_word, end_word)) # Print the path length and endpoints
  cat("\nPath:", paste(path_words, collapse=" -> ")) # Print the sequence of words in the path
}

cat("\n")

cat("\n=== LARGEST COMMUNITY DETAILED ANALYSIS ===\n")

# Identify the largest community by size
largest_comm_id <- which.max(sizes(communities))  # Get the ID of the largest community
largest_comm_vertices <- V(wordweb)[communities[[largest_comm_id]]] # Get vertices in the largest community
largest_comm <- induced_subgraph(wordweb, largest_comm_vertices) # Create a subgraph for the largest community

# --- Basic Community Information ---
cat("\n--- Basic Community Information ---")
cat("\nNumber of Words:", vcount(largest_comm)) # Total number of words (nodes) in the largest community
cat("\nNumber of Connections:", ecount(largest_comm)) # Total number of connections (edges) in the largest community

# --- Local Centrality Analysis ---
cat("\n\n--- Local Centrality Analysis ---")

# Calculate degree centrality for the largest community
local_deg <- degree(largest_comm)
cat("\nDegree Statistics in Community:")
cat("\nMean Degree:", round(mean(local_deg), 2)) # Average degree
cat("\nMedian Degree:", median(local_deg)) # Median degree
cat("\nMax Degree:", max(local_deg)) # Maximum degree
cat("\nMin Degree:", min(local_deg)) # Minimum degree

# Print the top 10 highest-degree nodes in the community
cat("\n\nTop 10 Highest Degree Words/Characters in Community:") 

top_deg <- head(sort(local_deg, decreasing=TRUE), 10) # Extract top 10 degrees
print(data.frame(
  word = names(top_deg),# Node names
  degree = top_deg # Degree centrality values
))

# Calculate betweenness centrality for the largest community
local_btw <- betweenness(largest_comm)
cat("\nBetweenness Statistics in Community:")
cat("\nMean Betweenness:", round(mean(local_btw), 2)) # Average betweenness
cat("\nMedian Betweenness:", median(local_btw)) # Median betweenness
cat("\nMax Betweenness:", round(max(local_btw), 2)) # Maximum betweenness


# --- Structural Analysis ---
cat("\n\n--- Structural Analysis ---")
# Analyze word lengths in the community
comm_words <- V(largest_comm)$name # Get the words in the largest community
word_lengths <- nchar(comm_words) # Calculate the length of each word
cat("\nWord Length Statistics:")
cat("\nMean Word Length:", round(mean(word_lengths), 2)) 
cat("\nMedian Word Length:", median(word_lengths)) 
cat("\nMode Word Length:", as.numeric(names(sort(table(word_lengths), decreasing=TRUE)[1]))) 


# --- Path and Distance Metrics ---
cat("\n\n--- Path and Distance Metrics ---")
# Diameter
comm_diameter <- diameter(largest_comm) # Calculate the diameter (longest shortest path in the subgraph)
cat("\nCommunity Diameter:", comm_diameter)

# --- Density Analysis ---
cat("\n\n--- Density Analysis ---")
comm_density <- edge_density(largest_comm) # Calculate the density (connections relative to possible connections)
cat("\nCommunity Density:", round(comm_density, 4))
cat("\n")


# === Visualizations ===
# Degree distribution histogram
dev.new() # Open a new plot window
hist(local_deg,
     main="Degree Distribution in Largest Community",  # Title of the histogram
     xlab="Degree",
     ylab="Frequency",
     col="lightblue",
     border="white")
grid()

# Word length distribution bar plot
dev.new()
barplot(table(word_lengths), # Frequency of word lengths
        main="Word Length Distribution in Largest Community",
        xlab="Word Length",
        ylab="Frequency",
        col="lightgreen")
grid()

# Visualize the structure of the largest community
dev.new()
plot(largest_comm,
     vertex.size = 3,
     vertex.label = NA,
     main = "Largest Community Structure")


