# Install the required packages if not already installed
required_packages <- c("igraph", "dplyr", "ggplot2", "ReactomePA", "clusterProfiler", "org.Hs.eg.db")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the libraries
lapply(required_packages, library, character.only = TRUE)

# Loading the excel file and creating a graph
load_network_from_excel <- function(excel_file, sheet = 1) {
  edge_list <- readxl::read_excel(excel_file, sheet = sheet)
  if (!all(c("source", "target") %in% names(edge_list))) {
    stop("The Excel sheet must contain 'source' and 'target' columns.")
  }
  G <- graph_from_data_frame(edge_list, directed = FALSE)
  return(G)
}

# Centralities (degree, betweenness, closeness)
calculate_centralities <- function(G) {
  degree_centrality <- degree(G)
  betweenness_centrality <- betweenness(G)
  closeness_centrality <- closeness(G)
  
  centrality_df <- data.frame(
    Node = V(G)$name,
    Degree = degree_centrality,
    Betweenness = betweenness_centrality,
    Closeness = closeness_centrality
  )
  return(centrality_df)
}

# Identify nodes in the top 5% for each centrality
get_top_5_percent_nodes <- function(centrality_df) {
  top_5_percent_threshold <- 0.95
  degree_threshold <- quantile(centrality_df$Degree, probs = top_5_percent_threshold)
  betweenness_threshold <- quantile(centrality_df$Betweenness, probs = top_5_percent_threshold)
  closeness_threshold <- quantile(centrality_df$Closeness, probs = top_5_percent_threshold)
  
  top_5_degree <- centrality_df %>% filter(Degree >= degree_threshold)
  top_5_betweenness <- centrality_df %>% filter(Betweenness >= betweenness_threshold)
  top_5_closeness <- centrality_df %>% filter(Closeness >= closeness_threshold)
  
  common_nodes <- Reduce(intersect, list(top_5_degree$Node, top_5_betweenness$Node, top_5_closeness$Node))
  
  return(common_nodes)
}

#  enrichment analysis using Reactome
perform_enrichment_analysis <- function(common_nodes) {
  # Convert gene symbols to Entrez IDs (required for ReactomePA)
  entrez_ids <- bitr(common_nodes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = "org.Hs.eg.db")
  
  if (nrow(entrez_ids) == 0) {
    stop("No valid Entrez IDs found for the input nodes.")
  }
  
  enrich_result <- enrichPathway(gene = entrez_ids$ENTREZID, organism = "human", pvalueCutoff = 0.05, readable = TRUE)
  
  # If no pathways are enriched
  if (is.null(enrich_result) || nrow(enrich_result) == 0) {
    print("No significant pathways found.")
    return(NULL)
  }
  
  # Print enrichment results
  print(head(enrich_result))
  
  # Plot the enrichment results : bar plot
  barplot_plot <- barplot(enrich_result, showCategory = 10, title = "Top 10 Enriched Reactome Pathways")
  print(barplot_plot)  # Ensure the plot is displayed
  
  # Plot the enrichment results : dot plot
  dotplot_plot <- dotplot(enrich_result, showCategory = 10, title = "Dot Plot of Enriched Pathways")
  print(dotplot_plot)  # Ensure the plot is displayed

  return(enrich_result)
}


# Main function to run the analysis
run_analysis <- function(excel_file, sheet = 1) {
  
  G <- load_network_from_excel(excel_file, sheet)
  

  centrality_df <- calculate_centralities(G)
  
  common_nodes <- get_top_5_percent_nodes(centrality_df)
  print("Common nodes in top 5% for Degree, Betweenness, and Closeness Centralities:")
  print(common_nodes)
  
  # enrichment analysis on the common nodes
  enrich_result <- perform_enrichment_analysis(common_nodes)
  
  return(list(centralities = centrality_df, common_nodes = common_nodes, enrichment = enrich_result))
}

# Example: 

analysis_results <- run_analysis('demonetwork.xlsx', sheet = 1)
