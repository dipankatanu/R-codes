# Gene Symbol to Entrez ID Conversion Script
# Author: Dipanka Tanu
# Description: This script converts a list of gene symbols to Entrez IDs
#using biomaRt and org.Hs.eg.db.

#____________________Method 1
# Load necessary libraries
if (!requireNamespace("biomaRt", quietly = TRUE)) {
  install.packages("biomaRt")
}
if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
  install.packages("org.Hs.eg.db")
}
library(biomaRt)
library(org.Hs.eg.db)

# Function to convert gene symbols to Entrez IDs
convert_gene_symbols_to_entrez <- function(gene_symbols) {
  # Create a connection to the Ensembl Biomart database
  ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  
  # Perform the conversion
  converted <- getBM(attributes = c("hgnc_symbol", "entrezgene_id"),
                     filters = "hgnc_symbol",
                     values = gene_symbols,
                     mart = ensembl)
  
  # Merge with input to ensure all input genes are represented
  result <- merge(data.frame(hgnc_symbol = gene_symbols), converted, by = "hgnc_symbol", all.x = TRUE)
  
  # Return the result
  return(result)
}

# Example usage
gene_symbols <- c("TP53", "EGFR", "KRAS", "BRCA1")  # Replace this with your list of gene symbols

# Convert gene symbols to Entrez IDs
conversion_result <- convert_gene_symbols_to_entrez(gene_symbols)

# Display the result
print(conversion_result)

# Save the result to a CSV file
write.csv(conversion_result, "gene_symbol_to_entrez_conversion.csv", row.names = FALSE)



#____________________Method 2

# Gene Symbol to Entrez ID Conversion Script using AnnotationDbi
# Author: [Your Name]
# Description: This script converts a list of gene symbols to Entrez IDs using org.Hs.eg.db.

# Load necessary libraries
if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
  install.packages("AnnotationDbi")
}
if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
  install.packages("org.Hs.eg.db")
}
library(AnnotationDbi)
library(org.Hs.eg.db)

# Function to convert gene symbols to Entrez IDs
convert_gene_symbols_to_entrez <- function(gene_symbols) {
  # Use the mapIds function to convert gene symbols to Entrez IDs
  entrez_ids <- mapIds(org.Hs.eg.db,
                       keys = gene_symbols,
                       column = "ENTREZID",
                       keytype = "SYMBOL",
                       multiVals = "first")  # Returns only the first match if there are multiple
  
  # Create a data frame of results
  result <- data.frame(GeneSymbol = gene_symbols, EntrezID = entrez_ids, stringsAsFactors = FALSE)
  
  # Return the result
  return(result)
}

# Example usage
gene_symbols <- c("TP53", "EGFR", "KRAS", "BRCA1")  # Replace with your list of gene symbols


entrez_ids <- mapIds(org.Hs.eg.db,
                     keys = gene_symbols,
                     column = "ENTREZID",
                     keytype = "SYMBOL",
                     multiVals = "first")  # Retu


# Convert gene symbols to Entrez IDs
conversion_result <- convert_gene_symbols_to_entrez(gene_symbols)

# Display the result
print(conversion_result)

# Save the result to a CSV file
write.csv(conversion_result, "gene_symbol_to_entrez_conversion.csv", row.names = FALSE)

# Example usage
gene_symbols <- c("TP53", "EGFR", "KRAS", "BRCA1")  # Replace with your list of gene symbols

# Convert gene symbols to Entrez IDs
conversion_result <- convert_gene_symbols_to_entrez(gene_symbols)

# Display the result
print(conversion_result)

# Save the result to a CSV file
write.csv(conversion_result, "gene_symbol_to_entrez_conversion.csv", row.names = FALSE)

# End of script

