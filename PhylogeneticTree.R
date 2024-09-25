# Phylogenetic Tree in 7 steps


library(ape)
library(Biostrings)
library(muscle)
library(ggplot2)

# Step 1: Define the species and corresponding gene accession numbers
# This informartioin can be taken form https://www.ncbi.nlm.nih.gov/nuccore/
species <- c("Homo sapiens", "Mus musculus", "Rattus norvegicus", "Danio rerio",
             "Xenopus tropicalis","Gallus gallus","Pan troglodytes")
accessions <- c("NM_013438.5", "NM_026842.4", "NM_053747.2", "XM_051833723.1",
                "NM_203890.1","XM_004949194.5","XM_009456778.4")

# Step 2: Fetch the sequences from NCBI
sequences <- lapply(accessions, function(acc) {
  seq <- readDNAStringSet(paste0("https://www.ncbi.nlm.nih.gov/sviewer/viewer.fcgi?id=", acc, "&db=nuccore&report=fasta"))
  return(seq)
})

# Step 3: Combine the sequences into one object
combined_sequences <- do.call(c, sequences)

# Step 4: Perform multiple sequence alignment using MUSCLE
aligned_sequences <- muscle(combined_sequences) 

# Step 5: Create a distance matrix for nucleotide sequences
distance_matrix <- dist.dna(as.DNAbin(aligned_sequences))  # Convert aligned sequences to DNAbin for distance calculation

# Step 6: Construct a phylogenetic tree using the distance matrix
phylo_tree <- nj(distance_matrix)

# Step 7:Visualization using ggtree
library(ggtree)
p <- ggtree(phylo_tree) +
  geom_tiplab(size = 4, align = TRUE, linetype="dashed") +  # Label size and alignment
  geom_nodepoint(color="blue", size=3) +  # Node points
  theme_tree2() +  # Use a cleaner theme
  ggtitle("Phylogenetic Tree of KRAS Gene") +  # Title
  theme(plot.title = element_text(hjust = 0.5, size = 14))  # Center title

print(p)
