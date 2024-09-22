# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("homologene", quietly = TRUE)) {
  install.packages("homologene")
}
library(tidyverse)
library(homologene)

# Function to retrieve orthologous genes between human and mouse
get_human_mouse_orthologs <- function() {
  #  human (9606) and mouse (10090)
  all_human_mouse_orthologs <- homologeneData2 %>%
    filter(Taxonomy %in% c(9606, 10090)) 
  
  human_genes <- all_human_mouse_orthologs %>%
    filter(Taxonomy == 9606) %>%
    select(HID, Gene.ID, Gene.Symbol) %>%
    rename_with(~paste0(., "_human"), -HID)
  
  mouse_genes <- all_human_mouse_orthologs %>%
    filter(Taxonomy == 10090) %>%
    select(HID, Gene.ID, Gene.Symbol) %>%
    rename_with(~paste0(., "_mouse"), -HID)
  
  # Merge 
  human_mouse_pairs <- merge(human_genes, mouse_genes, by = "HID")
  
  return(human_mouse_pairs)
}

human_mouse_orthologs <- get_human_mouse_orthologs()

# View the first few rows of the results
head(human_mouse_orthologs)

# write_csv(human_mouse_orthologs, "human_mouse_orthologs.csv")
