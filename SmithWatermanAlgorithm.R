# Smith-Waterman Algorithm for local alignment in R
#Smith–Waterman algorithm aligns two sequences by matches/mismatches
#(also known as substitutions), insertions, and deletions.

smith_waterman <- function(seq1, seq2, match = 2, mismatch = -1, gap = -2) {
  # Initialize score matrix
  n <- nchar(seq1) + 1
  m <- nchar(seq2) + 1
  score_matrix <- matrix(0, n, m)
  traceback_matrix <- matrix("", n, m)
  
  # Set maximum score
  max_score <- 0
  max_pos <- c(0, 0)
  
  # Fill in the score matrix and traceback matrix
  for (i in 2:n) {
    for (j in 2:m) {
      char1 <- substr(seq1, i - 1, i - 1)
      char2 <- substr(seq2, j - 1, j - 1)
      
      # Scoring for match or mismatch
      if (char1 == char2) {
        score_diag <- score_matrix[i - 1, j - 1] + match
      } else {
        score_diag <- score_matrix[i - 1, j - 1] + mismatch
      }
      
      # Scoring for gaps
      score_up <- score_matrix[i - 1, j] + gap
      score_left <- score_matrix[i, j - 1] + gap
      
      # Choose the maximum score
      max_local_score <- max(0, score_diag, score_up, score_left)
      score_matrix[i, j] <- max_local_score
      
   
      if (max_local_score == score_diag) {
        traceback_matrix[i, j] <- "D"
      } else if (max_local_score == score_up) {
        traceback_matrix[i, j] <- "U"
      } else if (max_local_score == score_left) {
        traceback_matrix[i, j] <- "L"
      }
      
      
      if (max_local_score > max_score) {
        max_score <- max_local_score
        max_pos <- c(i, j)
      }
    }
  }
  
  # Traceback to get the aligned sequences
  aligned_seq1 <- ""
  aligned_seq2 <- ""
  i <- max_pos[1]
  j <- max_pos[2]
  
  while (score_matrix[i, j] > 0) {
    if (traceback_matrix[i, j] == "D") {
      aligned_seq1 <- paste0(substr(seq1, i - 1, i - 1), aligned_seq1)
      aligned_seq2 <- paste0(substr(seq2, j - 1, j - 1), aligned_seq2)
      i <- i - 1
      j <- j - 1
    } else if (traceback_matrix[i, j] == "U") {
      aligned_seq1 <- paste0(substr(seq1, i - 1, i - 1), aligned_seq1)
      aligned_seq2 <- paste0("-", aligned_seq2)
      i <- i - 1
    } else if (traceback_matrix[i, j] == "L") {
      aligned_seq1 <- paste0("-", aligned_seq1)
      aligned_seq2 <- paste0(substr(seq2, j - 1, j - 1), aligned_seq2)
      j <- j - 1
    }
  }
  
  return(list(aligned_seq1 = aligned_seq1, aligned_seq2 = aligned_seq2, score_matrix = score_matrix))
}

## Example
seq1 <- "AGTACGCA"
seq2 <- "TATGC"

result <- smith_waterman(seq1, seq2)

# Print
cat("Aligned Sequence 1: ", result$aligned_seq1, "\n")
cat("Aligned Sequence 2: ", result$aligned_seq2, "\n")
print(result$score_matrix)
