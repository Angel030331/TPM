# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


TPM <- function(count_data, col_start, col_end, col_gene_length) {
  count_matrix <- count_data[, col_start:col_end]
  count_matrix <- count_matrix / count_data[, col_gene_length]
  df_colSums <- as.data.frame(colSums(count_data[, col_start:col_end]))

  TPM_count <- t(count_matrix) * 1000000 * 1000 / df_colSums[,1]
  TPM_count <- t(TPM_count)

  return(TPM_count)
}

threstest <- function(TPM_count, threshold) {
  TPM_count_threshold <- TPM_count[rowSums(TPM_count) > threshold, ]
  TPM_count_threshold <- as.data.frame(TPM_count_threshold)
  TPM_count_threshold$SYMBOL <- rownames(TPM_count_threshold)
  TPM_count_threshold <- TPM_count_threshold[,
                                             c(ncol(TPM_count_threshold),
                                               1:ncol(TPM_count_threshold)-1)]
  file_name <- paste0("TPM_count_", threshold, ".txt")
  write.table(TPM_count_threshold, file = file_name, sep = "\t", quote = FALSE,
              na = '', row.names = FALSE)
}
