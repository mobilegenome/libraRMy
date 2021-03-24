#' Calculate average expression across a Seurat object for a set of gene symbols.
#' Returns a tibble.
#' importFrom magrittr "%>%"
#'
calcAvgGenesetExpressionZScore.Seurat <- function(object, geneset, scorename="gene.set.score") {
  genes_selected <- intersect(rownames(seu),  geneset)

  selected_assay <- Seurat::GetAssayData(object, slot = "scale.data")[which(rownames(object) %in% genes_selected), ]
  avg_values <- selected_assay %>% as_tibble() %>% colMeans()

  #
  # # store expression means
  # # according to https://github.com/satijalab/seurat/issues/528
  # if (all(names(x = avg_values) == rownames(x = object@meta.data))) {
  #   cat("Cell names order match in 'mean.exp' and 'object@meta.data':\n",
  #       "adding gene set mean expression values in 'object@meta.data$gene.set.score'")
  #   object@meta.data[scorename] <- avg_values
  # }
  avg.exp.df <- dplyr::tibble(cellID = names(avg_values))
  avg.exp.df[[scorename]] = unlist(avg_values)

  umap_avgExpr_df <- Seurat::Embeddings(object, "umap") %>% as_tibble(rownames = "cellID")
  umap_avgExpr_df <- dplyr::left_join(umap_avgExpr_df, avg.exp.df)
  umap_avgExpr_df[[paste0(scorename, "_Zscore")]] <- scale(umap_avgExpr_df[[scorename]])

  return(umap_avgExpr_df)
}


#' Calculate the z-score for a geneset per cell in a Seurat object.
#' Return a tibble with columns 'cellID', 'geneSymbol', Zscore' and 'genset'.
#' importFrom magrittr "%>%"

calcZscore.Seurat <- function(object, geneset, geneset_label, celllabel_prefix = "HNSCC") {
  genes_selected <- intersect(rownames(object),  geneset)

  selected_assay <- Seurat::GetAssayData(object, slot = "scale.data")[which(rownames(object) %in% genes_selected), ]

  zscores <-
    scale(t(selected_assay)) %>%
    t() %>%
    dplyr::as_tibble(rownames = "geneSymbol") %>%
    dplyr::pivot_longer(cols = dplyr::matches(celllabel_prefix), names_to = "cellID", values_to = "Zscore") %>%
    dplyr::mutate(cellID = reorder(cellID, Zscore)) %>%
    dplyr::mutate(geneset = geneset_label) %>%
    dplyr::group_by(cellID, geneSymbol) %>%
    dplyr::arrange(Zscore) %>%
    dplyr::ungroup()

  return(zscores)

}
