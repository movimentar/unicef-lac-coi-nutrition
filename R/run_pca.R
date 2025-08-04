# Function to perform PCA and hierarchical clustering on coverage costs data
# from coverage_costs and emergency_list, runs PCA and hierarchical clustering
# analyses. Outputs a list with a PCA Biplot, eigenvalues table, variable
# contributions, screeplot, PCA clustered plot, and dendogram.
run_pca <- function(coverage_costs, emergency_list) {
  # Prepare data for PCA
  coverages_pca_data <- coverage_costs %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(coverage == 0 | is.na(coverage))) %>% # Remove 0 coverage and NAs
    dplyr::left_join(emergency_list, by = c("emergency", "country")) %>%
    dplyr::select(intervention_id, coverage, emergency_id) %>%
    tidyr::pivot_wider(names_from = emergency_id, values_from = coverage) %>%
    dplyr::arrange(intervention_id) %>%
    tibble::column_to_rownames("intervention_id")
  
  coverages_pca_data[is.na(coverages_pca_data)] <- 0 # Replace NAs with 0s
  
  # Create PCA object
  coverage_pca <- FactoMineR::PCA(coverages_pca_data, scale.unit = FALSE, graph = FALSE)
  
  # Get eigenvalue table
  eigenvalues <- factoextra::get_eigenvalue(coverage_pca) %>%
    tibble::as_tibble()
  
  # Get variable contribution table
  var_contributions <- factoextra::get_pca_var(coverage_pca)$contrib %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "intervention_id")
  
  # Get screeplot
  screeplot <- factoextra::fviz_screeplot(coverage_pca) + 
    guides(scale = "none")
  
  # Get PCA clustered and dendogram object
  intervention_hcpc <- FactoMineR::HCPC(coverage_pca, nb.clust = 3, graph = FALSE)
  
  # Suppress specific ggplot2 warning
  suppressWarnings({
    # Return PCA results
    pca_results <- list(
      pca_biplot = factoextra::fviz_pca_biplot(coverage_pca) + 
        guides(scale = "none"),
      eigenvalues = eigenvalues,
      var_contributions = var_contributions,
      screeplot = screeplot,
      clusters = factoextra::fviz_cluster(intervention_hcpc) + 
        guides(scale = "none"),
      dendogram = factoextra::fviz_dend(intervention_hcpc) + 
        guides(scale = "none")
    )
  })
  
  return(pca_results)
}
