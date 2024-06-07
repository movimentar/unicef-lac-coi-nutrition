# from coverage_costs and emergency_list, runs PCA and hierarchical clustering
# analyses. Outputs a list with a PCA Biplot, egeinvalues table, variable
# contributions, screeplot, PCA clustered plot and dendogram.
run_pca <- function(coverage_costs, emergency_list) {
  # prepare data for PCA
  coverages_pca_data <- coverage_costs %>%
    ungroup() %>%
    filter(!(coverage == 0 |
               is.na(coverage))) %>% # remove 0 coverage and NAs
    left_join(emergency_list, by = c("emergency", "country")) %>%
    select(intervention_id, coverage, emergency_id) %>%
    pivot_wider(names_from = emergency_id,
                values_from = coverage) %>%
    arrange(intervention_id) %>%
    column_to_rownames("intervention_id")
  
  coverages_pca_data[is.na(coverages_pca_data)] <- 0 # replace NAs by 0s
  
  # create PCA object
  coverage_pca <- PCA(coverages_pca_data,
                      scale.unit = FALSE)
  
  # get eigenvalue table
  eigenvalues <- get_eigenvalue(coverage_pca) %>%
    as_tibble()
  
  # get variable contribution table
  var_contributions <- get_pca_var(coverage_pca)$contrib %>%
    as.data.frame() %>%
    rownames_to_column(var = "intervention_id")
  
  # get screeplot
  screeplot <- fviz_screeplot(coverage_pca)
  
  #get PCA clustered and dendogram object
  intervention_hcpc <-
    HCPC(coverage_pca,
         nb.clust = 4)
  
  # return PCA results
  pca_results <- list(
    pca_biplot = fviz_pca_biplot(coverage_pca),
    eigenvalues = eigenvalues,
    var_contributions = var_contributions,
    screeplot = screeplot,
    clusters = fviz_cluster(intervention_hcpc),
    dendogram = fviz_dend(intervention_hcpc)
  )
  
  return(pca_results)
}


