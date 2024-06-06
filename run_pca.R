tar_load(c("coverage_costs", "emergency_list"))

# prepare data for PCA
coverages_pca_data <- coverage_costs %>% 
  ungroup() %>% 
  filter(!(coverage == 0 | is.na(coverage))) %>% # remove 0 coverage and NAs
  left_join(emergency_list, by = c("emergency", "country")) %>% 
  select(intervention_id, coverage, emergency_id) %>% 
  pivot_wider(names_from = emergency_id,
              values_from = coverage) %>%
  arrange(intervention_id) %>% 
  column_to_rownames("intervention_id")

coverages_PCA_data[is.na(coverages_PCA_data)] <- 0

# PCA 
coverage_pca <- PCA(coverages_pca_data,
                        scale.unit = FALSE)

get_eigenvalue(coverage_pca) %>%
  as_tibble()

get_pca_var(coverage_pca)$contrib %>%
  as.data.frame() %>%
  rownames_to_column(var = "intervention_id") %>% 
  make_dt()

fviz_screeplot(coverage_pca)

fviz_pca_ind(coverage_pca)

#PCA clustering
intervention_hcpc <-
  HCPC(
    coverage_pca,
    nb.clust = 4
  )
