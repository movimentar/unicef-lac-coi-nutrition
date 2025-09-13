# ----- EXPORT TARGETS TO RDS (robust) -----
library(targets)
tar_config_set(store = "_targets")  # adjust if needed

# Build if empty or missing metadata
if (!dir.exists("_targets")) {
  message("No _targets store found. Running tar_make()...")
  tar_make()
}
if (nrow(tar_meta()) == 0L) {
  message("No metadata found. Running tar_make()...")
  tar_make()
}

all_names <- tar_meta(fields = name)$name
message("Found ", length(all_names), " targets in the store.")

# Desired logical objects & regex fallbacks
wish <- list(
  intervention_list  = c("^intervention_list$", "intervention.*list"),
  coi_costs          = c("^coi_costs$", "coi.*cost"),
  coi_coverages      = c("^coi_coverages$", "coi.*cover"),
  coi_dir_benefits   = c("^coi_dir_benefits$", "dir.*benefit|coi.*dir"),
  coi_indir_benefits = c("^coi_indir_benefits$", "indir.*benefit|coi.*indir"),
  gni_forecast       = c("^gni_forecast$", "gni.*forecast|forecast.*gni"),
  income_share       = c("^income_share$", "income.*share|labour.*share|labor.*share")
)

# Fuzzy matcher: first exact match; then first regex hit
match_one <- function(key, patterns, nm) {
  if (key %in% nm) return(key)
  for (pat in patterns) {
    hit <- grep(pat, nm, value = TRUE, ignore.case = TRUE)
    if (length(hit) >= 1) return(hit[1])
  }
  NA_character_
}

resolved <- vapply(names(wish), function(k) match_one(k, wish[[k]], all_names), character(1))
resolved_df <- data.frame(
  desired = names(wish),
  matched = resolved,
  stringsAsFactors = FALSE
)

message("\nResolution table:")
print(resolved_df, row.names = FALSE)

out_dir <- "coi_gpt_app/data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

exported <- character()
skipped  <- character()

for (i in seq_len(nrow(resolved_df))) {
  want <- resolved_df$desired[i]
  hit  <- resolved_df$matched[i]
  if (is.na(hit)) {
    message("Skipping (no match): ", want)
    skipped <- c(skipped, want)
    next
  }
  # Programmatic read (avoid NSE): tar_read_raw()
  obj <- try(tar_read_raw(hit), silent = TRUE)
  if (inherits(obj, "try-error")) {
    message("Failed to read target '", hit, "' for desired '", want, "'. Skipping.")
    skipped <- c(skipped, want)
    next
  }
  saveRDS(obj, file.path(out_dir, paste0(want, ".rds")))
  message("Saved: ", file.path(out_dir, paste0(want, ".rds")),
          "  (from target '", hit, "')")
  exported <- c(exported, want)
}

manifest <- list(
  exported_objects = exported,
  skipped_objects  = skipped,
  matched_targets  = resolved_df,
  timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  store_path       = normalizePath("_targets", winslash = "/")
)
saveRDS(manifest, file.path(out_dir, "_manifest.rds"))

message("\nExport complete.")
message("Exported: ", paste(exported, collapse = ", "))
if (length(skipped)) message("Skipped: ", paste(skipped, collapse = ", "))
message("RDS folder: ", normalizePath(out_dir, winslash = "/"))
