library(MultiplexPCRAnalyser)

# Load parameters
source("/path/set_parameters.R") # set the path to the set_parameters.R file


information <- read_files(
  xlsx_file,
  csv_file,
  csv_skip,
  remove_channel,
  rm_zero_channel_wells
)

# Read input files
in_csv <- information[[1]]
dtQC   <- information[[2]]

# Quality control and preprocessing
dtQC <- sufficient_droplets(dtQC, threshold)
dtQC <- add_dilution_factor(dtQC, dilution_factor)
if (!all(unique(dtQC$`Sample description 1`) %in% names(tar_mio_factor))) {
  warning("Set tar_mio_factor to 1")
  s_desc <- unique(dtQC$`Sample description 1`)
  tar_mio_factor <- setNames(rep(1, length(s_desc)), s_desc)
}

# Split wells and tables
shear_wells <- unique(dtQC[dtQC$Target %in% shear_name, "Well"])
water_wells <- unique(dtQC[dtQC$Target %in% water_name, "Well"])
data_wells  <- setdiff(unique(dtQC$Well), union(shear_wells, water_wells))

group_ids <- get_group_id(dtQC)
dtQC$group_id <- group_ids[dtQC$Well]
shear_table <- dtQC[dtQC$Well %in% shear_wells, ]
water_table <- dtQC[dtQC$Well %in% water_wells, ]
water_csv   <- in_csv[in_csv$Well %in% water_wells, ]

data_table  <- dtQC[dtQC$Well %in% data_wells, ]
data_csv    <- in_csv[in_csv$Well %in% data_wells, ]

data_table <- data_table[
  , grep("Ch", colnames(data_table), value = TRUE, invert = TRUE)
]

# Shearing factor computation
shear_table <- compute_shearing_factor(
  shear_table,
  mean_copies_factor,
  mean_cells_per_reac_factor
)

# Main table computations
conf_mat <- create_confusion_matrix(
  data_csv,
  data_table,
  ch_dye,
  target_channel
)
tab <- merge_tables(data_table, conf_mat, shear_table)
tab <- compute_target_means(tab)

# Multi-positive analysis
for (multip in multi_positives) {
  tab <- get_multi_pos(tab, multip, tar_mio_factor)
}

# Total HIV quantification
total_HIV_dict <- setNames(
  unlist(lapply(unique(tab$group_id), function(x) {
    compute_total_HIV(tab[tab$group_id == x, ])
  })),
  unique(tab$group_id)
)

tab[["total HIV DNA/Mio cells"]] <-
  total_HIV_dict[as.character(tab$group_id)]

total_HIV_dict_envpsi <- setNames(
  unlist(lapply(unique(tab$group_id), function(x) {
    compute_total_HIV_envPsi(tab[tab$group_id == x, ])
  })),
  unique(tab$group_id)
)

tab[["total HIV DNA/Mio cells (Env.Psi)"]] <-
  total_HIV_dict_envpsi[as.character(tab$group_id)]

# Water control analysis
if (length(water_wells) > 0) {
  h2o_conf_mat <- create_confusion_matrix(
    water_csv,
    water_table,
    ch_dye,
    target_channel
  )
  
  h2o_tab <- merge_tables(water_table, h2o_conf_mat, shear_table)
  h2o_tab <- h2o_tab[, 1:(ncol(h2o_tab) - 2)]
} else {
  h2o_tab <- NULL
}

# Output generation
out_tables <- lapply(unique(tab$group_id), function(x) {
  tab[
    tab$group_id == x,
    grep("group_id", names(tab), value = TRUE, invert = TRUE)
  ]
})

write_output_file(
  out_tables,
  conf_mat,
  output_file,
  h2o_tab,
  multi_positives,
  shear_table
)
