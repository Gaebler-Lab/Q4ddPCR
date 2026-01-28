# ======= 0. define variables ==========
library(MultiplexPCRAnalyser)
getwd()
source("/path/set_parameters.R") # put the path to the set_parameters.R file here


# ======= 1. read files ==========
# that should be mostly untouched I think (for now, keep as before)
# read files
information <- read_files(xlsx_file, csv_file, csv_skip, remove_channel, rm_zero_channel_wells)
in_csv <- information[[1]]
dtQC <- information[[2]]

# do some quality controls like sufficient droplets here already!
# add stuff like dilution factor here already

# QC checks
## check for sufficient droplets
dtQC <- sufficient_droplets(dtQC, threshold)
dtQC <- add_dilution_factor(dtQC, dilution_factor)

# revise this, but keep at this position
if(!all(unique(dtQC$`Sample description 1`) %in% names(tar_mio_factor))){
  warning("Set tar_mio to 1")
  # TODO: do
}
# ======= 2. split tables ==========
# if possible, split H2O, RPP, and experiment data into different tables
# this should be based on dtQC$Target
shear_wells <- unique(dtQC[dtQC$Target %in% shear_name,"Well"])
water_wells <- unique(dtQC[dtQC$Target %in% water_name ,"Well"])
data_wells <- setdiff(unique(dtQC$Well), union(shear_wells, water_wells))

# add group ids to well based on `Sample description 1` and combination of targets
group_ids <- get_group_id(dtQC)
dtQC$group_id <- group_ids[dtQC$Well]

# split tables
shear_table <- dtQC[dtQC$Well %in% shear_wells,]
water_table <- dtQC[dtQC$Well %in% water_wells,]
water_csv <- in_csv[in_csv$Well %in% water_wells,]
data_table <- dtQC[dtQC$Well %in% data_wells,]
data_csv <- in_csv[in_csv$Well %in% data_wells,]

# remove Ch columns from data table
data_table <- data_table[,grep("Ch", colnames(data_table), value=T, invert = T)]
# ======= 3. do shearing computations ==========
# should translate quickly
shear_table <- compute_shearing_factor(shear_table, mean_copies_factor,
                                       mean_cells_per_reac_factor)

# ======= 4. do table computations ==========
# this changes a looooooot
# much of the for loop can be done without splitting the data and then later by 
# grouping along the added group_ID column


# create one (!) confusion matrix
conf_mat <- create_confusion_matrix(data_csv, data_table, ch_dye, target_channel) # add arguments

# combine tables
tab <- merge_tables(data_table, conf_mat, shear_table)

# compute means per target
tab <- compute_target_means(tab)


# get multiple positives
## keep function but adapt grouping to account for group_id

combinations <- get_multipos(c("Psi", "Env", "Pol", "Gag")) # can be a parameter or extracted from data somwhow

for (multip in combinations) {
  tab <- get_multi_pos(tab, multip, tar_mio_factor)
}

# compute total HIV
total_HIV_dict <- setNames(unlist(lapply(unique(tab$group_id), function(x){
  compute_total_HIV(tab[tab$group_id == x,])
})), unique(tab$group_id))
tab[["total HIV DNA/Mio cells"]] <- total_HIV_dict[as.character(tab$group_id)]


# compute envPsi total HIV
total_HIV_dict_envpsi <- setNames(unlist(lapply(unique(tab$group_id), function(x){
  compute_total_HIV_envPsi(tab[tab$group_id == x,])
})), unique(tab$group_id))

tab[["total HIV DNA/Mio cells (Env.Psi)"]] <- total_HIV_dict_envpsi[as.character(tab$group_id)]


### continue here
####################

# also get information for H2O tables if necessary
if(length(water_wells) > 0){
  # create one (!) confusion matrix for the water table
  h2o_conf_mat <- create_confusion_matrix(water_csv, water_table, ch_dye, target_channel) # add arguments
  
  # combine water tables
  h2o_tab <- merge_tables(water_table, h2o_conf_mat, shear_table)
  h2o_tab <- h2o_tab[,1:(ncol(h2o_tab)-2)]
} else {
  h2o_tab <- NULL
}
# ======= 5. output to xlsx file ==========
# this might need some adaptations
out_tables <- lapply(unique(tab$group_id), function(x){tab[tab$group_id == x,grep("group_id", names(tab), value=T, invert=T)]})
write_output_file(out_tables, conf_mat, output_file, h2o_tab, multi_positives, shear_table)
