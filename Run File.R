library(MultiplexPCRAnalyser)
setwd("/directory/") #set the directory
getwd()

# set arguments
csv_file <- "/directory/CSV-File.csv" #add the directory of the .csv file that you exported by clicking 'Export Cluster Data'
csv_skip <- 4 # number of rows before the table starts
xlsx_file <- "/directory/Excel-file.xlsx" #add the directory of the .xlsx file that you exported under 'Data table'
output_file <- "/directory/Output_NAME.xlsx" #add the directory and name of the result file you would like to receive'

ch_dye <- c("Ch1" = "FAM",
            "Ch2" = "VIC",
            "Ch3" = "Cy5",
            "Ch5" = "ROX",
            "Ch6" = "ATTO590")

custom_dilution_factor <- TRUE


dilution_factor <- c("Sample 1" = 100,
                     "Sample 2" = 100,
                     "Sample 3" = 100,
                     "Sample 4" = 100, 
                     "Sample 5" = 100, 
                     "Sample 6" = 100) #add the dilution factor if you used a lower DNA concentration for th RPP30-wells than for the HIV-reaction wells


remove_channel <- c("A07","B07","C07","D07","E07","F07","G07","H07") #add the wells you want to exclude from your analysis, add all the wells with positive, negative and no templte controls

rm_zero_channel_wells <- FALSE # remove wells that have concentration 0 for at least one channel
# will not remove H2O channels

compute_all_positives_for <- c("Psi", "Env", "Gag", "Pol")

multi_positives <- get_multipos(compute_all_positives_for)

tar_mio_factor <- c("Sample 1" = 2,
                    "Sample 2" = 2,
                    "Sample 3" = 2,
                    "Sample 4" = 2, 
                    "Sample 5" = 2, 
                    "Sample 6" = 2) #reflects number of RPP30 copies per cell. 


threshold <- 7500 #minimum number of accepted droplets to continue with well
mean_copies_factor <- 20 #volume per well before droplet generation
mean_cells_per_reac_factor <- c("Sample 1" = 2,
                                "Sample 2" = 1,
                                "Sample 3" = 2,
                                "Sample 4" = 2, 
                                "Sample 5" = 2, 
                                "Sample 6" = 2) #number of replicates divided by 2 to determe number of cell equivalents

# ================== execute functions =========================================
# read files
information <- read_files(xlsx_file, csv_file, csv_skip, remove_channel, rm_zero_channel_wells)
in_csv <- information[[1]]
dtQC <- information[[2]]

# group data according to sample description and targets
grouped_data <- define_groups(dtQC, dilution_factor)

# create standard table and values
standards <- create_household_table(dtQC, grouped_data, threshold, mean_copies_factor, mean_cells_per_reac_factor)
tab1 <- standards[[1]]
grouped_data <- standards[[2]]

# TODO: adapt below
# create output tables
output <- create_tables(grouped_data, in_csv, dtQC, ch_dye, multi_positives, threshold, tar_mio_factor, tab1)
output_tables <- output[[1]]
conf_mats <- output[[2]]
h2o_tables <- output[[3]]

# write to xlsx file
write_output_file(output_tables, conf_mats, tab1, output_file, h2o_tables, multi_positives) 











 

