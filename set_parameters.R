# Please install the MultiplexPCRAnalyser package v1.0.0 to use with this file. This can be found here: https://github.com/buchauer-lab/Q4ddPCR_Analysis
# This file is used to set plate and sample specific parameters for the analysis of Q4ddPCR

library(MultiplexPCRAnalyser)

# set file paths
csv_file <- "/CSV-file.csv" # file path to the .csv file that you exported by clicking 'Export Cluster Data'
csv_skip <- 4 # number of rows before the table starts
xlsx_file <- "/Excel-file.xlsx" # file path to the .xlsx file that you exported under 'Data table'
output_file <- "/Q4ddPCRoutput_example.xlsx" # the directory and name of the result file you would like to receive

# define which dye was used in which channel
ch_dye <- c("Ch1" = "FAM",
            "Ch2" = "VIC",
            "Ch3" = "Cy5",
            "Ch6" = "ATTO590")

# define which Target column (in the csv file) corresponds to which channel
target_channel <- c("Target 1" = "Ch1",
                    "Target 2" = "Ch2",
                    "Target 3" = "Ch3",
                    "Target 4" = "Ch6")

# add the dilution factor if you used a lower DNA concentration for th RPP30-wells than for the HIV-reaction wells
custom_dilution_factor <- TRUE

dilution_factor <- c("Sample 1" = 100,
                     "Sample 2" = 100,
                     "Sample 3" = 100,
                     "Sample 4" = 100, 
                     "Sample 5" = 100, 
                     "Sample 6" = 100) 

# add the wells you want to exclude from your analysis, add all the wells with positive, negative and no template controls
remove_channel <- c("A07","B07","C07","D07","E07","F07","G07","H07") 

# remove wells that have concentration 0 for at least one channel
rm_zero_channel_wells <- FALSE 

# define the used target genes
compute_all_positives_for <- c("Psi", "Env", "Gag", "Pol")
# this computes the possible combinations of target genes (doublets, triplets)
multi_positives <- get_multipos(compute_all_positives_for)

# reflects number of RPP30 copies per cell.
tar_mio_factor <- c("Sample 1" = 2,
                    "Sample 2" = 2,
                    "Sample 3" = 2,
                    "Sample 4" = 2, 
                    "Sample 5" = 2, 
                    "Sample 6" = 2)  

# minimum number of accepted droplets to continue with well
threshold <- 7500 

# volume per well before droplet generation
mean_copies_factor <- 20 

# number of replicates divided by 2 to determe number of cell equivalents
mean_cells_per_reac_factor <- c("Sample 1" = 2,
                                "Sample 2" = 1,
                                "Sample 3" = 2,
                                "Sample 4" = 2, 
                                "Sample 5" = 2, 
                                "Sample 6" = 2) 

# names used in shear controls and water controls
shear_name <- c("RPP30", "RPP30Shear")
water_name <- c("H20")
