# Q4ddPCR
This is a repository for a) an example on how to use the R-package for Q4ddPCR analysis (available in [https://github.com/buchauer-lab/Q4ddPCR_Analysis] and b) the code for log-linear mixed effect model to describe the decay of HIV reservoir size measured by Q4ddPCR and IPDA.

# Instructions on how to use the Run File for Q4ddPCR analysis
1) After gating in QX Manager Software, you have to export the cluster data as a csv-file under ‘Export Cluster Data’. Then go to 'Data table' and export the data table of individual wells as an Excel file for the analysis. It has to be an .xlsx file. Tested with QX Manager Software, Standard Edition v2.0. 
2) Download set_parameters.R and Q4ddPCR_Workflow.R. Fill the set_parameters.R file with plate and sample specific parameters: path to csv-file, Excel-file and output file, dilution factors for RPP30-reaction, wells to remove, threshold for accepted droplets, number of replicates per sample performed
4) Install R-package for Q4ddPCR analysis (available in https://github.com/buchauer-lab/Q4ddPCR_Analysis). Use v1.0.0
5) Run the code in the set_parameters.R, then run the code in Q4ddPCR_Workflow.R
6) The resulting output Excel-file will have quality control markers (shearing index, number of cell equivaleents) and every combination of Q4ddPCR targets as concentration (copies/µL), counts/E6 cells and shear corrected counts/E6 cells. It also provides total HIV DNA/E6 cells calculated using all 4 targets or only env-Psi. This information will be provided as summary per sample (Sheet 1) and for every single well (one sheet per sample). THe last sheet diplays the RPP30 reaction.
7) For further processing results and applying the decision tree, use the Processing_Q4ddPCR.R file. Use the output from the Q4ddPCR_Workflow.R as input for the Processing_Q4ddPCR.R. This script applies the decision tree and calculates the intact fraction as well as defective proviruses. If desired, it also calculates IPDA results. The script also displays warning if one of the targets is detected below 10 counts/E6 cells, if shearing is high or a low number of cell equivalents is measured by RPP30. Individual settings per participants are possible.

If you are using one of the codes please cite doi: 10.1101/2025.07.28.667202.
