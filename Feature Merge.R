# This code has been written by Julien Saint-Vanne @jsaintvanne
# Pour Ting, création du mgf en ajoutant les valeurs souhaitées
library(openxlsx)

liste <- read.xlsx("~/LABERCA/TESTS/Ting_mgf_creating/second_round/Ting  STDs.xlsx")
prev_mgf <- readLines("~/LABERCA/TESTS/Ting_mgf_creating/second_round/Ting STDs feature list-20240305.mgf")
outputfile <- file("~/LABERCA/TESTS/Ting_mgf_creating/second_round/results.mgf", open = "wt")

rttol = 1
mgf_ids <- NULL
no_match_table <- NULL
too_much_table <- NULL
tour = 0
for(i in grep("SCANS",prev_mgf)){
  tour = tour + 1
  this_scan <- as.numeric(strsplit(prev_mgf[i], "=")[[1]][2])
  matched_scan_lines <- liste[which(liste$SCANS == as.numeric(this_scan)),]
  this_compound_name <- strsplit(prev_mgf[i+2], "=")[[1]][2]
  matched_scan_compound_lines <- matched_scan_lines[which(matched_scan_lines$COMPOUND_NAME == this_compound_name),]
  if(nrow(matched_scan_compound_lines) == 0) {
    cat("This mass and rt couple has no matched ! Save it in no match table\n")
    no_match_table <- rbind(no_match_table, c(scan = this_scan, compound_name = this_compound_name))
    next
  }else if(nrow(matched_scan_compound_lines) > 1){
    cat("This mass and rt couple has more than 1 match ! Save it into too much table\n")
    too_much_table <- rbind(too_much_table, c(scan = this_scan, compound_name = this_compound_name))
    next
  }
  if(is.null(matched_scan_compound_lines$INCHIAUX)) matched_scan_compound_lines$INCHIAUX <- "N/A"
  writeLines(sprintf("BEGIN IONS\nPEPMASS=%s\nRTINSECONDS=%s\nCOMPOUND_NAME=%s\nSCANS=%s\nCHARGE=%s\nMSLEVEL=%s\nSOURCE_INSTRUMENT=%s\nSEQ=%s\nIONMODE=%s\nPI=%s\nDATACOLLECTOR=%s\nSMILES=%s\nINCHI=%s\nINCHIAUX=%s\nPUBMED=%s\nLIBRARYQUALITY=%s", 
                     matched_scan_compound_lines$PEPMASS, matched_scan_compound_lines$RTINSECONDS, this_compound_name,this_scan,
                     strsplit(prev_mgf[i+3], "=")[[1]][2],as.numeric(strsplit(prev_mgf[i+4], "=")[[1]][2]),
                     matched_scan_compound_lines$INSTRUMENT,matched_scan_compound_lines$SEQ,matched_scan_compound_lines$IONMODE, 
                     matched_scan_compound_lines$PI, matched_scan_compound_lines$DATACOLLECTOR, matched_scan_compound_lines$SMILES, 
                     matched_scan_compound_lines$INCHI, matched_scan_compound_lines$INCHIAUX, matched_scan_compound_lines$PUBMED, 
                     matched_scan_compound_lines$LIBQUALITY), outputfile)
  for(d in (i+6):grep("END IONS", prev_mgf)[tour]){
    writeLines(prev_mgf[d], outputfile)
  } 
  writeLines("\n", outputfile)
}
close(outputfile)

write.csv(no_match_table, "~/LABERCA/TESTS/Ting_mgf_creating/second_round/no_match_values.csv")
write.csv(too_much_table, "~/LABERCA/TESTS/Ting_mgf_creating/second_round/too_much_values.csv")

