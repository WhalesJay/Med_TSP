#This is just a quick script to create the table for displaying all results

full_projections = as.data.frame(matrix(nrow = (38), ncol = (31)))
colnames(full_projections[2:31]) = rep(colnames(row_number)) 
colnames(full_projections[1]) = "Projection"
