# Kevin P. Keegan - all rights reserved - 11-12-12
# Function to remove "single counts -- or any count that is <= abundance_limit
remove_singletons <- function(
                              my_matrix, abundance_limit = 1, debug=FALSE
                              )
{

  if ( nargs() == 0 ){
     writeLines("
     You supplied no arguments

     DESCRIPTION: (remove_singletons):
     This is a script to remove singleton counts (or any counts that fail to meet a specified threshold)
     from a data matrix.  It assumes that columns are samples, rows are observations (taxons or functions).
     Counts that fail to meet the abundance limit (set by default to 1 to remove singletons) are replaced with
     zeros.  If an entire row is populated by nothing but zeros, it is 

     USAGE:
     my_matrix.without_singletons <- remove_singletons(my_matrix, abundance_limit = 1, debug=FALSE)

     EXAMPLE:
     my_matrix.without_singletons <- remove_singletons(my_matrix, abundance_limit = 1, debug=FALSE)
     
     )
     "
                )
   }
  
  dim_matrix <- dim(my_matrix)
  num_row <- dim_matrix[1]
  num_col <- dim_matrix[2]
  new_matrix <<- matrix(0,num_row,num_col)
  dimnames(new_matrix)[1] <<- dimnames(my_matrix)[1]
  dimnames(new_matrix)[2] <<- dimnames(my_matrix)[2]
  row_sums <<- matrix(0, num_row, 1)
  zero_row_count <<- 0

  # first replace all the NA's and values < abundance_limit to 0
  my_matrix[ is.na(my_matrix) ]<-0
  
  for (i in 1:num_row){
    for (j in 1:num_col){
      if (my_matrix[i,j] > abundance_limit){
        new_matrix[i,j] <<- my_matrix[i,j] 
      }
    }
    row_sums[i,1] <<- sum(new_matrix[i,])
    if (debug == TRUE){print(paste("row:", i, "sum:", row_sums[i,1]))}
    if (row_sums[i,1]==0){
      zero_row_count <<- zero_row_count + 1
    }
  }

  new_matrix.screened <<- matrix(0, (num_row - zero_row_count), num_col)
  dimnames(new_matrix.screened)[[1]] <<- c(1:(num_row - zero_row_count))
  dimnames(new_matrix.screened)[[2]] <<- dimnames(my_matrix)[[2]]

  # now build a new matrix that tosses any rows entirely populated with zeros
  screen.row_count = 1
  for (i in 1:num_row){
    if (row_sums[i,1] > abundance_limit){
      for (j in 1:num_col){
        new_matrix.screened[screen.row_count, j] <<- new_matrix[i,j]
        if(debug==TRUE){print(paste("i: ",i))}
        if(debug==TRUE){print(paste("screen.row_count: ",screen.row_count))}
        if(debug==TRUE){print(paste("j: ",j))}
        dimnames(new_matrix.screened)[[1]][screen.row_count] <<- dimnames(new_matrix)[[1]][i]
      }
      screen.row_count = screen.row_count + 1
    }
  }

  return(new_matrix.screened)
  
}
