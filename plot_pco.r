plot_pco <- function(
                     file_in,
                     input_dir = "./",
                     output_PCoA_dir = "./",
                     print_dist = 1,
                     output_DIST_dir = "./",
                     dist_method = "euclidean",
                     headers = 1,
                     PC1 = "PC1",
                     PC2 = "PC2",
                     )
  
{
  # load packages
  suppressPackageStartupMessages(library(matlab))      
  suppressPackageStartupMessages(library(ecodist))
  #suppressPackageStartupMessages(library(Cairo))
  #suppressPackageStartupMessages(library(gplots))

  # define sub functions
  func_usage <- function() {
    writeLines("
     You supplied no arguments

     DESCRIPTION: (MGRAST_plot_pco.r):
     This script will perform a PCoA analysis on the inputdata
     using the selected distance metric.  Output always produces a
     *.PCoA file that has the normalized eigenvalues (top n lines)
     and eigenvectors (bottom n x m matris, n lines) where n is the
     number of variables (e.g.subsystems), and m the number of
     samples. You can also choose to produce *.DIST files that contain
     the distance matrix used to generate the PCoA.

     USAGE: MGRAST_plot_pca(
                            file_in = no default arg                               # (string)  input data file            
                            input_dir = \"./\"                                       # (string)  directory(path) of input
                            output_PCoA_dir = \"./\"                                 # (string)  directory(path) for output PCoA file
                            print_dist = 0                                         # (boolean) print the DIST file (distance matrix)
                            output_DIST_dir = \"./\"                                 # (string)  directory(path) for output DIST file 
                            dist_method = \"bray-curtis\"                            # (string)  distance/dissimilarity metric,
                                          (choose from one of the following options)
                                          \"euclidean\" | \"maximum\"     | \"canberra\"    |
                                          \"binary\"    | \"minkowski\"   | \"bray-curtis\" |
                                          \"jacccard\"  | \"mahalanobis\" | \"sorensen\"    |
                                          \"difference\"| \"manhattan\"
                            headers = 0                                            # (booealan) print headers in output PCoA file 
                            )\n"
               )
    stop("MGRAST_plot_pco stopped\n\n")
  }
  
  find_dist <- function(my_data, dist_method)
    {
      switch(dist_method,
             "euclidean" = dist(my_data, method = "euclidean"), 
             "maximum" = dist(my_data, method = "maximum"),
             "manhattan" = dist(my_data, method = "manhattan"),
             "canberra" = dist(my_data, method = "canberra"),
             "binary" = dist(my_data, method = "binary"),
             "minkowski" = dist(my_data, method = "minkowski"),
             
             #"bray-curtis" = distance(my_data, method = "bray-curtis"), # could not handle large data 1-12-12
             
             "bray-curtis" = bcdist(my_data), # 1-12-12
             #"bray-curtis" = vegdist(my_data, method="bray"), # 1-12-12
             #"bray-curtis" = designdist(my_data, method = "(A+B-2*J)/(A+B)") # 1-12-12
             
             "jaccard" = distance(my_data, method = "jaccard"),
             "mahalanobis" = distance(my_data, method = "mahalanobis"),
             "sorensen" = distance(my_data, method = "sorensen"),
             "difference" = distance(my_data, method = "difference")
             # unifrac
             # weighted_unifrac

             # distance methods with {stats}dist: dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
             #      euclidean maximum manhattan canberra binary minkowski

             # distance methods with {ecodist}distance: distance(x, method = "euclidean")
             #      euclidean bray-curtis manhattan mahalanobis jaccard "simple difference" sorensen

             )
    }


  # stop and give the usage if the proper number of arguments is not given
  if ( nargs() == 0 ){
    func_usage()
  }

  # load data

  #writeLines("FILE-IN")
  #writeLines(file_in)
  input_data_path = gsub(" ", "", paste(input_dir, file_in))
  #writeLines("INPUT-DATA-PATH")
  #writeLines(input_data_path)
  #my_data <<- flipud(rot90(data.matrix(read.table(input_data_path, row.names=1, header=TRUE, sep="\t", comment.char="", quote=""))))
  my_data <- file_in

  
  num_data_rows = dim(my_data)[1] # substitute 0 for NA's if they exist in the data
  num_data_cols = dim(my_data)[2]
  for (row_num in (1:num_data_rows)){
    for (col_num in (1:num_data_cols)){
      #my_data[row_num, col_num] = as.integer(my_data[row_num, col_num]) # added 1-12-12 to fix "Error in vector("double", length) : vector size cannot be NA ...
      if (is.na(my_data[row_num, col_num])){
        my_data[row_num, col_num] <<- 0
      }
    }
  }


   
  # calculate distance matrix
  dist_matrix <<- find_dist(my_data, dist_method)
  DIST_file_out <- gsub(" ", "", paste(output_DIST_dir, file_in, ".", dist_method, ".DIST"))
  if (print_dist > 0) { write_file(file_name = DIST_file_out, data = data.matrix(dist_matrix)) }

  # perform the pco
  my_pco <<- ecodist::pco(dist_matrix)

  # scale eigen values from 0 to 1, and label them
  eigen_values <<- my_pco$values
  scaled_eigen_values <<- (eigen_values/sum(eigen_values))
  for (i in (1:dim(as.matrix(scaled_eigen_values))[1])) {names(scaled_eigen_values)[i]<<-gsub(" ", "", paste("PCO", i))}
  scaled_eigen_values <<- data.matrix(scaled_eigen_values)
  #for (i in (1:dim(as.matrix(scaled_ev))[1])) dimnames(scaled_ev)[i]<<-gsub(" ", "", paste("PCO", i))

  # label the eigen vectors
  eigen_vectors <<- data.matrix(my_pco$vectors) 
  dimnames(eigen_vectors)[[1]] <<- dimnames(my_data)[[1]]

  # write eigen values and then eigen vectors to file_out
  PCoA_file_out = gsub(" ", "", paste(output_PCoA_dir, file_in, ".", dist_method, ".PCoA"))

  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("# file_in    :", file_in,
            "\n# dist_method:", dist_method,
            "\n#________________________________",
            "\n# EIGEN VALUES (scaled 0 to 1) >",
            "\n#________________________________"),
          append=FALSE)
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t")
  }else{
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = FALSE, sep="\t")
  }
  
  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("#________________________________",
            "\n# EIGEN VECTORS >",
            "\n#________________________________"),
          append=TRUE)
  }

  write.table(eigen_vectors, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t")

  pdf(file = gsub(" ", "", paste(file_in, ".", dist_method,".PCoA.pdf")))
    plot(
         #min_PC1:max_PC1,
         #min_PC2:max_PC2,
         x<-my_pco[,PC1],
         y<-my_pco[,PC2],
         
         type="n",
         #points(sorted_my_data[,PC1],sorted_my_data[,PC2], pch=23, col="blue", bg = "blue", cex=4),
         #main = paste(file_in,"\n", "PC", PC1, "vs PC", PC2 ),
         xlab = eigen_value.names[PC1],
         #parse.eigen_value.name(eigen_value.names[PC1]),
         ylab = eigen_value.names[PC2],
         cex = 0.8
         )
    my_cex <- 1
    #points(x=sorted_my_data[,PC1],y=sorted_my_data[,PC2], pch=23, col="blue", bg = "blue", cex=2)
    points(x=((sorted_my_data[,PC1])[1:5]), y=((sorted_my_data[,PC2])[1:5]), pch=23, col="green", bg = "green", cex=my_cex) #C
    points(x=((sorted_my_data[,PC1])[6:10]), y=((sorted_my_data[,PC2])[6:10]), pch=21, col="purple", bg = "purple", cex=my_cex) #ch1
    points(x=((sorted_my_data[,PC1])[11:15]), y=((sorted_my_data[,PC2])[11:15]), pch=22, col="purple", bg = "purple", cex=my_cex) #ch2
    points(x=((sorted_my_data[,PC1])[16:20]), y=((sorted_my_data[,PC2])[16:20]), pch=23, col="purple", bg = "purple", cex=my_cex) #ch3
    points(x=((sorted_my_data[,PC1])[21:25]), y=((sorted_my_data[,PC2])[21:25]), pch=24, col="purple", bg = "purple", cex=my_cex) #ch4
    points(x=((sorted_my_data[,PC1])[26:30]), y=((sorted_my_data[,PC2])[26:30]), pch=25, col="purple", bg = "purple", cex=my_cex) #ch5
    points(x=((sorted_my_data[,PC1])[31:35]), y=((sorted_my_data[,PC2])[31:35]), pch=21, col="blue", bg = "blue", cex=my_cex) #E1
    points(x=((sorted_my_data[,PC1])[36:40]), y=((sorted_my_data[,PC2])[36:40]), pch="X", col="orange", bg = "orange", cex=0.7*my_cex, lwd=3) #R
    points(x=((sorted_my_data[,PC1])[41:45]), y=((sorted_my_data[,PC2])[41:45]), pch="+", col="brown", bg = "brown", cex=my_cex, lwd=3) #W
    
    title( (paste(file_in,"\n", "PC", PC1, "vs PC", PC2 )), cex.main = 0.8)
        
    dev.off()





  
}


write_file <- function(file_name, data) {
  write.table(data, file=file_name, col.names=NA, row.names=TRUE, append = FALSE, sep="\t", quote = FALSE)
}

