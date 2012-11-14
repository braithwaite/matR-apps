plot_pcoa <- function(
                      file_in,
                        #bps_indexed = 1,
                      figure_width = 500,
                      figure_height = 500,
                      figure_res = NA,
                      debug = FALSE,
                      PC1 = 1,
                      PC2 = 2
                      )

  {

    # read file into a list
    con <- file(file_in)
    num_lines <- length(readLines(con))
    if(debug==TRUE){print(paste("num_lines:", num_lines))}
    open(con)
    results.list <<- list();
    current.line <- 1
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
      results.list[[current.line]] <<- line
      #results.list[[current.line]] <- line #as.integer(unlist(strsplit(line, split=" ")))
      #if(debug==TRUE){print(paste(current.line, line))}
      current.line <- current.line + 1
      #results.list[current.line] <<- line
      #if(debug==TRUE){print(paste(current.line, line))}
    } 
    close(con)    

    # parse file into Eigen values and Eigen vectors

    num_eigen_values <- length(grep("PC", results.list)) # need to figure out how to do emty line regex in R ...

    eigen_value.list <<- vector("list", num_eigen_values)
    eigen_vector.list <<- vector("list", num_eigen_values)
    value.count <- 1
    vector.count <- 1

    for (i in 1:num_lines){
      my_line <<- noquote(results.list[i])
      if (any(grep("^#", my_line))==TRUE){
        # skip comment line
        
      }else{
      #if(any(my_line)==TRUE){

        #if(debug==TRUE){print(paste(i, my_line))}
        if (any(grep("PC", my_line))==TRUE){ # pull out the eigen value
          #if(debug==TRUE){print("made it here (3)")}
          eigen_value.list[value.count] <<- my_line 
          value.count <- value.count + 1
          if(debug==TRUE){print(paste("PC", my_line))}
        }else if (any(grep("[0-9]", my_line))==TRUE){ # pull out the eigen vectors
          eigen_vector.list[vector.count] <<- my_line
          vector.count <- vector.count + 1
          #if(debug==TRUE){print(paste("vector", line))}
        }else{print("encountered empty variable")}
        
      }
    }

    test_something <<- strsplit(as.character(eigen_vector.list), split="\t")

    my_names <- vector("list", num_eigen_values)
    for (i in 1:num_eigen_values){
      my_names[i] <- test_something[[i]][1]
    }

    my_data <<- matrix(ncol = num_eigen_values, nrow = num_eigen_values) 
    for (i in 1:num_eigen_values){
      for (j in 1:num_eigen_values){
        my_data[i,j] <<- test_something[[i]][j+1]
      }
    }

    #names(my_data) <<- my_names
    #set eigen values as the column names, and the samples as the vector names
    rownames(my_data) <<- my_names


    eigen_value.names <- vector("list", num_eigen_values)
    for (i in 1:num_eigen_values){
      pcName_pcEigen <- unlist(strsplit(as.character(eigen_value.list[i]), split="\t"))
      pcName <- gsub("\"", "", (pcName_pcEigen[1]))
      pcEigen <- gsub(" ", "", paste(  (round( as.numeric(pcName_pcEigen[2]), digits = 4))*100, "%"))
      eigen_value.names[i] <- paste( pcName, "::", pcEigen, "of observed variation")
    } 
    ###eigen_value.names <- gsub("\t", " ", eigen_value.list)
    #test = unlist(strsplit(as.character(eigen_value.list[1]), split="\t"))

                                        #split_eigen_values <- strsplit(as.character(eigen_value.list), split="\t")

    sorted_my_data <<- my_data[order(rownames(my_data)), ]
    colnames(sorted_my_data) <<- eigen_value.names
    #colnames(my_data) <<- eigen_value.names
    #print("bp")
    #dev.new()

    #parse.eigen_value.name(eigen_value.names[PC1])

    min_PC1 <- min(sorted_my_data[,PC1])
    max_PC1 <- max(sorted_my_data[,PC1])
    min_PC2 <- min(sorted_my_data[,PC2])
    max_PC2 <- max(sorted_my_data[,PC2])

    pdf(file = gsub(" ", "", paste(file_in, ".pdf")))
    plot(
         #min_PC1:max_PC1,
         #min_PC2:max_PC2,
         x<-sorted_my_data[,PC1],
         y<-sorted_my_data[,PC2],
         
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