# Kevin P. Keegan - all rights reserved - 11-12-12
# Simple function to dump data for populated metadata fields
# requires metadata for a collection, and an mgrastid from that collection
# collection_metadata <- metadata(collection)
metadata_dump <- function(
                          collection_metadata, mgrast_id
                          )
{
    my_metadata <- collection_metadata[[mgrast_id]]
    num_fields <- dim(as.matrix(my_metadata))[1]
    my_metadata_dump <- vector(length=num_fields)
    for (i in 1:num_fields){    
      my_metadata_dump[names(my_metadata[i])] = my_metadata[i]
    }
    return(my_metadata_dump)
  }    
