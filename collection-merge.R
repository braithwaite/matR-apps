setMethod("+", signature("collection", "collection"), 
          function (e1, e2) {
            mm <- merge (e1, e2, all = TRUE, by = 0, sort = TRUE)
            rownames (mm) <- mm$Row.names
            mm$Row.names <- NULL
            as.matrix (mm)

#             if (!identical (viewnames (e1), viewnames(e2)))
#               warning ("only views in common will be retained")
#             
#             cc <- new("collection")
#             cc@sel <- new ("selection",
#                            ids = append (e1@sel@ids, e2@sel@ids)
### must check for default grouping
#                            groups = as.factor (append (groups (e1), groups (e2)))
#                            metadata = ...,
#                            ids.spec = ...,
#                            resources.spec = ...,
#                            metadata.extent = ...)
#             names (cc) <- append (names (e1), names (e2))
#             cc@views <- intersect (views (e1), views (e2))
#             viewnames (cc) <- intersect (viewnames (e1), viewnames (e2))
# 
#             for (v in viewnames (cc)) {
#               cc@data [[v]]  <- merge (e1@data [[v]], e2@data [[v]], all = TRUE, by = 0, sort = TRUE)
#               rownames (cc@data [[v]]) <- cc@data [[v]] $Row.names
#               cc@data [[v]] $Row.names <- NULL
#               cc@data [[v]] <- new ("mmatrix", data = cc@data [[v]])
#           }
#           cc
} )
