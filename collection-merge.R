setMethod ("+", signature ("collection", "collection"), 
          function (e1, e2) {
#             mm <- merge (e1$count, e2$count, all = TRUE, by = 0, sort = TRUE)
#             rownames (mm) <- mm$Row.names
#             mm$Row.names <- NULL
#             as.matrix (mm)
            
            if (!identical (viewnames (e1), viewnames(e2)))
              warning ("only views with names in common will be retained")

            cc <- new("collection")
            cc@sel <- new ("selection",
                           ids = append (e1@sel@ids, e2@sel@ids),
                           groups = as.factor (append (groups (e1), groups (e2))))
            ### need to fix metadata     metadata = ...,
            #                            ids.spec = ...,
            #                            resources.spec = ...,
            #                            metadata.extent = ...)
            names (cc) <- append (names (e1), names (e2))
            v <- intersect (viewnames (e1), viewnames (e2))
            v1 <- views (e1) [v]
            v2 <- views (e2) [v]
            if (!identical (v1, v2))
              warning ("possibly merging views with same name that are not same")
            cc@views <- v1
            cc@data <- list()
            for (j in v) {
              m <- merge (e1@data [[j]], e2@data [[j]], all = TRUE, by = 0, sort = TRUE)
              rownames (m) <- m$Row.names
              m$Row.names <- NULL
		m <- as.matrix (m)
		m [is.na(m)] <- 0
              cc@data [[j]] <- new ("mmatrix", data = Matrix::Matrix(m))
            }
            viewnames (cc) <- v
            cc
          } )
