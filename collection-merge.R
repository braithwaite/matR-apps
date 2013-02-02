setMethod ("+", signature ("collection", "collection"), 
          function (e1, e2) {
#             mm <- merge (e1$count, e2$count, all = TRUE, by = 0, sort = TRUE)
#             rownames (mm) <- mm$Row.names
#             mm$Row.names <- NULL
#             as.matrix (mm)

            cc <- new("collection")
            mm <- character (0)
            class (mm) <- "metadata"
            cc@sel <- new ("selection",
            							 ids = append (e1@sel@ids, e2@sel@ids),
            							 groups = as.factor (append (groups (e1), groups (e2))),
            							 metadata = mm)
            
            if (length (metadata (e1)) != 0 && length (metadata (e2)) != 0) 
            	warning ("dropping metadata")

            n <- sum (is.null (names (e1)), is.null (names (e2)))
            if (n == 1) warning ("not both collections are named; dropping names")
            else if (n == 2) names (cc) <- append (names (e1), names (e2))

            v <- intersect (viewnames (e1), viewnames (e2))
            if (!identical (viewnames (e1), viewnames (e2)))
          		warning ("only views with names in common will be retained: ", paste (v, collapse = ", "))

            cc@views <- list()
            for (j in v) {
            	view <- matR:::view.of.matrix (e1@views [[j]])
            	if (!identical (view, matR:::view.of.matrix (e2@views [[j]]))) {
            		warning ("not-same views named same; dropping: ", j)
            		next
            	}

            	m <- merge (e1@views [[j]], e2@views [[j]], all = TRUE, by = 0, sort = TRUE)
              rownames (m) <- m$Row.names
              m$Row.names <- NULL
            	m <- as.matrix (m)
            	attributes (m) [names (view)] <- view

            	cc@views [[j]] <- m
            }
            cc
          } )
