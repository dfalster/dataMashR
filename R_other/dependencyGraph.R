


plotFoodWeb <- function(filenames=NULL, prune=".onAttach", plotit=TRUE){  
  
  
  summary.foodweb <- function(x,...){
    
    
    l <- apply(x$funmat, 1, function(f)names(f[f==1]))
    emp <- sapply(l, length)
    theseemp <- which(emp==0)
    l2 <- l[setdiff(1:length(l), theseemp)]
    
    return(l2)
  }
  
  if(is.null(filenames))
    filenames <- list.files(pattern="[.][rR]$", path="./R", full.names=TRUE)
  
  for(x in filenames)source(x)

  f <- foodweb(funs=as.character(lsf.str("package:dataMashR")), plotting=FALSE)
  fs <- summary(f)
  
  # prune functions:
  if(length(prune) > 0 && all(is.character(prune)))
    fs[[prune]] <- NULL

  # make graph of function dependencies
  # (mostly written by :
  # Francois Romain 'Rcpp reverse dependency graph' (http://romainfrancois.blog.free.fr)
  graph <- character(0)
  for(i in 1:length(fs)){
    for(j in 1:length(fs[[i]])){
      fun <- names(fs)[i]
      dep <- fs[[i]][j]
      
      # It seems graphViz does not like special characters here...
      # Is there a workaround?
      fun <- gsub("\\.", "", fun)
      dep <- gsub("\\.", "", dep)
      
      graph <- c(graph, sprintf("%s->%s",fun,dep)) 
    }
  }
  
  # Make .dot file (input for graphViz)
  if(plotit){
    
    output <- file("dep.dot", open = "w" )
    writeLines( "digraph G {", output )
    writeLines( "   rankdir=LR;", output )
    writeLines( sprintf( "%s ; ", graph), output )
    writeLines( "}", output )
    close(output)
    shell("dot -Tpng dep.dot > dep.png")
  } 
  
  return(invisible(fs))
  
}

