# lp.maxflow.r

lp.maxflow <- function(edges, source.id, dest.id){
  if(!("from" %in% names(edges)) || !("to" %in% names(edges)) || !("capacity" %in% names(edges))){
    print("Need from, to, and capacity columns.")
  }
  else{
    
    # Infer the nodes
    nodes <- data.frame(id = sort(unique(c(unique(edges$from), unique(edges$to)))))
    
    # Connect source.id to dest.id
    BIG_M = max(edges$capacity)*10
    edges <- rbind(edges, data.frame(from = c(dest.id),
                                     to = c(source.id), 
                                     capacity = c(BIG_M)))
    
    
    
    # Build up edge constraints 
    Amat <- matrix(0, nrow = nrow(edges), ncol = nrow(edges))
    Amat_dir <- rep("<=", nrow(Amat))
    Amat_rhs <- c()
    
    for(i in 1:ncol(Amat)){
      Amat[i,i] <- 1
      Amat_rhs <- c(Amat_rhs, edges$capacity[i])
    }
    
    # Build up node constraints
    Bmat <- matrix(0, nrow = nrow(nodes), ncol = nrow(edges))
    Bmat_dir <- rep("==", nrow(Bmat))
    Bmat_rhs <- rep(0, nrow(Bmat))
    
    for(i in 1:nrow(Bmat)){
      node_id <- nodes[i, "id"]
      for(j in 1:ncol(Bmat)){
        edge_from <- edges[j,"from"]
        edge_to <- edges[j, "to"]
        edge_id <- edges[j, "id"]
        
        if(node_id == edge_from){
          Bmat[i,j] = -1
        }
        else if(node_id == edge_to){
          Bmat[i,j] = 1
        }
        else{
          Bmat[i,j] = 0
        }
      }
    }
    
    # Join all model parameters
    f.obj <- c(rep(0, nrow(edges)-1), 1)
    f.cons <- rbind(Amat, Bmat)
    f.rhs <- c(Amat_rhs, Bmat_rhs)
    f.dir <- c(Amat_dir, Bmat_dir)
    
    results <- lp(direction = "max",  
                  objective.in = f.obj, 
                  const.mat = f.cons, 
                  const.dir = f.dir, 
                  const.rhs = f.rhs)
    edges$flow <- results$solution
  }
  
  list(edges=edges, flow = results$solution, maxflow = results$objval, results = results)
}


# edges <- data.frame(from = c(0,0,1,1,3,2), to = c(1,2,2,3,4,4), capacity = c(2,3,3,4,1,2))
# lp.maxflow(edges, source.id = 0, dest.id = 4)
