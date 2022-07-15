# lp.mincost.maxflow.r


#
# @lp.mincost.maxflow: data.frame, integer, integer, integer or list -> list
#   function outputs an edge list with flows.
#
# @edges: data.frame, should have from, to, and capacity columns for each edge in the network. from and to columns should contain unique node ids as integers from 0 to N.
# @source.id: integer, unique node id
# @dest.id: integer, unique node id
# @flow.demand: integer or list, if integer create a demand signal to push that much from source.id to dest.id, if a list expectation is that it sums to 0 and will contain each nodes supply (if positive) and demand (if negative).


lp.mincost.maxflow <- function(edges, source.id, dest.id, flow.demand){
  if(!("from" %in% names(edges)) || 
     !("to" %in% names(edges)) ||
     !("upper_bound" %in% names(edges)) ||
     !("lower_bound" %in% names(edges))){
    print("Need from, to, and capacity columns.")
  }
  else{
    
    # Infer the nodes
    nodes <- data.frame(id = sort(unique(c(unique(edges$from), unique(edges$to)))))
    
    # Infer the demand to flow through the network
    if(length(flow.demand)>1){
      if(sum(flow.demand) == 0){
        demand <- flow.demand
      }else{
        print("Flow demand doesn't add up to 0.")
      }
    }
    else{
      demand <- c(flow.demand, rep(0, nrow(nodes)-2), -flow.demand)
    }
    
    # Get arc capacity constraints
    create_arc_capacity_constraints <- function(edges){
      
      # For upper
      Amat <- matrix(0, nrow = nrow(edges), ncol = nrow(edges))
      Amat_dir <- rep("<=", nrow(Amat))
      Amat_rhs <- c()
      
      for(i in 1:ncol(Amat)){
        Amat[i,i] <- 1
        Amat_rhs <- c(Amat_rhs, edges$upper_bound[i])
      }
      
      # For lower
      Bmat <- matrix(0, nrow = nrow(edges), ncol = nrow(edges))
      Bmat_dir <- rep(">=", nrow(Bmat))
      Bmat_rhs <- c()
      
      for(i in 1:ncol(Bmat)){
        Bmat[i,i] <- 1
        Bmat_rhs <- c(Bmat_rhs, edges$lower_bound[i])
      }
      
      list(Amat = rbind(Amat, Bmat),
           Amat_dir = c(Amat_dir, Bmat_dir),
           Amat_rhs = c(Amat_rhs, Bmat_rhs))
    }
    
    results <- create_arc_capacity_constraints(edges)
    Amat <- results$Amat
    Amat_dir <- results$Amat_dir
    Amat_rhs <- results$Amat_rhs
    
    # Create node flow constraints (in = out)
    create_node_constraints <- function(nodes, edges){
      Bmat <- matrix(0, nrow = nrow(nodes), ncol = nrow(edges))
      Bmat_dir <- rep("==", nrow(Bmat))
      Bmat_rhs <- rep(0, nrow(Bmat))
      
      for(i in 1:nrow(Bmat)){
        node_id <- nodes[i, "id"]
        for(j in 1:ncol(Bmat)){
          edge_from <- edges[j,"from"]
          edge_to <- edges[j, "to"]
          
          if(node_id == edge_from){
            # print(paste("Outbound for ", node_id, "From: ",edge_from, "to: ", edge_to))
            Bmat[i,j] = 1
          }
          else if(node_id == edge_to){
            # print(paste("Inbound for ", node_id, "From: ",edge_from, "to: ", edge_to))
            Bmat[i,j] = -1
          }
          else{
            Bmat[i,j] = 0
          }
        }
        Bmat_rhs[i] <- demand[i]
      }
      
      list(Bmat = Bmat,
           Bmat_dir = Bmat_dir,
           Bmat_rhs = Bmat_rhs
      )
    }
    
    results <- create_node_constraints(nodes, edges)
    Bmat <- results$Bmat
    Bmat_dir <- results$Bmat_dir
    Bmat_rhs <- results$Bmat_rhs
    
    # Bring together
    f.obj <- edges$cost
    f.cons <- rbind(Amat, Bmat)
    f.rhs <- c(Amat_rhs, Bmat_rhs)
    f.dir <- c(Amat_dir, Bmat_dir)
    results <- lp(direction = "min",  
                  objective.in = f.obj, 
                  const.mat = f.cons, 
                  const.dir = f.dir, 
                  const.rhs = f.rhs)
    
    edges$flow <- results$solution
  }
  
  list(edges = edges, results = results)
}

# edges <- data.frame(from = c(1, 1, 2, 2, 5, 4, 4, 3, 3), 
#                     to = c(2, 3, 5, 4, 6, 5, 6, 5, 4),
#                     lower_bound = 0,
#                     upper_bound = c(800, 600, 100, 600, 600, 600, 400, 400, 300),
#                     cost = c(10, 50, 70, 30, 30, 30, 60, 60, 10))
# 
# lp.mincost.maxflow(edges = edges, source.id = 1, dest.id = 6, flow.demand = 900)
