# bit hard to provide a simple conversion without definitions of the class 'Node', the template 'DirectedGraph' and the function 'Writed'!
#
# note that R arrays start at index 1.
#
# by the way:
# - your curly braces don't match,
# - not all elements of P are initialised before they are used.

#------------------------------------
# original code (cleaned to make comparison easier).
#------------------------------------
#Random r = new Random();
#DirectedGraph<SimpleNode> graph = GetGraph();
#decimal B = 0.1m;
#decimal D = 0.05m;
#int nodes = graph.NodesCount;
#decimal[] E = new decimal[nodes];
#decimal[] P = new decimal[nodes];
#
#for (int i = 7; i <= 10; ++i) P[i] = (decimal)r.NextDouble();
#
#for (int t = 0; t < 100; ++t){
#    Writed(P, "P");
#
#    foreach (SimpleNode n in graph.Nodes) {
#        int id = graph.index[n];
#
#        decimal product = 1;
#        foreach (var item in graph.GetAdjacentNodes(n)){
#            int j = graph.index[item];
#            product *= (1 - B * P[j]);
#        }
#
#        E[id] = product;
#    }
#
#    foreach (SimpleNode n in graph.Nodes){
#        int i = graph.index[n];
#        P[i] = 1 - ((1 - P[i]) * E[i] + D * (1 - P[i]) * E[i] + 0.5m * D * P[i] * (1 - E[i]));
#        if (P[i] < 0) P[i] = 0;
#    }
#}
#
#}


#------------------------------------
# drop-in for your method getGraph (produces a 10 'random' node directed graph)
#------------------------------------
library(igraph) # to install, type "install.packages('igraph')"
GetGraph <- function() graph.adjacency(matrix(sample(0:1, size=100, replace=T), nrow=10))
grph.t <- GetGraph()
P.t <- runif(nodes) # assume you meant to initialise all elements of P

#------------------------------------
# A 'mirror' implementation. Some of the code relies
# on the specifics of package igraph, but I've tried to
# be as similar as possible. Hope it still makes sense!
#
# You'll obviously have to provide your own implementation
# of "GetGraph".
#------------------------------------
B <- 0.1
D <- 0.05
grph <- grph.t
nodes <- vcount(grph)
E <- numeric(nodes)
P <- P.t

for(t in 0:99){
    cat('P:', P, '\n')# is this equivalent to 'Writed(P, "P")' ???
    graph.Nodes <- get.adjlist(grph) # returns a list of vectors, where each vector is the nodes a node is connected to.
    id <- 0 # we loop over the vectors and so must index separately
    for(n in graph.Nodes){ # n is a vector containing the nodes node at index id+1 are connected to.
        id <- id+1
        product <- 1;
        for(item in n){
            product <- product * (1 - B * P[item+1]); # verticies (nodes) are indexed from 0. no operator*= in R.
        }
        E[id] <- product;
    }
        
    at <- 0
    for(i in 1:nodes){
        P[i] <- 1 - ((1 - P[i]) * E[i] + D * (1 - P[i]) * E[i] + 0.5 * D * P[i] * (1 - E[i])); # we are accessing nodes in order so the indexes are also ordered.
        if (P[i] < 0) P[i] <- 0;
    }
}

P # print the result

#------------------------------------
# a more 'R-ish' implementation.
#------------------------------------
B <- 0.1 # I don't know what the postfix 'm' means
D <- 0.05 # I don't know what the postfix 'm' means
P <- P.t
grph <- grph.t

for(t in 0:99){
    E <- sapply(get.adjlist(grph), function(node) prod(1-B*P[node+1]))
    P <- 1 - ((1 - P) * E + D * (1 - P) * E + 0.5 * D * P * (1 - E))
}

P # print the result
