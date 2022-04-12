draw_tree <- function(t)
{
    require(ggtree)
    require(ape)
    t <- ape::root(t, outgroup="Orussusa", edgelabel=TRUE)
    t$node.label[1]<-""
    ggtree(t) + geom_tiplab(size=3) + geom_nodelab(size=3,hjust=0)
}
