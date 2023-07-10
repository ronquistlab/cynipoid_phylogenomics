library(ape)
library(cowplot)
library(tibble)
library(tidyr)
library(tidytree)
library(treeio)
library(ggplot2)
library(ggtree)

draw_tree <- function(tree, hexp, cyn_mono=TRUE, leg=TRUE, treescale_height=8.0)
{
    require(ggtree)
    require(ape)

    # Set tip labels and desired display names
    source("names.R")

    # Desired order of tips
    taxonOrder <- c(36,37,35,34,32,33,31,8,1,2,3,4,5,6,7,26,27,28,29,30,19,20,21,22,23,24,25,18,17,16,15,14,13,12,11,10,9)

    # Root the trees correctly
    if ("Orussusa" %in% tree$tip.label)
        tree <- ape::root(tree, outgroup="Orussusa", edgelabel=TRUE)
    else
        tree <- ape::root(tree, outgroup="Cecin_ib", edgelabel=TRUE)
    tree$node.label[1]<-""

    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    cynipidae_core <- MRCA(tb, "Andr_gro", "Hedic_le")$node
    figitidae <- MRCA(tb,"Parn_nig", "Lepto_he")$node
    diplolepidae <- MRCA(tb, "Dipl_spi", "Pedia_ac")$node
    diplo <- tb$node[match("Dipl_spi",tb$label)]
    pedia <- tb$node[match("Pedia_ac",tb$label)]

    # Add the lineage info to the tree (using the "group" attribute)
    clades <- c(Phytophagous=cynipidae_core, Parasitoid=figitidae)
    tree <- groupClade(tree, clades)
    x <- as.character(attr(tree,"group"))
    if ("Escha_ac" %in% tb$label) {
        escha <- tb$node[match("Escha_ac",tb$label)]
        x[escha] <- "Phytophagous"
        if ("Orussusa" %in% tb$label) {
            cynipidae <- MRCA(tb, "Andr_gro", "Escha_ac")$node
            x[cynipidae] <- "Phytophagous"
        }
    }
    x[diplolepidae] = "Phytophagous"
    x[diplo] = "Phytophagous"
    x[pedia] = "Phytophagous"
    attr(tree,"group") <- as.factor(x)

    # Set the colors
    cols <- c(Phytophagous="green4", Parasitoid="black")

    # Change display names and only show support < 100%
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]
    for ( i in 1:length(tree$node.label) ) {
        if ( tree$node.label[i] == "100" )
            tree$node.label[i] <- ""
    }

    if (leg==TRUE) {
        leg.pos <- c(.2,.78)
    }
    else {
        leg.pos <- "none"
    }

    ggtree(tree, aes(color = group), ladderize = TRUE) +
        geom_tree(size=0.6, key_glyph="rect") +
        hexpand(hexp) +
        geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 1.5) +
        geom_nodelab(size=1.2,hjust=0) +
        geom_treescale(x = 0.0, y = treescale_height, width = 0.05, fontsize=1.5) +
        theme_tree(legend.position = leg.pos, legend.key.size = unit(0.04,'cm'), legend.title = element_text(size=9), legend.text = element_text(size=7)) +
        scale_color_manual(values = c("green3", "black", "black"), na.value = "black", name = "Larval feeding mode",
                           breaks = c("Phytophagous", "Parasitoid"),
                           guide = guide_legend(override.aes = list(size = 3)))
}

t1 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_C60_I_G5.contree")
t2 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_no_cecin_C60_I_G5.contree")
t3 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_no_escha_C60_I_G5.contree")
t4 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_no_outgr_C60_I_G5.contree")
t5 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_no_cecin_escha_C60_I_G5.contree")
t6 <- read.tree("../iqtree/iq_clustal34_ag_lt0.26_no_outgr_escha_C60_I_G5.contree")

p1 <- draw_tree(t1, 0.22, TRUE, TRUE, 8.0)
p2 <- draw_tree(t2, 0.22, FALSE, FALSE)
p3 <- draw_tree(t3, 0.22, FALSE, FALSE)
p4 <- draw_tree(t4, 0.22, FALSE, FALSE, 30.0)
p5 <- draw_tree(t5, 0.22, FALSE, FALSE)
p6 <- draw_tree(t6, 0.22, FALSE, FALSE, 30.0)

labels <- c("A", "B", "C", "D", "E", "F")
cowplot::plot_grid(p1,p2,p3,p4,p5,p6,ncol=2,labels=labels, label_size=12, hjust=0.0) + theme(plot.margin=unit(c(3,3,3,3), "pt"))

ggsave("Fig_S4.svg", device="svg")

