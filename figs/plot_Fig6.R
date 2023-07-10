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

    # Read in the tip labels and desired display names
    source("names.R")

    # Read in colors adapted for color blind
    source("colors.R")

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
    cynipidae <- MRCA(tb, "Andr_gro", "Escha_ac")$node
    figitidae <- MRCA(tb, "Parn_nig", "Lepto_he")$node
    diplolepidae <- MRCA(tb, "Dipl_spi", "Pedia_ac")$node
#    diplo <- tb$node[match("Dipl_spi",tb$label)]
#    pedia <- tb$node[match("Pedia_ac",tb$label)]
#    aylax <- tb$node[match("Aylax_hy",tb$label)]
#    phanacis <- tb$node[match("Phana_JH",tb$label)]
    paraulacidae <- tb$node[match("Cecin_ib",tb$label)]

    # Add the lineage info to the tree (using the "group" attribute)
#    clades <- c(Phytophagous=cynipidae_core, Parasitoid=figitidae)
#    tree <- groupClade(tree, clades)
#    x <- as.character(attr(tree,"group"))
#    if ("Escha_ac" %in% tb$label) {
#        escha <- tb$node[match("Escha_ac",tb$label)]
#        x[escha] <- "Phytophagous"
#        if ("Orussusa" %in% tb$label) {
#            cynipidae <- MRCA(tb, "Andr_gro", "Escha_ac")$node
#            x[cynipidae] <- "Phytophagous"
#        }
#    }
#    x[diplolepidae] = "Phytophagous"
#    x[diplo] = "Phytophagous"
#    x[pedia] = "Phytophagous"
#    attr(tree,"group") <- as.factor(x)

    # Set the colors
#    cols <- c(Phytophagous="green4", Parasitoid="black")

    # Change display names and only show support < 100%
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]
    for ( i in 1:length(tree$node.label) ) {
        if ( tree$node.label[i] == "1" )
            tree$node.label[i] <- ""
        else
            tree$node.label[i] <- round(as.numeric(tree$node.label[i])*100)
    }

    if (leg==TRUE) {
        leg.pos <- c(.2,.78)
    }
    else {
        leg.pos <- "none"
    }

    font_size <- 3.0    # default is 3.88
    cyn_offset <- -0.14
    fig_offset <- -0.14
    dip_offset <- -0.18
    cyn_offset_text <- 0.02
    fig_offset_text <- 0.02
    dip_offset_text <- 0.02
    cyn_color <- z[2]
    ggtree(tree, ladderize = TRUE) + geom_nodelab(size=2,hjust=0) +
        geom_tree(size=0.8) + geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 2.2) +
        geom_treescale(x = 0.0, y = treescale_height, width = 0.1, fontsize=2.0) +
        geom_strip('Andricus cur', 'Protobalandricus spe', barsize=1, fontsize=font_size, color='black', label="Cynipini", offset=cyn_offset+0.04, offset.text=cyn_offset_text) +
        geom_strip('Ceroptres mas', 'Ceroptres mas', barsize=1, fontsize=font_size, color='black', label="Ceroptresini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Iraella his', 'Iraella his', barsize=1, fontsize=font_size, color='black', label="Aylacini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Diastrophus kin', 'Periclistus sp', barsize=1, fontsize=font_size, color='black', label="Diastrophini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Synergus jap', 'Synergus ito', barsize=1, fontsize=font_size, color='black', label="Synergini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Qwaqwaia sco', 'Qwaqwaia sco', barsize=1, fontsize=font_size, color='black', label="Qwaqwaiini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Aulacidea tav', 'Fumariphilus hyp', barsize=1, fontsize=font_size, color='black', label="Aulacideini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Phanacis sp', 'Phanacis sp', barsize=1, fontsize=font_size, color='black', label="Phanacidini", offset=cyn_offset, offset.text=cyn_offset_text) +
        geom_strip('Eschatocerus aca', 'Eschatocerus aca', barsize=1, fontsize=font_size, color='black', label="Eschatocerini", offset=0.15, offset.text=0.02) +
        geom_strip('Leptopilina cla', 'Ganaspis sp', barsize=1, fontsize=font_size, color='black', label="Eucoilinae", offset=0.0, offset.text=0.02) +
        geom_strip('Callaspidia not', 'Callaspidia not', barsize=1, fontsize=font_size, color='black', label="Aspicerinae", offset=fig_offset, offset.text=fig_offset_text) +
        geom_strip('Phaenoglyphis vil', 'Alloxysta arc', barsize=1, fontsize=font_size, color='black', label="Charipinae", offset=fig_offset, offset.text=fig_offset_text) +
        geom_strip('Parnips nig', 'Parnips nig', barsize=1, fontsize=font_size, color='black', label="Parnipinae", offset=fig_offset-0.06, offset.text=fig_offset_text) +
        geom_strip('Diplolepis spi', 'Diplolepis spi', barsize=1, fontsize=font_size, color='black', label="Diplolepidini", offset=dip_offset, offset.text=dip_offset_text) +
        geom_strip('Pediaspis ace', 'Pediaspis ace', barsize=1, fontsize=font_size, color='black', label="Pediaspidini", offset=dip_offset, offset.text=dip_offset_text) +
        geom_strip('Cecinothofagus iba', 'Cecinothofagus iba', barsize=1, fontsize=font_size, color='black', label="Paraulacini", offset=-0.16, offset.text=.02) +
        geom_strip('Nasonia vit', 'Orussus abi', barsize=1, fontsize=4.0, color='gray50', label="Outgroups", offset=0.0, offset.text=.02) +
        geom_hilight(node=cynipidae, fill=z[2], type="rect", extend=0.18) +
        geom_hilight(node=figitidae, fill=z[1], type="rect", extend=0.32) +
        geom_hilight(node=diplolepidae, fill=z[3], type="rect", extend=0.35) +
        geom_hilight(node=paraulacidae, fill=z[4], type="rect", extend=0.36) +
        geom_strip('Andricus cur', 'Eschatocerus aca', barsize=0, fontsize=4.5, label="Cynipidae (s. str.)", offset=0.16, offset.text=0.02) +
        geom_strip('Leptopilina cla', 'Parnips nig', barsize=0, fontsize=4.5, label="Figitidae", offset=0.17, offset.text=0.02) +
        geom_strip('Diplolepis spi', 'Pediaspis ace', barsize=0, fontsize=4.5, label="Diplolepididae", offset=0.01, offset.text=0.02) +
        geom_strip('Cecinothofagus iba', 'Cecinothofagus iba', barsize=0, fontsize=4.5, label="Paraulacidae", offset=0.0, offset.text=0.02) +
        hexpand(hexp)
}

t1 <- read.tree("../phylobayes/clustal34_ag_lt0.26.con.tre")

p1 <- draw_tree(t1, 0.22, TRUE, TRUE, 8.0)

p1

ggsave("Fig_6.svg", device="svg")

