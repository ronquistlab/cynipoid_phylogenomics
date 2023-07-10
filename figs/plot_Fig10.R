library(ape)
library(cowplot)
library(tibble)
library(tidyr)
library(tidytree)
library(treeio)
library(ggplot2)
library(ggtree)

draw_tree <- function(tree, hexp, leg=TRUE, type)
{
    require(ggtree)
    require(ape)

    # Read in tip labels and desired display labels
    source("names.R")

    # Desired order of tips
    taxonOrder <- c(36,37,35,34,32,33,31,8,1,2,3,4,5,6,7,26,27,28,29,30,19,20,21,22,23,24,25,18,17,16,15,14,13,12,11,10,9)

    # Root the tree correctly
    tree <- ape::root(tree, outgroup="Orussusa", edgelabel=TRUE)
    tree$node.label[1]<-""

    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    cynipidae <- MRCA(tb, "Andr_gro", "Escha_ac")$node
    figitidae_core <- MRCA(tb,"Phaen_vi", "Lepto_he")$node
    figitidae <- MRCA(tb,"Parn_nig", "Lepto_he")$node
    diplolepidae <- MRCA(tb, "Dipl_spi", "Pedia_ac")$node
    pedia <- tb$node[match("Pedia_ac",tb$label)]
    diplo <- tb$node[match("Dipl_spi",tb$label)]
    escha <- tb$node[match("Escha_ac",tb$label)]
    parnips <- tb$node[match("Parn_nig",tb$label)]
    cecin <- tb$node[match("Cecin_ib",tb$label)]
    cyn_fig <- MRCA(tb, "Andr_gro", "Parn_nig")$node
    cyn_fig_dip <- MRCA(tb, "Andr_gro", "Dipl_spi")$node
    cynipoidea <- MRCA(tb, "Andr_gro", "Cecin_ib")$node

    # Add the lineage info to the tree (using the "group" attribute)
    clades <- c(Gall_inducer_inquiline=cynipidae, Koinobiont_other=figitidae_core)
    tree <- groupClade(tree, clades)
    x <- as.character(attr(tree,"group"))
    x[parnips] <- "Koinobiont_gall"
    x[figitidae] <- "Koinobiont_gall"
    x[diplo] <- "Gall_inducer_inquiline"
    x[pedia] <- "Gall_inducer_inquiline"
    x[diplolepidae] <- "Gall_inducer_inquiline"
    x[cecin] <- "Koinobiont_gall"
    
    for (i in 1:length(x))
       if (x[i]=="0") x[i]<-"Parasitoid_other"

    y <- character(length(x))
    if (type == "galler_twice") {
        x[cyn_fig] <- "Koinobiont_gall"
        x[cyn_fig_dip] <- "Koinobiont_gall"
        x[cynipoidea] <- "Koinobiont_gall"
        for (i in 1:length(y)) {
            if (i==cynipidae || i==diplolepidae)
                y[i] <- "G"
            else if (i==cynipoidea)
                y[i] <- "K"
            else
                y[i] <- NA
        }
    }
    else if (type == "parasitoid_twice") {
        x[cyn_fig] <- "Gall_inducer_inquiline"
        x[cyn_fig_dip] <- "Gall_inducer_inquiline"
        x[cynipoidea] <- "Gall_inducer_inquiline"
        for (i in 1:length(y)) {
            if (i==cynipoidea)
                y[i] <- "G"
            else if (i==figitidae || i==cecin)
                y[i] <- "K"
            else
                y[i] <- NA
        }
    }
    attr(tree,"group") <- as.factor(x)
    attr(tree,"branch_label") <- as.factor(y)

    # Read in colors adapted for color blind
    source("colors.R")

    # Set the colors
    cols <- c(Gall_inducer_inquiline=z[3], Koinobiont_gall=z[2], Koinobiont_other=z[1], Parasitoid_other=z[4])
    
    # Change display names
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]

    if (leg==TRUE) {
        leg.pos <- c(.2,.78)
    }
    else {
        leg.pos <- "none"
    }

    ggtree(tree, aes(color = group), ladderize = TRUE) +
        geom_tree(size=0.8, key_glyph="rect") + 
        geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 1.5) +
        hexpand(hexp) +
        geom_strip('Andricus cur', 'Phanacis sp', barsize=0.2, color="gray60", fontsize=2, label="Core Cynipidae (s. str.)",
            offset=-0.09, offset.text=0.04, angle=270, hjust=0.5) +
        geom_strip('Leptopilina cla', 'Parnips nig', barsize=0.2, color="gray60", fontsize=2, label="Figitidae",
            offset=0.02, offset.text=0.04, angle=270, hjust=0.5) +
        geom_strip('Diplolepis spi', 'Pediaspis ace', barsize=0.2, color="gray60", fontsize=2, label="Diplolepididae",
            offset=-0.18, offset.text=0.02) +
        geom_strip('Cecinothofagus iba', 'Cecinothofagus iba', color="gray60", barsize=0.2, fontsize=2, label="Paraulacidae",
            offset=-0.14, offset.text=0.02) +
        geom_label(aes(x=branch, label=branch_label), color="white", fill="blue", size=3, label.size=0.1, label.padding=unit(0.05,'cm'), show.legend=FALSE, na.rm=TRUE) + 
        theme_tree(legend.position=leg.pos, legend.key.size=unit(0.06,'cm'), legend.title=element_text(size=9), legend.text=element_text(size=7)) +
        scale_color_manual(values = c(cols, "black"), na.value = "black", name = "Larval feeding mode",
            breaks = c("Gall_inducer_inquiline", "Koinobiont_gall", "Koinobiont_other", "Parasitoid_other"),
            guide = guide_legend(override.aes = list(size = 4)))
}

t1 <- read.tree("../phylobayes/clustal34_ag_lt0.26.con.tre")
t2 <- t1

p1 <- draw_tree(t1, 0.22, TRUE, type="galler_twice")
p2 <- draw_tree(t2, 0.22, FALSE, type="parasitoid_twice")

labels <- c("A", "B")
cowplot::plot_grid(p1,p2,ncol=2,labels=labels, label_size=14, hjust=0.0) + theme(plot.margin=unit(c(3,3,3,3), "pt"))

ggsave("Fig_10.svg", device="svg", height=3.5)

