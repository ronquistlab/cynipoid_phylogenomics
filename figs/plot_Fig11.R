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

    # Read in tip labels and desired display names
    # Note the need for an extended list for this tree
    source("names2.R") 

    # Add a root edge
    tree$root.edge <- 0.02

    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    synergini_core <- MRCA(tb, "Syne_jap", "Syne_gif")$node
    syne_ito <- tb$node[match("Syne_ito",tb$label)]
    synergini <- MRCA(tb, "Syne_jap", "*Synergus_clade1")$node
    synergini1 <- MRCA(tb, "Syne_jap", "*Synophrus+")$node
    synergini2 <- MRCA(tb, "Syne_jap", "*Synergus_clade2")$node
    synergini3 <- MRCA(tb, "Syne_jap", "*Synergus_clade3")$node
    synergini4 <- MRCA(tb, "Syne_jap", "*Saphonecrus+")$node
    synergini5 <- MRCA(tb, "Syne_jap", "*Lithosaphonecrus")$node
    synergini6 <- MRCA(tb, "Syne_jap", "Syne_ito")$node
    synergus1 <- tb$node[match("*Synergus_clade1",tb$label)]
    synergus2 <- tb$node[match("*Synergus_clade2",tb$label)]
    synergus3 <- tb$node[match("*Synergus_clade3",tb$label)]
    synophrus <- tb$node[match("*Synophrus+",tb$label)]
    saphonecrus <- tb$node[match("*Saphonecrus+",tb$label)]
    lithosaphonecrus <- tb$node[match("*Lithosaphonecrus",tb$label)]
    cynipini <- MRCA(tb, "Andr_cur", "Prot_spe")$node
    phana_aula <- MRCA(tb, "Aula_tav", "Phana_JH")$node
    cerop <- tb$node[match("Cerop_ma",tb$label)]
    iraella <- tb$node[match("Irae_his",tb$label)]
    peric <- tb$node[match("Peric_JH",tb$label)]
    diast <- tb$node[match("Diast_ki",tb$label)]
    xest <- tb$node[match("*Xestophanes",tb$label)]
    synoph <- tb$node[match("*Synophromorpha",tb$label)]
    qwaqwaia <- tb$node[match("Qwaq_sco",tb$label)]
    rhoophilus <- tb$node[match("*Rhoophilus",tb$label)]
    cynipidae_core <- MRCA(tb, "Andr_gro", "Hedic_le")$node

    # Add the lineage info to the tree (using the "group" attribute)
    clades <- c(Gall_inducer=cynipini, Gall_inducer2=phana_aula, Inquiline=synergini_core)
    tree <- groupClade(tree, clades)
    x <- as.character(attr(tree,"group"))
    for (i in 1:length(x)) if (x[i]=="Gall_inducer2") x[i] <- "Gall_inducer"
    x[cerop] <- "Inquiline"
    x[peric] <- "Inquiline"
    x[synoph] <- "Inquiline"
    x[rhoophilus] <- "Inquiline"
    x[iraella] <- "Gall_inducer"
    x[diast] <- "Gall_inducer"
    x[xest] <- "Gall_inducer"
    x[qwaqwaia] <- "Gall_inducer"
    x[syne_ito] <- "Gall_inducer"
    x[synergus1] <- "Inquiline"
    x[synergus2] <- "Inquiline"
    x[synergus3] <- "Inquiline"
    x[synophrus] <- "Inquiline"
    x[saphonecrus] <-  "Inquiline"
    x[lithosaphonecrus] <- "Inquiline"

    y <- character(length(x))
    if (type == "inquiline_first") {
        for (i in 1:length(x))
            if (x[i]=="0") x[i]<-"Inquiline"
        x[synergini] <- "Inquiline"
        for (i in 1:length(y)) {
            if (i %in% c(phana_aula, qwaqwaia, syne_ito, diast, xest, iraella, cynipini))
                y[i] <- "G"
            else
                y[i] <- NA
        }
    }
    else if (type == "galler_first") {
        for (i in 1:length(x))
            if (x[i]=="0") x[i]<-"Gall_inducer"
        x[synergini] <- "Gall_inducer"
        for (i in 1:length(y)) {
            if (i %in% c(peric, synoph, cerop, rhoophilus, synergini_core, synergus1, synergus2, synergus3, saphonecrus, synophrus, lithosaphonecrus))
                y[i] <- "I"
            else
                y[i] <- NA
        }
    }
    else if (type == "parsimonious") {
        for (i in 1:length(x))
            if (x[i]=="0") x[i]<-"Gall_inducer"
        x[synergini] <- "Inquiline"
        x[synergini1] <- "Inquiline"
        x[synergini2] <- "Inquiline"
        x[synergini3] <- "Inquiline"
        x[synergini4] <- "Inquiline"
        x[synergini5] <- "Inquiline"
        x[synergini6] <- "Inquiline"
        for (i in 1:length(y)) {
            if (i==peric || i==synoph || i==cerop || i==synergini || i==rhoophilus)
                y[i] <- "I"
            else if (i==syne_ito)
                y[i] <- "G"
            else
                y[i] <- NA
        }
    }
    attr(tree,"group") <- as.factor(x)
    attr(tree,"branch_label") <- as.factor(y)

    # Read in color palette adapted for color blind
    source("colors.R")
    cols <- c(Gall_inducer=z[3], Inquiline=z[2])
    
    # Change display names
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]

    if (leg==TRUE) {
        leg.pos <- c(.5,.8)
    }
    else {
        leg.pos <- "none"
    }

    fill_col <- z[1]
    if (type == "galler_first" || type == "parsimonious")
        root_col <- cols[1]
    else
        root_col <- cols[2]

    ggtree(tree, aes(color = group), ladderize = TRUE) +
        geom_rootedge(size=0.8, color=root_col) +
        geom_tree(size=0.8, key_glyph="rect") +
        geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 1.5) +
        hexpand(hexp) +
        geom_strip('Andricus cur', 'Protobalandricus spe', barsize=0.2, color="gray60", fontsize=2, label="Cynipini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Ceroptres mas', 'Ceroptres mas', barsize=0.2, color="gray60", fontsize=2, label="Ceroptresini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Iraella his', 'Iraella his', barsize=0.2, color="gray60", fontsize=2, label="Aylacini",
            offset=0.022, offset.text=0.003) +
        geom_strip('*Synophromorpha', 'Periclistus sp', barsize=0.2, color="gray60", fontsize=2, label="Diastrophini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Qwaqwaia sco', 'Qwaqwaia sco', barsize=0.2, color="gray60", fontsize=2, label="Qwaqwaiini",
            offset=0.022, offset.text=0.003) +
        geom_strip('*Rhoophilus', '*Rhoophilus', barsize=0.2, color="gray60", fontsize=2, label="Rhoophilini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Synergus jap', '*Synergus_clade1', barsize=0.2, color="gray60", fontsize=2, label="Synergini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Aulacidea tav', 'Fumariphilus hyp', barsize=0.2, color="gray60", fontsize=2, label="Aulacideini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Phanacis sp', 'Phanacis sp', barsize=0.2, color="gray60", fontsize=2, label="Phanacidini",
            offset=0.022, offset.text=0.003) +
        geom_label(aes(x=branch, label=branch_label), color="white", fill=fill_col, size=3, label.size=0.1, label.padding=unit(0.05,'cm'), show.legend=FALSE, na.rm=TRUE) + 
        theme_tree(legend.position = leg.pos, legend.key.size = unit(0.04,'cm'), legend.title = element_blank()) +
        scale_color_manual(values = c(cols, "black"), na.value = "black", name = "Life history",
            breaks = c("Gall_inducer", "Inquiline", "Uncertain"),
            guide = guide_legend(override.aes = list(size = 3)))
}

t1 <- read.tree("fig11.tre")
t1 <- ape::root(t1, outgroup="Orussusa", edgelabel=TRUE)
tb <- as_tibble(t1)
cynipidae_core <- MRCA(tb, "Andr_gro", "Hedic_le")$node
t1 <- ape::subtrees(t1)[[cynipidae_core-length(t1$tip.label)]]
t2 <- t1
t3 <- t1

p1 <- draw_tree(t1, 0.35, FALSE, type="inquiline_first")
p2 <- draw_tree(t2, 0.35, FALSE, type="galler_first")
p3 <- draw_tree(t3, 0.35, FALSE, type="parsimonious")
p4 <- draw_tree(t2, 0.35, TRUE, type="galler_first")
p5 <- cowplot::get_legend(p4)

labels <- c("A", "B","C","")
cowplot::plot_grid(p1,p2,p3,p5,ncol=2,labels=labels, label_size=14, hjust=0.0) + theme(plot.margin=unit(c(3,3,3,3), "pt"))

ggsave("Fig_11.svg", device="svg", height=7.0)
ggsave("Fig_11.pdf", device="pdf", height=7.0)

