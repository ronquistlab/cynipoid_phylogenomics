library(ape)
library(cowplot)
library(tibble)
library(tidyr)
library(tidytree)
library(treeio)
library(ggplot2)
library(ggtree)

draw_tree <- function(tree, hexp, cyn_mono=TRUE, leg=TRUE)
{
    require(ggtree)
    require(ape)

    # Taxon names actually used in the trees
    taxonNames <- c(
    "Allox_ar","Phaen_vi","Callas_no","Gana_sp1","Lepto_bo","Lepto_cl",
    "Lepto_he","Parn_nig","Andr_cur","Andr_gro","Andr_qrm","Andr_qln",
    "Beloc_tr","Bior_pal","Calli_sp","Prot_spe","Cerop_ma","Irae_his",
    "Diast_ki","Peric_JH","Qwaq_sco","Syne_gif","Syne_jap","Syne_umb",
    "Syne_ito","Aula_tav","Isoc_cen","Aylax_hy","Hedic_le","Phana_JH",
    "Escha_ac","Dipl_spi","Pedia_ac","Cecin_ib","Nasoniav","Orussusa",
    "Micropld"
    )

    # Choose the corresponding display names we want to use
    displayNames <- c(
    "Alloxysta arc","Phaenoglyphis vil","Callaspidia not","Ganaspis sp","Leptopilina bou","Leptopilina cla",
    "Leptopilina het","Parnips nig","Andricus cur","Andricus gro","Andricus qrm","Andricus qln",
    "Belonocnema kin","Biorhiza pal","Neuroterus val","Protobalandricus spe","Ceroptres mas","Iraella his",
    "Diastrophus kin","Periclistus sp","Qwaqwaia sco","Synergus gif","Synergus jap","Synergus umb",
    "Synergus ito","Aulacidea tav","Isocolus cen","\"Aylax\" hyp","Hedickiana lev","Phanacis sp",
    "Eschatocerus aca","Diplolepis spi","Pediaspis ace","Cecinothofagus iba","Nasonia vit","Orussus abi",
    "Microplitis dem"
    )

    # Desired order of tips
    taxonOrder <- c(36,37,35,34,32,33,31,8,1,2,3,4,5,6,7,26,27,28,29,30,19,20,21,22,23,24,25,18,17,16,15,14,13,12,11,10,9)

    # Root the trees correctly
    tree <- ape::root(tree, outgroup="Orussusa", edgelabel=TRUE)
    tree$node.label[1]<-""
    
    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    aulacideini <- MRCA(tb, "Hedic_le", "Aylax_hy")$node
    cynipini <- MRCA(tb, "Prot_spe", "Andr_gro")$node
    cynipini1 <- MRCA(tb, "Calli_sp", "Andr_gro")$node
    cynipini2 <- tb$node[match("Prot_spe",tb$label)]
    escha <- tb$node[match("Escha_ac",tb$label)]
    phana <- tb$node[match("Phana_JH",tb$label)]
    aylac <- tb$node[match("Irae_his",tb$label)]

    # Add the lineage info to the tree (using the "group" attribute)
    if (cyn_mono == FALSE)
       int_clades <- c(Cynipini=cynipini1, Aulacideini=aulacideini)
    else
       int_clades <- c(Cynipini=cynipini, Aulacideini=aulacideini)
    tree <- groupClade(tree, int_clades)
    x <- as.character(attr(tree,"group"))
    if (!cyn_mono)
        x[cynipini2] <- "Cynipini"
    x[escha] = "Eschatocerini"
    x[phana] = "Phanacidini"
    x[aylac] = "Aylacini"
    attr(tree,"group") <- as.factor(x)

    # Set the colors
    cols <- c(Cynipini="blue", Eschatocerini="orange", Phanacidini="green3", Aylacini="deeppink", Aulacideini="green")
    
    # Change display names and only show support < 100%
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]
    for ( i in 1:length(tree$node.label) ) {
        if ( tree$node.label[i] == "100" )
            tree$node.label[i] <- ""
    }

    # Find out whether we should print the legend
    if (leg==TRUE)
        leg.pos <- c(.2,.78)
    else
        leg.pos <- "none"
    
    ggtree(tree, aes(color = group), ladderize = TRUE) + geom_nodelab(size=2,hjust=0) +
        geom_tree(size=0.8) + geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 2.0) +
        geom_treescale(x = 0.0, y = 8.0, width = 0.1, fontsize=2) + guides(color = guide_legend(override.aes = list(size = 4, shape = 15))) +
        theme_tree(legend.position = leg.pos, legend.key.size = unit(0.05,'cm'), legend.title = element_text(size=9)) +
        hexpand(hexp) +
        scale_color_manual(values = c(cols, "black"), na.value = "black", name = "Lineage",
            breaks = c("Cynipini", "Aylacini", "Phanacidini", "Aulacideini","Eschatocerini"))
}

t1 <- read.tree("../iqtree/b-ag-c37.contree")
t2 <- read.tree("../iqtree/iq_36_c60.contree")
t3 <- read.tree("../iqtree/iq_35_C60_I_G5.contree")
t4 <- read.tree("../iqtree/iq_34_c60.contree")

p1 <- draw_tree(t1, 0.25, TRUE, TRUE)
p2 <- draw_tree(t2, 0.22, FALSE, FALSE)
p3 <- draw_tree(t3, 0.22, FALSE, FALSE)
p4 <- draw_tree(t4, 0.22, FALSE, FALSE)

labels <- c("A", "B", "C", "D")
cowplot::plot_grid(p1,p2,p3,p4,ncol=2,labels=labels, label_size=14, hjust=0.0) + theme(plot.margin=unit(c(3,3,3,3), "pt"))

ggsave("Fig_1.png", device="png")

