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

    # Add a root edge
    tree$root.edge <- 0.02

    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    synergini_core <- MRCA(tb, "Syne_jap", "Syne_gif")$node
    syne_ito <- tb$node[match("Syne_ito",tb$label)]
    synergini <- MRCA(tb, "Syne_jap", "Syne_ito")$node
    cynipini <- MRCA(tb, "Andr_cur", "Prot_spe")$node
    phana_aula <- MRCA(tb, "Aula_tav", "Phana_JH")$node
    cerop <- tb$node[match("Cerop_ma",tb$label)]
    iraella <- tb$node[match("Irae_his",tb$label)]
    peric <- tb$node[match("Peric_JH",tb$label)]
    diast <- tb$node[match("Diast_ki",tb$label)]
    qwaqwaia <- tb$node[match("Qwaq_sco",tb$label)]
    cynipidae_core <- MRCA(tb, "Andr_gro", "Hedic_le")$node

    # Add the lineage info to the tree (using the "group" attribute)
    clades <- c(Gall_inducer=cynipini, Gall_inducer2=phana_aula, Inquiline=synergini_core)
    tree <- groupClade(tree, clades)
    x <- as.character(attr(tree,"group"))
    for (i in 1:length(x)) if (x[i]=="Gall_inducer2") x[i] <- "Gall_inducer"
    x[cerop] <- "Inquiline"
    x[peric] <- "Inquiline"
    x[iraella] <- "Gall_inducer"
    x[diast] <- "Gall_inducer"
    x[syne_ito] <- "Gall_inducer"
    x[qwaqwaia] <- "Gall_inducer"

    y <- character(length(x))
    if (type == "inquiline_first") {
        for (i in 1:length(x))
            if (x[i]=="0") x[i]<-"Inquiline"
        x[synergini] <- "Inquiline"
        for (i in 1:length(y)) {
            if (i %in% c(phana_aula, qwaqwaia, syne_ito, diast, iraella, cynipini))
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
            if (i==peric || i==cerop || i==synergini_core)
                y[i] <- "I"
            else
                y[i] <- NA
        }
    }
    attr(tree,"group") <- as.factor(x)
    attr(tree,"branch_label") <- as.factor(y)

    # Set the colors
    cols <- c(Gall_inducer="green3", Inquiline="blue")
    
    # Change display names
    for ( i in 1:length(tree$tip.label) )
        tree$tip.label[i] <- displayNames[ match(tree$tip.label[i],taxonNames) ]

    if (leg==TRUE) {
        leg.pos <- c(.2,.93)
    }
    else {
        leg.pos <- "none"
    }

    fill_col <- "red"
    if (type == "galler_first")
        root_col <- cols[1]
    else
        root_col <- cols[2]

    ggtree(tree, aes(color = group), ladderize = TRUE) + geom_rootedge(size=0.8, color=root_col) +
        geom_tree(size=0.8) + geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 1.5) +
        guides(color = guide_legend(override.aes = list(size = 3, shape = 15))) +
        theme_tree(legend.position = leg.pos, legend.key.size = unit(0.04,'cm'), legend.title = element_blank()) +
        hexpand(hexp) +
        geom_strip('Andricus cur', 'Protobalandricus spe', barsize=0.2, color="gray60", fontsize=2, label="Cynipini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Ceroptres mas', 'Ceroptres mas', barsize=0.2, color="gray60", fontsize=2, label="Ceroptresini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Iraella his', 'Iraella his', barsize=0.2, color="gray60", fontsize=2, label="Aylacini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Diastrophus kin', 'Periclistus sp', barsize=0.2, color="gray60", fontsize=2, label="Diastrophini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Qwaqwaia sco', 'Qwaqwaia sco', barsize=0.2, color="gray60", fontsize=2, label="Qwaqwaiini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Synergus jap', 'Synergus ito', barsize=0.2, color="gray60", fontsize=2, label="Synergini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Aulacidea tav', '"Aylax" hyp', barsize=0.2, color="gray60", fontsize=2, label="Aulacideini",
            offset=0.022, offset.text=0.003) +
        geom_strip('Phanacis sp', 'Phanacis sp', barsize=0.2, color="gray60", fontsize=2, label="Phanacidini",
            offset=0.022, offset.text=0.003) +
        geom_label(aes(x=branch, label=branch_label), color="white", fill=fill_col, size=3, label.size=0.1, label.padding=unit(0.05,'cm'), show.legend=FALSE, na.rm=TRUE) + 
        scale_color_manual(values = c(cols, "black"), na.value = "black", name = "Life history",
            breaks = c("Gall_inducer", "Inquiline", "Uncertain"))
}

t1 <- read.tree("../phylobayes/clustal34_ag_lt0.26.con.tre")
t1 <- ape::root(t1, outgroup="Orussusa", edgelabel=TRUE)
tb <- as_tibble(t1)
cynipidae_core <- MRCA(tb, "Andr_gro", "Hedic_le")$node
t1 <- ape::subtrees(t1)[[cynipidae_core-length(t1$tip.label)]]
t2 <- t1

p1 <- draw_tree(t1, 0.35, TRUE, type="inquiline_first")
p2 <- draw_tree(t2, 0.35, FALSE, type="galler_first")

labels <- c("A", "B")
cowplot::plot_grid(p1,p2,ncol=2,labels=labels, label_size=14, hjust=0.0) + theme(plot.margin=unit(c(3,3,3,3), "pt"))

ggsave("Fig_7.png", device="png", height=3.5)

