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
    
    # Taxon names actually used in the trees
    taxonNames <- c(
    "Acanthaegilips_bra","Acraspis_vil","Disholcaspis_ace","Disholcaspis_qma","Disholcaspis_las","Andricus_lan","Atrusca_bel","Cynips_plu","Philonix_ful","Biorhiza_pal_phylo","Cynips_div","Andricus_clf1","Andricus_clf2","Callirhytis_whe","Bassettia_ten","Holocynips_har","Andricus_cor_phylo","Andricus_kol_phylo","Aphelonix_per","Andricus_ste","Andricus_gro","Andricus_luc","Andricus_cur_phylo","Andricus_for","Andricus_sp1","Andricus_sph","Amphibolips_hid","Callirhytis_eld","Zapatella_dav","Andricus_qop","Dryocosmus_ril","Neuroterus_dis","Dryocosmus_kur2","Dryocosmus_kur1","Plagiotrochus_sub","Protobalandricus_spe_phylo","Ceroptres_cor","Ceroptres_mas_phylo","Iraella_his_phylo","Diastrophus_kin_phylo","Diastrophus_kin","Diastrophus_sp1","Diastrophus_tur","Synophromorpha_syl","Synophromorpha_ter","Periclistus_sp_phylo","Periclistus_sp1","Periclistus_sp2","Synergus_umb_phylo","Synergus_jap_phylo","Synergus_sp1","Synergus_gif_phylo","Synergus_ito_phylo","Synergus_sp2","Synophrus_pilulae","Synergus_lae","Qwaqwaia_sco_phylo","Rhoophilus_loe","Antistrophus_min","Antistrophus_ruf","Aylax_sal","Hedickinia_lev_phylo","Aylax_hyp_phylo","Aulacidea_hie","Aulacidea_pod","Aulacidea_tav_phylo","Isocolus_cen_phylo","Isocolus_rog","Isocolus_sca","Phanacis_sp2_phylo","Phanacis_sp1","Eschatocerus_aca_phylo","Eschatocerus_nig","Callihormius_bif","Leptocybe_inv","Propsilomma_col","Nixonia_wat","Platygaster_sp1","Sparasion_cul","Trissolcus_sp1","Cecinothofagus_ibarrai","Paraulacini_sp1","Cecinothofagus_iba_phylo","Paraulax_que","Diplolepis_bic","Diplolepis_ros","Diplolepis_may","Diplolepis_spin_phylo","Pediaspis_ace_phylo","Pediaspis_aceris","Parnips_nig","Parnips_nig_phylo","Euceroptres_montanus_1322161","Oberthuerella_len","Pseudibalia_sp1","Paramblynotus_vir1","Paramblynotus_vir2","Heteribalia_sp1","Ibalia_leu","Ibalia_anc","Mikeius_har","Myrtopsen_mim","Scutimica_sp1","Plectocynips_pil","Anacharoides_pal","Callaspidia_sp1","Callaspidia_not-phylo","Aspicera_har","Paraspicera_sp1","Prosaspicera_sp1","Callaspidia_sp2","Omalaspis_sp1","Melanips_sp1","Figites_sp1","Neralsia_sp1","Xyalophora_sp1","Lonchidia_sp1","Sarothrus_sp1","Sarothrus_sp2","Ganaspis_bra","Ganaspis_bra","Ganaspis_sp1_phylo","Striatovertex_sp1","Hexacola_sp1","Nordlandiella_sem","Ganaspis_sp1","Odonteucoila_sp1","Ganaspis_sp2","Kleidotoma_sp1","Zaeucoila_rob","Gronotoma_sp1","Leptopilina_bou","Leptopilina_jap","Rhoptromeris_sp1","Trichoplasta_sp1","Trybliographa_sp1","Leptolamina_pon","Thoreauella_sp1","Thoreauella_sp2","Tylosema_day","Alloxysta_arc_phylo","Lytoxysta_bre","Alloxysta_sp1","Apocharips_trap","Dilyta_sp1","Phaenoglyphis_sp1","Phaenoglyphis_vil_phylo","Anacharis_sp1","Anacharis_sp2","Aegilips_chi","Aegilips_sp3","Aegilips_sp1","Xyalaspis_sp1","Hexacharis_fla"
    )
    
    # Choose the corresponding display names we want to use
    displayNames <- c(
    "Acanthaegilips_bra","Acraspis_vil","Disholcaspis_ace","Disholcaspis_qma","Disholcaspis_las","Andricus_lan","Atrusca_bel","Cynips_plu","Philonix_ful","Biorhiza_pal_phylo","Cynips_div","Andricus_clf1","Andricus_clf2","Callirhytis_whe","Bassettia_ten","Holocynips_har","Andricus_cor_phylo","Andricus_kol_phylo","Aphelonix_per","Andricus_ste","Andricus_gro","Andricus_luc","Andricus_cur_phylo","Andricus_for","Andricus_sp1","Andricus_sph","Amphibolips_hid","Callirhytis_eld","Zapatella_dav","Andricus_qop","Dryocosmus_ril","Neuroterus_dis","Dryocosmus_kur2","Dryocosmus_kur1","Plagiotrochus_sub","Protobalandricus_spe_phylo","Ceroptres_cor","Ceroptres_mas_phylo","Iraella_his_phylo","Diastrophus_kin_phylo","Diastrophus_kin","Diastrophus_sp1","Diastrophus_tur","Synophromorpha_syl","Synophromorpha_ter","Periclistus_sp_phylo","Periclistus_sp1","Periclistus_sp2","Synergus_umb_phylo","Synergus_jap_phylo","Synergus_sp1","Synergus_gif_phylo","Synergus_ito_phylo","Synergus_sp2","Synophrus_pilulae","Synergus_lae","Qwaqwaia_sco_phylo","Rhoophilus_loe","Antistrophus_min","Antistrophus_ruf","Aylax_sal","Hedickinia_lev_phylo","Fumariphilus_hyp_phylo","Aulacidea_hie","Aulacidea_pod","Aulacidea_tav_phylo","Isocolus_cen_phylo","Isocolus_rog","Isocolus_sca","Phanacis_sp2_phylo","Phanacis_sp1","Eschatocerus_aca_phylo","Eschatocerus_nig","Callihormius_bif","Leptocybe_inv","Propsilomma_col","Nixonia_wat","Platygaster_sp1","Sparasion_cul","Trissolcus_sp1","Cecinothofagus_ibarrai","Paraulacini_sp1","Cecinothofagus_iba_phylo","Paraulax_que","Diplolepis_bic","Diplolepis_ros","Diplolepis_may","Diplolepis_spin_phylo","Pediaspis_ace_phylo","Pediaspis_aceris","Parnips_nig","Parnips_nig_phylo","Euceroptres_mon","Oberthuerella_len","Pseudibalia_sp1","Paramblynotus_vir1","Paramblynotus_vir2","Heteribalia_sp1","Ibalia_leu","Ibalia_anc","Mikeius_har","Myrtopsen_mim","Scutimica_sp1","Plectocynips_pil","Anacharoides_pal","Callaspidia_sp1","Callaspidia_not_phylo","Aspicera_har","Paraspicera_sp1","Prosaspicera_sp1","Callaspidia_sp2","Omalaspis_sp1","Melanips_sp1","Figites_sp1","Neralsia_sp1","Xyalophora_sp1","Lonchidia_sp1","Sarothrus_sp1","Sarothrus_sp2","Ganaspis_bra1","Ganaspis_bra2","Ganaspis_sp1_phylo","Striatovertex_sp1","Hexacola_sp1","Nordlandiella_sem","Ganaspis_sp1","Odonteucoila_sp1","Ganaspis_sp2","Kleidotoma_sp1","Zaeucoila_rob","Gronotoma_sp1","Leptopilina_bou","Leptopilina_jap","Rhoptromeris_sp1","Trichoplasta_sp1","Trybliographa_sp1","Leptolamina_pon","Thoreauella_sp1","Thoreauella_sp2","Tylosema_day","Alloxysta_arc_phylo","Lytoxysta_bre","Alloxysta_sp1","Apocharips_trap","Dilyta_sp1","Phaenoglyphis_sp1","Phaenoglyphis_vil_phylo","Anacharis_sp1","Anacharis_sp2","Aegilips_chi","Aegilips_sp3","Aegilips_sp1","Xyalaspis_sp1","Hexacharis_fla"
    )
    
    # Desired order of tips
    #    taxonOrder <- c(36,37,35,34,32,33,31,8,1,2,3,4,5,6,7,26,27,28,29,30,19,20,21,22,23,24,25,18,17,16,15,14,13,12,11,10,9)
    
    # Root the trees correctly
    if ("Orussusa" %in% tree$tip.label)
    tree <- ape::root(tree, outgroup="Orussusa", edgelabel=TRUE)
    else
    tree <- ape::root(tree, outgroup="Callihormius_bif", edgelabel=TRUE)
    tree$node.label[1]<-""
    
    # Get indices for clades and branches we want to color
    tb <- as_tibble(tree)
    cynipidae <- MRCA(tb, "Acraspis_vil", "Eschatocerus_nig")$node
    figitidae <- MRCA(tb, "Parnips_nig", "Acanthaegilips_bra")$node
    diplolepidae <- MRCA(tb, "Diplolepis_bic", "Pediaspis_aceris")$node
    paraulacidae <- MRCA(tb,"Paraulax_que", "Cecinothofagus_ibarrai")$node
    #    diplo <- tb$node[match("Dipl_spi",tb$label)]
    #    pedia <- tb$node[match("Pedia_ac",tb$label)]
    #    aylax <- tb$node[match("Aylax_hy",tb$label)]
    #    phanacis <- tb$node[match("Phana_JH",tb$label)]
    
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
    tree$node.label[is.na(tree$node.label)]<-""
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
    
    font_size <- 2.5    # default is 3.88
    cyn_offset <- -0.05
    fig_offset <- -0.05
    dip_offset <- -0.05
    cyn_offset_text <- 0.01
    fig_offset_text <- 0.01
    dip_offset_text <- 0.01

    # Read in color palette adapted for color blind
    source("colors.R")
    
    # Build tree
    ggtree(tree, ladderize = TRUE) +
    geom_tree(size=0.2) +
    geom_nodelab(size=1.0,hjust=0) +
    geom_tiplab(aes(label = paste0("italic('", label, "')")), parse = TRUE, size = 1) +
    geom_treescale(x = 0.225, y = treescale_height, width = 0.1, fontsize=2.0) +
    geom_strip('Disholcaspis_qma', 'Protobalandricus_spe_phylo', barsize=1, fontsize=font_size, color='black', label="Cynipini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Ceroptres_mas_phylo','Ceroptres_cor',  barsize=1, fontsize=font_size, color='black', label="Ceroptresini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Iraella_his_phylo', 'Iraella_his_phylo', barsize=1, fontsize=font_size, color='black', label="Aylacini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Diastrophus_tur', 'Periclistus_sp_phylo', barsize=1, fontsize=font_size, color='black', label="Diastrophini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Synergus_jap_phylo','Synergus_lae',  barsize=1, fontsize=font_size, color='black', label="Synergini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Rhoophilus_loe','Qwaqwaia_sco_phylo',  barsize=1, fontsize=font_size, color='black', label="Qwaqwaiini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Isocolus_rog', 'Fumariphilus_hyp_phylo', barsize=1, fontsize=font_size, color='black', label="Aulacideini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Phanacis_sp1', 'Phanacis_sp2_phylo', barsize=1, fontsize=font_size, color='black', label="Phanacidini", offset=cyn_offset, offset.text=cyn_offset_text) +
    geom_strip('Eschatocerus_nig', 'Eschatocerus_aca_phylo', barsize=1, fontsize=font_size, color='black', label="Eschatocerini", offset=0.03, offset.text=0.01) +
    geom_strip( 'Nordlandiella_sem','Leptolamina_pon', barsize=1, fontsize=font_size, color='black', label="Eucoilinae", offset=-0.02, offset.text=0.01) +
    geom_strip('Prosaspicera_sp1','Melanips_sp1',  barsize=1, fontsize=font_size, color='black', label="Aspicerinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Sarothrus_sp2','Figites_sp1',  barsize=1, fontsize=font_size, color='black', label="Figitinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Aegilips_sp3','Anacharis_sp1',  barsize=1, fontsize=font_size, color='black', label="Anacharitinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Paramblynotus_vir2','Oberthuerella_len',  barsize=1, fontsize=font_size, color='black', label="Liopteridae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Euceroptres_mon','Euceroptres_mon',  barsize=1, fontsize=font_size-0.6, color='black', label="Euceroptresinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Ibalia_leu','Ibalia_anc',  barsize=1, fontsize=font_size, color='black', label="Ibaliidae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Lytoxysta_bre', 'Phaenoglyphis_sp1', barsize=1, fontsize=font_size, color='black', label="Charipinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Thoreauella_sp2', 'Tylosema_day', barsize=1, fontsize=font_size, color='black', label="Emargiinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Scutimica_sp1', 'Mikeius_har', barsize=1, fontsize=font_size, color='black', label="Thrasorinae (s. lat.)", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Parnips_nig', 'Parnips_nig_phylo', barsize=1, fontsize=font_size-0.6, color='black', label="Parnipinae", offset=fig_offset, offset.text=fig_offset_text) +
    geom_strip('Diplolepis_spin_phylo', 'Diplolepis_bic', barsize=1, fontsize=font_size, color='black', label="Diplolepidini", offset=dip_offset, offset.text=dip_offset_text) +
    geom_strip('Pediaspis_aceris', 'Pediaspis_ace_phylo', barsize=1, fontsize=font_size, color='black', label="Pediaspidini", offset=dip_offset, offset.text=dip_offset_text) +
    geom_strip('Cecinothofagus_ibarrai', 'Paraulax_que', barsize=1, fontsize=font_size, color='black', label="Paraulacini", offset=-0.05, offset.text=.01) +
    geom_strip('Trissolcus_sp1', 'Callihormius_bif', barsize=1, fontsize=4.0, color='gray50', label="Outgroups", offset=0.02, offset.text=.01) +
    geom_hilight(node=cynipidae, fill=z[2], extend=0.16) +
    geom_hilight(node=figitidae, fill=z[1], extend=0.229) +
    geom_hilight(node=diplolepidae, fill=z[3], extend=0.265) +
    geom_hilight(node=paraulacidae, fill=z[4], extend=0.271) +
    geom_strip('Acraspis_vil', 'Eschatocerus_nig', barsize=0, fontsize=4.5, label="Cynipidae (s. str.)", offset=0.01, offset.text=0.01) +
    geom_strip('Nordlandiella_sem', 'Parnips_nig', barsize=0, fontsize=4.5, label="Figitidae", offset=0.01, offset.text=0.02) +
    geom_strip('Diplolepis_bic', 'Pediaspis_aceris', barsize=0, fontsize=4.5, label="Diplolepididae", offset=0.01, offset.text=0.02) +
    geom_strip('Cecinothofagus_ibarrai', 'Paraulax_que', barsize=0, fontsize=4.5, label="Paraulacidae", offset=0.0, offset.text=0.02) +
    ggexpand(hexp, direction = 1, side = "h")
}

t1 <- read.tree("../iqtree/extended_UCE/GblockTreeModelEst.treefile")

p1 <- draw_tree(t1, 0.05, TRUE, TRUE, 0.1)

p1

ggsave("Fig_S7.svg", device="svg")


