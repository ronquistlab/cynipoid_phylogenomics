# Script for analyzing odseq quality scores

require(seqinr)
require(Biostrings)
require(msa)
require(odseq)

D <- read.csv("allgaps34_sets.csv",header=TRUE) 

D$odseq <- numeric(length(D$file))
for (i in 1:length(D$file))
{
    cat ("Now processing file ", i, "\n")

    fileName <- paste ("../allgaps_34+_phylip_format/", D$file.ag[i], sep="")

    cat ("File is ", fileName, "\n")
    X <- read.alignment( fileName, format="phylip" )

    Y <- AAMultipleAlignment(as.character(X$seq))
    Z <- odseq(Y, distance_metric = "affine", B = 1000, threshold = 0.025)

    D$odseq[i] <- sum(Z)
}

write.csv(D,file="allgaps34_odseq.csv")

