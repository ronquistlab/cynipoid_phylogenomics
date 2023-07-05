require(Biostrings)

D <- read.table("../clustal30_ag_fasta_format/alignment_files.txt")
colnames(D) <- "file.ag"

prefix1 <- "../clustal30_ag_fasta_format/"
prefix2 <- "../clustal30_ag_phylip_format/"

for (i in 1:length(D$file.ag))
{
    inFile <- paste0(prefix1, D$file.ag[i])

    E <- readAAMultipleAlignment(inFile)

    outFile <- paste0(prefix2, D$file.ag[i],".phy")

    write.phylip(E, outFile)
}

