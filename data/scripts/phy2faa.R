require(seqinr)

D <- read.csv("allgaps34_sets.csv")

prefix1 <- "../allgaps_34+_phylip_format/"
prefix2 <- "../allgaps_34+_faa_format/"

for (i in 1:length(D$file.ag))
{
    inFile <- paste(prefix1, D$file.ag[i], sep="")

    E <- read.alignment(inFile, format="phylip")
    for (j in 1:length(E$seq))
        E$seq[j] <- toupper(E$seq[j])

    outFile <- paste(prefix2, D$file[i], sep="")

    write.fasta(outFile, sequences=E$seq, names=E$nam)
}

prefix1 <- "../clustal_alignments_phylip_format/"
prefix2 <- "../clustal_34+_faa_format/"

for (i in 1:length(D$file))
{
    inFile <- paste(prefix1, D$file[i], sep="")

    E <- read.alignment(inFile, format="phylip")
    for (j in 1:length(E$seq))
        E$seq[j] <- toupper(E$seq[j])

    outFile <- paste(prefix2, D$file[i], sep="")

    write.fasta(outFile, sequences=E$seq, names=E$nam)
}

