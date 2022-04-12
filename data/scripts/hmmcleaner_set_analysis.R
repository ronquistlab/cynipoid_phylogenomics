D <- read.csv("allgaps34_sets.csv")

numDirtySites <- numeric( length(D$file) )
numDirtyTaxa  <- numeric( length(D$file) )

for ( i in 1:length(D$file) )
{
    inFile <- paste( "../allgaps_34+_faa_format/", D$file[i], ".tsv", sep="" )

    X <- read.table( inFile )
    colnames(X) <- c("file", "taxon", "sites")

    numDirtySites[i] <- sum( X$sites )
    numDirtyTaxa[i]  <- sum( X$sites > 0 )
}

D$allgaps_numDirtySites <- numDirtySites
D$allgaps_numDirtyTaxa  <- numDirtyTaxa
D$allgaps_propDirtySites <- D$allgaps_numDirtySites / (D$allgaps_nsites*D$ntaxa)
D$allgaps_propDirtyTaxa  <- D$allgaps_numDirtyTaxa / D$ntaxa

for ( i in 1:length(D$file) )
{
    inFile <- paste( "../clustal_34+_faa_format/", D$file[i], ".tsv", sep="" )

    X <- read.table( inFile )
    colnames(X) <- c("file", "taxon", "sites")

    numDirtySites[i] <- sum( X$sites )
    numDirtyTaxa[i]  <- sum( X$sites > 0 )
}

D$numDirtySites <- numDirtySites
D$numDirtyTaxa  <- numDirtyTaxa
D$propDirtySites <- D$numDirtySites / (D$nsites*D$ntaxa)
D$propDirtyTaxa  <- D$numDirtyTaxa / D$ntaxa

write.table(file="34_hmmsets.tsv", D)

