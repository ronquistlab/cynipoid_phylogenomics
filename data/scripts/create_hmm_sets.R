# Read in results from hmmcleaner analysis

D <- read.table("34_hmmsets.tsv")

# Create six partitions based on hmmcleaner results for unfiltered clustal omega alignments
# The boundaries used below create six partitions with approx 35,000 aa sites in each

cat(file="allgaps34_hmm-uf_lt0.018.list", D$file.ag[D$propDirtySites<0.018], sep="\n")
cat(file="allgaps34_hmm-uf_0.018-0.035.list", D$file.ag[D$propDirtySites>=0.018 & D$propDirtySites<0.035], sep="\n")
cat(file="allgaps34_hmm-uf_0.035-0.049.list", D$file.ag[D$propDirtySites>=0.035 & D$propDirtySites<0.049], sep="\n")
cat(file="allgaps34_hmm-uf_0.049-0.074.list", D$file.ag[D$propDirtySites>=0.049 & D$propDirtySites<0.074], sep="\n")
cat(file="allgaps34_hmm-uf_0.074-0.110.list", D$file.ag[D$propDirtySites>=0.074 & D$propDirtySites<0.110], sep="\n")
cat(file="allgaps34_hmm-uf_gte0.110.list", D$file.ag[D$propDirtySites>=0.110], sep="\n")

