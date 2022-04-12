# Read in results from hmm_ag analysis

D <- read.table("clustal34_hmm_ag_sets.tsv")

# Create six partitions based on clustal omega alignments, filtered with hmmcleaner and then with gblocks clustal
# The boundaries used below create six partitions with approx 32,574 aa sites in each

cat(file="clustal34_hmm_ag_lt0.165.list", D$file.hmm.ag[D$hmm_ag_trim_prop<0.165], sep="\n")
cat(file="clustal34_hmm_ag_0.165-0.288.list", D$file.hmm.ag[D$hmm_ag_trim_prop>=0.165 & D$hmm_ag_trim_prop<0.288], sep="\n")
cat(file="clustal34_hmm_ag_0.288-0.396.list", D$file.hmm.ag[D$hmm_ag_trim_prop>=0.288 & D$hmm_ag_trim_prop<0.396], sep="\n")
cat(file="clustal34_hmm_ag_0.396-0.522.list", D$file.hmm.ag[D$hmm_ag_trim_prop>=0.396 & D$hmm_ag_trim_prop<0.522], sep="\n")
cat(file="clustal34_hmm_ag_0.522-0.641.list", D$file.hmm.ag[D$hmm_ag_trim_prop>=0.522 & D$hmm_ag_trim_prop<0.641], sep="\n")
cat(file="clustal34_hmm_ag_gte0.641.list", D$file.hmm.ag[D$hmm_ag_trim_prop>=0.641], sep="\n")

