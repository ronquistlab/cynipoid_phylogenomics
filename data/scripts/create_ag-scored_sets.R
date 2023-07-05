# Read in basic data
D <- read.csv("allgaps34_sets.csv")

# Create six partitions of clustal omega alignments based on trim proportions of gblocks allgaps filtering
# but do not filter the alignments with gblocks
# The boundaries used below create six partitions with approx 35,000 aa sites in each after gblocks filtering

cat(file="clustal34_ag-scored_lt0.13.list", D$file.ag[D$trim_prop<0.13], sep="\n")
cat(file="clustal34_ag-scored_0.13-0.26.list", D$file.ag[D$trim_prop>=0.13 & D$trim_prop<0.26], sep="\n")
cat(file="clustal34_ag-scored_0.26-0.37.list", D$file.ag[D$trim_prop>=0.26 & D$trim_prop<0.37], sep="\n")
cat(file="clustal34_ag-scored_0.37-0.47.list", D$file.ag[D$trim_prop>=0.37 & D$trim_prop<0.47], sep="\n")
cat(file="clustal34_ag-scored_0.47-0.59.list", D$file.ag[D$trim_prop>=0.47 & D$trim_prop<0.59], sep="\n")
cat(file="clustal34_ag-scored_gte0.59.list", D$file.ag[D$trim_prop>=0.59], sep="\n")

