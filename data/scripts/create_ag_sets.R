# Read in basic data
D <- read.csv("allgaps34_sets.csv")

# Create six partitions based on trim proportions of gblocks allgaps filtering of clustal alignments
# The boundaries used below create six partitions with approx 35,000 aa sites in each

cat(file="clustal34_ag_lt0.13.list", D$file.ag[D$trim_prop<0.13], sep="\n")
cat(file="clustal34_ag_0.13-0.26.list", D$file.ag[D$trim_prop>=0.13 & D$trim_prop<0.26], sep="\n")
cat(file="clustal34_ag_0.26-0.37.list", D$file.ag[D$trim_prop>=0.26 & D$trim_prop<0.37], sep="\n")
cat(file="clustal34_ag_0.37-0.47.list", D$file.ag[D$trim_prop>=0.37 & D$trim_prop<0.47], sep="\n")
cat(file="clustal34_ag_0.47-0.59.list", D$file.ag[D$trim_prop>=0.47 & D$trim_prop<0.59], sep="\n")
cat(file="clustal34_ag_gte0.59.list", D$file.ag[D$trim_prop>=0.59], sep="\n")

