#!/usr/bin/python

#add the missing species in a phylip file

import argparse
import sys

default_spids=["Allox_ar", "Andr_cur", "Andr_gro", "Andr_qln", "Andr_qrm", "Aula_tav", "Aylax_hy", "Beloc_tr", "Bior_pal", 'Callas_no', 'Calli_sp', 'Cecin_ib', 'Cerop_ma', 'Diast_ki', 'Dipl_spi', 'Escha_ac', 'Gana_sp1', 'Hedic_le', 'Irae_his', 'Isoc_cen', 'Lepto_bo', 'Lepto_cl', 'Lepto_he', 'Parn_nig', 'Pedia_ac', 'Peric_JH', 'Phaen_vi', 'Phana_JH', 'Prot_spe', 'Qwaq_sco', 'Syne_gif', 'Syne_ito', 'Syne_jap', 'Syne_umb', "Nasoniav", "Micropld", "Orussusa"]

manual_1="nope"
manual_2="nope"

parser=argparse.ArgumentParser(description=manual_1, epilog=manual_2)
parser.add_argument("filelist", help="list of input phylip files")
parser.add_argument("--species", help="list of species to be in the final file", nargs="*", default="sample")
parser.add_argument("-o", "--outfile", nargs='?', type=argparse.FileType('w'), default=sys.stdout)
parser.add_argument("--log", nargs="?", type=argparse.FileType("w"), default=sys.stderr, help="write a log file")
args=parser.parse_args()

sys.stderr=args.log

def logentry(message): #useful if I later want fo add warnings and such
        args.log.write(message+"\n")

if args.species=="sample": #this is to decide if the species argument is ...
        spids=default_spids
elif len(args.species)==1: #... a file listing the species or ...
        with open(args.species[0]) as s:
                spids=[line.strip() for line in s.readlines() if line.strip()!=""]
else:
        spids=args.species #... a command line list

with open(args.filelist) as l: #create the list of input files
    files=[line.strip() for line in l.readlines() if line.strip()!=""]

o=args.outfile

def readstockholm(stk): #I might later add support for stockholm format
        with open(stk) as i:
                content=i.readlines()
        blocks=list()
        b=list()
        for line in content:
                if line[0]=="#" or line[0]=="/":
                        pass
                elif line.strip()=="":
                        if b!=[]:
                                blocks.append(b)
                                b=list()
                else:
                        b.append(line)
        return blocks

def readintlv(phy):
        "read an interleaved .phy file, returns the taxa ids and the alignment blocks"
        with open(phy) as i:
                content=i.readlines()
        blocks=list()
        b=list()
        alninfo=content[0].split()
        ntax=int(alninfo[0])
        nchar=int(alninfo[1])
        taxids=list()
        for line in content[1:ntax+1]: #first block has headers
                row=line.split()
                taxids.append(row[0])
                b.append("".join(row[1:])+"\n")
        for line in content[ntax+1:]: #blocks after the first have no headers
                if line.strip()=="": #empty lines separate blocks
                        if b!=[]: #don't add an empty block if there is an extra empty line
                                blocks.append(b) #add the last block to output
                                b=list()
                else:
                        if len(b)==ntax: #if block is full, we start a new one
                                blocks.append(b)
                                b=list()
                        b.append("".join(line.split())+"\n")
        if b!=[]: #the final block is added even if the file does not end in an empty line
                blocks.append(b)
        return taxids, blocks

def getallglen(files):
        genelengths=list()
        for f in files:
                gene=f.split(".")[0] #genename is filename without extension
                with open(f, "r") as i:
                        firstline=i.readline()
                        #n_taxa.append(int(firstline.split()[0]))
                        nchar=int(firstline.split()[1])
                        genelengths.append(nchar)
        return genelengths

def append_aln(phy):
        aln=readintlv(phy)
        headers=aln[0]
        blocks=aln[1]
        idindex=list()
        #seqs=["" for s in spids]
        for sp in spids:
                if sp in headers:
                        idindex.append(headers.index(sp))
                else:
                        idindex.append(-1)
        for b in blocks:
                blen=len(b[0])-1 #last char is newline
                for s in range(len(spids)):
                        i=idindex[s]
                        if i==-1:
                                o.write(("-"*blen)+"\n")
                        else:
                                o.write(b[i])
                o.write("\n")

def mergephy(o):
        o.write(str(len(spids)))
        o.write("\t")
        o.write(str(sum(genelengths)))
        o.write("\n")
        aln=readintlv(files[0])
        headers=aln[0]
        blocks=aln[1]
        idindex=list()
        blen=len(blocks[0][0])-1
        for sp in spids:
                if sp in headers:
                        i=headers.index(sp)
                        idindex.append(i)
                        o.write(sp+"\t"+blocks[0][i])
                else:
                        idindex.append(-1)
                        o.write(sp+"\t"+("-"*blen)+"\n")
        o.write("\n")
        for b in blocks[1:]:
                blen=len(b[0])-1
                for s in range(len(spids)):
                        i=idindex[s]
                        if i==-1:
                                o.write(("-"*blen)+"\n")
                        else:
                                o.write(b[i])
                o.write("\n")
        for phy in files[1:]:
                append_aln(phy)

genelengths=getallglen(files)
mergephy(o)
o.close()
