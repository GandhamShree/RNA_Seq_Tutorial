library(DESeq2)
counts <- read.table('FeatureCounts_final.txt', header=TRUE, row.names=1)
class(counts)
head(counts)
countdata=data.matrix(counts)
class(countdata)
Design <- data.frame(row.names =colnames(counts), condition = c("Control", "Control","Control", "Treated", "Treated","Treated"), libType = c("paired-end", "paired-end","paired-end","paired-end","paired-end", "paired-end"))
Design
dds <- DESeqDataSetFromMatrix(countData = round(countdata),colData = Design,design = ~condition)
dds
dds <- DESeq(dds)
res <- results(dds)
resOrdered <- res[order(res$padj),]
head(resOrdered)
write.table(resOrdered,"DESeq2_FC.txt",sep="\t",quote=F,col.names=T)
