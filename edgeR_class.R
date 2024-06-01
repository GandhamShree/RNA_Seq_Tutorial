library(edgeR)
counts <- read.table('FeatureCounts_final.txt', header = TRUE, row.names = 1)
countdata <- data.matrix(counts)
head(counts)
Design <- data.frame(row.names = colnames(counts), condition = c("Control", "Control","Control", "Treated", "Treated","Treated"), libType = c("paired-end", "paired-end","paired-end","paired-end","paired-end", "paired-end"))
dge <- DGEList(counts = countdata, group = Design$condition)
keep <- rowSums(cpm(dge) > 1) >= 2
dge <- dge[keep,]
dge <- calcNormFactors(dge)
head(dge)
design <- model.matrix(~condition, data = Design)
dge <- estimateGLMCommonDisp(dge, design)
dge <- estimateGLMTrendedDisp(dge, design)
dge <- estimateGLMTagwiseDisp(dge, design)
fit <- glmFit(dge, design)
lrt <- glmLRT(fit, contrast = c(0, 1))
DEGs <- topTags(lrt, n = Inf)$table
head(DEGs)
write.table(DEGs, "EdgeR_FC.txt", sep = "\t", quote = FALSE, col.names = TRUE)
