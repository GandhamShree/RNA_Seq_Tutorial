library(DESeq2)
library(ggplot2)
library(ggrepel)
pdf("dfn.pdf",width=6,height = 4)
counts <- read.table('FeatureCounts_final.txt', header = TRUE, row.names = 1)
class(counts)
head(counts)
countdata <- data.matrix(counts)
class(countdata)
countdata <- round(countdata)
Design <- data.frame(
  row.names = colnames(countdata),
  condition = c("Control", "Control", "Control", "Case", "Case", "Case"),
  libType = c("Single-end", "Single-end", "Single-end", "Single-end", "Single-end", "Single-end")
)
Design
dds <- DESeqDataSetFromMatrix(countData = countdata, colData = Design, design = ~ condition)
dds
vsd <- vst(dds, blind=FALSE)
pcaData <- plotPCA(vsd, intgroup = c("condition"), returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
custom_colors <- c("Control" = "dodgerblue", "Case" = "tomato")
pcaPlot <- ggplot(pcaData, aes(PC1, PC2, color = condition, label = name)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  coord_fixed() +theme(legend.position = "right",
      legend.background = element_rect(color = "black", 
                                       fill = "white", 
                                       size = 0.5, 
                                       linetype = "solid"),panel.border = element_rect(color = "black", fill = NA, size = 1))
p<-pcaPlot+ theme(axis.title=element_text(size=12,face="bold"),axis.text.x = element_text(size = 10,face="bold"),axis.text.y = element_text(size = 10,face="bold"),axis.line = element_blank())
print(p)
dev.off()





