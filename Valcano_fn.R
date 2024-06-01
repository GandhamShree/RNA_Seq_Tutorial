library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
df <- read_delim("For_valcanofn.txt", delim = "\t")
df <- df %>%
  mutate(padj = p.adjust(pvalue, method = "BH"))
df <- df %>%
  filter(!is.na(log2FoldChange) & !is.na(padj))
log2FoldChange_limit <- 10
padj_limit <- 1e-300
df <- df %>%
  filter(abs(log2FoldChange) <= log2FoldChange_limit & padj >= padj_limit)
df <- df %>%
  mutate(
    regulation = case_when(
      padj < 0.05 & log2FoldChange > 2 ~ "Up-regulated",
      padj < 0.05 & log2FoldChange < -2 ~ "Down-regulated",
      TRUE ~ "Not significant"
    )
  )
genes_to_annotate <- c("SOX9", "FST", "PDPN")
volcano_plot <- ggplot(df, aes(x = log2FoldChange, y = -log10(padj), color = regulation)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_manual(values = c("Not significant" = "grey", "Up-regulated" = "red", "Down-regulated" = "green4")) +
  theme_minimal() +
  labs(
    title = "Volcano Plot of Differentially Expressed Genes",
    x = expression(Log[2]~Fold~Change),
    y = expression(-Log[10]~Adjusted~P-value)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "right",
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
    legend.text = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),  
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12,face = "bold")
    #axis.title = element_text(face = "bold",size = 12),axis.text = element_text(size = 12,face = "bold")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
geom_text_repel(
  data = df %>% filter(Genes %in% genes_to_annotate),
  aes(label = Genes),
  box.padding = 0.5,
  point.padding = 0.5,
  segment.color = 'grey50',
  max.overlaps = Inf,
  fontface = "bold",
  size = 3
)
print(volcano_plot)
ggsave("volcano_plot.pdf", plot = volcano_plot, width = 8, height = 6)

