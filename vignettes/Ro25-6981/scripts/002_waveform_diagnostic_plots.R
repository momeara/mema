

library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(seriation)
library(viridis)
library(gplots)
load("intermediate_data/demo_waveform.Rdata")


#~~~~~~~~~~~~~~~~~~~~~~~~
# Simple lattice plot of the waveforms
ggplot(data=waveform) +
  theme_bw() +
  geom_line(mapping=aes(x=time_step, y=voltage)) +
  facet_wrap(~neuron_index) +
  ggtitle("Neuron waveform cluster mean") +
  scale_x_continuous("microsecond") +
  scale_y_continuous("Voltage")

ggsave("product/demo_waveforms_190513.pdf", width=10, height=10)
ggsave("product/demo_waveforms_190513.png", width=10, height=10)

#~~~~~~~~~~~~~~~~~~~~~~~~
correlations <- waveform %>%
  reshape2::acast(time_step ~ neuron_index, value.var="voltage") %>%
  cor()

d <- dist(correlations)
o_row <- seriate(d, method="OLO", control=NULL)[[1]]
args=list(
  trace="none",
  density.info="none",
  col=viridis::viridis(100),
  cexRow=1,
  cexCol=1,
  dendrogram="none",
  key=FALSE,
  keysize=0.03,
  x=correlations,
  Colv=as.dendrogram(o_row),
  Rowv=as.dendrogram(o_row))
pdf("product/demo_waveform_correlation_matrix_190513.pdf", heigh=6, width=6)
  do.call(gplots::heatmap.2,args=args)
dev.off()
png("product/demo_waveform_correlation_matrix_190513.png", heigh=600, width=600)
  do.call(gplots::heatmap.2,args=args)
dev.off()

