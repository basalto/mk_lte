library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

#setwd("/home/rjdinis/development/R/mk_lte")
setwd("/home/rjdinis/development/R/mk_lte")

# Importar ficheiro com dados Raw do FTP UL 4G
ftp_ul_4g <- read.table("FTP_UL_4G_All_v2.csv", header=TRUE, fill=TRUE, sep=',')

# Corrige variaveis
ftp_ul_4g$NumRBs <- ftp_ul_4g$NumRBs/1000

ftp_ul_4g$DLBandWidth <- factor(ftp_ul_4g$DLBandWidth)
ftp_ul_4g$Technology <- factor(ftp_ul_4g$Technology)
ftp_ul_4g$TM <- factor(ftp_ul_4g$TM)


# Falta BLER UL, Modulation UL, MCS UL
Throughput.UL.min.filter <- 100
ftp_ul_4g_ie <- select(ftp_ul_4g, RNC, Technology, DLBandWidth, RSRP=Serving.RSRP..dBm., RSRQ=Serving.RSRQ..dB., SNR=Serving.RS.SNR..dB., CQI=Rank1.CQI, AvgMCS, RB.UL=NumRBs, MCS.UL = AvgMCS, PuschTxPower, TxPower=Tx.Power..dBm., RxPower=Rx.Power..dBm., Throughput.UL.MAC=MAC.UL.Throughput..kb.sec., Throughput.UL.RLC=RLC.UL.Throughput..kb.sec., Throughput.UL.PDCP=PDCP.UL.Throughput..kb.sec., Throughput.UL.APP=App.Throughput.UL..kbit.s.)
ftp_ul_4g_filter <- filter(ftp_ul_4g_ie, Throughput.UL.MAC > Throughput.UL.min.filter)


(h1.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h2.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRQ)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h3.ul <- ggplot(ftp_ul_4g_filter, aes(x=SNR)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h4.ul <- ggplot(ftp_ul_4g_filter, aes(x=CQI)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(m1.ul <- arrangeGrob(h1.ul, h2.ul, h3.ul, h4.ul, ncol=2, nrow=2))
(ggsave(m1, file=paste0("./Charts/Histograms_RF_UL.jpeg"), width=20, height=10, dpi=100))


(h5.ul <- ggplot(ftp_ul_4g_filter, aes(x=PuschTxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h6.ul <- ggplot(ftp_ul_4g_filter, aes(x=TxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h7.ul <- ggplot(ftp_ul_4g_filter, aes(x=RxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(m2.ul <- arrangeGrob(h5.ul, h6.ul, h7.ul, ncol=3, nrow=1))
(ggsave(m2.ul, file=paste0("./Charts/Histograms_Power_UL.jpeg"), width=20, height=10, dpi=100))


(h9.ul <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.MAC)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h10.ul <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.RLC)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
#(h11.ul <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.PDCP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h12.ul <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.APP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(m3.ul <- arrangeGrob(h9.ul, h10.ul, h12.ul, ncol=3, nrow=1))
(ggsave(m3.ul, file=paste0("./Charts/Histograms_Throughput_UL.jpeg"), width=20, height=10, dpi=100))


(h13.ul <- ggplot(ftp_ul_4g_filter, aes(x=RB.UL)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(h14.ul <- ggplot(ftp_ul_4g_filter, aes(x=MCS.UL)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red"))
(m4.ul <- arrangeGrob(h13.ul, h14, ncol=2, nrow=1))
(ggsave(m4.ul, file=paste0("./Charts/Histograms_Scheduler_UL.jpeg"), width=20, height=10, dpi=100))


# Create Bandwidth Histograms
(h15.ul <- ggplot(ftp_ul_4g_filter, aes(x=factor(DLBandWidth))) + geom_histogram(fill="red", rm.na=TRUE) + labs(x="Bandwidth (MHz)", y="Numeber of Samples"))
(h16.ul <-ggplot(ftp_ul_4g_filter, aes(x = DLBandWidth), na.omit(mydataf)) + geom_bar(aes(y = (..count..)/sum(..count..)), labels = percent_format(), fill="red") + scale_y_continuous(breaks=seq(0, 1, 0.2), limits=c(0, 1)) + labs(x="Bandwidth (MHz)", y="Percentage of Samples"))
(m5.ul <- arrangeGrob(h15.ul, h16.ul, ncol=2, nrow=1))
ggsave(m5.ul, file=paste0("./Charts/Histograms_Bandwidth_UL.jpeg"), width=20, height=10, dpi=100)


(c1.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=RSRQ)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(c2.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=SNR)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(c3.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=CQI)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(c4.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=PuschTxPower)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(m6.ul <- arrangeGrob(c1.ul, c2.ul, c3.ul, c4.ul, ncol=2, nrow=2))
(ggsave(m6.ul, file=paste0("./Charts/Scatter_RF_UL.jpeg"), width=20, height=10, dpi=100))


(s1.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(s2.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRQ, y=Throughput.UL.MAC)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(s3.ul <- ggplot(ftp_ul_4g_filter, aes(x=SNR, y=Throughput.UL.MAC)) + geom_point(colour="red", size=0.9, na.rm = TRUE))
(s4.ul <- ggplot(ftp_ul_4g_filter, aes(x=PuschTxPower, y=Throughput.UL.MAC)) + geom_jitter(colour="red", size=0.9, na.rm = TRUE))
(m7.ul <- arrangeGrob(s1.ul, s2.ul, s3.ul, s4.ul, ncol=2, nrow=2))
(ggsave(m7.ul, file=paste0("./Charts/Scatter_Throughput_UL.jpeg"), width=20, height=10, dpi=100))


# Create Throughput cuts
(s5.ul <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC)) + geom_point(colour="red", size=1) + facet_wrap(~ RNC, ncol=2))
(ggsave(s5.ul, file=paste0("./Charts/Scatter_Throughput_UL_by_RNC.jpeg"), width=20, height=10, dpi=100))


# Other charts
ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour=Throughput.UL.MAC)) + geom_jitter(size=1.5, na.rm = TRUE) + facet_wrap(~ DLBandWidth)
ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour="red")) + geom_jitter(size=1.5, na.rm = TRUE) + facet_wrap(~ Technology) + guides(colour=FALSE)

ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour="red")) + geom_jitter(na.rm = TRUE) + facet_wrap(~ RNC, ncol=2) + guides(colour=FALSE)
#ggsave(m1, file="Throughput.jpeg", path="/home/rjdinis/development/R/Throughput_4G/Charts")


#+ scale_y_continuous(labels = percent_format())
#+ geom_text(aes(label = DLBandWidth_label))


# Example of Bar with labels
# df <- data.frame(x=factor(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)))
# dfTab <- as.data.frame(table(df))
# colnames(dfTab)[1] <- "x"
# dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))
# dfTab$lab <- paste(dfTab$Freq,paste("(",dfTab$lab,"%)",sep=""),sep=" ")
# ggplot(df) + geom_bar(aes(x,fill=x)) + 
#   geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
#   theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),legend.title=element_blank(),
#        axis.title.y=element_blank())


# Example of table with freq and p
# perc <- function(x) x/count(x)
# ddply(ftp_ul_4g, .(DLBandWidth), transform, percentage=perc(DLBandWidth))
# ddply(ftp_ul_4g, .(AvgMCS), transform, percentage=perc(AvgMCS))

#cbind(freq = table(ftp_ul_4g$DLBandWidth), percentage = prop.table(table(ftp_ul_4g$DLBandWidth))*100)
#+ scale_y_continuous(labels = percent_format())