library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

#setwd("/home/rjdinis/development/R/mk_lte")
setwd("/home/rjdinis/development/R/mk_lte")

ftp_dl_4g <- read.table("FTP_DL_4G_All_v2.csv", header=TRUE, fill=TRUE, sep=',')

# Format table fields
ftp_dl_4g$DLBandWidth <- as.factor(ftp_dl_4g$DLBandWidth)


# Filter smaples with residual throughput
Throughput.DL.min.filter <- 10
ftp_dl_4g_ie <- select(ftp_dl_4g, RNC, Technology, DLBandWidth, RSRP=Serving.RSRP..dBm., RSRQ=Serving.RSRQ..dB., SNR=Serving.RS.SNR..dB., CQI=Rank1.CQI, BLER=PDSCH.BLER.Total...., Modulation=PDSCH.Modulation.CW0, MCS=AvgMCS, RB_DL=AvgRBsFrame, TM=TransmissionMode, Throughput.DL.MAC=MAC.DL.Throughput..kb.sec., Throughput.DL.RLC=RLC.DL.Throughput..kb.sec., Throughput.DL.PDCP=PDCP.DL.Throughput..kb.sec., Throughput.DL.APP=App.Throughput.DL..kbit.s.)
ftp_dl_4g_filter <- filter(ftp_dl_4g_ie, Throughput.DL.MAC > Throughput.DL.min.filter)


# Create RF Signal Histograms
(h1.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h2.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRQ)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h3.dl <- ggplot(ftp_dl_4g_filter, aes(x=SNR)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h4.dl <- ggplot(ftp_dl_4g_filter, aes(x=CQI)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(m1.dl <- arrangeGrob(h1.dl, h2.dl, h3.dl, h4.dl, ncol=2, nrow=2))
(ggsave(m1.dl, file=paste0("./Charts/Histograms_RF_DL.jpeg"), width=20, height=10, dpi=100))


# Create Scheduler Histograms
(h5.dl <- ggplot(ftp_dl_4g_filter, aes(x=MCS)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h6.dl <- ggplot(ftp_dl_4g_filter, aes(x=factor(TM))) + geom_histogram(aes(fill = ..count..), rm.na=TRUE) + labs(x="Transmission Mode") + scale_fill_gradient("Count", low = "green", high = "red"))
(h7.dl <- ggplot(ftp_dl_4g_filter, aes(x=Modulation)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h8.dl <- ggplot(ftp_dl_4g_ie, aes(x=RB_DL)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(m2.dl <- arrangeGrob(h5.dl, h6.dl, h7.dl, h8.dl, ncol=2, nrow=2))
ggsave(m2.dl, file=paste0("./Charts/Histograms_Scheduler_DL.jpeg"), width=20, height=10, dpi=100)


# Create Throughput Histograms
(h9.dl <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.MAC)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h10.dl <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.RLC)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
#(h11.dl <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.PDCP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h12.dl <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.APP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(h13.dl <- ggplot(ftp_dl_4g_filter, aes(x=BLER)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red"))
(m3.dl <- arrangeGrob(h9.dl, h10.dl, h12.dl, h13.dl, ncol=2, nrow=2))
ggsave(m3.dl, file=paste0("./Charts/Histograms_Throughput_DL.jpeg"), width=20, height=10, dpi=100)


# Create Bandwidth Histograms
(h20.dl <- ggplot(ftp_dl_4g_filter, aes(x=factor(DLBandWidth))) + geom_histogram(fill="red", rm.na=TRUE) + labs(x="Bandwidth (MHz)", y="Numeber of Samples"))
(h21.dl <-ggplot(ftp_dl_4g_filter, aes(x = DLBandWidth), na.omit(mydataf)) + geom_bar(aes(y = (..count..)/sum(..count..)), labels = percent_format(), fill="red") + scale_y_continuous(breaks=seq(0, 1, 0.2), limits=c(0, 1)) + labs(x="Bandwidth (MHz)", y="Percentage of Samples"))
(m4.dl <- arrangeGrob(h20.dl, h21.dl, ncol=2, nrow=1))
ggsave(m4.dl, file=paste0("./Charts/Histograms_Bandwidth_DL.jpeg"), width=20, height=10, dpi=100)


# Create RF Signal Scatter charts
(c1.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=RSRQ)) + geom_point(colour="red", size=0.9))
(c2.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=SNR)) + geom_point(colour="red", size=0.9))
(c3.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=CQI)) + geom_point(colour="red", size=1))
(m5.dl <- arrangeGrob(c1.dl, c2.dl, c3.dl, ncol=3, nrow=1))
ggsave(m5.dl, file=paste0("./Charts/Scatter_RF_DL.jpeg"), width=20, height=10, dpi=100)

# Create RF Thrgouhput Scatter charts
(s1.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=Throughput.DL.MAC)) + geom_point(colour="red", size=0.9))
(s2.dl <- ggplot(ftp_dl_4g_filter, aes(x=RSRQ, y=Throughput.DL.MAC)) + geom_point(colour="red", size=0.9))
(s3.dl <- ggplot(ftp_dl_4g_filter, aes(x=SNR, y=Throughput.DL.MAC)) + geom_point(colour="red", size=0.9))
(s4.dl <- ggplot(ftp_dl_4g_filter, aes(x=CQI, y=Throughput.DL.MAC)) + geom_point(colour="red", size=0.9))
(s5.dl <- ggplot(ftp_dl_4g_filter, aes(x=MCS, y=Throughput.DL.MAC)) + geom_point(colour="red", size=0.9))
(s6.dl <- ggplot(ftp_dl_4g_filter, aes(x=RB_DL, y=Throughput.DL.MAC)) + geom_point(colour="red", size=1))
(m6.dl <- arrangeGrob(s1.dl, s2.dl, s3.dl, s4.dl, s5.dl, s6.dl, ncol=3, nrow=2))
ggsave(m6.dl, file=paste0("./Charts/Scatter_Throughput_DL.jpeg"), width=20, height=10, dpi=100)


# Create Interference Scatter charts
(s10.dl <- ggplot(ftp_dl_4g_filter, aes(x=SNR, y=MCS)) + geom_point(colour="red", size=1))
(s11.dl <- ggplot(ftp_dl_4g_filter, aes(x=CQI, y=SNR)) + geom_point(colour="red", size=1))
(s12.dl <- ggplot(ftp_dl_4g_filter, aes(x=SNR, y=BLER)) + geom_point(colour="red", size=1))
(m7.dl <- arrangeGrob(s10.dl, s11.dl, s12.dl, ncol=3, nrow=1))
ggsave(m7.dl, file=paste0("./Charts/Scatter_Interference_DL.jpeg"), width=20, height=10, dpi=100)


# Create Throughput Cuts
(s20.dl <- ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(colour="red", size=1) + facet_wrap(~ Modulation))
ggsave(s20.dl, file=paste0("./Charts/Scatter_Throughput_DL_by_Modulation.jpeg"), width=20, height=10, dpi=100)
(s21.dl <- ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(colour="red", size=1) + facet_wrap(~ DLBandWidth))
ggsave(s21.dl, file=paste0("./Charts/Scatter_Throughput_DL_by_Bandwidt.jpeg"), width=20, height=10, dpi=100)
(s22.dl <- ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(colour="red", size=1) + facet_wrap(~ Technology))
ggsave(s.22.dl, file=paste0("./Charts/Scatter_Throughput_DL_by_Technology.jpeg"), width=20, height=10, dpi=100)
(s23.dl <- ggplot(ftp_dl_4g_filter, aes(x=SNR, y=Throughput.DL.MAC)) + geom_point(colour="red", size=1) + facet_wrap(~ RNC, ncol=2))
ggsave(s23.dl, file=paste0("./Charts/Scatter_Throughput_DL_by_RNC.jpeg"), width=20, height=10, dpi=100)

# Other charts
ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.MAC, y=BLER)) + geom_point(colour="red", size=0.9)
ggplot(ftp_dl_4g_filter, aes(x=SNR, y=BLER)) + geom_point(colour="red", size=0.9)


# Create table with count and percentage
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
}
ag.DLBandwidth <- tblFun(ftp_dl_4g$DLBandWidth)

