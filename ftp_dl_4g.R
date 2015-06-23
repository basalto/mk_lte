#library(XLConnect)
#library(gdata)
library(ggplot2)
library(scatterplot3d)
library(dplyr)

setwd("/home/rjdinis/development/R/mk_lte")
source("multiplot.R")

ftp_dl_4g <- read.table("/home/rjdinis/development/R/Throughput_4G/FTP_DL_4G_All_v2.csv", header=TRUE, fill=TRUE, sep=',')

#wb = loadWorkbook("/home/rjdinis/development/R/Throughput_4G/FTP_DL_4G_All.xlsx")
#df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)
#theData <- readWorksheet(loadWorkbook("/home/rjdinis/development/R/Throughput_4G/FTP_DL_4G_All.xlsx"),sheet=1)
#df = read.xls ("/home/rjdinis/development/R/Throughput_4G/FTP_DL_4G_All.xlsx"), sheet = 1, header = TRUE)


ftp_dl_4g_ie <- select(ftp_dl_4g, RNC, Technology, DLBandWidth, RSRP=Serving.RSRP..dBm., RSRQ=Serving.RSRQ..dB., SNR=Serving.RS.SNR..dB., CQI=Rank1.CQI, BLER=PDSCH.BLER.Total...., Modulation=PDSCH.Modulation.CW0, MCS=AvgMCS, RB_DL=AvgRBsFrame, TM=TransmissionMode, Throughput.DL.MAC=MAC.DL.Throughput..kb.sec., Throughput.DL.RLC=RLC.DL.Throughput..kb.sec., Throughput.DL.PDCP=PDCP.DL.Throughput..kb.sec., Throughput.DL.APP=App.Throughput.DL..kbit.s.)
ftp_dl_4g_filter <- filter(ftp_dl_4g_ie, Throughput.DL.MAC > 10)

h1 <- ggplot(ftp_dl_4g_filter, aes(x=RSRP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h2 <- ggplot(ftp_dl_4g_filter, aes(x=RSRQ)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h3 <- ggplot(ftp_dl_4g_filter, aes(x=SNR)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h4 <- ggplot(ftp_dl_4g_filter, aes(x=CQI)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h1, h2, h3, h4, cols=2)

h5 <- ggplot(ftp_dl_4g_filter, aes(x=MCS)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h6 <- ggplot(ftp_dl_4g_filter, aes(x=TM)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h7 <- ggplot(ftp_dl_4g_filter, aes(x=Modulation)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h8 <- ggplot(ftp_dl_4g_filter, aes(x=BLER)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h5, h6, h7, h8, cols=2)

h9 <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.MAC)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h10 <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.RLC)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h11 <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.PDCP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h12 <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.APP)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h9, h10, h11, h12, cols=2)

h20 <- ggplot(ftp_dl_4g, aes(x=DLBandWidth)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")
h21 <- ggplot(ftp_dl_4g_ie, aes(x=RB_DL)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red")


c1 <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=RSRQ)) + geom_point() + geom_density2d()
c2 <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=SNR)) + geom_point() + geom_density2d()
c3 <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=CQI)) + geom_point() + geom_density2d()
multiplot(c1, c2, c3, cols=3)

s1 <- ggplot(ftp_dl_4g_filter, aes(x=RSRP, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
s2 <- ggplot(ftp_dl_4g_filter, aes(x=RSRQ, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
s3 <- ggplot(ftp_dl_4g_filter, aes(x=SNR, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
s4 <- ggplot(ftp_dl_4g_filter, aes(x=CQI, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
s5 <- ggplot(ftp_dl_4g_filter, aes(x=MCS, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
s6 <- ggplot(ftp_dl_4g_filter, aes(x=Throughput.DL.MAC, y=BLER)) + geom_point() + geom_density2d()
multiplot(s1, s2, s3, s4, s5, s6, cols=3)

ggplot(ftp_dl_4g_filter, aes(x=SNR, y=Throughput.DL.MAC, cut=Modulation)) + geom_point() + geom_density2d()
ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(size=1) + facet_wrap(~ Modulation)
ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(size=1) + facet_wrap(~ DLBandWidth)
ggplot(ftp_dl_4g_ie, aes(x=SNR, y=Throughput.DL.MAC)) + geom_jitter(size=1) + facet_wrap(~ Technology)

ggplot(ftp_dl_4g_ie, aes(x=RB_DL, y=Throughput.DL.MAC)) + geom_point() + geom_density2d()
ggplot(ftp_dl_4g_filter, aes(x=SNR, y=Throughput.DL.MAC)) + geom_point() + facet_wrap(~ RNC, ncol=2)

s3d <- scatterplot3d(ftp_dl_4g_ie$SNR,ftp_dl_4g_ie$RB_DL,ftp_dl_4g_ie$Throughput.DL.MAC, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")
fit <- lm(ftp_dl_4g_ie$SNR ~ ftp_dl_4g_ie$RB_DL+ftp_dl_4g_ie$Throughput.DL.MAC)
s3d$plane3d(fit)
