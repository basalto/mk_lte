#library(XLConnect)
#library(gdata)
library(ggplot2)
library(scatterplot3d)
library(plyr)
library(dplyr)

setwd("/home/rjdinis/development/R/mk_lte")

# Importar function para criar multiplots
source("multiplot.R")

# Importar ficheiro com dados Raw do FTP UL 4G
ftp_ul_4g <- read.table("/home/rjdinis/development/R/Throughput_4G/FTP_UL_4G_All_v2.csv", header=TRUE, fill=TRUE, sep=',')

# Corrige variaveis
ftp_ul_4g$NumRBs <- ftp_ul_4g$NumRBs/1000

ftp_ul_4g$DLBandWidth <- factor(ftp_ul_4g$DLBandWidth)
ftp_ul_4g$Technology <- factor(ftp_ul_4g$Technology)
ftp_ul_4g$TM <- factor(ftp_ul_4g$TM)


# Falta BLER UL, Modulation UL, MCS UL
ftp_ul_4g_ie <- select(ftp_ul_4g, RNC, Technology, DLBandWidth, RSRP=Serving.RSRP..dBm., RSRQ=Serving.RSRQ..dB., SNR=Serving.RS.SNR..dB., CQI=Rank1.CQI, AvgMCS, B_UL=NumRBs, PuschTxPower, TxPower=Tx.Power..dBm., RxPower=Rx.Power..dBm., Throughput.UL.MAC=MAC.UL.Throughput..kb.sec., Throughput.UL.RLC=RLC.UL.Throughput..kb.sec., Throughput.UL.PDCP=PDCP.UL.Throughput..kb.sec., Throughput.UL.APP=App.Throughput.UL..kbit.s.)


# Filtrar amostras para remover pontos com Throughput  a 100
ftp_ul_4g_filter <- filter(ftp_ul_4g_ie, Throughput.UL.MAC > 100)


h1 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h2 <- ggplot(ftp_ul_4g_filter, aes(x=RSRQ)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h3 <- ggplot(ftp_ul_4g_filter, aes(x=SNR)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h4 <- ggplot(ftp_ul_4g_filter, aes(x=CQI)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h1, h2, h3, h4, cols=2)


h5 <- ggplot(ftp_ul_4g_filter, aes(x=PuschTxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h6 <- ggplot(ftp_ul_4g_filter, aes(x=TxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h7 <- ggplot(ftp_ul_4g_filter, aes(x=RxPower)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h5, h6, h7, cols=3)

h9 <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.MAC)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h10 <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.RLC)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h11 <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.PDCP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h12 <- ggplot(ftp_ul_4g_filter, aes(x=Throughput.UL.APP)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
multiplot(h9, h10, h12, cols=3)


h13 <- ggplot(ftp_ul_4g_filter, aes(x=RB_UL)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h14 <- ggplot(ftp_ul_4g_filter, aes(x=AvgMCS)) + geom_histogram(aes(fill = ..count..), na.rm = TRUE) + scale_fill_gradient("Count", low = "green", high = "red")
h15 <- ggplot(ftp_ul_4g) +  geom_bar(aes(x=DLBandWidth), fill = "red")
multiplot(h13, h14, h15, cols=3)


c1 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=RSRQ)) + geom_point(na.rm = TRUE) + geom_density2d()
c2 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=SNR)) + geom_point(na.rm = TRUE) + geom_density2d()
c3 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=CQI)) + geom_point(na.rm = TRUE) + geom_density2d()
c4 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=PuschTxPower)) + geom_point(na.rm = TRUE) + geom_density2d()
multiplot(c1, c2, c3, c4, cols=2)

s1 <- ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC)) + geom_point(na.rm = TRUE) + geom_density2d()
s2 <- ggplot(ftp_ul_4g_filter, aes(x=RSRQ, y=Throughput.UL.MAC)) + geom_point(na.rm = TRUE) + geom_density2d()
s3 <- ggplot(ftp_ul_4g_filter, aes(x=SNR, y=Throughput.UL.MAC)) + geom_point(na.rm = TRUE) + geom_density2d()
s4 <- ggplot(ftp_ul_4g_filter, aes(x=PuschTxPower, y=Throughput.UL.MAC)) + geom_jitter(na.rm = TRUE)
multiplot(s1, s2, s3, s4, cols=2)

ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour=Throughput.UL.MAC)) + geom_jitter(size=1.5, na.rm = TRUE) + facet_wrap(~ DLBandWidth)
ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour="red")) + geom_jitter(size=1.5, na.rm = TRUE) + facet_wrap(~ Technology) + guides(colour=FALSE)

ggplot(ftp_ul_4g_filter, aes(x=RSRP, y=Throughput.UL.MAC, colour="red")) + geom_jitter(na.rm = TRUE) + facet_wrap(~ RNC, ncol=2) + guides(colour=FALSE)
#ggsave(m1, file="Throughput.jpeg", path="/home/rjdinis/development/R/Throughput_4G/Charts")

#s3d <- scatterplot3d(ftp_ul_4g_ie$RSRP,ftp_ul_4g_ie$PuschTxPower,ftp_ul_4g_ie$Throughput.UL.MAC, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")
#fit <- lm(ftp_ul_4g_ie$RSRP ~ ftp_ul_4g_ie$PuschTxPower+ftp_ul_4g_ie$Throughput.UL.MAC)
#s3d$plane3d(fit)

library(scales)
DLBandwidth_label <- ddply(ftp_ul_4g, .(DLBandWidth), summarize, y=length(DLBandWidth))
ggplot(ftp_ul_4g) +
  geom_bar(aes(x=DLBandWidth), fill = "red")

#+ scale_y_continuous(labels = percent_format())
#+ geom_text(aes(label = DLBandWidth_label))


# Example of Bar with labels
df <- data.frame(x=factor(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)))
dfTab <- as.data.frame(table(df))
colnames(dfTab)[1] <- "x"
dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))
dfTab$lab <- paste(dfTab$Freq,paste("(",dfTab$lab,"%)",sep=""),sep=" ")
ggplot(df) + geom_bar(aes(x,fill=x)) + 
  geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
  theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
       axis.title.x=element_blank(),legend.title=element_blank(),
       axis.title.y=element_blank())


# Example of table with freq and p
perc <- function(x) x/count(x)
ddply(ftp_ul_4g, .(DLBandWidth), transform, percentage=perc(DLBandWidth))
ddply(ftp_ul_4g, .(AvgMCS), transform, percentage=perc(AvgMCS))

#cbind(freq = table(ftp_ul_4g$DLBandWidth), percentage = prop.table(table(ftp_ul_4g$DLBandWidth))*100)
#+ scale_y_continuous(labels = percent_format())