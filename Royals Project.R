pitchdata<-read.csv("/Users/matthewheath/Downloads/PitcherLeaderboardData.csv")
duplicates<-pitchdata[duplicated(pitchdata),]
pitchdata<-pitchdata[!duplicated(pitchdata),]
#identified and removed duplicate values
table(pitchdata$Name)

pitchtype<-split(pitchdata,pitchdata$pitch_type)
brk<-as.data.frame(pitchtype$BRK)
ch<-as.data.frame(pitchtype$CH)
fb<-as.data.frame(pitchtype$FB)
#converted the list into separate lists for each pitch type

fbVelo<-aggregate(velo~Name, fb, mean)
brkVelo<-aggregate(velo~Name, brk, mean)
chVelo<-aggregate(velo~Name, ch, mean)
#computed average velocities for each pitcher

fbsr<-aggregate(spin_rate~Name, fb, mean)
brksr<-aggregate(spin_rate~Name, brk, mean)
chsr<-aggregate(spin_rate~Name, ch, mean)
#computed average spin rates for each pitcher

fbext<-aggregate(extension~Name, fb, mean)
brkext<-aggregate(extension~Name, brk, mean)
chext<-aggregate(extension~Name, ch, mean)
#computed average extension for each pitcher

library(dplyr)
fbv15<-fbVelo %>% top_n(15,velo) %>% arrange(desc(velo))
brkv15<-brkVelo %>% top_n(15,velo) %>% arrange(desc(velo))
chv15<-chVelo %>% top_n(15,velo) %>% arrange(desc(velo))
Velo15<-as.data.frame(cbind(fbv15,brkv15,chv15))
#found top 15 average velocities

library(ggplot2)
library(gridExtra)
fbvPlot<-ggplot(data = fbv15, aes(x=reorder(Name, velo), y=velo))+geom_bar(stat="identity", fill="blue")+xlab("Name")+ylab("Velocity")+ggtitle("Fastball")+geom_text(aes(label=round(velo, digits=5), hjust=0), size=2)+coord_flip(ylim = c(90,96.5))
brkvPlot<-ggplot(data = brkv15, aes(x=reorder(Name, velo), y=velo))+geom_bar(stat="identity", fill="gold")+xlab("Name")+ylab("Velocity")+ggtitle("Breaking Ball")+geom_text(aes(label=round(velo, digits=5), hjust=0), size=2)+coord_flip(ylim = c(80,88))
chvPlot<-ggplot(data = chv15, aes(x=reorder(Name, velo), y=velo))+geom_bar(stat="identity")+xlab("Name")+ylab("Velocity")+ggtitle("Changeup")+geom_text(aes(label=round(velo, digits=5), hjust=0), size=2)+coord_flip(ylim = c(80,90.5))
grid.arrange(fbvPlot,brkvPlot,chvPlot, ncol=3, top="Velocity Leaderboard")
#displayed average velocities with bar charts

fbs15<-fbsr %>% top_n(15,spin_rate) %>% arrange(desc(spin_rate))
brks15<-brksr %>% top_n(15,spin_rate) %>% arrange(desc(spin_rate))
chs15<-chsr %>% top_n(15,spin_rate) %>% arrange(desc(spin_rate))
spin_rate15<-as.data.frame(cbind(fbs15,brks15,chs15))
#found top 15 average spin rates

fbsPlot<-ggplot(data = fbs15, aes(x=reorder(Name, spin_rate), y=spin_rate))+geom_bar(stat="identity", fill="blue")+xlab("Name")+ylab("Spin Rate")+ggtitle("Fastball")+geom_text(aes(label=round(spin_rate, digits=5), hjust=0), size=2)+coord_flip(ylim = c(2100,2800))
brksPlot<-ggplot(data = brks15, aes(x=reorder(Name, spin_rate), y=spin_rate))+geom_bar(stat="identity", fill="gold")+xlab("Name")+ylab("Spin Rate")+ggtitle("Breaking Ball")+geom_text(aes(label=round(spin_rate, digits=5), hjust=0), size=2)+coord_flip(ylim = c(2100,3200))
chsPlot<-ggplot(data = chs15, aes(x=reorder(Name, spin_rate), y=spin_rate))+geom_bar(stat="identity")+xlab("Name")+ylab("Spin Rate")+ggtitle("Changeup")+geom_text(aes(label=round(spin_rate, digits=5), hjust=0), size=2)+coord_flip(ylim = c(1800,2700))
grid.arrange(fbsPlot,brksPlot,chsPlot, ncol=3, top="Spin Rate Leaderboard")
#displayed average spin rates with bar charts

fbe15<-fbext %>% top_n(15,extension) %>% arrange(desc(extension))
brke15<-brkext %>% top_n(15,extension) %>% arrange(desc(extension))
che15<-chext %>% top_n(15,extension) %>% arrange(desc(extension))
extension15<-as.data.frame(cbind(fbe15,brke15,che15))
#found top 15 average extensions

fbePlot<-ggplot(data = fbe15, aes(x=reorder(Name, extension), y=extension))+geom_bar(stat="identity", fill="blue")+xlab("Name")+ylab("Extension")+ggtitle("Fastball")+geom_text(aes(label=round(extension, digits=5), hjust=0), size=2)+coord_flip(ylim = c(6,7.5))
brkePlot<-ggplot(data = brke15, aes(x=reorder(Name, extension), y=extension))+geom_bar(stat="identity", fill="gold")+xlab("Name")+ylab("Extension")+ggtitle("Breaking Ball")+geom_text(aes(label=round(extension, digits=5), hjust=0), size=2)+coord_flip(ylim = c(5.5,7))
chePlot<-ggplot(data = che15, aes(x=reorder(Name, extension), y=extension))+geom_bar(stat="identity")+xlab("Name")+ylab("Extension")+ggtitle("Changeup")+geom_text(aes(label=round(extension, digits=5), hjust=0), size=2)+coord_flip(ylim = c(6,7))
grid.arrange(fbePlot,brkePlot,chePlot, ncol=3, top="Extension Leaderboard")
#displayed average extensions with bar charts


##Part 2
fbv<-fbVelo %>% arrange(desc(velo))
fbv$fbRank<-c(30:1)
brkv<-brkVelo %>% arrange(desc(velo))
brkv$brkRank<-c(30:1)
chv<-chVelo %>% arrange(desc(velo))
chv$chRank<-c(29:1)
#ranked average velocities of all pitchers

velo<-merge(fbv,chv,by="Name")
diff<-velo$velo.x-velo$velo.y
diff<-as.data.frame(diff)
diff$Name<-velo$Name
diff<- diff %>% arrange(desc(diff))
diff$diffRank<-c(29:1)
velo<-merge(velo,diff, by="Name")
#found average differences between velocities of fastball and changeup

fbs<-fbsr %>% arrange(desc(spin_rate))
fbs$fbsRank<-c(30:1)
brks<-brksr %>% arrange(desc(spin_rate))
brks$brksRank<-c(30:1)
chs<-chsr %>% arrange(desc(spin_rate))
chs$chsRank<-c(1:29)
#ranked average spin rates for all pitchers

fbspin<-merge(fbs,fbv, by="Name")
fbspin$Bauer<-fbspin$spin_rate/fbspin$velo
fbspin<-fbspin[-c(2:5)] %>% arrange(desc(fbspin$Bauer))
fbspin$fbsRank<-c(30:1)

brkspin<-merge(brks,brkv, by="Name")
brkspin$Bauer<-brkspin$spin_rate/brkspin$velo
brkspin<-brkspin[-c(2:5)] %>% arrange(desc(brkspin$Bauer))
brkspin$brksRank<-c(30:1)

chspin<-merge(chs,chv, by="Name")
chspin$Bauer<-chspin$spin_rate/chspin$velo
chspin<-chspin[-c(2:5)] %>% arrange(desc(chspin$Bauer))
chspin$chsRank<-c(29:1)
#normalized average spin rates for all three pitches

fbe<-fbext %>% arrange(desc(extension))
fbe$fbeRank<-c(30:1)
#ranked average extension on fastballs for all pitchers

Rank<-Reduce(function(x,y) merge(x = x, y = y, by = "Name"), 
             list(velo, fbspin, brkspin, chspin, fbe))
newRow<- data.frame(Name="Jim Bowers", velo.x= 93.93180, fbRank=28, velo.y=0, chRank=0, diff=0, diffRank=0, Bauer.x=25.23991, fbsRank=22, Bauer.y=32.17374, brksRank=24, Bauer=0, chsRank=0, extension=7.087771, fbeRank=30)
Rank<-rbind(Rank, newRow)
Rank$score<-Rank$fbRank+Rank$diffRank+Rank$fbsRank+Rank$brksRank+Rank$chsRank+Rank$fbeRank
Rank15<-Rank %>% top_n(15,score) %>% arrange(desc(score))
rankPlot<-ggplot(data = Rank15, aes(x=reorder(Name, score), y=score))+geom_bar(stat="identity", fill="blue")+xlab("Name")+ylab("Score")+ggtitle("Overall Ranking")+geom_text(aes(label=score, hjust=0), size=5)+coord_flip(ylim = c(50,150))
#Created Final Overall Pitcher Ranking

Fastball<-merge(fbVelo,fbsr)
Fastball<-merge(Fastball,fbext)
ggplot(Fastball, aes(x=velo,y=spin_rate))+geom_point()+geom_smooth(method='lm')+geom_text(aes(label=Name, hjust=-0.1), size=2.5)
lm(Fastball$velo~Fastball$spin_rate)
cor(Fastball$velo,Fastball$spin_rate)^2


