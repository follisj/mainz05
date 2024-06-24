library(tidyverse)
library(rvest)
library(ggtext)
library(patchwork)
library(png)
library(cowplot)

###################################

#####. Create Data Set

###################################

mainz.games <- NULL
for (i in 2009:2023) {
  url <- paste0("https://fbref.com/en/squads/a224b06a/",i,"-",i+1,"/Mainz-05-Stats")
  xxx.mainz <- (url %>%
                  read_html() %>%
                  html_table(fill=T))[[2]] %>%
    filter(Comp=="Bundesliga") %>%
    mutate(points.game=case_when(
      Result=="W" ~ 3,
      Result=="L" ~ 0,
      Result=="D" ~ 1
    ),
    GF=as.numeric(GF),
    GA=as.numeric(GA),
    goal.diff=GF-GA,
    year=i) %>%
    mutate(W=ifelse(Result=="W",1,0),L=ifelse(Result=="L",1,0),D=ifelse(Result=="D",1,0)) %>%
    mutate(Wins=cumsum(W),Losses=cumsum(L),Draws=cumsum(D),WDL=paste0(Wins,"-",Draws,"-",Losses),
           Points=cumsum(points.game),Goals_For=cumsum(GF),Goals_Vs=cumsum(GA),Goal_D=cumsum(goal.diff)) %>%
    mutate(Match=as.numeric(str_extract(Round,"[0-9]+")),year2=as.factor(year))
  if(i < 2017) {
    mainz.games <- rbind(mainz.games,xxx.mainz)
  } else {
    mainz.games <- rbind(mainz.games,xxx.mainz %>% select(-xG,-xGA))
  }
}

season.filter = mainz.games %>% filter(Match==34) %>% select(year,Points) %>%
  slice(which.min(Points),which.max(Points),15) %>% distinct()

###################################

#####. Create tables

###################################

season=2009:2023

### first half records

xxx.tab <- mainz.games %>% filter(Match==17) %>%
  select(year,WDL,Points) %>%
  mutate(numid=1:length(season),numid2=1:length(season)) %>% 
  gather(numid,value,-c(numid,numid2)) %>% 
  mutate(xval=ifelse(numid=="year",1,ifelse(numid=="WDL",2,3)),
         value=ifelse(numid=="year",paste0(value,"-",str_sub(as.numeric(value)+1,3,4)),value)) %>%
  rbind(.,data.frame(numid2=c(16,16,16),numid=c("x","x","x"),value=c("Season","W-D-L","Points"),
                     xval=c(1,2,3))) %>%
  ggplot(aes(xval,numid2,label=value)) +
  geom_point(alpha=0)+
  geom_text(aes(label=value)) +
  theme_void() +
  scale_x_continuous(limits=c(.25,3.25))+
  labs(title="**First Half**",
       subtitle="*Matches 1-17*")+
  theme(plot.title=element_markdown(hjust=.5),
        plot.subtitle=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#f4f4f4"),
        panel.background = element_rect(fill="#f4f4f4",color="#f4f4f4")
  )

### second half records

xxx.tab2 <- cbind(mainz.games %>% filter(Match==17) %>% select(year,w17=Wins,d17=Draws,l17=Losses,p17=Points),
                  mainz.games %>% filter(Match==34) %>% select(w34=Wins,d34=Draws,l34=Losses,p34=Points)) %>%
  mutate(Wins=w34-w17,Draws=d34-d17,Losses=l34-l17,WDL=paste0(Wins,"-",Draws,"-",Losses),Points=p34-p17) %>%
  select(year,WDL,Points) %>%
  mutate(numid=1:length(season),numid2=1:length(season)) %>% 
  gather(numid,value,-c(numid,numid2)) %>% 
  mutate(xval=ifelse(numid=="year",1,ifelse(numid=="WDL",2,3)),
         value=ifelse(numid=="year",paste0(value,"-",str_sub(as.numeric(value)+1,3,4)),value)) %>%
  rbind(.,data.frame(numid2=c(16,16,16),numid=c("x","x","x"),value=c("Season","W-D-L","Points"),
                     xval=c(1,2,3))) %>%
  ggplot(aes(xval,numid2,label=value)) +
  geom_point(alpha=0)+
  geom_text(aes(label=value)) +
  theme_void() +
  scale_x_continuous(limits=c(.25,3.25))+
  labs(title="**Second Half**</br>",
       subtitle="*Matches 18-34*")+
  theme(plot.title=element_markdown(hjust=.5),
        plot.subtitle=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#f4f4f4"),
        panel.background = element_rect(fill="#f4f4f4",color="#f4f4f4"))

### full season records

xxx.tab.a <- mainz.games %>% filter(Match==34) %>%
  select(year,WDL,Points) %>%
  mutate(numid=1:length(season),numid2=1:length(season)) %>% 
  gather(numid,value,-c(numid,numid2)) %>% 
  mutate(xval=ifelse(numid=="year",1,ifelse(numid=="WDL",2,3)),
         value=ifelse(numid=="year",paste0(value,"-",str_sub(as.numeric(value)+1,3,4)),value)) %>%
  rbind(.,data.frame(numid2=c(16,16,16),numid=c("x","x","x"),value=c("Season","W-D-L","Points"),
                     xval=c(1,2,3))) %>%
  ggplot(aes(xval,numid2,label=value)) +
  geom_point(alpha=0)+
  geom_text(aes(label=value)) +
  theme_void() +
  scale_x_continuous(limits=c(.25,3.25))+
  labs(title="**Season Records**",
       subtitle="*Matches 1-34*")+
  theme(plot.title=element_markdown(hjust=.5),
        plot.subtitle=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#f4f4f4",color="black"),
        panel.background = element_rect(fill="#f4f4f4",color="#f4f4f4"))

### import Mainz logo
mainz_logo_png <- readPNG("fsv-mainz-05-logo-1.png") #https://logodownload.org/wp-content/uploads/2019/12/fsv-mainz-05-logo-1.png
mainz_logo_plot <- ggdraw()+draw_image(mainz_logo_png,scale=.65)

###################################

#####. Create main plot

###################################

### annotation text

best.season <- "<span style='font-size:14pt; color:#f4f4f4'>**Best Season: 2010-11**</span><br>
<span style='font-size:12pt; color:#f4f4f4'>Tied Bundeliga record with 7 wins in first 7 games<br>
Finished in 5th place, qualified for Europa League</span>"

worst.season <- "<span style='font-size:14pt; color:#f4f4f4'>**Current/Worst Season: 2023-24**</span><br>
<span style='font-size:12pt; color:#f4f4f4'>Only two wins and 16 points in first 25 matches<br>
5 wins and 19 points in last 9 matches to avoid relegation</span>"

perf.text <- "<span style='font-size:14pt; color:#585a59'>**Average Points**</span><br><br>
<span style='font-size:12pt; text-align: left; color:#585a59'>1st Half: 20.4<br><br>
2nd Half: 22.8<br><br>
***Overall: 43.2***</span><br><br>
<span style='font-size:14pt; color:#585a59'>**Standings**</span><br><br>
<span style='font-size:12pt; text-align: left; color:#585a59'>Best Finish: 5 (one time)<br><br>
Worst Finish: 15 (one time)<br><br>
***Average Finish: 10.7***</span>"

### main plot

mg.plot <- ggplot()+
  geom_line(data=mainz.games %>% filter(!year %in% season.filter$year,Match<18),
            aes(Match,Points,group=year2),alpha=.25)+
  geom_text(data=mainz.games %>% filter(!year %in% season.filter$year,Match==17),
            aes(x=17.5,Points,label=Points),alpha=.5,size=5)+
  geom_line(data=mainz.games %>% filter(!year %in% season.filter$year,Match>17) %>% 
              mutate(Match=Match+2),
            aes(Match,Points,group=year2),alpha=.25)+
  geom_text(data=mainz.games %>% filter(!year %in% season.filter$year,Match==34),
            aes(x=36.5,Points,label=Points),alpha=.5,size=5)+
  geom_line(data=mainz.games %>% filter(year %in% season.filter$year,Match<18),
            aes(Match,Points,col=year2),size=1)+
  geom_text(data=mainz.games %>% filter(year %in% season.filter$year,Match==17),
            aes(x=17.5,Points,label=Points,col=year2),show.legend = F,size=5)+
  geom_line(data=mainz.games %>% filter(year %in% season.filter$year,Match>17) %>% mutate(Match=Match+2),
            aes(Match,Points,col=year2),size=1)+
  geom_text(data=mainz.games %>% filter(year %in% season.filter$year,Match==34),
            aes(x=36.5,Points,label=Points,col=year2),show.legend = F,size=5)+
  scale_color_manual(values=c("2010"="black","2023"="#BF1523"),
                     labels=c("Best Season (2010-11)","Current/Worst Season (2023-24)"))+
  scale_x_continuous(breaks=c(1,6,11,17,20,26,31,36),
                     labels=c(1,6,11,17,18,24,29,34),
                     limits=c(0,37))+
  
  annotate('richtext',x=27.5,y=58,label=best.season,fill="black")+
  annotate('richtext',x=27.5,y=4,label=worst.season,fill="#BF1523")+
  annotate('richtext',x=1,y=52,label=perf.text,hjust=0,fill="#ececec")+
  
  
  annotate("text",x=19,y=24.5,label="Mid-Season",angle=90,fontface="italic",size=8,alpha=.5)+
  annotate("text",x=17.5,y=35.5,label="1st Half\nPoints",fontface="bold",size=4)+
  annotate("text",x=36.5,y=60.5,label="Final\nPoints",fontface="bold",size=4)+
  
  theme_bw()+
  labs(#title="*Mainz 05 Seasons 2009/10 Through 2023/24*",
    x="Match Day")+
  theme(#plot.title=element_text(hjust=.5),
    legend.position="none", #c(.8,.2),
    legend.text = element_text(size=14),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f4f4f4"),
    legend.key = element_rect(fill = "#f4f4f4"),
    plot.title=element_markdown(hjust=.5,size=24),
    panel.border = element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_text(size=12),
    axis.title.x=element_text(size=14),
    plot.background = element_rect(fill="#f4f4f4"),
    panel.background = element_rect(fill="#f4f4f4"))

###################################

#####. Create Dashboard

###################################

patch.x <- ((plot_spacer()/xxx.tab/plot_spacer()/
               xxx.tab2/plot_spacer()+plot_layout(heights=c(.01,1,.1,1,.01)) &
               theme(panel.background=element_rect(color="black")))| plot_spacer() |
              (mg.plot & theme(panel.background=element_rect(color="black")))| plot_spacer() |
              (plot_spacer()/(xxx.tab.a & theme(panel.background=element_rect(color="black")))/mainz_logo_plot+plot_layout(ncol=1,heights=c(.1,2,1))))+
  plot_layout(ncol=5,widths=c(1,.05,4,.05,1))  & 
  theme(plot.background=element_rect(fill="#ececec",color="#ececec"))

patch.x + plot_annotation(
  title = "Mainz 05 Bundesliga Performance",
  subtitle = "2009-10 through 2023-24",
  caption = "Data: FBref.com<br>Logo: Wikimedia Commons",
  theme = theme(plot.title = element_markdown(hjust = 0.5,size=28),
                plot.subtitle = element_markdown(hjust = 0.5,size=20),
                plot.caption = element_markdown(hjust = 0.5,size=12),
                plot.margin=margin(.5,.75,.5,.75,"cm"),
                plot.background = element_rect(fill="#ececec")
  )
)

ggsave("mainz05_bundesliga_performance.png",width=18,height=12,units="in")
