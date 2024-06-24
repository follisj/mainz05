library(tidyverse)
library(ggtext)
library(rvest)

### function to get standings

bundesliga.standings <- function(week,season){
  url <- paste0("https://www.bundesliga.com/en/bundesliga/matchday/",season,"/",week)
  bund.team <- url %>% read_html() %>%
    html_nodes(".d-md-block") %>%
    html_text()
  bund.team <- data.frame(
    xxx=bund.team %>% data.frame() %>% filter(row_number() %% 2 == 0),
    yyy=bund.team %>% data.frame() %>% filter(row_number() %% 2 != 0)) %>%
    'colnames<-'(c("away","home"))
  bund.score <- url %>% read_html() %>%
    html_nodes(".score") %>%
    html_text()
  bund.score <- data.frame(
    xxx=bund.score %>% data.frame() %>% filter(row_number() %% 2 == 0),
    yyy=bund.score %>% data.frame() %>% filter(row_number() %% 2 != 0)) %>%
    'colnames<-'(c("away.goal","home.goal"))
  match.day.x <- cbind(bund.team[1:nrow(bund.score),],bund.score)
  match.day.x <- match.day.x %>% mutate(match=row_number(),
                                        away.goal=as.numeric(away.goal),
                                        home.goal=as.numeric(home.goal)) %>% 
    gather(home.away,team,-match,-away.goal,-home.goal) %>%
    mutate(match.day=week,
           WLD=case_when(
             home.away=="away" & away.goal-home.goal > 0 ~ "W",
             home.away=="away" & away.goal-home.goal == 0 ~ "D",
             home.away=="away" & away.goal-home.goal < 0 ~ "L",
             home.away=="home" & away.goal-home.goal > 0 ~ "L",
             home.away=="home" & away.goal-home.goal == 0 ~ "D",
             home.away=="home" & away.goal-home.goal < 0 ~ "W"
           ),
           Points=case_when(
             WLD=="W" ~ 3,
             WLD=="D" ~ 1,
             WLD=="L" ~ 0
           ),
           goal.diff=ifelse(home.away=="away",away.goal-home.goal,home.goal-away.goal),
           goal.for=case_when(
             home.away=="away" ~ away.goal,
             home.away=="home" ~ home.goal
           ),
           goal.vs=case_when(
             home.away=="away" ~ home.goal,
             home.away=="home" ~ away.goal
           ),
           #team=str_replace(team,"1. ","")
    )
  return(match.day.x)
}

### get team names and records
url <- "https://www.bundesliga.com/en/bundesliga/table"
team.names <- url %>% read_html() %>%
  html_nodes(".d-sm-inline-block") %>% 
  html_text() %>% data.frame() %>%
  `colnames<-`(c("team")) %>%
  mutate(rank=1:18)

wdl <- url %>% read_html() %>%
  html_nodes(".losses , .draws , .wins,.pts") %>% 
  html_text()
wdl <- matrix(wdl[5:76],ncol=4,byrow=T) %>% data.frame() %>%
  mutate(WDL=paste(X1,X2,X3,sep="-")) %>% select(WDL,X4)

team.names <- team.names %>% cbind(wdl)


###################################

#####. Create Data Set for weekly point totals

###################################

standings24 <- data.frame()
for(i in 1:34) {
    standings24 <- rbind(standings24,bundesliga.standings(i,"2023-2024"))  ## uses the bundesliga standings function
  }
  
weekly.tables <- list()
for(i in 1:max(standings24$match.day)) {
    weekly.tables[[i]] <- standings24 %>% group_by(team) %>% 
      filter(match.day <= i) %>%
      summarize(Points=sum(Points),Goal.Diff=sum(goal.diff),GFor=sum(goal.for),
                Gvs=sum(goal.vs),games=n()) %>% 
      arrange(desc(Points),desc(Goal.Diff))
  }
  
wk2wk.pts <- data.frame()
for(i in 1:34) {
    wk2wk.pts <- rbind(wk2wk.pts,
                       weekly.tables[[i]] %>%
                         mutate(match.day=i, rank=1:18)
    )
  }

###################################

#####. Act I

###################################

act1.text <- data.frame(
  team=NA,
  x=rep(12.75,3),
  y=c(13.75,15.95,17.5),
  label=c("**Rough Start**<br>Following consecutive top 10 finishes, hopes were high for the 2023-24 season.  However, that hope was short-lived<br>
  Managing to earn only three points in the first 9 matches, and yet to win their first match, 
  the team was at the bottom of the table.<br>
  Following Match Day 9, Bo Svensson and Mainz parted ways by mutual agreement.<br>
          Would this change provide the spark to move up the table?",
          "**Looking Ahead**<br>
          It was still early, with 25 matches remaining and only 4 points separating the team from 13th place." ,
          "**Unfortunate**<br>
          It was tough to see Bo go because of his success the previous seasons, and the 
          great turnaround in 2020-21 (from 18th (7 points) to 12th (39 points) in the second half of the season). 
          But the team was in a tough spot.")
)


act1 <- wk2wk.pts %>% filter(rank>12,match.day<10) %>%
  ggplot(aes(match.day,rank,col=team))+
  geom_point()+
  #geom_text(aes(label=paste0(team," ",Points,"pts")),angle=45,hjust=0,vjust=-1)+
  geom_text(aes(label=ifelse(team=="Mainz",NA,team)),
            angle=45,hjust=0,vjust=-1,size=5)+
  geom_text(aes(label=ifelse(team=="Mainz","Mainz",NA)),
            angle=45,hjust=0,vjust=-1,size=6,col="#BF1523")+
  geom_text(aes(label=paste(Points,"pts")),angle=45,hjust=1,vjust=2)+
  scale_color_manual(values=c("gray80","gray40","gray80","black","gray80",
                              "gray20","gray80","gray80","#BF1523","gray80"))+
  
  geom_line(data=. %>% filter(team=="Mainz"),
            aes(x=match.day,y=rank),col="#BF1523",lty=2,alpha=.75)+
  
  geom_vline(xintercept=1:9,lty=2,col="black",alpha=.25)+
  geom_line(data=data.frame(x=c(1,1,11,11),y=c(15.4,16.5,15.4,16.5),
                            group=c(1,2,1,2)),aes(x=x,y=y,group=group),col="black",lty=2)+
  
  geom_text(aes(x=10.25,y=14,label="Bundesliga 2024-25"),size=5,angle=270,col="black")+
  geom_text(aes(x=10.25,y=15.95,label="Relegation Playoff"),size=5,angle=270,col="black")+
  geom_text(aes(x=10.25,y=17.5,label="Relegation to 2. Bundesliga"),size=5,angle=270,col="black")+
  
  geom_textbox(data=act1.text,aes(x=x,y=y,label=label),width=unit(.2,"npc"),
               hjust=.5,fill="#ececed",col="black",size=6)+
  
  labs(
    title="Mainz 05 - 2023-24 The Battle Against Relegation, Act I",
    subtitle="Match Days 1-9 - *The end of the Bo Svenson era*",
    caption="Sources: Bundesliga.com, Wikipedia",
    x="Match Day",
    y="League Position"
  )+
  
  scale_x_continuous(breaks=1:9,limits=c(1,14),labels=1:9)+
  scale_y_reverse(limits=c(18,12.5),breaks=18:13,labels=18:13)+
  #xlim(0,12)+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=24,hjust=0),
        plot.subtitle = element_markdown(size=18,hjust=0),
        plot.caption=element_text(size=12,hjust=.94),
        plot.background=element_rect(fill="#f0f0f0"),
        panel.background=element_rect(fill="#f0f0f0",color="#f0f0f0"),
        plot.margin = margin(0.5,0.5,0.75,0.75,"cm"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12,vjust=-3),
        axis.title.x=element_text(size=16,vjust=-3,hjust=.375),
        axis.title.y=element_text(size=16,vjust=3)
  )

ggsave("Mainz05_2023-24_Season_Act_I.png",act1,width=16.5,height=14)


###################################

#####. Act II

###################################

act2.text <- data.frame(
  team=NA,
  x=rep(24.75,3),
  y=c(13.75,15.95,17.5),
  label=c("**Righting the Ship**?<br>Following Match Day 9, Jan Siewert was appointed appointed interim manager, 
  and made permanent manager following Match Day 16.<br>
  The team notched it's first win of the season in his first match and drew the next 2 matches 
  to some hope the season could be salvaged.  But the team's struggles continued, 
  with only four points in the following 9 matches, leaving them 9 points from safety at 
  the end of Match Day 21.<br>
  Following the their third straight loss on Match Day 21, Jan Siewert was removed as manager.<br>
          Even though they had changed managers, the team was still in the relegation zone.",
          "**Current Position**<br>
          Trailing Union Berlin by 9 points, Köln by four points for the relegation playoff, 
          and tied with Darmstadt on points." ,
          "**A Miracle?**<br>
          To avoid relegation, they would need to overcome the teams above them in the table,
          and stay ahead of a team they were tied with.<br>
          Would the final 13 matches be enough?")
)

act2 <- wk2wk.pts %>% filter(rank>12,match.day %in% 10:21) %>%
  ggplot(aes(match.day,rank,col=team))+
  geom_point()+
  #geom_text(aes(label=paste0(team," ",Points,"pts")),angle=45,hjust=0,vjust=-1)+
  geom_text(aes(label=ifelse(team=="Mainz",NA,team)),
            angle=45,hjust=0,vjust=-1,size=5)+
  geom_text(aes(label=ifelse(team=="Mainz","Mainz",NA)),
            angle=45,hjust=0,vjust=-1,size=6,col="#BF1523")+
  geom_text(aes(label=paste(Points,"pts")),angle=45,hjust=1,vjust=2)+
  scale_color_manual(values=c("gray80","gray40","gray80","black","gray80",
                              "gray20","gray80","#BF1523","gray60"))+
  
  geom_line(data=. %>% filter(team=="Mainz"),
            aes(x=match.day,y=rank),col="#BF1523",lty=2,alpha=.75)+
  
  geom_vline(xintercept=10:21,lty=2,col="black",alpha=.25)+
  geom_line(data=data.frame(x=c(10,10,23,23),y=c(15.4,16.5,15.4,16.5),
                            group=c(1,2,1,2)),aes(x=x,y=y,group=group),col="black",lty=2)+
  
  geom_text(aes(x=22.25,y=14,label="Bundesliga 2024-25"),size=5,col="black",angle=270)+
  geom_text(aes(x=22.25,y=15.95,label="Relegation Playoff*"),size=5,col="black",angle=270)+
  geom_text(aes(x=22.25,y=17.5,label="Relegation to 2. Bundesliga"),size=5,col="black",angle=270)+
  
  geom_textbox(data=act2.text,aes(x=x,y=y,label=label),width=unit(.2,"npc"),
               hjust=.5,fill="#ececed",col="black",size=6)+
  
  labs(
    title="Mainz 05 - 2023-24 The Battle Against Relegation, Act II",
    subtitle="Match Days 10-21, *Jan Siewert takes the helm*",
    caption="Sources: Bundesliga.com, Wikipedia",
    x="Match Day",
    y="League Position"
  )+
  
  scale_x_continuous(breaks=10:21,limits=c(10,26),labels=10:21)+
  scale_y_reverse(limits=c(18,12.5),breaks=18:13,labels=18:13)+
  #xlim(9,24)+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=24,hjust=0),
        plot.subtitle = element_markdown(size=18,hjust=0),
        plot.caption=element_text(size=12,hjust=.94),
        plot.background=element_rect(fill="#f0f0f0"),
        panel.background=element_rect(fill="#f0f0f0",color="#f0f0f0"),
        plot.margin = margin(0.5,0.5,0.75,0.75,"cm"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12,vjust=-3),
        axis.title.x=element_text(size=16,vjust=-3,hjust=.375),
        axis.title.y=element_text(size=16,vjust=3)
  )


ggsave("Mainz05_2023-24_Season_Act_II.png",act2,width=18,height=16)


###################################

#####. Act III

###################################

act3.text <- data.frame(
  team=NA,
  x=rep(37.75,3),
  y=c(13.75,15.95,17.5),
  label=c("**SUCCESS!!!**<br>Following Match Day 21, Bo Henriksen was named the new manager.<br>
  The team won it's first match under Henriksen, but success was not immediate as the 
  team continued struggle with only 1 point in the next 3 matches, capped by an 8-1 loss to Bayern,
  leaving them 9 points from safety with 9 matches remaining. The relegation playoff 
  seemed to be the best hope.<br>
  But that game would be the turning point of the season.<br>
  Beginning with a win on Match Day 26, Mainz would go on to earn 19 of a possible 27 points in 
          their last nine matches, resulting in a 13th place finish and avoiding relegation.",
          "**Relegation Playoff**<br>Vfl Bochum won the two-leg playoff (6-5 on penalites) and will remain in the Bundesliga",
          "**Relegation/Promotion**<br>Darmstadt and Köln were relegated to the 2. Bundesliga, while St. Pauli and Holstein 
          Kiel earned promotion to the Bundesliga for the 2024-25 season.")
)

act3 <- wk2wk.pts %>% filter(rank>12,match.day>21) %>%
  ggplot(aes(match.day,rank,col=team))+
  geom_point()+
  geom_text(aes(label=ifelse(team=="Mainz",NA,team)),
            angle=45,hjust=0,vjust=-1,size=5)+
  geom_text(aes(label=ifelse(team=="Mainz","Mainz",NA)),
            angle=45,hjust=0,vjust=-1,size=6,col="#BF1523")+
  geom_text(aes(label=paste(Points,"pts")),angle=45,hjust=1,vjust=2)+
  scale_color_manual(values=c("gray80","gray40","black","gray20",
                              "gray80","#BF1523","gray60","gray80"))+
  
  geom_line(data=. %>% filter(team=="Mainz"),
            aes(x=match.day,y=rank),col="#BF1523",lty=2,alpha=.75)+
  
  geom_vline(xintercept=22:34,lty=2,col="black",alpha=.25)+
  geom_line(data=data.frame(x=c(22,22,36,36),y=c(15.4,16.5,15.4,16.5),
                            group=c(1,2,1,2)),aes(x=x,y=y,group=group),col="black",lty=2)+
  
  geom_text(aes(x=35.25,y=14,label="Bundesliga 2024-25"),size=5,angle=270)+
  geom_text(aes(x=35.25,y=15.95,label="Relegation Playoff"),size=5, angle=270)+
  geom_text(aes(x=35.25,y=17.5,label="Relegation to 2. Bundesliga"),size=5,angle=270)+
  
  geom_textbox(data=act3.text,aes(x=x,y=y,label=label),width=unit(.2,"npc"),
               hjust=.5,fill="#ececed",col="black",size=5)+
  
  labs(
    title="Mainz 05 - 2023-24 The Battle Against Relegation, Act III",
    subtitle="Match Days 22-34 - *The Bo Henriksen Era Begins*",
    caption="Sources: Bundesliga.com, Wikipedia",
    x="Match Day",
    y="League Position"
  )+
  
  scale_x_continuous(breaks=seq(22,34,1),limits=c(22,39),labels=22:34)+
  scale_y_reverse(limits=c(18,12.5),breaks=18:13,labels=18:13)+
  #xlim(21,38)+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=28,hjust=0),
        plot.subtitle = element_markdown(size=20,hjust=0),
        plot.caption=element_text(size=12,hjust=.94),
        plot.background=element_rect(fill="#f0f0f0"),
        panel.background=element_rect(fill="#f0f0f0",color="#f0f0f0"),
        plot.margin = margin(0.5,0.5,0.75,0.75,"cm"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12,vjust=-3),
        axis.title.x=element_text(size=16,vjust=-3,hjust=.375),
        axis.title.y=element_text(size=16,vjust=3)
  )

ggsave("Mainz05_2023-24_Season_Act_III.png",act3,width=18,height=16)


###################################

#####. Overall

###################################

act.all <- wk2wk.pts %>% filter(rank>12) %>%
  ggplot(aes(match.day,rank,col=team))+
  geom_point()+
  geom_text(aes(label=ifelse(team=="Mainz",NA,team)),
            angle=45,hjust=0,vjust=-1,size=5)+
  geom_text(aes(label=ifelse(team=="Mainz","Mainz",NA)),
            angle=45,hjust=0,vjust=-1,size=6,col="#BF1523")+
  geom_text(aes(label=paste(Points,"pts")),angle=45,hjust=1,vjust=2)+
  scale_color_manual(values=c("gray80","gray40","gray80","black","gray80",
                              "gray20","gray80","gray80","#BF1523","gray60","gray80"))+
  
  geom_line(data=. %>% filter(team=="Mainz"),
            aes(x=match.day,y=rank),col="#BF1523",lty=2,alpha=.75)+
  
  geom_vline(xintercept=1:34,lty=2,col="black",alpha=.25)+
  geom_line(data=data.frame(x=c(1,1,36,36),y=c(15.4,16.5,15.4,16.5),
                            group=c(1,2,1,2)),aes(x=x,y=y,group=group),col="black",lty=2)+
  
  geom_text(aes(x=36.25,y=14,label="Bundesliga 2024-25"),size=5,angle=270)+
  geom_text(aes(x=36.25,y=15.95,label="Relegation Playoff*"),size=5, angle=270)+
  geom_text(aes(x=36.25,y=17.5,label="Relegation to 2. Bundesliga"),size=5,angle=270)+
  
  labs(
    title="Bundesliga 2023-24 - The Battle Against Relegation",
    subtitle="The Battle to Avoid Relegation",
    caption="Sources: Bundesliga.com, Wikipedia",
    x="Match Day",
    y="League Position"
  )+
  
  scale_x_continuous(breaks=seq(1,34,1),limits=c(1,36.5),labels=1:34)+
  scale_y_reverse(limits=c(18,12.5))+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=24,hjust=0),
        plot.subtitle = element_markdown(size=18,hjust=0),
        plot.caption=element_text(size=12,hjust=.94),
        plot.background=element_rect(fill="#f0f0f0"),
        panel.background=element_rect(fill="#f0f0f0",color="#f0f0f0"),
        plot.margin = margin(0.5,0.5,0.75,0.75,"cm"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12,vjust=-3),
        axis.title.x=element_text(size=16,vjust=-3,hjust=.45),
        axis.title.y=element_text(size=16,vjust=3)
  )

ggsave(act.all,"Mainz05_2023-24_Season_Act_all.png",act.all,width=18,height=14)



###################################

#####. Season by Match Day

###################################

season.notes <- data.frame(
  match.day=NA,
  x=rep(39,12),
  y=c(18,16.5,14.5,12.5,11,9.8,8,6.2,5.1,4,3,1.5),
  label=c("Leverkusen with an invincible season",
          "Stuttgart, a surprise 2nd place finish, edged past Bayern on the last day after Bayern had spent most of the season 2nd place",
          "The top 5 teams remained in the top 5 after Match Day 8, with Leigzig and Dortmund occupying 4th or 5th place most of the season",
          "Frankfurt and Hoffenheim spent the majority of their seasons in position for European play",
          "Heidenheim was outside the top 8 until the final Match Day",
          "Freiberg spent most of the season in the top 8, but lost a European place on the last day",
          "Bremen, Augsburg and Wolfsburg spent most of the season between European qualification and relegation, each avoidng the relegation zone after Match Day 2 and spending some time in the top 8",
          "Mainz spent the most weeks in the relegation zone, but finished 13th",
          "Gladbach spent all but one week above relegation and outside of European qualification",
          "Union Berlin managed to just do enough to stay above relegation",
          "Bochum won the relegation playoff, and will remain in the Bundesliga",
          "Darmstadt and Köln stayed in relegation zone for the second half of the season")
)

season.table <- wk2wk.pts %>% filter(match.day==34) %>% 
  mutate(rank2 = rank,team.names=team.names$team,rank.rev=18:1) %>%
  select(team,rank2,team.names,rank.rev) %>%
  right_join(wk2wk.pts, by="team") %>%
  mutate(team.names=fct_reorder(team.names, rank2)) %>%
  mutate(pos.col=case_when(rank<=5~"Champions League",
                           rank==6 | rank==7 ~"Europa League",
                           rank<=8~"Europa Conf. League Qualifying",
                           rank >= 9 & rank <=15 ~"No Man's Land",
                           rank==16~"Relegation Playoff",
                           rank==17 | rank==18~"Relegation")) %>%
  mutate(pos.col=fct_reorder(pos.col,rank)) %>%
  ggplot(aes(match.day,team.names)) +
  geom_tile(aes(fill=pos.col),col="#F4F4F4")+
  geom_text(aes(label=rank,size=Points))+
  geom_text(data=team.names,aes(x=-8,y=abs(rank-18)+1.17,label=team),
            size=5,hjust=0,fontface="bold")+
  geom_text(data=team.names,aes(x=-8,y=abs(rank-18)+.78,label=paste0("W-D-L: ",WDL,"   PTS: ",X4)),
            size=4.5,hjust=0,fontface="bold")+

  scale_fill_manual(values=c("#028A0f","#21AFED","#55D8FF",
                             "gray","orange","#F44336FF"))+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(breaks=1:34,labels=1:34,limits=c(-8,43))+
  scale_size_continuous(range=c(2,6))+
  
  geom_textbox(data=season.notes,aes(x=x,y=y,label=label),width=unit(.14,"npc"),
               hjust=.5,fill="#ececed",col="black",size=4)+
  
  geom_line(data=data.frame(
    x=c(rep(-8,3),rep(43,3)),
    y=c(3.5,10.5,13.5,3.5,10.5,13.5),
    group=c(1,2,3,1,2,3),
    match.day=NA),
    aes(x=x,y=y,group=group),col="black",size=.5,alpha=.75)+
  geom_line(data=data.frame(
    x=c(rep(-8,12),rep(34.5,12)),
    y=c(1.5,4.5:8.5,9.5,12.5,14.5:17.5,
        1.5,4.5:8.5,9.5,12.5,14.5:17.5),
    group=c(1:12,1:12),
    match.day=NA),
    aes(x=x,y=y,group=group),col="gray80",size=.5)+
  geom_line(data=data.frame(
    x=c(-8,43,-8,43),
    y=c(2.5,2.5,11.5,11.5),
    group=c(1,1,2,2),
    match.day=NA),
    aes(x=x,y=y,group=group),col="black",size=.5,alpha=.75,lty=2)+
  
  labs(title="2023-2024 Bundesliga Table",
       subtitle="Team Placement by Match Day\n",
       caption="Source: Bundesliga.com",
       x="Match Day",
       #y="Team",
       fill="Points"
  )+
  theme_void()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=10.5),
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle = element_text(size=18,hjust=.5),
        plot.caption=element_text(size=12,hjust=.94),
        panel.background = element_rect(fill = "#F4F4F4",color="#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        plot.margin = margin(0.5,0.5,0.5,0,"cm"),
        axis.text.y = element_blank(), #element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,hjust=.52,vjust=-2.5),
        axis.title.y=element_blank())+
  guides(size=F)

ggsave(season.table,"bundesliga_table_by_match.png",width=20,height=14)




