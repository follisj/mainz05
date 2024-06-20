library(tidyverse)
library(rvest)
library(gt)
library(janitor)

## function to get match day results
bundesliga.standings <- function(week,season){
  url <- paste0("https://www.bundesliga.com/en/bundesliga/matchday/",season,"/",week)
  bund.team <- url %>% read_html() %>%
    html_nodes(".d-xl-block") %>%
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


###################################

#####. Create Final Table

###################################


standings <- ## create data set with all match day results
  data.frame()
for(i in 1:34) {
  standings <- rbind(standings,bundesliga.standings(i,"2023-2024"))
}

## create data frame with team names and records
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

### create data frame for the final table
current.table <- standings %>% 
  group_by(team) %>% 
  summarize(games=sum(table(Points)),Points=sum(Points,na.rm=T),GFor=sum(goal.for,na.rm=T),
            Gvs=sum(goal.vs,na.rm=T),
            Goal.Diff=sum(goal.diff,na.rm=T)) %>% 
  arrange(desc(Points),desc(Goal.Diff)) %>%
  mutate(place=1:18) %>%
  select(place,team,games,Points,GFor,Gvs,Goal.Diff)

current.table <- cbind(current.table[,-2],team.names) %>%
  select(place,team,WDL,Points,GFor,Gvs,Goal.Diff)

## find Mainz position in table
mainz <- grep("Mainz",current.table$team)

## create final table
final.table <- current.table %>% gt() %>%
  tab_header(title="Bundesliga Table",
             subtitle=md(
               "2023-24 Final Standings<br/>May 18, 2024")
               ) %>%
  tab_options(
    heading.title.font.size = px(30),
    heading.subtitle.font.size = px(20),
    table.background.color="#F4F4F4",
  ) %>%
  tab_spanner(label="Goals",columns=c(5:7)) %>% 
  tab_style(style=cell_text(weight="bold",size="large",color="black"),
            locations=cells_column_spanners(
              spanners=c("Goals")
            )) %>%
  cols_align(align="center",
             columns=vars(WDL,Points,GFor,Gvs,Goal.Diff)) %>% 
  
  tab_style(style=
              cell_text(color="red"),
            locations=cells_body(
              columns="Goal.Diff",
              rows = Goal.Diff < 0
            )
  ) %>%
  tab_style(style=
              cell_text(color="darkblue"),
            locations=cells_body(
              columns="Goal.Diff",
              rows = Goal.Diff > 0
            )
  ) %>%
  
  tab_style(style=cell_fill(color="#028A0f"),
            locations=cells_body(
              columns=1,
              rows=1:5
            )
  ) %>%
  tab_style(style=cell_fill(color="#21AFED"),
            locations=cells_body(
              columns=1,
              rows=6:7
            )
  ) %>%
  tab_style(style=cell_fill(color="#55D8FF"),
            locations=cells_body(
              columns=1,
              rows=8
            )
  ) %>%
  tab_style(style=cell_fill(color="orange"),
            locations=cells_body(
              columns=1,
              rows=16
            )
  ) %>%
  tab_style(style=cell_fill(color="#F44336FF"),
            locations=cells_body(
              columns=1,
              rows=17:18
            )
  ) %>%
  tab_style(style=cell_fill(color="#FFFF00"),
            locations=cells_body(
              columns=2:7,
              rows=mainz
            )
  ) %>%
  tab_style(style=list(
    cell_borders(
      sides="left",
      color="black",
      weight=px(3)
    )
  ),
  locations=list(
    cells_body(
      columns=vars(GFor)
    )
  )) %>% 
  tab_source_note(html(
    paste("<svg width='15' height='12'><rect width='12' height='12' style='fill:#028A0f'/>",
          "</svg><span style='color:black'>Champions League</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#21AFED'/>",
          "</svg><span style='color:black'>Europa League</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#55D8FF'/>",
          "</svg><span style='color:black'>Europa Conf. League Qualifying</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:orange'/>",
          "</svg><span style='color:black'>Relegation Playoff</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#F44336FF'/>",
          "</svg><span style='color:black'>Relegation</span>")
  )
  ) %>%
  tab_source_note(md("Data from: [https://www.bundesliga.com](https://www.bundesliga.com/en/bundesliga)")) %>%
  cols_label(place="",team="",WDL=md("**W-D-L**"),Points=md("**Points**"),GFor=md("**Scored**"),
             Gvs=md("**Allowed**"),Goal.Diff=md("**Differential**"))

gtsave(final.table,"bundesliga_final_table.png")


###################################

#####. Create Midseason Table

###################################


standings17 <- ## create data set with all match day results through match day 17
  data.frame()
for(i in 1:17) {
  standings17 <- rbind(standings17,bundesliga.standings(i,"2023-2024"))
}

## create data frame with team names and records
url <- "https://www.bundesliga.com/en/bundesliga/table"
team.names <- url %>% read_html() %>%
  html_nodes(".d-sm-inline-block") %>% 
  html_text() %>% data.frame() %>%
  `colnames<-`(c("team")) %>%
  mutate(rank=1:18)

wdl17 <- standings17 %>% tabyl(team,WLD) %>%
  data.frame() %>%
  mutate(WDL=paste(W,D,L,sep="-")) %>%
  select(team,WDL)

### create data frame for the table through match day 17
current.table17 <- standings17 %>%
  group_by(team) %>% 
  summarize(games=sum(table(Points)),Points=sum(Points,na.rm=T),GFor=sum(goal.for,na.rm=T),
            Gvs=sum(goal.vs,na.rm=T),
            Goal.Diff=sum(goal.diff,na.rm=T)) %>% 
  arrange(desc(Points),desc(Goal.Diff)) %>%
  mutate(place=1:18) %>%
  select(place,team,games,Points,GFor,Gvs,Goal.Diff)

current.table17 <- current.table17 %>% left_join(wdl17) %>%
  select(place,team,WDL,Points,GFor,Gvs,Goal.Diff)
current.table17 <- current.table17 %>%
  mutate(team=ifelse(team=="Borussia M'gladbach","Borussia Mönchengladbach",team)) %>%
  mutate(team=ifelse(team=="SC Freiburg","Sport-Club Freiburg",team))

## find Mainz position in table
mainz17 <- grep("Mainz",current.table17$team)

## create midseason table
midseason.table <- current.table17 %>% gt() %>%
  tab_header(title="Bundesliga Table",
             subtitle=md(
               "2023-24 Midseason Standings<br/>January 12, 2024")
  ) %>%
  tab_options(
    heading.title.font.size = px(30),
    heading.subtitle.font.size = px(20),
    table.background.color="#F4F4F4",
  ) %>%
  tab_spanner(label="Goals",columns=c(5:7)) %>% 
  tab_style(style=cell_text(weight="bold",size="large",color="black"),
            locations=cells_column_spanners(
              spanners=c("Goals")
            )) %>%
  cols_align(align="center",
             columns=vars(WDL,Points,GFor,Gvs,Goal.Diff)) %>% 
  
  tab_style(style=
              cell_text(color="red"),
            locations=cells_body(
              columns="Goal.Diff",
              rows = Goal.Diff < 0
            )
  ) %>%
  tab_style(style=
              cell_text(color="darkblue"),
            locations=cells_body(
              columns="Goal.Diff",
              rows = Goal.Diff > 0
            )
  ) %>%
  
  tab_style(style=cell_fill(color="#028A0f"),
            locations=cells_body(
              columns=1,
              rows=1:5
            )
  ) %>%
  tab_style(style=cell_fill(color="#21AFED"),
            locations=cells_body(
              columns=1,
              rows=6:7
            )
  ) %>%
  tab_style(style=cell_fill(color="#55D8FF"),
            locations=cells_body(
              columns=1,
              rows=8
            )
  ) %>%
  tab_style(style=cell_fill(color="orange"),
            locations=cells_body(
              columns=1,
              rows=16
            )
  ) %>%
  tab_style(style=cell_fill(color="#F44336FF"),
            locations=cells_body(
              columns=1,
              rows=17:18
            )
  ) %>%
  tab_style(style=cell_fill(color="#FFFF00"),
            locations=cells_body(
              columns=2:7,
              rows=mainz17
            )
  ) %>%
  tab_style(style=list(
    cell_borders(
      sides="left",
      color="black",
      weight=px(3)
    )
  ),
  locations=list(
    cells_body(
      columns=vars(GFor)
    )
  )) %>% 
  tab_source_note(html(
    paste("<svg width='15' height='12'><rect width='12' height='12' style='fill:#028A0f'/>",
          "</svg><span style='color:black'>Champions League</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#21AFED'/>",
          "</svg><span style='color:black'>Europa League</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#55D8FF'/>",
          "</svg><span style='color:black'>Europa Conf. League Qualifying</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:orange'/>",
          "</svg><span style='color:black'>Relegation Playoff</span>",
          "<svg width='15' height='12'><rect width='12' height='12' style='fill:#F44336FF'/>",
          "</svg><span style='color:black'>Relegation</span>")
  )
  ) %>%
  tab_source_note(md("Data from: [https://www.bundesliga.com](https://www.bundesliga.com/en/bundesliga)")) %>%
  cols_label(place="",team="",WDL=md("**W-D-L**"),Points=md("**Points**"),GFor=md("**Scored**"),
             Gvs=md("**Allowed**"),Goal.Diff=md("**Differential**"))

gtsave(midseason.table,"bundesliga_midseason_table.png")
