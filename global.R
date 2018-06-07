library(tidyverse)
library(igraph)
# require(RSelenium)
# require(XML)
# # sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
# remDr <- remoteDriver(port = 4445L)
# remDr$open()
# remDr$navigate("http://www.oddsportal.com/soccer/world/world-cup-2018/")
# webElem <- remDr$findElement(using = 'css', value = '.normal')
# webElem$clickElement()
# doc <- XML::htmlParse(remDr$getPageSource()[[1]])
# remDr$close()
# doc %>% XML::readHTMLTable() %>% .$tournamentTable %>% 
#   as_tibble() %>% 
#   dplyr::filter(V3!="X" & V3!="") %>% 
#   select(V2,V3,V4,V5) %>% 
#   separate(V2,c("home","away"),sep=" - ") %>% 
#   mutate_at(vars(3:5),as.numeric) %>% 
#   mutate(ph=(V4*V5)/(V3*V4+V3*V5+V5*V4),
#          pd=(V3*V5)/(V3*V4+V3*V5+V5*V4),
#          pa=(V3*V4)/(V3*V4+V3*V5+V5*V4)) %>% 
#   select(home,away,ph,pd,pa) -> odds_group
# url <- "https://www.fifa.com/common/fifa-world-ranking/_ranking_matchpoints_totals.js"
# fifa_rank <- jsonlite::fromJSON(url) %>% select(countrycode,points)

##############################################################################
#source ----
source("ranking_methods.R")
#css ---
button_style <- "color: #fff; background-color: #2D4571; border-color: #AB8339; padding-left:20px; padding-right:30px"
# data ----
odds <- readr::read_csv("odds_group.csv")
games <- readr::read_csv("games.csv")
fifa_rks <- read_csv("fifa_ranks.csv")
elo <- read_csv("elo.csv")
#groups ----
groups <- list()
groups[[1]] <- c("Russia","Egypt","Uruguay","Saudi Arabia")
groups[[2]] <- c("Portugal","Spain","Morocco","Iran")
groups[[3]] <- c("France","Australia","Peru","Denmark")
groups[[4]] <- c("Argentina","Iceland","Croatia","Nigeria")
groups[[5]] <- c("Brazil","Switzerland","Costa Rica","Serbia")
groups[[6]] <- c("Germany","Mexico","Sweden","South Korea")
groups[[7]] <- c("Belgium","Panama","Tunisia","England")
groups[[8]] <- c("Poland","Senegal","Colombia","Japan")
names(groups) <- LETTERS[1:8]
countries <- unlist(groups) %>% unname()

################################################################################
# simulation functions ----
################################################################################
sim_groups <- function(odds,groups){
  grp_res <- vector("list",8)
  names(grp_res) <- LETTERS[1:8]
  odds$res <- apply(odds[,3:5],1,function(x) sample(c(1,0,2),1,prob = x))
  for(gr in LETTERS[1:8]){
    tmp <- odds %>% dplyr::filter(odds$home%in%groups[[gr]])
    tibble(Country=c(tmp$home,tmp$away),
           res=c(ifelse(tmp$res==1,3,ifelse(tmp$res==0,1,0)),
                 ifelse(tmp$res==2,3,ifelse(tmp$res==0,1,0)))) %>% 
      group_by(Country) %>% 
      dplyr::summarise(Points=sum(res)) %>% arrange(-Points) -> grp_res[[gr]]
  }
  grp_res
}

sim_groups_1000 <- function(odds,groups){
  grp_res <- vector("list",8)
  names(grp_res) <- LETTERS[1:8]
  wh <- 100
  for(i in 1:wh){
    tmp <- sim_groups(odds,groups)
    for(gr in LETTERS[1:8]){
      tmp[[gr]] <- tmp[[gr]] %>% mutate(rk=row_number())
      grp_res[[gr]] <- rbind(grp_res[[gr]],tmp[[gr]])
    }
  }
  for(gr in LETTERS[1:8]){
  grp_res[[gr]] <- grp_res[[gr]] %>% 
    group_by(Country) %>% 
    dplyr::summarise(Last16 = round(sum(rk<=2)/wh,2)) %>% 
    arrange(-Last16)
  }
  grp_res
}

create_sixteen <- function(grp_res){
  sixteen <- tibble(home=character(8),away=character(8))
  sixteen$home <- c(grp_res[["A"]]$Country[1],
                    grp_res[["C"]]$Country[1],
                    grp_res[["E"]]$Country[1],
                    grp_res[["G"]]$Country[1],
                    
                    grp_res[["B"]]$Country[1],
                    grp_res[["D"]]$Country[1],
                    grp_res[["F"]]$Country[1],
                    grp_res[["H"]]$Country[1])
  
  sixteen$away <- c(grp_res[["B"]]$Country[2],
                    grp_res[["D"]]$Country[2],
                    grp_res[["F"]]$Country[2],
                    grp_res[["H"]]$Country[2],
                    
                    grp_res[["A"]]$Country[2],
                    grp_res[["C"]]$Country[2],
                    grp_res[["E"]]$Country[2],
                    grp_res[["G"]]$Country[2])
  sixteen
}

create_quarter <- function(sixteen){
  quarter <- tibble(home=character(4),away=character(4))
  quarter$home <- sapply(seq(1,8,2),function(x)sixteen[x,sixteen$res[x]] %>% unname) %>% unlist
  quarter$away <- sapply(seq(2,8,2),function(x)sixteen[x,sixteen$res[x]] %>% unname) %>% unlist
  quarter
}

create_semi <- function(quarter){
  semi <- tibble(home=character(2),away=character(2))
  semi$home <- sapply(seq(1,4,2),function(x)quarter[x,quarter$res[x]] %>% unname) %>% unlist
  semi$away <- sapply(seq(2,4,2),function(x)quarter[x,quarter$res[x]] %>% unname) %>% unlist
  semi
}

create_finals <- function(semi){
  finals <- tibble(home=character(2),away=character(2))
  finals$home[1] <- unname(semi[1,semi$res[1]]) %>% unlist
  finals$away[1] <- unname(semi[2,semi$res[2]]) %>% unlist
  finals$home[2] <- unname(semi[1,-semi$res[1]+3]) %>% unlist
  finals$away[2] <- unname(semi[2,-semi$res[2]+3]) %>% unlist
  
  finals
}

################################################################################
#sim WC ----
sim_wc <- function(odds,groups,elo,countries,wh=10){
  wc_final <- tibble(country = countries,sixteen=0,quarter=0,semi=0,
                     fourth=0,third=0,second=0,winner=0)
  for(i in 1:wh){
    grp_res <- sim_groups(odds,groups)
    
    sixteen <- create_sixteen(grp_res)
    sixteen <- sixteen %>% 
      left_join(elo,by=c("home"="team")) %>% 
      rename(elo_home=points) %>% 
      left_join(elo,by=c("away"="team")) %>% 
      rename(elo_away=points) %>% 
      mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
      rowwise() %>% 
      mutate(res=sample(c(1,2),1,prob = c(ph,pa))) %>% 
      ungroup() %>% 
      select(home,away,res)
    
    quarter <- create_quarter(sixteen)
    quarter <- quarter %>% 
      left_join(elo,by=c("home"="team")) %>% 
      rename(elo_home=points) %>% 
      left_join(elo,by=c("away"="team")) %>% 
      rename(elo_away=points) %>% 
      mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
      rowwise() %>% 
      mutate(res=sample(c(1,2),1,prob = c(ph,pa))) %>% 
      ungroup() %>% 
      select(home,away,res)
    
    semi <- create_semi(quarter)
    semi <- semi %>% 
      left_join(elo,by=c("home"="team")) %>% 
      rename(elo_home=points) %>% 
      left_join(elo,by=c("away"="team")) %>% 
      rename(elo_away=points) %>% 
      mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
      rowwise() %>% 
      mutate(res=sample(c(1,2),1,prob = c(ph,pa))) %>% 
      ungroup() %>% 
      select(home,away,res)
    
    finals <- create_finals(semi)
    finals <- finals %>% 
      left_join(elo,by=c("home"="team")) %>% 
      rename(elo_home=points) %>% 
      left_join(elo,by=c("away"="team")) %>% 
      rename(elo_away=points) %>% 
      mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
      rowwise() %>% 
      mutate(res=sample(c(1,2),1,prob = c(ph,pa))) %>% 
      ungroup() %>% 
      select(home,away,res)
    winner <- c(finals$home[1],finals$away[1])[finals$res[1]]
    second <- setdiff(c(finals$home[1],finals$away[1]),winner)
    third <- c(finals$home[2],finals$away[2])[finals$res[2]]
    fourth <- setdiff(c(finals$home[2],finals$away[2]),third)
    
    idx <- match(c(sixteen$home,sixteen$away),wc_final$country)
    wc_final$sixteen[idx] <- wc_final$sixteen[idx] +1
    
    idx <- match(c(quarter$home,quarter$away),wc_final$country)
    wc_final$quarter[idx] <- wc_final$quarter[idx] +1
    
    idx <- match(c(semi$home,semi$away),wc_final$country)
    wc_final$semi[idx] <- wc_final$semi[idx] +1
    
    idx <- match(fourth,wc_final$country)
    wc_final$fourth[idx] <- wc_final$fourth[idx] +1
    
    idx <- match(third,wc_final$country)
    wc_final$third[idx] <- wc_final$third[idx] +1
    
    idx <- match(second,wc_final$country)
    wc_final$second[idx] <- wc_final$second[idx] +1
    
    idx <- match(winner,wc_final$country)
    wc_final$winner[idx] <- wc_final$winner[idx] +1
  }
  list(tbl=wc_final,grps=grp_res,sixteen=sixteen,
       quarter=quarter,semi=semi,finals=finals,winner=winner)

}
# sim advanced ----
sim_groups_goals <- function(odds,groups){
  mgoals <- 2.63
  grp_tab <- vector("list",8)
  grp_res <- vector("list",8)
  names(grp_res) <- LETTERS[1:8]
  names(grp_tab) <- LETTERS[1:8]
  # odds$res <- apply(odds[,3:5],1,function(x) sample(c(1,0,2),1,prob = x))
  for(gr in LETTERS[1:8]){
    tmp <- odds %>% dplyr::filter(odds$home%in%groups[[gr]])
    tmp %>% 
      rowwise() %>%
      mutate(GH = rpois(1,mgoals*ph),
             GA = rpois(1,mgoals*pa)) %>% 
      ungroup %>% 
      mutate(hpts = ifelse(GH>GA,3,ifelse(GH==GA,1,0)),
             apts = ifelse(GH<GA,3,ifelse(GH==GA,1,0))) %>% 
      select(home,away,GH,GA,hpts,apts)  -> tmp 
    
    tibble(Country=c(tmp$home,tmp$away),
           goals=c(tmp$GH,tmp$GA),against=c(tmp$GA,tmp$GH),
           pts=c(tmp$hpts,tmp$apts)) %>% 
      group_by(Country) %>% 
      dplyr::summarise(g=sum(goals),a=sum(against),Points=sum(pts)) %>% 
      arrange(desc(Points),desc(g-a)) -> tab
    
    grp_res[[gr]] <- tmp %>% 
      unite(result,c("GH","GA"),sep=":") %>% 
      select(home,away,result)
    grp_tab[[gr]] <- tab %>% 
      unite(Goals,c("g","a"),sep=":") %>% 
      select(Country,Goals,Points)
  }
  return(list(results=grp_res,tables=grp_tab))
}
sim_sixteen <- function(grp_tab,elo){
  mgoals <- 2.63
  sixteen <- create_sixteen(grp_tab)
  sixteen <- sixteen %>% 
    left_join(elo,by=c("home"="team")) %>% 
    rename(elo_home=points) %>% 
    left_join(elo,by=c("away"="team")) %>% 
    rename(elo_away=points) %>% 
    mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
    rowwise() %>% 
    mutate(GH=rpois(1,mgoals*ph),GA=rpois(1,mgoals*pa)) %>% 
    mutate(nVh=ifelse(GH==GA,GH+rpois(1,mgoals*ph*0.3),NA),
           nVa=ifelse(GH==GA,GA+rpois(1,mgoals*pa*0.3),NA)) %>% 
    mutate(nEh=ifelse(!is.na(nVh) & nVh==nVa,rpois(1,5),NA),
           nEa=ifelse(!is.na(nVa) & nVh==nVa,nEh+sample(c(-1,1),1),NA)) %>% 
    mutate(res = ifelse(GH>GA,1,ifelse(GA>GH,2,NA))) %>% 
    mutate(res = ifelse(is.na(res) & nVh>nVa,1,
                        ifelse(is.na(res) & nVa>nVh,2,res))) %>% 
    mutate(res = ifelse(is.na(res) & nEh>nEa,1,
                        ifelse(is.na(res) & nEa>nEh,2,res))) %>% 
    ungroup()
  sixteen
}
  
sim_quarter <- function(sixteen,elo){
  mgoals <- 2.63
  quarter <- create_quarter(sixteen)
  quarter <- quarter %>% 
    left_join(elo,by=c("home"="team")) %>% 
    rename(elo_home=points) %>% 
    left_join(elo,by=c("away"="team")) %>% 
    rename(elo_away=points) %>% 
    mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
    rowwise() %>% 
    mutate(GH=rpois(1,mgoals*ph),GA=rpois(1,mgoals*pa)) %>% 
    mutate(nVh=ifelse(GH==GA,GH+rpois(1,mgoals*ph*0.3),NA),
           nVa=ifelse(GH==GA,GA+rpois(1,mgoals*pa*0.3),NA)) %>% 
    mutate(nEh=ifelse(!is.na(nVh) & nVh==nVa,rpois(1,5),NA),
           nEa=ifelse(!is.na(nVa) & nVh==nVa,nEh+sample(c(-1,1),1),NA)) %>% 
    mutate(res = ifelse(GH>GA,1,ifelse(GA>GH,2,NA))) %>% 
    mutate(res = ifelse(is.na(res) & nVh>nVa,1,
                        ifelse(is.na(res) & nVa>nVh,2,res))) %>% 
    mutate(res = ifelse(is.na(res) & nEh>nEa,1,
                        ifelse(is.na(res) & nEa>nEh,2,res))) %>% 
    ungroup()
  quarter
}

sim_semi <- function(quarter,elo){
  mgoals <- 2.63
  semi <- create_semi(quarter)
  semi <- semi %>% 
    left_join(elo,by=c("home"="team")) %>% 
    rename(elo_home=points) %>% 
    left_join(elo,by=c("away"="team")) %>% 
    rename(elo_away=points) %>% 
    mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
    rowwise() %>% 
    mutate(GH=rpois(1,mgoals*ph),GA=rpois(1,mgoals*pa)) %>% 
    mutate(nVh=ifelse(GH==GA,GH+rpois(1,mgoals*ph*0.3),NA),
           nVa=ifelse(GH==GA,GA+rpois(1,mgoals*pa*0.3),NA)) %>% 
    mutate(nEh=ifelse(!is.na(nVh) & nVh==nVa,rpois(1,5),NA),
           nEa=ifelse(!is.na(nVa) & nVh==nVa,nEh+sample(c(-1,1),1),NA)) %>% 
    mutate(res = ifelse(GH>GA,1,ifelse(GA>GH,2,NA))) %>% 
    mutate(res = ifelse(is.na(res) & nVh>nVa,1,
                        ifelse(is.na(res) & nVa>nVh,2,res))) %>% 
    mutate(res = ifelse(is.na(res) & nEh>nEa,1,
                        ifelse(is.na(res) & nEa>nEh,2,res))) %>% 
    ungroup()
  semi
}

sim_finals <- function(semi,elo){
  mgoals <- 2.63
  finals <- create_finals(semi)
  finals <- finals %>% 
    left_join(elo,by=c("home"="team")) %>% 
    rename(elo_home=points) %>% 
    left_join(elo,by=c("away"="team")) %>% 
    rename(elo_away=points) %>% 
    mutate(ph=1/(1+10^(-(elo_home-elo_away)/400)),pa=1-ph) %>% 
    rowwise() %>% 
    mutate(GH=rpois(1,mgoals*ph),GA=rpois(1,mgoals*pa)) %>% 
    mutate(nVh=ifelse(GH==GA,GH+rpois(1,mgoals*ph*0.3),NA),
           nVa=ifelse(GH==GA,GA+rpois(1,mgoals*pa*0.3),NA)) %>% 
    mutate(nEh=ifelse(!is.na(nVh) & nVh==nVa,rpois(1,5),NA),
           nEa=ifelse(!is.na(nVa) & nVh==nVa,nEh+sample(c(-1,1),1),NA)) %>% 
    mutate(res = ifelse(GH>GA,1,ifelse(GA>GH,2,NA))) %>% 
    mutate(res = ifelse(is.na(res) & nVh>nVa,1,
                        ifelse(is.na(res) & nVa>nVh,2,res))) %>% 
    mutate(res = ifelse(is.na(res) & nEh>nEa,1,
                        ifelse(is.na(res) & nEa>nEh,2,res))) %>% 
    ungroup()
  finals
}
