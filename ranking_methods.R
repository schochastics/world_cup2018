sports_ranking <- function(games,method="massey",...){
  if(method=="massey"){
    ratings <- .massey(games)
  }
  else if(method=="massey_advanced"){
    ratings <- .massey_advanced(games)
  }
  else if(method=="colley"){
    ratings <- .colley(games)
  }
  else if(method=="keener"){
    ratings <- .keener(games,...)
  }
  else if(method=="markov"){
    ratings <- .markov(games,...)
  }
  else if(method=="btm"){
    ratings <- .bradley_terry(games,...)
  }
  else if(method=="odrm"){
    ratings <- .odrm(games,...)
  }
  else if(method=="elo"){
    ratings <- .elo(games,...)
  }
  else if(method=="elo_women"){
    ratings <- .elo_women(games,...)
  }
  else if(method=="pr"){
    ratings <- .page_rank(games,...)
  }
  return(ratings)
}

## helper -------------------------

#massey ----
.massey <- function(games){
  M <- games %>% 
    select(home,away) %>% 
    as.matrix %>% 
    graph_from_edgelist(directed = F) %>% 
    get.adjacency(sparse=F)
  M <- -M[order(rownames(M)),order(rownames(M))]  
  home <- games %>% 
    group_by(home) %>% 
    dplyr::summarise(goal_diffh=sum(GH-GA),gamesh=n())
  away <- games %>% 
    group_by(away) %>% 
    dplyr::summarise(goal_diffa=sum(GA-GH),gamesa=n())
  agreg <- full_join(home,away,by=c("home"="away")) %>% 
    mutate(goal_diff=goal_diffh+goal_diffa,
           games=gamesh+gamesa) %>% 
    arrange(home) %>% 
    select(goal_diff,games) %>% 
    mutate_all(funs(coalesce(.,0L)))
  p <- agreg$goal_diff
  diag(M)=agreg$games
  M[nrow(M),]=1
  p[length(p)]=0
  r <- solve(M,p)
  tibble(team=names(r),rating=unname(r))
}

#massey advanced----
.massey_advanced <- function(games){
  M <- games %>% 
    select(home,away) %>% 
    as.matrix %>% 
    graph_from_edgelist(directed = F) %>% 
    get.adjacency(sparse=F)
  
  P <- M[order(rownames(M)),order(rownames(M))]
  
  M <- -P
  home <- games %>% 
    group_by(home) %>% 
    dplyr::summarise(goal_diffh=sum(GH-GA),gamesh=n())
  away <- games %>% 
    group_by(away) %>% 
    dplyr::summarise(goal_diffa=sum(GA-GH),gamesa=n())
  agreg <- full_join(home,away,by=c("home"="away")) %>% 
    mutate(goal_diff=goal_diffh+goal_diffa,
           games=gamesh+gamesa) %>% 
    arrange(home) %>% 
    select(goal_diff,games) %>% 
    mutate_all(funs(coalesce(.,0L))) %>% 
    select(goal_diff,games)
  p <- agreg$goal_diff
  diag(M)=agreg$games
  Tdiag <- diag(diag(M))
  M[nrow(M),]=1
  p[length(p)]=0
  r <- solve(M,p)
  
  home <- games %>% 
    group_by(home) %>% 
    dplyr::summarise(goals_forh=sum(GH),goals_againsth=sum(GA))
  away <- games %>% 
    group_by(away) %>% 
    dplyr::summarise(goals_fora=sum(GA),goals_againsta=sum(GH))
  agreg <- full_join(home,away,by=c("home"="away")) %>% 
    mutate(goals_for=goals_forh+goals_fora,
           goals_against=goals_againsth+goals_againsta) %>% 
    select(goals_for,goals_against)
  f <- agreg$goals_for
  a <- agreg$goals_against
  d <- c(solve(Tdiag+P,Tdiag%*%r-f))
  o <- c(r-d)
  tibble(team=names(r),rating=unname(r),offense=o,defense=d)
}

# colley ----
.colley <- function(games){
  C <- games %>% 
    select(home,away) %>% 
    as.matrix %>% 
    graph_from_edgelist(directed = F) %>% 
    get.adjacency(sparse=F)
  C <- -C[order(rownames(C)),order(rownames(C))]
  diag(C) <- 2+abs(rowSums(C))
  
  Tie <- 
    bind_rows(select(games,home,away,result),
              tibble(home=games$away,
                     away=games$home,
                     result=1-games$result)) %>% 
    dplyr::filter(result==0.5) %>% 
    select(home,away) %>% 
    as.matrix() %>% 
    graph_from_edgelist(directed = T) %>% 
    get.adjacency(sparse=F)
  if(dim(C)[1]!=dim(Tie)[1]){
    idx <- which(!rownames(C)%in%rownames(Tie))
    for(i in 1:length(idx)){
      Tie <- cbind(rbind(Tie,0),0)
    }
    rownames(Tie)[nchar(rownames(Tie))==0] <- rownames(C)[idx]
    colnames(Tie)[nchar(colnames(Tie))==0] <- rownames(C)[idx]
  }
  Tie <- Tie[order(rownames(Tie)),order(rownames(Tie))]
  C <- C-Tie
  diag(C) <- diag(C)+rowSums(Tie)
  
  agreg <- 
  full_join(
    games %>% group_by(home) %>% count(result),
    games %>% group_by(away) %>% count(result) %>% mutate(result=1-result),
    by=c("home"="away","result")) %>% 
    mutate(n.x=coalesce(n.x,0L),n.y=coalesce(n.y,0L)) %>% 
    group_by(result) %>% 
    mutate(count=n.x+n.y) %>% 
    ungroup() %>% 
    select(home,result,count) %>% 
    mutate(result=recode(result,`0.0`="loss",`0.5`="draw",`1.0`="win")) %>% 
    spread(result,count,fill=0)
  
  w <- agreg$win#+0.5*agreg$draw
  l <- agreg$loss
  b <- 1+0.5*(w-l)
  r <- solve(C,b)
  tibble(team=names(r),rating=unname(r))
}

# keener ----

.keener <- function(games,score="wl",skew=F){
  if(score=="wl"){
    games <- games %>% select(home,away,result)
    el <- bind_rows(games,
                    tibble(home=games$away,
                           away=games$home,
                           result=1-games$result)) %>% 
      dplyr::filter(result!=0) %>% 
      group_by(home,away) %>% 
      dplyr::summarise(val=sum(result))
    g <- graph_from_edgelist(as.matrix(el[,1:2]))
    E(g)$weight=el$val
    A <- get.adjacency(g,"both",sparse=F,attr = "weight")
    if(!is.connected(g,"strong")){
      A <- A+1/vcount(g)
    }
    r <- eigen(A)$vectors[,1] %>% Re() %>% abs()
    rating <- tibble(team=V(g)$name,rating=r)
  }
  else if(score=="goals"){
    el <- bind_rows(select(games,home,away,GH),
                    tibble(home=games$away,
                           away=games$home,
                           GH=games$GA)) %>% 
      group_by(home,away) %>% 
      dplyr::summarise(val=sum(GH))
    g <- graph_from_edgelist(as.matrix(el[,1:2]))
    E(g)$weight=el$val
    A <- get.adjacency(g,"both",sparse=F,attr = "weight")
    A <- (A+1)/(A+t(A)+2)
    diag(A) <- 0
    if(!is.connected(g,"strong")){
      A <- A+1/vcount(g)
    }
    if(skew){
      A <- 0.5+(sign(A-0.5)*sqrt(abs(2*A-1)))/2
    }
    r <- eigen(A)$vectors[,1] %>% Re() %>% abs()
    rating <- tibble(team=V(g)$name,rating=r)
  } else{
   stop("score must be one of 'wl' or 'goals'") 
  }
  
}

#markov ----

.markov <- function(games,score="wl"){
  if(score=="wl"){
    games <- games %>% select(home,away,result)
    el <- bind_rows(games,
                    tibble(home=games$away,
                           away=games$home,
                           result=1-games$result)) %>% 
      dplyr::filter(result!=0) %>% 
      group_by(home,away) %>% 
      dplyr::summarise(val=sum(result))
    g <- graph_from_edgelist(as.matrix(el[,1:2]))
    E(g)$weight=el$val
    A <- get.adjacency(g,"both",sparse=F,attr = "weight") %>% t()
    if(any(rowSums(A)==0)){
      idx <- which(rowSums(A)==0)
      A[idx,]=1
    }
    A <- A/rowSums(A)
    r <- eigen(t(A))$vectors[,1] %>% Re()
    r <- r/sum(r)
    rating <- tibble(team=V(g)$name,rating=r)
  }
  else if(score=="goalswin"){
    el <- bind_rows(select(games,home,away,GH,result),
                    tibble(home=games$away,
                           away=games$home,
                           GH=games$GA,
                           result=1-games$result)) %>% 
      dplyr::filter(result!=0) %>% 
      group_by(home,away) %>% 
      dplyr::summarise(val=sum(GH))
    g <- graph_from_edgelist(as.matrix(el[,1:2]))
    E(g)$weight=el$val
    A <- get.adjacency(g,"both",sparse=F,attr = "weight") %>% t()
    if(any(colSums(A)==0)){
      idx <- which(rowSums(A)==0)
      A[idx,]=1
    }
    A <- A/rowSums(A)
    r <- eigen(t(A))$vectors[,1] %>% Re()
    r <- r/sum(r)
    rating <- tibble(team=V(g)$name,rating=r)
  }
  else if(score=="goalsall"){
    el <- bind_rows(select(games,home,away,GH,result),
                    tibble(home=games$away,
                           away=games$home,
                           GH=games$GA,
                           result=1-games$result)) %>% 
      group_by(home,away) %>% 
      dplyr::summarise(val=sum(GH))
    g <- graph_from_edgelist(as.matrix(el[,1:2]))
    E(g)$weight=el$val
    A <- get.adjacency(g,"both",sparse=F,attr = "weight") %>% t()
    if(any(colSums(A)==0)){
      idx <- which(rowSums(A)==0)
      A[idx,]=1
    }
    A <- A/rowSums(A)
    r <- eigen(t(A))$vectors[,1] %>% Re()
    r <- r/sum(r)
    rating <- tibble(team=V(g)$name,rating=r)
  }else{
    stop("score must be one of 'wl','goalswin' or 'goalsall'") 
  }
}

# Bradley Terry ----
.bradley_terry <- function(games,max.iter=100,tol=1*10^-8,weights){
  teams <- unique(c(games$home,games$away))
  n <- length(teams)
  W <- 
  bind_rows(select(games,home,away,result),
            tibble(home=games$away,
                   away=games$home,
                   result=1-games$result)) %>% 
    dplyr::filter(result==1) %>% 
    select(home,away) %>% 
    as.matrix() %>% 
    graph_from_edgelist(directed = T) %>% 
    get.adjacency(sparse=F)
  if(dim(W)[1]!=n){
    idx <- which(!teams%in%rownames(W))
    for(i in 1:length(idx)){
      W <- cbind(rbind(W,0),0)
    }
    rownames(W)[nchar(rownames(W))==0] <- teams[idx]
    colnames(W)[nchar(colnames(W))==0] <- teams[idx]
  }
  W <- W[order(rownames(W)),order(rownames(W))]

  Tie <- 
    bind_rows(select(games,home,away,result),
              tibble(home=games$away,
                     away=games$home,
                     result=1-games$result)) %>% 
    dplyr::filter(result==0.5) %>% 
    select(home,away) %>% 
    as.matrix() %>% 
    graph_from_edgelist(directed = T) %>% 
    get.adjacency(sparse=F)
  
  if(dim(Tie)[1]!=n){
    idx <- which(!teams%in%rownames(Tie))
    for(i in 1:length(idx)){
      Tie <- cbind(rbind(Tie,0),0)
    }
    rownames(Tie)[nchar(rownames(Tie))==0] <- teams[idx]
    colnames(Tie)[nchar(colnames(Tie))==0] <- teams[idx]
  }
  
  Tie <- Tie[order(rownames(Tie)),order(rownames(Tie))]
  N <- W+t(W)+Tie
  
  WT_i=2*rowSums(W)+rowSums(Tie)
  if(any(WT_i==0)){
    WT_i <- WT_i+1/n
  }
  
  theta <- length(which(games$result==0.5))/nrow(games)
  
  r_0=rep(1/nrow(W),nrow(W))
  r_old=rep(100,nrow(W))
  iter=0
  while(iter<=max.iter & sqrt(sum((r_old-r_0)^2))>tol){
    iter  <- iter+1
    r_old <- r_0
    gi    <- .g_func(N,r_0,theta)
    r_0   <-  WT_i/gi
    r_0   <- r_0/sum(r_0)
    h     <- sum(.h_func(N,r_0,theta)) 
    theta <- 0.5*sum(Tie)/h
  }
  rating <- tibble(team=names(r_0),rating=unname(r_0))
  return(list(rating=rating,theta=theta))  
}

#offense/defense----
.odrm <- function(games,tol=1*10^-8){
  el <- bind_rows(select(games,home,away,GH),
                  tibble(home=games$away,
                         away=games$home,
                         GH=games$GA)) %>% 
    group_by(home,away) %>% 
    dplyr::summarise(val=sum(GH))
  g <- graph_from_edgelist(as.matrix(el[,1:2]))
  E(g)$weight=el$val
  A <- get.adjacency(g,"both",sparse=F,attr = "weight") %>% t()
  A <- A+1/nrow(A)
  A <- A/rowSums(A)
  # A <- t(A)
  d_k <- rep(1,nrow(A))
  o_k <- rep(0,nrow(A))
  d_old <- rep(10,nrow(A))
  while(sqrt(sum((d_old-d_k)^2))>tol){
    d_old <- d_k
    o_k <- t(A)%*%(1/d_k)
    d_k <- A%*%(1/o_k)
  }
  rating <- tibble(team=rownames(A),offense=c(o_k),defense=c(d_k))
}

## elo ----
.elo <- function(games,k=20){
  teams <- unique(c(games$home,games$away))
  start_date <- min(games$date)-days(1)
  elo_points <- tibble(team=teams,points=1500,active=0)
  elo_history <- tibble(team=teams,points=1500,date=start_date)
  hfa <- 150
  active_dates <- unique(games$date)
  active_dates <- c(active_dates,as.Date(.eom(active_dates))) %>% unique() %>% sort()
  for(i in 1:length(active_dates)){
    on.exit(print(c(i,hfa)))
    # on.exit(print(c(current_day)))
    if(i%%50==0){
      print(paste("day",i,"of",length(active_dates)))
    }
    if(i!=1){
      last_day  <- current_day
    }
    current_day <- active_dates[i] %>% as.Date()

    day_games <- games %>% dplyr::filter(date==current_day)
    if(nrow(day_games)!=0){
      day_games <- day_games %>% 
        left_join(select(elo_points,team,points),by=(c("home"="team"))) %>% 
        dplyr::rename("elo_home"=points) %>% 
        left_join(select(elo_points,team,points),by=(c("away"="team"))) %>% 
        dplyr::rename("elo_away"=points)
      day_games$prob <- 1/(1+10^((day_games$elo_away-(day_games$elo_home+hfa))/400))
      # day_games$elo_diff <- k*(day_games$result-day_games$prob)
      day_games$elo_diff <- ifelse(day_games$level=="national",20,40)*(day_games$result-day_games$prob)
      
      #update hfa
      hfa_change <- sum(day_games$elo_diff)
      hfa <- hfa+0.075*hfa_change
      
      elo_day <- tibble(team=c(day_games$home,day_games$away),
                        elo_diff=c(day_games$elo_diff,-day_games$elo_diff))
      
      elo_day <- elo_day %>% 
        group_by(team) %>% 
        dplyr::summarise(elo_diff=sum(elo_diff))
      
      elo_update <- left_join(elo_points,elo_day,by="team")
      elo_points$active <- ((elo_points$active+ifelse(is.na(elo_update$elo_diff),0,1))>=1)+0
        
      
      elo_points$points <- elo_update$points+
        ifelse(is.na(elo_update$elo_diff),0,elo_update$elo_diff)
    }
    if(i!=1){
      # if(month(last_day)!=month(current_day) | i==length(active_dates)){
      if(as.Date(.eom(current_day))==current_day){
        elo_add <- elo_points %>% mutate(points=ifelse(active==1,points,NA),
                                         date=current_day) %>% 
          select(team,points,date)
        elo_history <- bind_rows(elo_history,elo_add)
        elo_points$active <- 0
      }
    }
    
  }
  return(elo_history)
}

#elo women ----
.elo_women <- function(games){
  # teams <- unique(c(games$home,games$away))
  start_date <- min(games$date)-days(1)
  top_leagues <- c("germany","france","sweden","england")
  good_leagues <- c("spain","russia","italy","denmark")
  middle_leagues <- c("czech-republic","austria","scotland","norway","switzerland")
  
  games <- all.games
  teams <- c(games$home,games$away)
  countries <- c(games$home.country,games$away.country)
  elo_points <- tibble(team=teams,country=countries) %>% distinct() %>% 
    mutate(points = case_when(country%in%top_leagues ~ 1500,
                              country%in%good_leagues ~ 1400,
                              country%in%middle_leagues ~ 1350,
                              TRUE ~ 1250),
           active = 0) %>% 
    select(team,points,active)
  # tibble(team=teams,points=1500,active=0)
  # elo_history <- tibble(team=teams,points=1500,date=start_date)
  elo_history <- elo_points %>% mutate(date = start_date) %>% select(-active)
  hfa <- 150
  active_dates <- unique(games$date)
  active_dates <- c(active_dates,as.Date(.eom(active_dates))) %>% unique() %>% sort()
  for(i in 1:length(active_dates)){
    on.exit(print(c(i,hfa)))
    # on.exit(print(c(current_day)))
    if(i%%50==0){
      print(paste("day",i,"of",length(active_dates)))
    }
    if(i!=1){
      last_day  <- current_day
    }
    current_day <- active_dates[i] %>% as.Date()
    
    day_games <- games %>% dplyr::filter(date==current_day)
    if(nrow(day_games)!=0){
      day_games <- day_games %>% 
        left_join(select(elo_points,team,points),by=(c("home"="team"))) %>% 
        dplyr::rename("elo_home"=points) %>% 
        left_join(select(elo_points,team,points),by=(c("away"="team"))) %>% 
        dplyr::rename("elo_away"=points)
      day_games$prob <- 1/(1+10^((day_games$elo_away-(day_games$elo_home+hfa))/400))
      # day_games$elo_diff <- k*(day_games$result-day_games$prob)
      day_games$elo_diff <- day_games$k*(day_games$result-day_games$prob)
      
      #update hfa
      hfa_change <- sum(day_games$elo_diff)
      hfa <- hfa+0.075*hfa_change
      
      elo_day <- tibble(team=c(day_games$home,day_games$away),
                        elo_diff=c(day_games$elo_diff,-day_games$elo_diff))
      
      elo_day <- elo_day %>% 
        group_by(team) %>% 
        dplyr::summarise(elo_diff=sum(elo_diff))
      
      elo_update <- left_join(elo_points,elo_day,by="team")
      elo_points$active <- ((elo_points$active+ifelse(is.na(elo_update$elo_diff),0,1))>=1)+0
      
      
      elo_points$points <- elo_update$points+
        ifelse(is.na(elo_update$elo_diff),0,elo_update$elo_diff)
    }
    if(i!=1){
      if(as.Date(.eom(current_day))==current_day){
        elo_add <- elo_points %>% mutate(points=ifelse(active==1,points,NA),
                                         date=current_day) %>% 
          select(team,points,date)
        elo_history <- bind_rows(elo_history,elo_add)
        elo_points$active <- 0
      }
    }
    
  }
  return(elo_history)
}

# page rank----
.page_rank <- function(games,alpha=0.85,weights){
  el <- tibble(teamA=c(games$home,games$away),
               teamB=c(games$away,games$home),
               result=c(games$result,1-games$result))
  if(!missing(weights)){
    el$result <- el$result*rep(weights,2)
  }
  g <- el %>% dplyr::filter(result!=0) %>%
    .[,c(2,1)] %>% 
    as.matrix() %>% 
    igraph::graph_from_edgelist() 
  E(g)$weight <- el %>% dplyr::filter(result!=0) %>%
    .$result
  g <- igraph::simplify(g,edge.attr.comb = "sum")
  tmp <- clusters(g,"weak")
  idx <- which(tmp$membership!=which.max(tmp$csize))
  g <- delete_vertices(g,idx)
  p <- igraph::page_rank(g,damping = alpha)$vector
  tibble(team=V(g)$name,rating=unname(p))
}
#helper ----
.g_func <- function(N,r,theta){
  rowSums((N*(2+theta*sqrt(t(outer(r,r,"/")))))/(outer(r,r,"+")+theta*sqrt(outer(r,r,"*"))))
}
.h_func <- function(N,r,theta){
  sqrt_outer <- sqrt(outer(r,r,"*"))
  sum((N*sqrt_outer)/(outer(r,r,"+")+theta*sqrt_outer))
}

.eom <- function(date) {
  date.lt <- as.POSIXlt(date)
  mon  <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13)
  mon[mon==13] <- 1
  iso <-  ISOdate(1900+year, mon, 1, hour=0, tz=attr(date.lt,"tz"))
  result  <-  as.POSIXct(iso) - 86400 
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

# projection ----

.wdl.probabilities=function(s,ra,rb,a,g){
  expra=exp(a*ra)
  exprb=exp(a*rb)
  expdraw=exp(g+0.5*(a*ra+a*rb))
  denom=expra+exprb+expdraw
  p<-ifelse(s==1,expra/denom,ifelse(s==0,exprb/denom,expdraw/denom))
  return(p)
}

.wdl.loglike=function(a,g){
  R=-sum(log(.wdl.probabilities(s,x,y,a,g)))
  return(R)
}

prediction.strength=function(prob,res){
  #prob: nx3 matrix with ph,pd,pa,res: vector 1=home win 0=draw 2=away win
  require(ROCR)
  auc=rep(0,3)
  names(auc)=c("home","draw","away")
  k=0
  x=list()
  y=list()
  for(i in c(1,0,2)){
    k=k+1
    target_pred=prob[,k]
    target_class=ifelse(res==i,1,0)
    pred <- prediction(target_pred, target_class)
    perf <- performance(pred,"tpr","fpr")
    x[[k]]=perf@x.values[[1]]
    y[[k]]=perf@y.values[[1]]
    
    
    auc[k]=performance(pred,"auc")@y.values[[1]][1]
  }
  
  return(list(auc=auc,fpr=x,tpr=y))
}