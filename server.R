library(shiny)

shinyServer(function(input, output) {
#tab pan 1----------
  observeEvent(input$sim1, {
    res <- sim_groups(odds,groups)
    output$grpA <- renderTable(res$A,digits = 0,rownames = TRUE,width = '80%')
    output$grpB <- renderTable(res$B,digits = 0,rownames = TRUE,width = '80%')
    output$grpC <- renderTable(res$C,digits = 0,rownames = TRUE,width = '80%')
    output$grpD <- renderTable(res$D,digits = 0,rownames = TRUE,width = '80%')
    output$grpE <- renderTable(res$E,digits = 0,rownames = TRUE,width = '80%')
    output$grpF <- renderTable(res$F,digits = 0,rownames = TRUE,width = '80%')
    output$grpG <- renderTable(res$G,digits = 0,rownames = TRUE,width = '80%')
    output$grpH <- renderTable(res$H,digits = 0,rownames = TRUE,width = '80%')
  })
  
  observeEvent(input$sim2, {
    res <- sim_groups_1000(odds,groups)
    output$grpA <- renderTable(res$A)
    output$grpB <- renderTable(res$B)
    output$grpC <- renderTable(res$C)
    output$grpD <- renderTable(res$D)
    output$grpE <- renderTable(res$E)
    output$grpF <- renderTable(res$F)
    output$grpG <- renderTable(res$G)
    output$grpH <- renderTable(res$H)
  })
#tab pan 2----------
  observeEvent(input$rank, {
    if(input$method %in%c("massey","massey_advanced","colley","pr")){
      ratings <- sports_ranking(games,input$method) %>% arrange(-rating)
    } else if(input$method=="markov"){
      ratings <- sports_ranking(games,method = "markov",score="goalsall") %>% arrange(-rating)
    } else if(input$method=="btm"){
      ratings <- sports_ranking(games,method = "btm") %>% .$rating %>% arrange(-rating)
    } else if(input$method=="odrm"){
      ratings <- sports_ranking(games,method = "odrm") %>% arrange(desc(offense-defense))
    } else if(input$method=="fifa"){
      ratings <- fifa_rks
    } else if(input$method=="elo"){
      ratings <- elo
    }
    ratings <- ratings %>% 
      dplyr::filter(team%in%countries) %>% 
      mutate_if(is.numeric,funs(round(.,4))) %>% 
      mutate(teamd = paste0("<img src='./flags/",team,".png' height='12'></img> ")) %>% 
      mutate(teamd = ifelse(team==input$focus,
                            paste0(teamd,"<b>",team,"</b>"),
                            paste0(teamd,team))) %>% 
      mutate(team=teamd) %>% 
      select(-teamd)
    output$ratings <- DT::renderDT(ratings,escape=FALSE,
                                   options=list(paging=FALSE,searching=FALSE))
  })
#tab pan 3----------
  observeEvent(input$simwc1, {
    output$grptit1 <- renderText({"Group Stage"})
    output$sixtit1 <- renderText({"Last 16"})
    output$quatit1 <- renderText({"Quarterfinal"})
    output$semitit1 <- renderText({"Semifinal"})
    output$finaltit1 <- renderText({"Final"})
    output$thirdtit1 <- renderText({"3rd Place"})
    res <- sim_wc(odds,groups,elo,countries,wh=1)
    output$grpA1 <- renderTable(res$grps$A,digits = 0,rownames = TRUE,width = '80%')
    output$grpB1 <- renderTable(res$grps$B,digits = 0,rownames = TRUE,width = '80%')
    output$grpC1 <- renderTable(res$grps$C,digits = 0,rownames = TRUE,width = '80%')
    output$grpD1 <- renderTable(res$grps$D,digits = 0,rownames = TRUE,width = '80%')
    output$grpE1 <- renderTable(res$grps$E,digits = 0,rownames = TRUE,width = '80%')
    output$grpF1 <- renderTable(res$grps$F,digits = 0,rownames = TRUE,width = '80%')
    output$grpG1 <- renderTable(res$grps$G,digits = 0,rownames = TRUE,width = '80%')
    output$grpH1 <- renderTable(res$grps$H,digits = 0,rownames = TRUE,width = '80%')
    output$sixteen <- renderTable(res$sixteen,digits = 0,rownames = FALSE)
    output$quarter <- renderTable(res$quarter,digits = 0,rownames = FALSE)
    output$semi <- renderTable(res$semi,digits = 0,rownames = FALSE)
    output$final <- renderTable(res$final[1,],digits = 0,rownames = FALSE)
    output$third <- renderTable(res$final[2,],digits = 0,rownames = FALSE)
    output$winner <- renderImage({
      filename <- normalizePath(
        file.path('./www/flags/',paste0(res$winner,".png")))
      list(src = filename)
      
    }, deleteFile = FALSE
    )
  })
#tab pan 4 ------------
  observeEvent(input$simwc2, {
    res <<- sim_groups_goals(odds,groups)
  })
  observeEvent(input$grpbtn,{
    output$grpR <- renderTable(res$results[[input$showgrp]],digits = 0,rownames = F,width = '80%')
    output$grpT <- renderTable(res$tables[[input$showgrp]],digits = 0,rownames = F,width = '80%')
    # output$grpBR <- renderTable(res$results$B,digits = 0,rownames = F,width = '80%')
    # output$grpCR <- renderTable(res$results$C,digits = 0,rownames = F,width = '80%')
    # output$grpDR <- renderTable(res$results$D,digits = 0,rownames = F,width = '80%')
    # output$grpER <- renderTable(res$results$E,digits = 0,rownames = F,width = '80%')
    # output$grpFR <- renderTable(res$results$F,digits = 0,rownames = F,width = '80%')
    # output$grpGR <- renderTable(res$results$G,digits = 0,rownames = F,width = '80%')
    # output$grpHR <- renderTable(res$results$H,digits = 0,rownames = F,width = '80%')
    
  })
  observeEvent(input$simsix, {
    sixteen <<- sim_sixteen(res$tables,elo)
    six_tbl <- sixteen %>% mutate(result=case_when(
      is.na(nEh) & is.na(nVh) ~ paste0(GH,":",GA),
      is.na(nEh) & !is.na(nVh) ~paste0(nVh,":",nVa,"E"),
      !is.na(nEh) ~ paste0(nEh,":",nEa,"P"))) %>% 
      select(home,away,result)
    output$sixteen1 <- renderTable(six_tbl,digits = 0,rownames = FALSE)
  })
  observeEvent(input$simquarter, {
    quarter <<- sim_quarter(sixteen,elo)
    qua_tbl <- quarter %>% mutate(result=case_when(
      is.na(nEh) & is.na(nVh) ~ paste0(GH,":",GA),
      is.na(nEh) & !is.na(nVh) ~paste0(nVh,":",nVa,"E"),
      !is.na(nEh) ~ paste0(nEh,":",nEa,"P"))) %>% 
      select(home,away,result)
    output$quarter1 <- renderTable(qua_tbl,digits = 0,rownames = FALSE)
  })
  observeEvent(input$simsemi, {
    semi <<- sim_semi(quarter,elo)
    semi_tbl <- semi %>% mutate(result=case_when(
      is.na(nEh) & is.na(nVh) ~ paste0(GH,":",GA),
      is.na(nEh) & !is.na(nVh) ~paste0(nVh,":",nVa,"E"),
      !is.na(nEh) ~ paste0(nEh,":",nEa,"P"))) %>% 
      select(home,away,result)
    output$semi1 <- renderTable(semi_tbl,digits = 0,rownames = FALSE)
  })
  observeEvent(input$simfinals, {
    finals <<- sim_finals(semi,elo)
    final_tbl <- finals %>% mutate(result=case_when(
      is.na(nEh) & is.na(nVh) ~ paste0(GH,":",GA),
      is.na(nEh) & !is.na(nVh) ~paste0(nVh,":",nVa,"E"),
      !is.na(nEh) ~ paste0(nEh,":",nEa,"P"))) %>% 
      mutate(R=c("Final","3rd Place")) %>% 
      select(R,home,away,result)
    output$finals1 <- renderTable(final_tbl,digits = 0,rownames = FALSE)
  })
})
