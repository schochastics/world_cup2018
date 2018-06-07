library(shiny)

navbarPage("World Cup 2018",theme = "styles.css",
  tabPanel("Odds",
    sidebarLayout(
      sidebarPanel(width=0,
        
        actionButton("sim1", "Simulate 1x", icon("futbol"), 
                     style=button_style),
        actionButton("sim2", "Simulate 1000x", icon("futbol"), 
                     style=button_style)
      ),
      mainPanel(width=12,
        fluidRow(
          column(3,tableOutput('grpA')),
          column(3,tableOutput('grpB')),
          column(3,tableOutput('grpC')),
          column(3,tableOutput('grpD'))
        ),
        fluidRow(
          column(3,tableOutput('grpE')),
          column(3,tableOutput('grpF')),
          column(3,tableOutput('grpG')),
          column(3,tableOutput('grpH'))
        )      
      )
    )
  ),
#ranking tab ----
  tabPanel("Rankings",
    sidebarLayout(
      sidebarPanel(width=2,
        selectInput("method", "Select Ranking:",
                    c("FIFA"="fifa","Elo"="elo","Massey"="massey",
                      "Massey Adv."="massey_advanced",
                      "Colley"="colley","Markov"="markov","Bradley Terry"="btm",
                      "Offense/Defense"="odrm","PageRank"="pr")
        ),
        selectInput("focus","Select focus:",
                    list( "none",
                          `Group A` = groups[[1]],
                          `Group B` = groups[[2]],
                          `Group C` = groups[[3]],
                          `Group D` = groups[[4]],
                          `Group E` = groups[[5]],
                          `Group F` = groups[[6]],
                          `Group G` = groups[[7]],
                          `Group H` = groups[[8]])),
        actionButton("rank", "rank!", icon("futbol"), 
                     style=button_style)
      ),
      mainPanel(width=10,
        column(1),
        column(6,DT::DTOutput("ratings"))
      )
    )
  ),
#wc sim tab ----
  tabPanel("Sim WC",
    sidebarLayout(
      sidebarPanel(width=0,
        actionButton("simwc1", "Simulate 1x", icon("futbol"), 
                                  style=button_style)
      ),
      mainPanel(width=12,
        
        h3(textOutput("grptit1")),
        fluidRow(
          column(3,tableOutput('grpA1')),
          column(3,tableOutput('grpB1')),
          column(3,tableOutput('grpC1')),
          column(3,tableOutput('grpD1'))
        ),
        fluidRow(
          column(3,tableOutput('grpE1')),
          column(3,tableOutput('grpF1')),
          column(3,tableOutput('grpG1')),
          column(3,tableOutput('grpH1'))
        ),
        fluidRow(
          h3(textOutput("sixtit1")),
          column(3,tableOutput('sixteen')),
          h3(textOutput("quatit1")),
          column(3,tableOutput('quarter')),
          h3(textOutput("semitit1")),
          column(3,tableOutput('semi')),
          column(3,
            h3(textOutput("finaltit1")),
            tableOutput('final'),
            h3(textOutput("thirdtit1")),
            tableOutput('third')
          )
        ),
        fluidRow(
          column(6),
          column(3,imageOutput("winner"))
        )
      )
    )
  ),
#Advanced Simulations tap -----
  tabPanel("Sim WC advanced",
    sidebarLayout(
      sidebarPanel(width=2,
       actionButton("simwc2", "Simulate Groups", icon("futbol"), 
                    style=button_style),
       selectInput("showgrp","Choose Group:",LETTERS[1:8]),
       actionButton("grpbtn", "Show Group", icon("futbol"), 
                    style=button_style),
       actionButton("simsix", "Simulate Last16", icon("futbol"), 
                    style=button_style),
       actionButton("simquarter", "Simulate Quarter", icon("futbol"), 
                    style=button_style),
       actionButton("simsemi", "Simulate Semi", icon("futbol"), 
                    style=button_style),
       actionButton("simfinals", "Simulate Finals", icon("futbol"), 
                    style=button_style)
      ),
      mainPanel(width=10,

        fluidRow(
          column(2),
          column(4,tableOutput('grpR')),
          column(4,tableOutput('grpT'))
        ),
        fluidRow(
          column(3,tableOutput('sixteen1')),
          column(3,tableOutput('quarter1')),
          column(3,tableOutput('semi1')),
          column(3,tableOutput('finals1'))
        )
      )
    )
  )
)
