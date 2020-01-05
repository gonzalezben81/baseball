#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(RSQLite)
library(DT)
source("global.R")

# Define UI for baseball application
ui <- fluidPage(
   
  htmlTemplate("baseball.html")
)

# Define server logic to interact with the baseball.html file
server <- function(input, output) {
   
  
  rv <- reactiveValues(app_mode = "search")
    
  search_results<- eventReactive(input$search_button,{
    
    # You can fetch all results:
    res <- dbSendQuery(con, paste0("SELECT * FROM master WHERE NAMES == '",input$search,"'"))
    dbFetch(res)
    

  })
  
  
  
  output$player_name <- renderText({
    search_results()$NAMES
  })
  
  output$player_name_two <- renderText({
    search_results()$NAMES
  })
  
  output$player_name_three <- renderText({
    search_results()$NAMES
  })
  output$player_city <- renderText({
    search_results()$birthCity
  })
  
  output$player_state <- renderText({
    state<- search_results()$birthState
    if(is.na(state)){
      return("Unknown")
    }else{
      return(state)
    }
  })
  
  output$player_country <- renderText({
    search_results()$birthCountry
  })
  
  output$player_bats <- renderText({
    
    batting<- search_results()$bats
    if(batting=="R"){
      return("Right")
    }else if(batting=="L"){
      return("Left")
    }else{
      return("Both")
    }
  })

  output$player_throws <- renderText({
    throwing<- search_results()$throws
    if(throwing=="R"){
      return("Right")
    }else if(throwing=="L"){
      return("Left")
    }else{
      return("Both")
    }
  })

  output$player_height <- renderText({
    search_results()$height
  })
  
  output$player_debut <- renderText({
    search_results()$debut
  })

  
  output$player_finalGame <- renderText({
    search_results()$finalGame
  })
  output$player_weight <- renderText({
    weight<- search_results()$weight
    return(paste0(weight," lbs"))
  })
  
  ###Batting Statistics
  ##Creates the interactive table returns the player's batting statistics
  batting_results <- eventReactive(input$search_button,{
    
    
    res <- dbSendQuery(con, paste0("SELECT * FROM master WHERE NAMES == '",input$search,"'"))
    res<- dbFetch(res)

    playerone <- res$playerID
    # You can fetch all results:
    Batting <- dbSendQuery(con, paste0("SELECT * FROM batting"))
    Batting <- dbFetch(Batting)
    playerstats <- subset(Batting,Batting$playerID == playerone)
    
    playerstats <- as.data.frame(playerstats)
    
    playerstats <- playerstats[,c(2,4:14)]
    
    colnames(playerstats) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
    
    playerstats
    
  })
  

  ###Renders the pitching statistics table that the user can search
  pitching_results <- eventReactive(input$search_button,{
    
    res <- dbSendQuery(con, paste0("SELECT * FROM master WHERE NAMES == '",input$search,"'"))
    res<- dbFetch(res)
    
    # playerinfo <- subset(Master,NAMES == input$name)
    
    player <- res$playerID
    
    # You can fetch all results:
    Pitching <- dbSendQuery(con, paste0("SELECT * FROM pitching"))
    Pitching <- dbFetch(Pitching)
    # playerstats <- subset(Batting,Batting$playerID == playerone)
    
    playerstats <- subset(Pitching,Pitching$playerID == player)
    
    playerstats <- as.data.frame(playerstats)
    
    playerstats <- playerstats[,c(2,4:17,20)]
    
    colnames(playerstats) <- c("Year","Team","League","W","L","G","GS","CG","SHO","SV","IPOUTS","H","ER","BB","SO","ERA")
    
    playerstats
    
  })

  
###Player Stats Info
  
  # Generate an HTML table view of the head of the data ----
  observeEvent(input$search_button,{output$table <- DT::renderDataTable({
    
    # data.frame(search_results())
    datatable(search_results())
  })
  })

  
  # Generate an HTML table view of the head of the data ----
  observeEvent(input$search_button,{output$battingStats <- DT::renderDataTable({
    
    # data.frame(search_results())
    datatable(batting_results())
  })
  })
  
  # Generate an DT table that has Pitching Statistics if no statistics return data saying so
  observeEvent(input$search_button,{
  output$pitching <-  renderUI({
    tagList(
    if(nrow(pitching_results())==0){
    output$pitching <-  renderText(paste0(input$search," has no pitching statistics"))
    }else{
    output$pitchingStats <- DT::renderDataTable({
    
    # data.frame(search_results())
    print(pitching_results())
    datatable(pitching_results())
    
  })
    }
  )
  })
  })
  
  
  observeEvent(input$search_button,{output$plot <- renderPlot({
    
    withProgress(message = 'Creating Home Run Comparison Graph',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    res <- dbSendQuery(con, paste0("SELECT * FROM master WHERE NAMES == '",input$search,"'"))
    res<- dbFetch(res)
    
    getinfo <- function(name){
      playerline <- subset(res,NAMES==name)
      name.code <- as.character(playerline$playerID)
      birthyear <- playerline$birthYear
      birthmonth <- playerline$birthMonth
      birthday <- playerline$birthDay
      byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
      list(name.code=name.code, byear=byear)}
    
    
    playerone <- getinfo(input$search)
    
    playertwo <- getinfo("Babe Ruth")
    
    playerthree <- getinfo("Hank Aaron")
    
    playerfour <- getinfo("Barry Bonds")
    
    # You can fetch all results:
    Batting <- dbSendQuery(con, paste0("SELECT * FROM batting"))
    Batting <- dbFetch(Batting)
    playeronedata <- subset(Batting, playerID == playerone$name.code)
    playeronedata$Age <- playeronedata$yearID - playerone$byear
    
    playertwodata <- subset(Batting, playerID == playertwo$name.code)
    playertwodata$Age <- playertwodata$yearID - playertwo$byear
    
    playerthreedata <- subset(Batting, playerID == playerthree$name.code)
    playerthreedata$Age <- playerthreedata$yearID - playerthree$byear
    
    playerfourdata <- subset(Batting, playerID == playerfour$name.code)
    playerfourdata$Age <- playerfourdata$yearID - playerfour$byear
    
    
    with(playeronedata, plot(Age, cumsum(HR), type="l", lty=3, lwd=2,
                             xlab="Age", ylab="Career Home Runs",
                             xlim=c(18, 45), ylim=c(0, 800),col = "blue",main = input$search))
    #with(playertwodata, lines(Age, cumsum(HR), lty=2, lwd=2,col="green"))
    #with(playerthreedata, lines(Age, cumsum(HR), lty=1, lwd=2,col ="red"))
    #with(playerfourdata, lines(Age, cumsum(HR), lty=4, lwd=2,col="orange"))
    # legend(20, 700, legend=c(playerone,
    #                          #, playertwo$NAMES, playerthree$NAMES, playerfour$NAMES),
    #        lty=1 : 4, lwd=2,col=c("blue")
    #                               #,"green","red","orange")
    #        )
    # )
  })})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

