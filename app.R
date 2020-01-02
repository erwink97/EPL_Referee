#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### LOADING LIBRARIES & INITIALIZING OBJECTS FOR UI #####
library(shiny)
library(dplyr)
library(RSQLite)
library(lme4)
library(sqldf)
# We set the working diectory before running/publishing the app
# setwd("C:/Users/Owner/Documents/Projects/EPL_ref_protect")

# Let's load in our data sets
data <- read.csv("data/all_seasons.csv") %>% filter(Season=="2018-19" | Season=="2017-18" | Season=="2016-17")
current_refs <- read.csv("data/referee_list.csv") %>% filter(Current==1)

# Now we create our list of eligible teams. These teams have played in the EPL during the last 3 seasons (2016-17 through to 2018-19)
n <- c()
c <- unique(data$HomeTeam)
s16_17 <- filter(data,Season=="2016-17")
s17_18 <- filter(data,Season=="2017-18")
s18_19 <- filter(data,Season=="2018-19")
for (i in c) {
  if (i %in% s16_17$HomeTeam & i %in% s17_18$HomeTeam & i %in% s18_19$HomeTeam) {
    n <- append(n,i)
  }
}
n <- sort(n)

# Record some information about metrics for our referee summary
m_FPG <- mean(current_refs$FPG, na.rm=TRUE)
sd_FPG <- sd(current_refs$FPG, na.rm=TRUE)

m_FPT <- mean(current_refs$FPT, na.rm=TRUE)
sd_FPT <- sd(current_refs$FPT, na.rm=TRUE)

m_PPG <- mean(current_refs$PPG, na.rm=TRUE)
sd_PPG <- sd(current_refs$PPG, na.rm=TRUE)

m_YPG <- mean(current_refs$YPG, na.rm=TRUE)
sd_YPG <- sd(current_refs$YPG, na.rm=TRUE)

m_RPG <- mean(current_refs$RPG, na.rm=TRUE)
sd_RPG <- sd(current_refs$RPG, na.rm=TRUE)

##### THE R SHINY APP DEVELOPMENT STARTS HERE #####

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   
   # Application title
   titlePanel("The Ideal EPL Referee"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # textInput(inputId="sel_team",
         #           label="Choose your Team",
         #           value=""),
         # textInput(inputId="opp_team",
         #           label="Choose your Opponent",
         #           value=""),
         selectInput(inputId="sel_team",
                     label="Choose your Team",
                     choices=n),
         selectInput(inputId="opp_team",
                     label="Choose your Opponent",
                     choices=n),
         selectInput(inputId="game_location",
                     label="Where would you like to play?",
                     choices=c("Away","Home")),
         actionButton("action", "Find me a referee!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         # tableOutput("test"), <- THIS PHASE OF TESTING IS COMPLETE
         h3(textOutput("top_ref")),
         htmlOutput("ref_pic"),
         h4(textOutput("ref_sum")),
         textOutput("ref_fpg"),
         textOutput("ref_fpt"),
         textOutput("ref_ppg"),
         textOutput("ref_ypg"),
         textOutput("ref_rpg")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get the name of the top ref
  top_referee <- eventReactive(input$action, {
  
  # First we validate the user input
  validate(
    need(input$sel_team != "", "Please select a team"),
    need(input$opp_team != "", "Please select an opponent"),
    need(input$sel_team != input$opp_team, "You cannot play against your own team")
  )
  
  new_data <- data %>% filter(HomeTeam == input$sel_team | AwayTeam == input$sel_team)
  
  new_data2 <- data %>% filter(HomeTeam==input$opp_team | AwayTeam==input$opp_team)
  
  
  # Create custom variables that we can use for future analysis
  new_data <- new_data %>% mutate(
    selected_GS=case_when(new_data$HomeTeam==input$sel_team~FTHG,new_data$AwayTeam==input$sel_team~FTAG),
    selected_half_GS=case_when(new_data$HomeTeam==input$sel_team~HTHG,new_data$AwayTeam==input$sel_team~HTAG),
    selected_shots=case_when(new_data$HomeTeam==input$sel_team~HS,new_data$AwayTeam==input$sel_team~AS),
    selected_shots_on=case_when(new_data$HomeTeam==input$sel_team~HST,new_data$AwayTeam==input$sel_team~AST),
    selected_yc=case_when(new_data$HomeTeam==input$sel_team~HY,new_data$AwayTeam==input$sel_team~AY),
    selected_rc=case_when(new_data$HomeTeam==input$sel_team~HR,new_data$AwayTeam==input$sel_team~AR),
    selected_name=case_when(new_data$HomeTeam==input$sel_team~HomeTeam,new_data$AwayTeam==input$sel_team~AwayTeam),
    selected_fouls_drawn=case_when(new_data$HomeTeam==input$sel_team~AF,new_data$AwayTeam==input$sel_team~HF),
    selected_corners=case_when(new_data$HomeTeam==input$sel_team~HC,new_data$AwayTeam==input$sel_team~AC),
    other_GS=case_when(new_data$HomeTeam==input$sel_team~FTAG,new_data$AwayTeam==input$sel_team~FTHG),
    other_half_GS=case_when(new_data$HomeTeam==input$sel_team~HTAG,new_data$AwayTeam==input$sel_team~HTHG),
    other_shots=case_when(new_data$HomeTeam==input$sel_team~AS,new_data$AwayTeam==input$sel_team~HS),
    other_shots_on=case_when(new_data$HomeTeam==input$sel_team~AST,new_data$AwayTeam==input$sel_team~HST),
    other_yc=case_when(new_data$HomeTeam==input$sel_team~AY,new_data$AwayTeam==input$sel_team~HY),
    other_rc=case_when(new_data$HomeTeam==input$sel_team~AR,new_data$AwayTeam==input$sel_team~HR),
    other_name=case_when(new_data$HomeTeam==input$sel_team~AwayTeam,new_data$AwayTeam==input$sel_team~HomeTeam),
    other_fouls_drawn=case_when(new_data$HomeTeam==input$sel_team~HF,new_data$AwayTeam==input$sel_team~AF),
    other_corners=case_when(new_data$HomeTeam==input$sel_team~AC,new_data$AwayTeam==input$sel_team~HC),
    game_location=case_when(new_data$HomeTeam==input$sel_team~"Home",new_data$AwayTeam==input$sel_team~"Away"),
    game_location_flag=case_when(new_data$HomeTeam==input$sel_team~1,new_data$AwayTeam==input$sel_team~0)
  )
  # Do the same for opposing team
  new_data2 <- new_data2 %>% mutate(
    selected_GS=case_when(new_data2$HomeTeam==input$opp_team~FTHG,new_data2$AwayTeam==input$opp_team~FTAG),
    selected_half_GS=case_when(new_data2$HomeTeam==input$opp_team~HTHG,new_data2$AwayTeam==input$opp_team~HTAG),
    selected_shots=case_when(new_data2$HomeTeam==input$opp_team~HS,new_data2$AwayTeam==input$opp_team~AS),
    selected_shots_on=case_when(new_data2$HomeTeam==input$opp_team~HST,new_data2$AwayTeam==input$opp_team~AST),
    selected_yc=case_when(new_data2$HomeTeam==input$opp_team~HY,new_data2$AwayTeam==input$opp_team~AY),
    selected_rc=case_when(new_data2$HomeTeam==input$opp_team~HR,new_data2$AwayTeam==input$opp_team~AR),
    selected_name=case_when(new_data2$HomeTeam==input$opp_team~HomeTeam,new_data2$AwayTeam==input$opp_team~AwayTeam),
    selected_fouls_drawn=case_when(new_data2$HomeTeam==input$opp_team~AF,new_data2$AwayTeam==input$opp_team~HF),
    selected_corners=case_when(new_data2$HomeTeam==input$opp_team~HC,new_data2$AwayTeam==input$opp_team~AC),
    other_GS=case_when(new_data2$HomeTeam==input$opp_team~FTAG,new_data2$AwayTeam==input$opp_team~FTHG),
    other_half_GS=case_when(new_data2$HomeTeam==input$opp_team~HTAG,new_data2$AwayTeam==input$opp_team~HTHG),
    other_shots=case_when(new_data2$HomeTeam==input$opp_team~AS,new_data2$AwayTeam==input$opp_team~HS),
    other_shots_on=case_when(new_data2$HomeTeam==input$opp_team~AST,new_data2$AwayTeam==input$opp_team~HST),
    other_yc=case_when(new_data2$HomeTeam==input$opp_team~AY,new_data2$AwayTeam==input$opp_team~HY),
    other_rc=case_when(new_data2$HomeTeam==input$opp_team~AR,new_data2$AwayTeam==input$opp_team~HR),
    other_name=case_when(new_data2$HomeTeam==input$opp_team~AwayTeam,new_data2$AwayTeam==input$opp_team~HomeTeam),
    other_fouls_drawn=case_when(new_data2$HomeTeam==input$opp_team~HF,new_data2$AwayTeam==input$opp_team~AF),
    other_corners=case_when(new_data2$HomeTeam==input$opp_team~AC,new_data2$AwayTeam==input$opp_team~HC),
    opp_game_location=case_when(new_data2$HomeTeam==input$opp_team~"Home",new_data2$AwayTeam==input$opp_team~"Away"),
    opp_game_location_flag=case_when(new_data2$HomeTeam==input$opp_team~1,new_data2$AwayTeam==input$opp_team~0)
  )
  
  # grab summary statistics
  avg_shots_on <- mean(new_data$selected_shots_on)
  avg_opp_shots_on <- mean(new_data2$selected_shots_on)
  avg_corners <- mean(new_data$selected_corners)
  avg_opp_corners <- mean(new_data2$selected_corners)
  avg_fouls_drawn <- mean(new_data$selected_fouls_drawn)
  avg_opp_fouls_drawn <- mean(new_data2$selected_fouls_drawn)
  
  
  # Write query to transform data
  query1 <- paste("select Date as date, Referee as referee, selected_GS, selected_half_GS, selected_shots, selected_shots_on, 
                  selected_yc, selected_rc, selected_name, other_GS, other_half_GS, other_shots, other_shots_on, other_yc, other_rc, other_name,
                  (selected_GS-other_GS) as goal_diff, selected_fouls_drawn, other_fouls_drawn, selected_corners, other_corners, game_location,
                  game_location_flag
                  from new_data")
  new_data <- sqldf(query1)
  
  query2 <- paste("select Date as opp_date, Referee as opp_referee, selected_GS as opp_GS, selected_half_GS as opp_half_GS, 
                  selected_shots as opp_shots,  selected_shots_on as opp_shots_on, selected_yc as opp_yc, selected_rc as opp_rc, selected_name as opp_name, 
                  other_GS as opp_other_GS, other_half_GS as opp_other_half_GS, other_shots as opp_other_shots, other_shots_on as opp_other_shots_on, 
                  other_yc as opp_other_yc, other_rc as opp_other_rc, other_name as opp_other_name, (selected_GS-other_GS) as opp_goal_diff, 
                  selected_fouls_drawn as opp_fouls_drawn, other_fouls_drawn as opp_other_fouls_drawn, selected_corners as opp_corners, 
                  other_corners as opp_other_corners, opp_game_location, opp_game_location_flag
                  from new_data2")
  new_data2 <- sqldf(query2)
  
  # Sort the dates and merge them
  # Sorting seems to be done automatically
  # new_data$date <- as.Date(new_data$date,"%d/%m/%Y")
  # new_data2$opp_date <- as.Date(new_data2$opp_date,"%d/%m/%Y")
  # new_data <- new_data[order(new_data$date),]
  # new_data2 <- new_data2[order(new_data2$opp_date),]
  final_data <- cbind(new_data,new_data2) %>% mutate(res=(goal_diff-opp_goal_diff))
  
  # Some more summary statistics
  avg_gd <- mean(new_data$goal_diff)
  avg_opp_gd <- mean(new_data2$opp_goal_diff)

  suppressWarnings({ 
    m5 <- lmer(res ~ referee + goal_diff + opp_goal_diff + selected_shots_on + opp_shots_on + game_location_flag + opp_game_location_flag +
                 selected_corners + opp_corners + selected_fouls_drawn + opp_fouls_drawn + (1 | referee), final_data)
    })
  
  suppressWarnings({
    vals <- unique(final_data$referee)
    vals2 <- current_refs$Name
    ref_list <- data.frame(referee=vals)
    curr <- data.frame(Name=vals2)
    # ref_list <- setdiff(ref_list,curr)
    ref_list <- inner_join(ref_list,curr,by=c("referee"="Name"))
  })
  
  
  # A function that determines the best ref for the job
  best_ref <- function(opponent, location_flag) {
    comp <- data.frame(Referee=character(), Score=double())
    for (ref in ref_list) {
      test <- data.frame(
        referee = ref,
        goal_diff = avg_gd,
        opp_goal_diff = avg_opp_gd,
        selected_shots_on = avg_shots_on,
        opp_shots_on = avg_opp_shots_on,
        game_location_flag = location_flag,
        opp_game_location_flag = abs(1-location_flag),
        selected_corners = avg_corners,
        opp_corners = avg_opp_corners,
        selected_fouls_drawn = avg_fouls_drawn,
        opp_fouls_drawn = avg_opp_fouls_drawn
      )
      # make prediction
      pred <- predict(m5, test)
      # add prediction to dataframe
      new <- data.frame(Referee=ref,Score=pred)
      comp <- rbind(comp,new)
    }
    # Once we have all predcitions, return the ref with the highest prediction
    best <- sqldf("select Referee from
                  (select Referee, max(Score)
                  from comp)")
    return(best[1,1])
  }
  # Run the function with the input data
  best_ref(input$opp_team,0)

  })
  
  get_info <- eventReactive(input$action, {
    # Let's return a list with the name, description and the image url
    suppressWarnings({
      temp <- data.frame(Referee=top_referee())
      use <- inner_join(temp,current_refs,by=c("Referee"="Name"))
      use <- sqldf("select Full_Name, FPG, FPT, PPG, YPG, RPG, Photo
                   from use")
    })
  })
  
  # Here we assign a category for how the ref compares to his compatriots for the recorded metrics
  fpg_level <- eventReactive(input$action, {
    m <- get_info()$FPG
    if (m <= m_FPG+sd_FPG & m >= m_FPG-sd_FPG) {
      "(Average)"
    } else if (m > m_FPG+sd_FPG) {
      "(High)"
    } else {
      "(Low)"
    }
  })
  fpt_level <- eventReactive(input$action, {
    m <- get_info()$FPT
    if (m <= m_FPT+sd_FPT & m >= m_FPT-sd_FPT) {
      "(Average)"
    } else if (m > m_FPT+sd_FPT) {
      "(High)"
    } else {
      "(Low)"
    }
  })
  ppg_level <- eventReactive(input$action, {
    m <- get_info()$PPG
    if (m <= m_PPG+sd_PPG & m >= m_PPG-sd_PPG) {
      "(Average)"
    } else if (m > m_PPG+sd_PPG) {
      "(High)"
    } else {
      "(Low)"
    }
  })
  ypg_level <- eventReactive(input$action, {
    m <- get_info()$YPG
    if (m <= m_YPG+sd_YPG & m >= m_YPG-sd_YPG) {
      "(Average)"
    } else if (m > m_YPG+sd_YPG) {
      "(High)"
    } else {
      "(Low)"
    }
  })
  rpg_level <- eventReactive(input$action, {
    m <- get_info()$RPG
    if (m <= m_RPG+sd_RPG & m >= m_RPG-sd_RPG) {
      "(Average)"
    } else if (m > m_RPG+sd_RPG) {
      "(High)"
    } else {
      "(Low)"
    }
  })
  
  
  observeEvent(input$action, {
    print("The action button has been clicked")
  })
  
  # Get name of referee
  output$top_ref <- renderText({
    # as.character(top_referee())
    new <- get_info()
    as.character(new$Full_Name)
  })
  
  output$ref_sum <- renderText({
    new <- get_info()
    "Referee summary (vs other referees in EPL)"
  })
  
  
  output$ref_fpg <- renderText({
    new <- get_info() 
    paste("Fouls called per game: ", new$FPG, " ", fpg_level())
  })
  
  output$ref_fpt <- renderText({
    new <- get_info() 
    paste("Fouls called per tackle: ", new$FPT, " ", fpt_level())
  })
  
  output$ref_ppg <- renderText({
    new <- get_info() 
    paste("Penalties awarded per game: ", new$PPG, " ", ppg_level())
  })
  
  output$ref_ypg <- renderText({
    new <- get_info() 
    paste("Yellow cards per game: ", new$YPG, " ", ypg_level())
  })
  
  output$ref_rpg <- renderText({
    new <- get_info() 
    paste("Red cards per game: ", new$RPG, " ", rpg_level())
  })
  
  # image output for picture of referee

  output$ref_pic <- renderText({
    new <- get_info()
    src <- as.character(new$Photo)
    c('<img src="',src,'" width="450px" height="320px"  ">')
  })
  
  # # This was created for testing. It is no longer needed
  # output$test <- renderTable({
  #   get_info()
  # })
  
}

# Run the application 
if (interactive()) {
  shinyApp(ui = ui, server = server)
}

shinyApp(ui = ui, server = server)
