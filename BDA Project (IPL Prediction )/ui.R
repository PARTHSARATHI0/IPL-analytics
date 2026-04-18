library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

ui <- dashboardPage(
  skin = "purple",

  # ── Header ──────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      icon("fire", lib = "font-awesome", style = "color: #FF7675;"),
      " IPL AI Insights Hub"
    ),
    titleWidth = 300
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("house"), selected = TRUE),
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Batting Analytics", tabName = "batting", icon = icon("baseball-bat-ball")),
      menuItem("Bowling Analytics", tabName = "bowling", icon = icon("bullseye")),
      menuItem("Venue & Toss", tabName = "venue", icon = icon("location-dot")),
      menuItem("Venue Deep Dive", tabName = "venue_deep", icon = icon("map-location-dot", class = "text-info")),

      # --- AI FEATURES ---
      menuItem("AI Player Recommender", tabName = "ai_recommender", icon = icon("robot", style = "color: #FDCB6E;")),
      menuItem("AI Scouting Report", tabName = "ai_analyst", icon = icon("file-signature", style = "color: #55EFC4;")),
      # ------------------------

      menuItem("Match Prediction", tabName = "ml", icon = icon("wand-magic-sparkles", style = "color: #FF7675;")),
      menuItem("Live Win Predictor", tabName = "live_ml", icon = icon("bolt", style = "color: #FDCB6E;")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    tags$br(),
    tags$div(
      style = "padding: 15px; color: #b8b8b8; font-size: 12px; text-align: center;",
      tags$p("IPL Seasons 2008 - 2019"),
      tags$p("756 Matches | 179K+ Deliveries"),
      tags$hr(style = "border-color: #444;"),
      tags$p("⚡ Powered by Advanced XAI")
    )
  ),

  # ── Body ────────────────────────────────────────────────────────────
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&display=swap');
        body { font-family: 'Poppins', sans-serif; background-color: #f4f6f9; }

        @keyframes bgSlideshow {
          0% { background: linear-gradient(rgba(244,246,249,0.85), rgba(244,246,249,0.95)), url('stadium.png') no-repeat center center fixed; background-size: cover; }
          33% { background: linear-gradient(rgba(244,246,249,0.85), rgba(244,246,249,0.95)), url('bowler.png') no-repeat center center fixed; background-size: cover; }
          66% { background: linear-gradient(rgba(244,246,249,0.9), rgba(244,246,249,0.98)), url('data.png') no-repeat center center fixed; background-size: cover; }
          100% { background: linear-gradient(rgba(244,246,249,0.85), rgba(244,246,249,0.95)), url('stadium.png') no-repeat center center fixed; background-size: cover; }
        }

        .content-wrapper {
          font-family: 'Poppins', sans-serif;
          animation: bgSlideshow 30s infinite;
          background-size: cover !important;
          background-position: center center !important;
          background-attachment: fixed !important;
        }

        .main-header .logo { background-color: #3498db !important; font-weight: 600; color: #fff !important; }
        .main-header .navbar { background-color: #3498db !important; }

        /* Glassmorphism Effect for Boxes */
        .box {
          border-radius: 12px !important;
          border: none !important;
          box-shadow: 0 4px 15px 0 rgba(31, 38, 135, 0.05) !important;
          background: rgba(255, 255, 255, 0.95) !important;
          transition: transform 0.2s ease;
        }
        .box:hover { transform: translateY(-3px); }

        .small-box { border-radius: 10px; overflow: hidden; box-shadow: 0 5px 15px rgba(0,0,0,0.05); }

        /* Sidebar Styling */
        .main-sidebar { background-color: #2c3e50 !important; }
        .sidebar-menu > li > a { color: #ecf0f1 !important; border-left: 4px solid transparent; }
        .sidebar-menu > li.active > a { background: #34495e !important; border-left: 4px solid #3498db !important; color: #fff !important; }

        .selectize-dropdown { z-index: 9999 !important; background-color: #ffffff !important; color: #2c3e50 !important; border-radius: 8px !important; box-shadow: 0 5px 15px rgba(0,0,0,0.1) !important; }
        .selectize-input { border-radius: 8px !important; color: #2c3e50 !important; }

        .ai-text-box {
          background: #ffffff;
          color: #2c3e50;
          padding: 20px;
          border-radius: 8px;
          border-left: 5px solid #3498db;
          font-family: 'Fira Code', monospace;
          box-shadow: 0 4px 10px rgba(0,0,0,0.05);
        }

        /* Vibrant Landing Cards */
        .landing-card-wrapper .action-button { width: 100%; text-decoration: none !important; display: block; }
        .vibrant-card {
          border-radius: 15px !important;
          color: #333 !important;
          padding: 25px 20px;
          margin-bottom: 25px;
          position: relative;
          overflow: hidden;
          box-shadow: 0 5px 15px rgba(0,0,0,0.08) !important;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
          text-align: center;
        }
        .vibrant-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 25px rgba(0,0,0,0.12) !important;
        }
        .vibrant-card::after { display: none; }
        .vibrant-card::before { display: none; }
        .landing-icon { font-size: 45px; margin-bottom: 12px; color: #3498db !important; }

        .card-gradient-1 { background: linear-gradient(rgba(255,255,255,0.8), rgba(255,255,255,0.95)), url('stadium.png') center/cover; }
        .card-gradient-2 { background: linear-gradient(rgba(255,255,255,0.7), rgba(255,255,255,0.98)), url('batsman.png') center/cover; }
        .card-gradient-3 { background: linear-gradient(rgba(255,255,255,0.7), rgba(255,255,255,0.98)), url('bowler.png') center/cover; }
        .card-gradient-4 { background: linear-gradient(rgba(255,255,255,0.8), rgba(255,255,255,0.95)), url('stadium.png') center/cover; }
        .card-gradient-5 { background: linear-gradient(rgba(255,255,255,0.7), rgba(255,255,255,0.98)), url('data.png') center/cover; }
        .card-gradient-6 { background: linear-gradient(rgba(255,255,255,0.7), rgba(255,255,255,0.98)), url('data.png') center/cover; }

        .landing-badge {
          position: absolute; top: 12px; left: 12px;
          background: rgba(52, 152, 219, 0.1); color: #3498db;
          border: 1px solid rgba(52, 152, 219, 0.2);
          padding: 4px 10px; border-radius: 8px;
          font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px;
        }
        .vibrant-card h3 { color: #2c3e50 !important; font-weight: 700; margin-top: 10px; margin-bottom: 10px; }
        .vibrant-card p { color: #596275 !important; font-size: 14px; line-height: 1.5; }

        .ipl-gallery-img {
          width: 100%;
          height: 220px;
          object-fit: cover;
          border-radius: 15px;
          box-shadow: 0 10px 20px rgba(0,0,0,0.15);
          transition: transform 0.3s ease;
        }
        .ipl-gallery-img:hover {
          transform: scale(1.03);
          box-shadow: 0 15px 30px rgba(0,0,0,0.25);
        }

      "))
    ),
    tabItems(
      # ── Home (Landing Page) ───────────────────────────────────────────
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12, align = "center",
            tags$div(
              style = "background: linear-gradient(rgba(52, 152, 219, 0.85), rgba(41, 128, 185, 0.85)), url('stadium.png') center/cover no-repeat; color: white; border-radius: 12px; padding: 50px 20px; margin-bottom: 30px; box-shadow: 0 5px 15px rgba(0,0,0,0.1); position: relative; overflow: hidden;",
              tags$h1(style = "font-weight: 700; margin-top: 0; font-size: 40px; letter-spacing: -0.5px;", "IPL AI Insights Hub ", tags$span(style = "color:#FFD700;", "🏏")),
              tags$h4(
                style = "opacity: 0.95; font-weight: 400; max-width: 800px; margin: 20px auto 0 auto; line-height: 1.6; font-size: 18px;",
                "Your ultimate interactive dashboard for exploring historical data, deep diving into geographical trends, comparing player stats, and leveraging advanced Artificial Intelligence to predict match outcomes."
              )
            )
          )
        ),
        fluidRow(
          class = "landing-card-wrapper",
          column(
            width = 4,
            actionLink("nav_overview", label = tags$div(
              class = "vibrant-card card-gradient-1",
              tags$span(class = "landing-badge", "Core Stats"),
              HTML("<div class='landing-icon'><i class='fas fa-chart-line'></i></div>"),
              tags$h3("Overview & Trends"),
              tags$p("Explore global match statistics, season-by-season growth and overall team performance.")
            ))
          ),
          column(
            width = 4,
            actionLink("nav_batting", label = tags$div(
              class = "vibrant-card card-gradient-2",
              tags$span(class = "landing-badge", "Deep Dive"),
              HTML("<div class='landing-icon'><i class='fas fa-baseball-bat-ball'></i></div>"),
              tags$h3("Batting Analytics"),
              tags$p("Interactive analysis of strike rates, boundary percentages, and top run scorers.")
            ))
          ),
          column(
            width = 4,
            actionLink("nav_bowling", label = tags$div(
              class = "vibrant-card card-gradient-3",
              tags$span(class = "landing-badge", "Deep Dive"),
              HTML("<div class='landing-icon'><i class='fas fa-bullseye'></i></div>"),
              tags$h3("Bowling Analytics"),
              tags$p("Analyze wicket distribution, economy rates, and top bowlers of the tournament.")
            ))
          )
        ),
        fluidRow(
          class = "landing-card-wrapper",
          column(
            width = 4,
            actionLink("nav_venue", label = tags$div(
              class = "vibrant-card card-gradient-4",
              tags$span(class = "landing-badge", "Spatial DB"),
              HTML("<div class='landing-icon'><i class='fas fa-map-location-dot'></i></div>"),
              tags$h3("Venue Deep Dive"),
              tags$p("Detailed scoring heatmaps and venue-based spatial analysis across host cities.")
            ))
          ),
          column(
            width = 4,
            actionLink("nav_ai", label = tags$div(
              class = "vibrant-card card-gradient-5",
              tags$span(class = "landing-badge", "AI Hub"),
              HTML("<div class='landing-icon'><i class='fas fa-robot'></i></div>"),
              tags$h3("AI Scouting"),
              tags$p("Discover similar players using K-Means clustering and read generated scout reports.")
            ))
          ),
          column(
            width = 4,
            actionLink("nav_data", label = tags$div(
              class = "vibrant-card card-gradient-6",
              tags$span(class = "landing-badge", "Raw Data"),
              HTML("<div class='landing-icon'><i class='fas fa-table'></i></div>"),
              tags$h3("Data Explorer"),
              tags$p("Directly scan, search and query the raw foundation datasets for IPL matches.")
            ))
          )
        ),
        fluidRow(
          class = "landing-card-wrapper",
          column(
            width = 4,
            actionLink("nav_ml", label = tags$div(
              class = "vibrant-card card-gradient-1",
              tags$span(class = "landing-badge", "AI Engine"),
              HTML("<div class='landing-icon'><i class='fas fa-wand-magic-sparkles'></i></div>"),
              tags$h3("Match Predictor"),
              tags$p("Pre-match ML modeling to predict winners based on historical face-offs & toss.")
            ))
          ),
          column(
            width = 4,
            actionLink("nav_live_ml", label = tags$div(
              class = "vibrant-card card-gradient-3",
              tags$span(class = "landing-badge", "Live Action"),
              HTML("<div class='landing-icon'><i class='fas fa-bolt'></i></div>"),
              tags$h3("Live Win Predictor"),
              tags$p("In-play dynamic win probability tracked using real-time RRR and match momentum.")
            ))
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h3("Iconic IPL Features", style = "font-weight: 700; color: #2D3436; margin-top: 20px; margin-bottom: 15px; text-align: center;"),
            tags$hr(style = "border-color: #ddd; border-width: 2px; width: 60px; margin: 0 auto 25px;")
          )
        ),
        fluidRow(
          column(
            width = 4,
            tags$img(src = "stadium.png", class = "ipl-gallery-img"),
            tags$p("Epic Stadium Views", style = "text-align:center; font-weight:600; margin-top:12px; color:#636E72;")
          ),
          column(
            width = 4,
            tags$img(src = "batsman.png", class = "ipl-gallery-img"),
            tags$p("Dynamic Batting", style = "text-align:center; font-weight:600; margin-top:12px; color:#636E72;")
          ),
          column(
            width = 4,
            tags$img(src = "bowler.png", class = "ipl-gallery-img"),
            tags$p("High-Speed Bowling", style = "text-align:center; font-weight:600; margin-top:12px; color:#636E72;")
          )
        )
      ),

      # ── Overview Tab ──────────────────────────────────────────────────
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("totalMatches", width = 4),
          valueBoxOutput("totalSeasons", width = 4),
          valueBoxOutput("totalTeams", width = 4)
        ),
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "primary", title = "Interactive Overview Analytics",
            selectInput("overviewPlotChoice", "Select Variable to Analyze:",
              choices = list(
                "League Growth" = c("Matches Played Per Season" = "season"),
                "Team Stats" = c("Total Wins by Team" = "wins"),
                "Match Factors" = c(
                  "Toss Impact on Match Outcome" = "toss",
                  "Top 10 High Activity Venues" = "venue"
                )
              ), width = "100%", selectize = TRUE
            ),
            tags$hr(style = "border-color: #eee; margin-top: 5px; margin-bottom: 15px;"),
            plotlyOutput("dynamicOverviewPlot", height = "450px")
          )
        )
      ),

      # ── Batting Tab ───────────────────────────────────────────────────
      tabItem(
        tabName = "batting",
        fluidRow(
          box(plotlyOutput("topBatsmen", height = "380px"), width = 6, solidHeader = TRUE, status = "success", title = "Top Run Scorers"),
          box(plotlyOutput("strikeRatePlot", height = "380px"), width = 6, solidHeader = TRUE, status = "info", title = "Strike Rate Analysis")
        ),
        fluidRow(
          box(plotlyOutput("deathOvers", height = "380px"), width = 12, solidHeader = TRUE, status = "primary", title = "Death Over Specialists")
        )
      ),

      # ── Bowling Tab ───────────────────────────────────────────────────
      tabItem(
        tabName = "bowling",
        fluidRow(
          box(plotlyOutput("topBowlers", height = "400px"), width = 7, solidHeader = TRUE, status = "danger", title = "Top 10 Wicket Takers"),
          box(plotlyOutput("wicketDonut", height = "400px"), width = 5, solidHeader = TRUE, status = "warning", title = "Dismissal Type Distribution")
        )
      ),

      # ── Venue Deep Dive Tab ──────────────────────────────────────────
      tabItem(
        tabName = "venue_deep",
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "info", title = "Venue Scoring Heatmaps",
            plotlyOutput("venueHeatmap", height = "600px")
          )
        ),
        fluidRow(
          box(
            width = 6, solidHeader = TRUE, status = "primary", title = "City-wise Match Count",
            plotlyOutput("cityMatches")
          ),
          box(
            width = 6, solidHeader = TRUE, status = "success", title = "Avg Runs per City",
            plotlyOutput("cityRuns")
          )
        )
      ),

      # ── Venue & Toss Tab ──────────────────────────────────────────────
      tabItem(
        tabName = "venue",
        fluidRow(
          box(plotlyOutput("venueAnalysis", height = "400px"), width = 7, solidHeader = TRUE, status = "primary", title = "Most Popular Venues"),
          box(plotlyOutput("tossImpact", height = "400px"), width = 5, solidHeader = TRUE, status = "success", title = "Toss Impact on Winning")
        )
      ),

      # ── AI Player Recommender Tab ─────────────────────────────────────
      tabItem(
        tabName = "ai_recommender",
        fluidRow(
          box(
            width = 4, solidHeader = TRUE, status = "warning", title = "AI Search Parameters",
            p("Our AI uses K-Means unsupervised machine learning to cluster players based on their batting profile (Average, Strike Rate, Runs)."),
            selectizeInput("rec_batsman", "Select Reference Batsman:", choices = NULL, options = list(dropdownParent = "body")),
            sliderInput("rec_min_runs", "Minimum Career Runs Filter:", min = 0, max = 3000, value = 300, step = 100),
            tags$hr(),
            h4("Player's AI Profile:"),
            uiOutput("playerProfileBadge")
          ),
          box(
            width = 8, solidHeader = TRUE, status = "primary", title = "Similar Batsmen (AI Recommendations)",
            DTOutput("recommenderTable")
          )
        ),
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "info", title = "3D Interactive AI Cluster Galaxy (Spin & Zoom)",
            plotlyOutput("clusterPlot", height = "600px")
          )
        )
      ),

      # ── AI Scouting Report Tab ────────────────────────────────────────
      tabItem(
        tabName = "ai_analyst",
        fluidRow(
          box(
            width = 4, solidHeader = TRUE, status = "success", title = "Subject Selection",
            selectizeInput("scout_batsman", "Generate Scouting Report for:", choices = NULL, options = list(dropdownParent = "body")),
            actionButton("btn_generate_report", "Generate AI Report", icon = icon("gears"), class = "btn-success btn-lg btn-block", style = "border-radius: 8px;")
          ),
          box(
            width = 8, solidHeader = TRUE, status = "info", title = "AI Scouting Analysis",
            uiOutput("aiScoutText")
          )
        ),
        fluidRow(
          box(
            width = 4, solidHeader = TRUE, status = "warning", title = "Scouting Radar Profile",
            plotlyOutput("scoutRadar", height = "350px")
          ),
          box(
            width = 4, solidHeader = TRUE, status = "danger", title = "Dismissal Vulnerability",
            plotlyOutput("scoutDismissals", height = "350px")
          ),
          box(
            width = 4, solidHeader = TRUE, status = "primary", title = "Run Scoring by Phase",
            plotlyOutput("scoutPhases", height = "350px")
          )
        )
      ),

      # ── ML Match Prediction Tab (Now with XAI) ────────────────────────
      tabItem(
        tabName = "ml",
        fluidRow(
          valueBoxOutput("modelAccuracy", width = 12)
        ),
        fluidRow(
          box(
            width = 4, solidHeader = TRUE, status = "primary", title = "Select Teams & Toss",
            selectInput("team1", "Team 1", choices = NULL),
            selectInput("team2", "Team 2", choices = NULL),
            selectInput("tossWinner", "Toss Winner", choices = c("Team 1", "Team 2")),
            selectInput("tossDecision", "Toss Decision", choices = c("bat", "field")),
            tags$br(),
            actionButton("predict", "Predict Winner", icon = icon("wand-magic-sparkles"), class = "btn-primary btn-lg btn-block", style = "border-radius: 10px; font-weight: 600;")
          ),
          box(
            width = 4, solidHeader = TRUE, status = "success", title = "Prediction Result",
            tags$div(style = "min-height: 250px; display: flex; align-items: center; justify-content: center;", uiOutput("prediction"))
          ),
          box(
            width = 4, solidHeader = TRUE, status = "warning", title = "Explainable AI (Feature Impact)",
            plotlyOutput("varImpPlot", height = "250px")
          )
        )
      ),

      # ── Live Predictor Tab ────────────────────────────────────────────
      tabItem(
        tabName = "live_ml",
        fluidRow(
          box(
            width = 4, solidHeader = TRUE, status = "danger", title = "Live Match Situation (Run Chase)",
            selectInput("live_batting_team", "Batting Team (Chasing):", choices = NULL),
            selectInput("live_bowling_team", "Bowling Team (Defending):", choices = NULL),
            numericInput("live_target", "Target Score:", value = 180, min = 1),
            numericInput("live_current", "Current Score:", value = 80, min = 0),
            sliderInput("live_overs", "Overs Completed (e.g. 10.5):", min = 0, max = 19.5, value = 10.0, step = 0.1),
            sliderInput("live_wickets", "Wickets Lost:", min = 0, max = 9, value = 2, step = 1),
            actionButton("btn_live_predict", "Calculate Live Win Probability", icon = icon("bolt"), class = "btn-warning btn-lg btn-block", style = "border-radius: 10px; font-weight: 600; color: #2D3436;")
          ),
          box(
            width = 4, solidHeader = TRUE, status = "info", title = "In-Play Metrics",
            valueBoxOutput("box_crr", width = 12),
            valueBoxOutput("box_rrr", width = 12),
            valueBoxOutput("box_runs_left", width = 12)
          ),
          box(
            width = 4, solidHeader = TRUE, status = "success", title = "Chasing Team Win Probability",
            plotlyOutput("live_prob_plot", height = "300px")
          )
        )
      ),

      # ── Data Explorer Tab ─────────────────────────────────────────────
      tabItem(
        tabName = "data",
        fluidRow(
          tabBox(
            width = 12, title = "Explore IPL Data",
            tabPanel("Match Results", DTOutput("matchesTable")),
            tabPanel("Batsman Stats", DTOutput("batsmenTable"))
          )
        )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPages
