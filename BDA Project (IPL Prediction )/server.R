library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(caret)
library(randomForest)
library(RColorBrewer)

server <- function(input, output, session) {
  # ── Navigation Observers (Landing Page Links) ───────────────────────
  observeEvent(input$nav_overview, {
    updateTabItems(session, "tabs", "overview")
  })
  observeEvent(input$nav_batting, {
    updateTabItems(session, "tabs", "batting")
  })
  observeEvent(input$nav_bowling, {
    updateTabItems(session, "tabs", "bowling")
  })
  observeEvent(input$nav_venue, {
    updateTabItems(session, "tabs", "venue_deep")
  })
  observeEvent(input$nav_ai, {
    updateTabItems(session, "tabs", "ai_recommender")
  })
  observeEvent(input$nav_data, {
    updateTabItems(session, "tabs", "data")
  })
  observeEvent(input$nav_ml, {
    updateTabItems(session, "tabs", "ml")
  })
  observeEvent(input$nav_live_ml, {
    updateTabItems(session, "tabs", "live_ml")
  })

  # ── 1. Load Data ───────────────────────────────────────────────────
  matches <- read.csv("data/matches.csv", stringsAsFactors = FALSE)
  deliveries <- read.csv("data/deliveries.csv", stringsAsFactors = FALSE)

  # Load the Live Match Predictor Model
  live_model_lr <- readRDS("data/live_model_lr.rds")

  matches$SeasonYear <- as.integer(gsub("IPL-", "", matches$Season))

  # ── 2. AI Precomputations (Clustering) ─────────────────────────────

  # Calculate dismissals per batsman natively
  dismissals <- deliveries %>%
    filter(player_dismissed != "") %>%
    group_by(player_dismissed) %>%
    summarise(outs = n(), .groups = "drop") %>%
    rename(batsman = player_dismissed)

  # Aggregate batsman stats
  bat_stats <- deliveries %>%
    group_by(batsman) %>%
    summarise(
      runs = sum(batsman_runs),
      balls = n(),
      fours = sum(batsman_runs == 4),
      sixes = sum(batsman_runs == 6),
      .groups = "drop"
    ) %>%
    filter(runs >= 200) %>% # Minimum 200 runs to be considered
    left_join(dismissals, by = "batsman") %>%
    mutate(
      outs = ifelse(is.na(outs) | outs == 0, 1, outs),
      average = runs / outs,
      strike_rate = (runs / balls) * 100,
      boundary_pct = ((fours * 4 + sixes * 6) / runs) * 100
    ) %>%
    na.omit()

  # K-Means Clustering on scaled features
  set.seed(123)
  features <- bat_stats %>% select(average, strike_rate, boundary_pct)
  scaled_f <- scale(features)
  km <- kmeans(scaled_f, centers = 4, nstart = 25)
  bat_stats$cluster <- as.factor(km$cluster)

  # Map cluster names logically based on centers
  # We find the cluster with highest average, highest SR, etc. to name them.
  centers <- data.frame(km$centers)
  centers$Cluster <- as.factor(1:4)
  # A rough heuristic naming map:
  cluster_labels <- c("1" = "Anchor/Sheet Anchor", "2" = "Aggressive Opener", "3" = "Middle Order Accumulator", "4" = "Power Finisher")
  bat_stats$Profile <- cluster_labels[as.character(bat_stats$cluster)]

  # ── 2. Global Predictor Inputs ─────────────────────────────────────
  # Predictor Inputs (team names from matches dataset)
  teams <- sort(unique(c(matches$team1, matches$team2)))
  updateSelectInput(session, "team1", choices = teams, selected = teams[1])
  updateSelectInput(session, "team2", choices = teams, selected = teams[2])

  updateSelectInput(session, "live_batting_team", choices = teams, selected = teams[1])
  updateSelectInput(session, "live_bowling_team", choices = teams, selected = teams[2])

  # Update UI Dropdowns
  all_batsmen <- sort(unique(bat_stats$batsman))
  updateSelectizeInput(session, "rec_batsman", choices = all_batsmen)
  updateSelectizeInput(session, "scout_batsman", choices = all_batsmen)

  # ── 3. Base Dashboard Logic ────────────────────────────────────────

  output$totalMatches <- renderValueBox({
    valueBox(nrow(matches), "Total Matches", icon = icon("cricket-bat-ball"), color = "purple")
  })
  output$totalSeasons <- renderValueBox({
    valueBox(length(unique(matches$Season)), "Seasons", icon = icon("calendar"), color = "blue")
  })
  output$totalTeams <- renderValueBox({
    teams_all <- unique(c(matches$team1, matches$team2))
    valueBox(length(teams_all), "Teams", icon = icon("users"), color = "green")
  })

  output$dynamicOverviewPlot <- renderPlotly({
    req(input$overviewPlotChoice)
    if (input$overviewPlotChoice == "season") {
      season_runs <- matches %>%
        group_by(SeasonYear) %>%
        summarise(matches_played = n(), .groups = "drop") %>%
        arrange(SeasonYear)
      p <- ggplot(season_runs, aes(SeasonYear, matches_played)) +
        geom_line(color = "#6C5CE7", size = 1.5) +
        geom_point(size = 3, color = "#A29BFE") +
        labs(title = "Matches Played Per Season", x = "Season", y = "Matches") +
        theme_minimal()
      ggplotly(p)
    } else if (input$overviewPlotChoice == "wins") {
      team_wins <- matches %>%
        filter(winner != "") %>%
        group_by(winner) %>%
        summarise(wins = n(), .groups = "drop") %>%
        arrange(desc(wins))
      p <- ggplot(team_wins, aes(reorder(winner, wins), wins)) +
        geom_col(fill = "#FDCB6E") +
        coord_flip() +
        labs(title = "Total Wins by Team", x = "", y = "Wins") +
        theme_minimal()
      ggplotly(p)
    } else if (input$overviewPlotChoice == "toss") {
      t_imp <- matches %>%
        filter(winner != "") %>%
        mutate(tww = ifelse(toss_winner == winner, "Yes", "No")) %>%
        group_by(tww) %>%
        summarise(count = n(), .groups = "drop")
      plot_ly(t_imp, labels = ~tww, values = ~count, type = "pie", marker = list(colors = c("#D63031", "#00B894")), hole = 0.4) %>% layout(title = "Did Toss Winner Win the Match?")
    } else if (input$overviewPlotChoice == "venue") {
      v_stat <- matches %>%
        group_by(venue) %>%
        summarise(m = n(), .groups = "drop") %>%
        arrange(desc(m)) %>%
        head(10)
      p <- ggplot(v_stat, aes(reorder(venue, m), m)) +
        geom_col(fill = "#6C5CE7") +
        coord_flip() +
        labs(title = "Top Venues by Matches", x = "", y = "Matches") +
        theme_minimal()
      ggplotly(p)
    }
  })

  output$topBatsmen <- renderPlotly({
    top_b <- bat_stats %>%
      arrange(desc(runs)) %>%
      head(10)
    p <- ggplot(top_b, aes(reorder(batsman, runs), runs)) +
      geom_col(fill = "#00B894") +
      coord_flip() +
      labs(title = "Top 10 Run Scorers", x = "", y = "Runs") +
      theme_minimal()
    ggplotly(p)
  })

  output$strikeRatePlot <- renderPlotly({
    p <- ggplot(bat_stats %>% filter(runs > 1500), aes(average, strike_rate, text = batsman)) +
      geom_point(size = 4, color = "#0984E3", alpha = 0.8) +
      labs(title = "Strike Rate vs Average (min 1500 runs)", x = "Batting Average", y = "Strike Rate") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  output$deathOvers <- renderPlotly({
    d_overs <- deliveries %>%
      filter(over >= 16) %>%
      group_by(batsman) %>%
      summarise(runs = sum(batsman_runs), .groups = "drop") %>%
      arrange(desc(runs)) %>%
      head(10)
    p <- ggplot(d_overs, aes(reorder(batsman, runs), runs)) +
      geom_col(fill = "#00CEC9") +
      coord_flip() +
      labs(title = "Best Death Over Batsmen", x = "", y = "Runs") +
      theme_minimal()
    ggplotly(p)
  })

  output$topBowlers <- renderPlotly({
    b_stats <- deliveries %>%
      filter(dismissal_kind != "" & !dismissal_kind %in% c("run out", "retired hurt", "obstructing the field")) %>%
      group_by(bowler) %>%
      summarise(w = n(), .groups = "drop") %>%
      arrange(desc(w)) %>%
      head(10)
    p <- ggplot(b_stats, aes(reorder(bowler, w), w)) +
      geom_col(fill = "#D63031") +
      coord_flip() +
      labs(title = "Top 10 Wicket Takers", x = "", y = "Wickets") +
      theme_minimal()
    ggplotly(p)
  })

  output$wicketDonut <- renderPlotly({
    w_dist <- deliveries %>%
      filter(dismissal_kind != "" & !dismissal_kind %in% c("retired hurt", "obstructing the field")) %>%
      group_by(dismissal_kind) %>%
      summarise(count = n(), .groups = "drop")
    plot_ly(w_dist, labels = ~dismissal_kind, values = ~count, type = "pie", hole = 0.6, marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))) %>% layout(title = "Dismissal Type Distribution")
  })

  # ── 3.5 Venue Deep Dive Logic ─────────────────────────────────────

  output$venueHeatmap <- renderPlotly({
    # Scoring by over block and venue
    v_over <- deliveries %>%
      left_join(matches %>% select(id, venue), by = c("match_id" = "id")) %>%
      mutate(over_block = case_when(
        over <= 6 ~ "Powerplay (1-6)",
        over <= 15 ~ "Middle (7-15)",
        TRUE ~ "Death (16-20)"
      )) %>%
      group_by(venue, over_block) %>%
      summarise(avg_runs = mean(total_runs) * 6, .groups = "drop") %>% # Avg runs per over in that block
      filter(venue %in% head(unique(matches$venue), 15)) # Top 15 venues for clarity

    p <- ggplot(v_over, aes(over_block, venue, fill = avg_runs)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#f1f2f6", high = "#ff7675") +
      labs(title = "Avg Runs Per Over by Phase & Venue", x = "Match Phase", y = "", fill = "Avg RPO") +
      theme_minimal()
    ggplotly(p)
  })

  output$cityMatches <- renderPlotly({
    c_m <- matches %>%
      filter(city != "") %>%
      group_by(city) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      head(10)
    p <- ggplot(c_m, aes(reorder(city, count), count, fill = city)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = "Matches Played by City", x = "", y = "Matches") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$cityRuns <- renderPlotly({
    c_r <- deliveries %>%
      left_join(matches %>% select(id, city), by = c("match_id" = "id")) %>%
      filter(city != "") %>%
      group_by(city) %>%
      summarise(avg_match_runs = sum(total_runs) / n_distinct(match_id), .groups = "drop") %>%
      arrange(desc(avg_match_runs)) %>%
      head(10)
    p <- ggplot(c_r, aes(reorder(city, avg_match_runs), avg_match_runs, fill = city)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "mako") +
      labs(title = "Highest Scoring Cities (Avg Runs)", x = "", y = "Avg Match Score") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$venueAnalysis <- renderPlotly({
    v_stat <- matches %>%
      group_by(venue) %>%
      summarise(m = n(), .groups = "drop") %>%
      arrange(desc(m)) %>%
      head(10)
    p <- ggplot(v_stat, aes(reorder(venue, m), m)) +
      geom_col(fill = "#6C5CE7") +
      coord_flip() +
      labs(title = "Top Venues by Matches", x = "", y = "Matches") +
      theme_minimal()
    ggplotly(p)
  })

  output$tossImpact <- renderPlotly({
    t_imp <- matches %>%
      filter(winner != "") %>%
      mutate(tww = ifelse(toss_winner == winner, "Yes", "No")) %>%
      group_by(tww) %>%
      summarise(count = n(), .groups = "drop")
    plot_ly(t_imp, labels = ~tww, values = ~count, type = "pie", marker = list(colors = c("#D63031", "#00B894")), hole = 0.4) %>% layout(title = "Did Toss Winner Win the Match?")
  })

  # ── 4. AI Player Recommender Logic ─────────────────────────────────

  observeEvent(input$rec_batsman, {
    req(input$rec_batsman)
    player_data <- bat_stats %>% filter(batsman == input$rec_batsman)
    if (nrow(player_data) == 0) {
      return()
    }

    p_cluster <- player_data$cluster[1]
    p_profile <- player_data$Profile[1]

    output$playerProfileBadge <- renderUI({
      tags$div(
        style = "background:#2d3436; color:#00cec9; padding:15px; border-radius:8px; text-align:center; font-weight:bold; font-size:18px;",
        icon("id-badge"), " ", p_profile
      )
    })

    output$recommenderTable <- renderDT(
      {
        bat_stats %>%
          filter(cluster == p_cluster & batsman != input$rec_batsman) %>%
          filter(runs >= input$rec_min_runs) %>%
          # simple distance metric for similarity inside cluster
          mutate(
            sim_dist = sqrt((scale(average)[, 1] - scale(player_data$average)[1])^2 +
              (scale(strike_rate)[, 1] - scale(player_data$strike_rate)[1])^2)
          ) %>%
          arrange(sim_dist) %>%
          head(15) %>%
          select(Batsman = batsman, Runs = runs, `Avg.` = average, `S.R.` = strike_rate, `Boundaries %` = boundary_pct) %>%
          mutate(across(c(`Avg.`, `S.R.`, `Boundaries %`), ~ round(., 2)))
      },
      options = list(pageLength = 8, scrollX = TRUE),
      selection = "none"
    )

    output$clusterPlot <- renderPlotly({
      plot_ly(bat_stats,
        x = ~average, y = ~strike_rate, z = ~boundary_pct,
        color = ~Profile, colors = "Set2",
        text = ~ paste("<b>", batsman, "</b><br>Runs:", runs, "<br>Avg:", round(average, 1), "<br>SR:", round(strike_rate, 1)),
        hoverinfo = "text",
        type = "scatter3d", mode = "markers",
        marker = list(
          size = ifelse(bat_stats$batsman == input$rec_batsman, 15, 6),
          line = list(color = ifelse(bat_stats$batsman == input$rec_batsman, "#2d3436", "transparent"), width = 2),
          opacity = ifelse(bat_stats$batsman == input$rec_batsman, 1, 0.75)
        )
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "Batting Average"),
            yaxis = list(title = "Strike Rate"),
            zaxis = list(title = "Boundary %")
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0),
          legend = list(orientation = "v", x = 0, y = 1)
        )
    })
  })

  # ── 5. AI Scouting Report Generator ────────────────────────────────

  observeEvent(input$btn_generate_report, {
    req(input$scout_batsman)
    p_name <- input$scout_batsman
    p_stat <- bat_stats %>% filter(batsman == p_name)

    if (nrow(p_stat) == 0) {
      output$aiScoutText <- renderUI({
        tags$div("Insufficient data for this player.")
      })
      return()
    }

    # Heuristics text generation
    avg_text <- if (p_stat$average > 40) "elite" else if (p_stat$average > 30) "solid" else "moderate"
    sr_text <- if (p_stat$strike_rate > 140) "explosive" else if (p_stat$strike_rate > 120) "reliable" else "conservative"
    runs_txt <- p_stat$runs

    report_html <- HTML(paste0(
      "<div class='ai-text-box'>",
      "> INITIALIZING AI SCOUTING MODULE... <br><br>",
      "> <span style='color:#74b9ff;'>SUBJECT:</span> <span class='ai-keyword'>", p_name, "</span><br>",
      "> <span style='color:#74b9ff;'>AI CLASSIFICATION:</span> <span class='ai-keyword'>", p_stat$Profile, "</span><br><br>",
      "<b>ANALYSIS SUMMARY:</b><br>",
      "Based on analysis of <span class='ai-metric'>", p_stat$balls, "</span> deliveries faced in the IPL, ",
      p_name, " presents a <span class='ai-keyword'>", sr_text, "</span> scoring pattern coupled with an <span class='ai-keyword'>", avg_text, "</span> average of <span class='ai-metric'>", round(p_stat$average, 1), "</span>. ",
      "Having amassed <span class='ai-metric'>", runs_txt, "</span> runs, they maintain a career Strike Rate of <span class='ai-metric'>", round(p_stat$strike_rate, 1), "</span>.<br><br>",
      "<b>BOUNDARY METRICS:</b><br>",
      "With a boundary reliance of <span class='ai-metric'>", round(p_stat$boundary_pct, 1), "%</span>, this player fits the archetypal mold of the <i>", p_stat$Profile, "</i> cluster.",
      "<br><br>> REPORT GENERATION COMPLETE.</div>"
    ))

    output$aiScoutText <- renderUI({
      report_html
    })

    # ── Advanced Scouting Visualizations ──
    output$scoutRadar <- renderPlotly({
      p_name <- input$scout_batsman
      p_stat <- bat_stats %>% filter(batsman == p_name)
      if (nrow(p_stat) == 0) {
        return(NULL)
      }

      med_sr <- median(bat_stats$strike_rate)
      med_avg <- median(bat_stats$average)
      med_bp <- median(bat_stats$boundary_pct)

      val_sr <- min((p_stat$strike_rate / med_sr) * 50, 100)
      val_avg <- min((p_stat$average / med_avg) * 50, 100)
      val_bp <- min((p_stat$boundary_pct / med_bp) * 50, 100)

      plot_ly(
        type = "scatterpolar",
        r = c(val_avg, val_sr, val_bp, val_avg),
        theta = c("Avg Index", "SR Index", "Boundary Index", "Avg Index"),
        fill = "toself",
        fillcolor = "rgba(52, 152, 219, 0.4)",
        line = list(color = "#3498db", width = 2),
        name = p_name
      ) %>% layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        showlegend = FALSE, margin = list(t = 10, b = 10, l = 10, r = 10)
      )
    })

    output$scoutDismissals <- renderPlotly({
      p_name <- input$scout_batsman
      d_hist <- deliveries %>%
        filter(player_dismissed == p_name) %>%
        group_by(dismissal_kind) %>%
        summarise(count = n(), .groups = "drop") %>%
        filter(dismissal_kind != "")
      if (nrow(d_hist) == 0) {
        return(NULL)
      }

      plot_ly(d_hist,
        labels = ~dismissal_kind, values = ~count, type = "pie", hole = 0.5,
        marker = list(colors = c("#e74c3c", "#f1c40f", "#3498db", "#9b59b6", "#e67e22", "#1abc9c"))
      ) %>%
        layout(showlegend = TRUE, margin = list(t = 10, b = 10, l = 10, r = 10))
    })

    output$scoutPhases <- renderPlotly({
      p_name <- input$scout_batsman
      p_runs <- deliveries %>%
        filter(batsman == p_name) %>%
        mutate(phase = case_when(
          over <= 6 ~ "1. Powerplay",
          over <= 15 ~ "2. Middle",
          TRUE ~ "3. Death"
        )) %>%
        group_by(phase) %>%
        summarise(runs = sum(batsman_runs), .groups = "drop")

      if (nrow(p_runs) == 0) {
        return(NULL)
      }

      plot_ly(p_runs,
        x = ~phase, y = ~runs, type = "bar",
        marker = list(color = c("#74b9ff", "#a29bfe", "#fdcb6e"))
      ) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Runs Scored"), margin = list(t = 10, b = 10, l = 10, r = 10))
    })
  })

  # ── 6. ML Model & Explainable AI (XAI) ─────────────────────────────

  model_data <- matches %>%
    filter(winner != "") %>%
    select(team1, team2, toss_winner, toss_decision, winner) %>%
    na.omit()
  all_teams <- sort(unique(c(model_data$team1, model_data$team2, model_data$toss_winner, model_data$winner)))

  model_data$team1 <- factor(model_data$team1, levels = all_teams)
  model_data$team2 <- factor(model_data$team2, levels = all_teams)
  model_data$toss_winner <- factor(model_data$toss_winner, levels = all_teams)
  model_data$toss_decision <- factor(model_data$toss_decision)
  model_data$winner <- factor(model_data$winner, levels = all_teams)

  set.seed(42)
  model <- randomForest(winner ~ team1 + team2 + toss_winner + toss_decision, data = model_data, ntree = 150, importance = TRUE)

  accuracy <- round(mean(predict(model, model_data) == model_data$winner) * 100, 1)

  output$modelAccuracy <- renderValueBox({
    valueBox(paste0(accuracy, "%"), "Model Training Accuracy", icon = icon("brain"), color = "red")
  })

  updateSelectInput(session, "team1", choices = all_teams)
  updateSelectInput(session, "team2", choices = all_teams)

  # XAI Plot
  output$varImpPlot <- renderPlotly({
    imp <- data.frame(importance(model))
    imp$Feature <- rownames(imp)
    # MeanDecreaseGini is a good universal metric for RF
    p <- ggplot(imp, aes(reorder(Feature, MeanDecreaseGini), MeanDecreaseGini)) +
      geom_col(fill = "#D63031", alpha = 0.9) +
      coord_flip() +
      labs(x = "Feature", y = "Importance (Gini)", title = "What drives the prediction?") +
      theme_minimal()
    ggplotly(p)
  })

  observeEvent(input$predict, {
    toss_w <- if (input$tossWinner == "Team 1") input$team1 else input$team2
    newdata <- data.frame(team1 = factor(input$team1, levels = all_teams), team2 = factor(input$team2, levels = all_teams), toss_winner = factor(toss_w, levels = all_teams), toss_decision = factor(input$tossDecision, levels = levels(model_data$toss_decision)))

    pred <- predict(model, newdata)
    probs <- predict(model, newdata, type = "prob")
    top_prob <- round(max(probs) * 100, 1)

    output$prediction <- renderUI({
      tags$div(
        style = "padding:20px; background:linear-gradient(135deg, #0984E3, #6C5CE7); border-radius:12px; color:white; text-align:center; box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
        tags$h3(style = "margin:0;", icon("trophy"), " Predicted Winner"),
        tags$h2(style = "margin:15px 0; font-weight:800;", as.character(pred)),
        tags$p(style = "margin:0; opacity:0.9; font-size:16px;", paste0("AI Confidence: ", top_prob, "%"))
      )
    })
  })

  # ── 7. Data Tables ─────────────────────────────────────────────────
  output$matchesTable <- renderDT(
    {
      matches %>%
        select(SeasonYear, date, city, team1, team2, winner, venue) %>%
        rename(Season = SeasonYear, Date = date, City = city, `Team 1` = team1, `Team 2` = team2, Winner = winner, Venue = venue)
    },
    options = list(pageLength = 15, scrollX = TRUE)
  )
  output$batsmenTable <- renderDT(
    {
      bat_stats %>% select(Batsman = batsman, Runs = runs, `Avg` = average, `Strike Rate` = strike_rate, `AI Profile` = Profile)
    },
    options = list(pageLength = 15)
  )

  # ── 8. Live Predictor ──────────────────────────────────────────────
  observeEvent(input$btn_live_predict, {
    req(input$live_target, input$live_current, input$live_batting_team, input$live_bowling_team)

    # Calculate overs and balls properly
    overs_int <- floor(input$live_overs)
    balls_rem <- (input$live_overs - overs_int) * 10
    total_balls_bowled <- overs_int * 6 + balls_rem

    balls_left <- 120 - total_balls_bowled
    runs_left <- input$live_target - input$live_current
    wickets_left <- 10 - input$live_wickets

    if (runs_left < 0) runs_left <- 0
    if (balls_left < 0) balls_left <- 0

    crr <- ifelse(total_balls_bowled > 0, (input$live_current * 6) / total_balls_bowled, 0)
    rrr <- ifelse(balls_left > 0, (runs_left * 6) / balls_left, runs_left * 6)

    output$box_crr <- renderValueBox({
      valueBox(round(crr, 2), "Current Run Rate", icon = icon("chart-line"), color = "blue")
    })
    output$box_rrr <- renderValueBox({
      valueBox(round(rrr, 2), "Required Run Rate", icon = icon("bolt"), color = ifelse(rrr > 10, "red", "green"))
    })
    output$box_runs_left <- renderValueBox({
      valueBox(runs_left, paste(balls_left, "balls left"), icon = icon("bullseye"), color = "purple")
    })

    # Predict Win Probability
    live_scen <- data.frame(
      batting_team = input$live_batting_team,
      bowling_team = input$live_bowling_team,
      runs_left = runs_left,
      balls_left = balls_left,
      wickets_left = wickets_left,
      target_score = input$live_target,
      crr = crr,
      rrr = rrr
    )

    prob_win <- predict(live_model_lr, newdata = live_scen, type = "response")
    prob_win_pct <- round(prob_win * 100, 1)

    output$live_prob_plot <- renderPlotly({
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = prob_win_pct,
        title = list(text = "Win Probability (%)"),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(NULL, 100)),
          bar = list(color = "#00B894"),
          steps = list(
            list(range = c(0, 30), color = "#FF7675"),
            list(range = c(30, 70), color = "#FDCB6E"),
            list(range = c(70, 100), color = "#55EFC4")
          )
        )
      ) %>% layout(margin = list(l = 20, r = 20, t = 50, b = 20))
    })
  })
}

shinyServer(server)
