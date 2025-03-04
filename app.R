# Load libraries
library(bslib)
library(ggplot2)
library(ggtext)
library(gridExtra)
library(shiny)
library(shinyWidgets)

# Function to create input fields for a player
create_player_input <- function(position, team, position_full) {
  
  team_color <- ifelse(team == "A", "blue", "red")
  
  tags$div(
    tags$h4(
      class = "position-title", 
      style = paste0("color: ", team_color, ";"), 
      paste0(position_full, " (", position, ")")
    ),
    div(
      class = "player-input-container",
      numericInput(
        inputId = paste0(position, "_T", team, "_number"), 
        label = "Number", 
        value = NA, 
        min = 1, 
        max = 99, 
        step = 1, 
        width = "80px"
      ),
      textInput(
        inputId = paste0(position, "_T", team, "_name"),
        label = "Name", 
        value = "", 
        width = "220px"
      )
    )
  )
  
}

# Function to create a team card block
create_team_card <- function(team_letter, team_color) {
  
  team_class <- paste0("card-team-", tolower(team_letter))
  
  btn_class <- paste0("btn-team-", tolower(team_letter))
  
  positions <- list(
    c("S", "Setter"),
    c("OPP", "Opposite hitter"),
    c("OH1", "Outside hitter 1"),
    c("OH2", "Outside hitter 2"),
    c("MB1", "Middle blocker 1"),
    c("MB2", "Middle blocker 2"),
    c("L", "Libero")
  )
  
  player_inputs <- lapply(positions, function(pos) {
    create_player_input(pos[1], team_letter, pos[2])
  })
  
  card(
    class = team_class,
    card_header(HTML(paste0('<i class="fas fa-users" style="margin-right: 8px;"></i>Team ', team_letter))),
    card_body(
      tags$div(player_inputs)
    ),
    card_footer(
      tags$div(
        class = "button-container",
        actionButton(
          inputId = paste0("btn_add_modify_players_T", team_letter), 
          label = NULL,
          icon = icon("user-plus"), 
          class = paste("btn", btn_class, "footer-button"),
          title = "Add/modify players"
        ),
        actionButton(
          inputId = paste0("btn_delete_players_T", team_letter), 
          label = NULL,
          icon = icon("eraser"), 
          class = paste("btn", btn_class, "footer-button"),
          title = "Delete players"
        ),
        actionButton(
          inputId = paste0("btn_add_rotate_cw_T", team_letter), 
          label = NULL,
          icon = icon("redo"), 
          class = paste("btn", btn_class, "footer-button"),
          title = "Rotate +1 position (clockwise)"
        ),
        actionButton(
          inputId = paste0("btn_add_rotate_ccw_T", team_letter), 
          label = NULL,
          icon = icon("undo"), 
          class = paste("btn", btn_class, "footer-button"),
          title = "Rotate -1 position (counterclockwise)"
        ),
        actionButton(
          inputId = paste0("btn_multi_rotate_T", team_letter), 
          label = NULL,
          icon = icon("exchange-alt"), 
          class = paste("btn", btn_class, "footer-button"),
          title = "Multiple rotation options"
        )
      )
    )
  )
  
}

# Function to create volleyball court visualization
f_volley_court <- function(team_A_labels, team_B_labels, rotation_A, rotation_B, plot_title = NULL) {

  # Map internal rotation numbers to displayed rotation numbers
  display_rotation_A <- ifelse(rotation_A == 1, 1, 8 - rotation_A)
  display_rotation_B <- ifelse(rotation_B == 1, 1, 8 - rotation_B)
  
  # Create data for volleyball court
  court <- data.frame(
    x = c(0, 9, 9, 0, 0), 
    y = c(0, 0, 18, 18, 0)
  )
  
  free_area <- data.frame(
    x = c(-3, 12, 12, -3, -3),
    y = c(-2, -2, 20, 20, -2)
  )
  
  # Define base font
  base_font <- "Lato, 'Helvetica Neue', Helvetica, Arial, sans-serif"
  
  # Create the plot
  p <- ggplot() +
    # Draw the volleyball court
    geom_polygon(data = free_area, aes(x = x, y = y), fill = "#8FBC8F", color = NA, linewidth = 1) +
    geom_polygon(data = court, aes(x = x, y = y), fill = "#FFD700", color = "black", linewidth = 1) +
    # Draw center line
    geom_segment(aes(x = -1, y = 9, xend = 10, yend = 9), color = "black", linewidth = 1) +
    # Draw 3m lines
    geom_segment(aes(x = 0, y = 6, xend = 9, yend = 6), color = "black", linewidth = 1) +
    geom_segment(aes(x = 0, y = 12, xend = 9, yend = 12), color = "black", linewidth = 1) +
    # Add dashed lines
    geom_segment(aes(x = -1.75, y = 6, xend = -1.60, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -1.40, y = 6, xend = -1.25, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -1.05, y = 6, xend = -0.90, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -0.70, y = 6, xend = -0.55, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -0.35, y = 6, xend = -0.20, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -1.75, y = 12, xend = -1.60, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -1.40, y = 12, xend = -1.25, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -1.05, y = 12, xend = -0.90, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -0.70, y = 12, xend = -0.55, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = -0.35, y = 12, xend = -0.20, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.20, y = 6, xend = 9.35, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.55, y = 6, xend = 9.70, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.90, y = 6, xend = 10.05, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 10.25, y = 6, xend = 10.40, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 10.60, y = 6, xend = 10.75, yend = 6), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.20, y = 12, xend = 9.35, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.55, y = 12, xend = 9.70, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 9.90, y = 12, xend = 10.05, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 10.25, y = 12, xend = 10.40, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    geom_segment(aes(x = 10.60, y = 12, xend = 10.75, yend = 12), color = "black", linetype = "dashed", linewidth = 1) +
    # Add posts
    geom_point(aes(x = -1, y = 9), color = "black", size = 3) +
    geom_point(aes(x = 10, y = 9), color = "black", size = 3) +
    # Add black border
    geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 18), color = "black", fill = NA, linewidth = 1) +
    # Set axis limits
    xlim(-3, 12) +
    ylim(-2, 20) +
    # Add player labels for Team A
    geom_richtext(aes(x = 4.5, y = 19.0, label = paste0("<b>", team_A_labels[1], " - Rotation ", display_rotation_A, "</b>")), 
                  color = "blue", size = 5.5, 
                  label.colour = NA, fill = NA, lineheight = 1.5, family = base_font) +
    geom_richtext(aes(x = 1.5, y = 15.0, label = team_A_labels[2]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 4.5, y = 15.0, label = team_A_labels[3]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 7.5, y = 15.0, label = team_A_labels[4]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 7.5, y = 10.5, label = team_A_labels[5]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 4.5, y = 10.5, label = team_A_labels[6]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 1.5, y = 10.5, label = team_A_labels[7]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = -1.5, y = 15.0, label = team_A_labels[8]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    # Add player labels for Team B
    geom_richtext(aes(x = 4.5, y = -1.0, label = paste0("<b>", team_B_labels[1], " - Rotation ", display_rotation_B, "</b>")), 
                  color = "red", size = 6, 
                  label.colour = NA, fill = NA, lineheight = 1.5, family = base_font) +
    geom_richtext(aes(x = 7.5, y = 3.0, label = team_B_labels[2]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 4.5, y = 3.0, label = team_B_labels[3]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 1.5, y = 3.0, label = team_B_labels[4]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 1.5, y = 7.5, label = team_B_labels[5]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 4.5, y = 7.5, label = team_B_labels[6]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 7.5, y = 7.5, label = team_B_labels[7]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    geom_richtext(aes(x = 10.5, y = 3.0, label = team_B_labels[8]), color = "black", size = 4.5, 
                  label.colour = NA, fill = NA, lineheight = 1.25, family = base_font) +
    coord_fixed(ratio = 1/1) +
    theme_void()
  
  # Add the title
  if (!is.null(plot_title)) {
    p <- p + 
      ggtitle(plot_title) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black", size = 22, margin = margin(t = 30, b = -15))
      )
  }
  
  return(p)
  
}

# Define CSS styles
css_styles <- "
.custom-title {
  font-family: 'Fredericka the Great', serif;
  text-align: left;
  font-size: 48px;
  font-weight: normal;
  color: black;
  padding: 10px;
  background-color: white;
}

.header {
  width: 100%;
  background-color: #f8f9fa;
  padding: 10px;
  text-align: center;
  border-bottom: 1px solid #e7e7e7;
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
}

hr {
  margin-top: 5px;
  margin-bottom: 5px;
}

.card-team-a, .card-team-b {
  width: 100%;
  margin: 0 auto;
}

.card-team-a .card-body, .card-team-b .card-body {
  padding: 15px;
  overflow-x: hidden;
}

.card-team-a .card-header, 
.card-team-b .card-header, 
.card-rotations .card-header,
.about-easysportsapps-card .card-header,
.about-volleyrotationsapp-card .card-header,
.license-card .card-header {
  font-size: 28px;
  font-weight: bold;
  margin-bottom: 10px;
}

.card-team-a .card-header {
  color: blue;
}

.card-team-b .card-header {
  color: red;
}

.card-rotations .card-header {
  color: black;
}

.card-rotations {
  flex: 1;
  display: flex;
  flex-direction: column;
  height: 100%;
}

.card-rotations .card-body {
  flex: 1;
  overflow-y: auto;
}

.rotations-container {
  display: flex;
  flex-direction: column;
  flex: 1;
  min-height: calc(100vh - 600px);
}

.position-title {
  font-size: 20px;
  font-weight: bold;
  color: black;
  margin-bottom: 8px;
  padding-bottom: 3px;
}

.input-label {
  font-size: 14px;
  font-weight: 500;
  color: #555;
}

.btn-team-a {
  background-color: blue;
  color: white;
  border: none;
}

.btn-team-a:hover {
  background-color: darkblue;
  color: white;
}

.btn-team-b {
  background-color: red;
  color: white;
  border: none;
}

.btn-team-b:hover {
  background-color: darkred;
  color: white;
}

html, body {
  height: 100%;
}

.main-layout {
  display: flex;
  flex-direction: column;
  min-height: calc(100vh - 150px);
}

.button-container {
  display: flex;
  flex-direction: row;
  width: 100%;
  justify-content: space-between;
}

.footer-button {
  flex-grow: 1;
  flex-basis: 0;
  margin: 0 4px;
  white-space: nowrap;
}

.footer-button:first-child {
  margin-left: 0;
}

.footer-button:last-child {
  margin-right: 0;
}

.player-input-container {
  display: flex;
  flex-direction: row;
  align-items: flex-end;
  gap: 10px;
  margin-bottom: 15px;
}

@media (max-width: 1080px) {
  .team-cards-container {
    display: block !important;
  }
  .team-cards-container > div {
    width: 100% !important;
    margin-bottom: 20px;
  }
  
  .button-container {
    flex-wrap: nowrap;
  }
  
  .player-input-container {
    flex-wrap: wrap;
  }
}
"

# Application UI
ui <- page_fillable(
  
  # Theme
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Head
  tags$head(
    tags$title("VolleyRotationsApp"),
    tags$style(HTML(css_styles)),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Fredericka+the+Great&display=swap", rel = "stylesheet")
  ),
  
  # Header
  div(
    class = "header",
    div(
      style = "display: flex; align-items: center; margin: 0;",
      actionButton(
        inputId = "restart_app", 
        label = "Restart App", 
        class = "btn btn-danger", 
        style = "margin: 0;"
      ),
      p(style = "margin: 0; margin-left: 20px;", 
        'Developer:', 
        a("Raúl Hileno, PhD", href = "https://orcid.org/0000-0003-3447-395X", target = "_blank")
      ),
      p(style = "margin: 0; margin-left: 20px;", 
        'Project:', 
        a("EasySportsApps", href = "https://github.com/EasySportsApps", target = "_blank")
      ),
      p(style = "margin: 0; margin-left: 20px;", 
        'License:', 
        a("CC BY-NC-ND 4.0", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/", target = "_blank")
      )
    )
  ),
  
  div(class = "custom-title", "VolleyRotationsApp v1.0"),
  
  
  tabsetPanel(
    
    # App panel
    tabPanel(
      title = "App",
      icon = icon("volleyball-ball"),
      
      div(
        class = "tab-content-container",
        style = "padding: 20px 0;",
        
        div(
          class = "team-cards-container",
          style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 20px;",
          
          div(
            style = "width: calc(50% - 10px);",
            create_team_card("A", "blue")
          ),
          
          div(
            style = "width: calc(50% - 10px);",
            create_team_card("B", "red")
          )
        ),
        div(
          class = "rotations-container",
          card(
            class = "card-rotations",
            height = "1400px",
            card_header(
              HTML('<i class="fas fa-sync-alt" style="margin-right: 8px;"></i>Rotations view')
            ),
            card_body(
              fluidRow(
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot1", height = "600px")),
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot2", height = "600px")),
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot3", height = "600px")),
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot4", height = "600px")),
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot5", height = "600px")),
                div(class = "col-lg-4 col-md-6 col-sm-12", plotOutput("rotationPlot6", height = "600px"))
              )
            )
          )
        )
      )
    ),
    
    # Info panel
    tabPanel(
      title = "Info",
      icon = icon("info-circle"),
      
      div(
        class = "tab-content-container",
        style = "padding: 20px 0;",
        
        # About EasySportsApps Project card
        card(
          class = "about-easysportsapps-card",
          style = "margin-bottom: 20px;",
          card_header(
            HTML('<i class="fas fa-project-diagram" style="margin-right: 8px;"></i>About EasySportsApps Project')
          ),
          card_body(
            p(
              HTML('VolleyRotationsApp is part of the <a href="https://github.com/EasySportsApps" target="_blank">EasySportsApps</a> project, driven and developed by <a href="https://orcid.org/0000-0003-3447-395X" target="_blank">Raúl Hileno, PhD</a>. The project focuses on creating practical and innovative digital solutions that address the dynamic needs of professionals in physical activity and sports.')
            )
          )
        ),
        
        # About VolleyRotationsApp card
        card(
          class = "about-volleyrotationsapp-card",
          style = "margin-bottom: 20px;",
          card_header(
            HTML('<i class="fas fa-volleyball-ball" style="margin-right: 8px;"></i>About VolleyRotationsApp')
          ),
          card_body(
            # App purpose
            h4(strong("Purpose")),
            p(
              "VolleyRotationsApp is an interactive and free web application that helps volleyball coaches visualize and plan team rotations with ease. 
               This tool allows you to create formations for two teams and see how they would match up across all six rotations. 
               The app is specifically designed for teams using a 5-1 offensive system with a S-OH-MB rotation order."
            ),
            
            hr(),
            
            # Quick guide
            h4(strong("Quick guide")),
            tags$ul(
              tags$li(strong("Add/modify players button:"), " Click to update the court diagrams after entering player information. For each player, you can enter a number (1-99) and a name (recommended maximum of 10 characters including spaces) to ensure proper display. Team A is displayed in blue, Team B in red to easily distinguish between teams."),
              tags$li(strong("Delete players button:"), " Removes all player data from the app and resets the court diagrams to blank state."),
              tags$li(strong("Rotation buttons:"), " Use the 'Rotate +1 position (clockwise)' and 'Rotate -1 position (counterclockwise)' buttons to adjust team rotations one position at a time. For multiple positions at once, click on the 'Multiple rotation options' button."),
              tags$li(strong("Rotations view:"), " All six possible rotations are shown simultaneously for both teams, allowing you to see how players match up across all rotations. The diagrams show the basic positions of each team in each rotation at the beginning of each point, that is, before position changes or permutations when the referee whistles.")
            ),
            
            hr(),
            
            # Volleyball positions
            h4(strong("Volleyball positions")),
            p("The 5-1 offensive system is composed of one setter, one opposite hitter, two outside hitters, two middle blockers, and one libero. Each player has specific responsibilities."),
            div(
              style = "display: flex; flex-wrap: wrap; gap: 15px; margin-bottom: 20px;",
              
              div(
                style = "flex: 1; min-width: 250px; background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                h5(strong("Setter (S)")),
                p("The player who directs the offense by calling plays and distributing the ball to attackers.")
              ),
              
              div(
                style = "flex: 1; min-width: 250px; background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                h5(strong("Opposite hitter (OPP)")),
                p("Attacker specialized in the right side of the court, positioned opposite to the setter in the rotation.")
              ),
              
              div(
                style = "flex: 1; min-width: 250px; background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                h5(strong("Outside hitters (OH)")),
                p("Attackers specialized in the left side of the court."),
                tags$ul(
                  tags$li(strong("OH1:"), " Main outside hitter, positioned closer to the setter."),
                  tags$li(strong("OH2:"), " Secondary outside hitter, positioned farther from the setter and opposite to OH1 in the rotation.")
                )
              ),
              
              div(
                style = "flex: 1; min-width: 250px; background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                h5(strong("Middle blockers (MB)")),
                p("Players specialized in the center of the net for quick attacks and blocking."),
                tags$ul(
                  tags$li(strong("MB1:"), " Main middle blocker, works closely with the setter."),
                  tags$li(strong("MB2:"), " Secondary middle blocker, positioned farther from the setter and opposite to MB1 in the rotation.")
                )
              ),
              
              div(
                style = "flex: 1; min-width: 250px; background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                h5(strong("Libero (L)")),
                p("A defensive specialist who wears a different colored jersey and has unique substitution privileges. Usually replaces the back-row middle blocker until the MB completes their serving turn.")
              )
            ),
            
            hr(),
            
            # Rotation number
            h4(strong("Rotation number")),
            p("In a 5-1 offensive system, the rotation number is determined by the setter's position."),
            div(
              style = "display: flex; flex-direction: column; max-width: 800px; margin: 0 auto; border: 1px solid #ccc; padding: 20px; border-radius: 8px;",
              
              div(
                style = "margin-bottom: 5px; text-align: center;",
                h5(style = "margin: 0 0 10px 0; color: blue; font-weight: bold;", "Team A")
              ),
              
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 1"), br(), "Setter in position 1", br(), "(back-right)")
                ),
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 6"), br(), "Setter in position 6", br(), "(back-middle)")
                ),
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 5"), br(), "Setter in position 5", br(), "(back-left)")
                )
              ),
              
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 2"), br(), "Setter in position 2", br(), "(front-right)")
                ),
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 3"), br(), "Setter in position 3", br(), "(front-middle)")
                ),
                
                div(
                  style = "flex: 1; background-color: #bbdefb; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #64b5f6;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 4"), br(), "Setter in position 4", br(), "(front-left)")
                )
              ),
              
              div(
                style = "border-top: 3px dashed #000000; margin: 5px 0; position: relative;",
                div(style = "position: absolute; top: -12px; left: 50%; transform: translateX(-50%); background-color: white; padding: 0 15px; font-weight: bold; color: #000000;", 
                    "NET")
              ),
              
              div(
                style = "display: flex; justify-content: space-between; margin-top: 15px;",
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 4"), br(), "Setter in position 4", br(), "(front-left)")
                ),
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 3"), br(), "Setter in position 3", br(), "(front-middle)")
                ),
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 2"), br(), "Setter in position 2", br(), "(front-right)")
                )
              ),
              
              div(
                style = "display: flex; justify-content: space-between; margin-top: 15px;",
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 5"), br(), "Setter in position 5", br(), "(back-left)")
                ),
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 6"), br(), "Setter in position 6", br(), "(back-middle)")
                ),
                
                div(
                  style = "flex: 1; background-color: #ffcdd2; padding: 10px; border-radius: 8px; margin: 0 5px; border: 1px solid #ef5350;",
                  p(style = "text-align: center; margin-bottom: 0;", strong("Rotation 1"), br(), "Setter in position 1", br(), "(back-right)")
                )
              ),
              
              div(
                style = "margin-top: 20px; text-align: center;",
                h5(style = "margin: 0 0 10px 0; color: red; font-weight: bold;", "Team B")
              )
            ),
            
            hr(),
            
            # Feedback section
            h4(strong("Feedback")),
            p(
              HTML("If you have any suggestions for improvement, feel free to share them in the <strong>Discussions</strong> section of our <a href='https://github.com/EasySportsApps' target='_blank'>GitHub repository</a> 
                   or send us an email at <a href='mailto:easysportsappsproject@gmail.com'>easysportsappsproject@gmail.com</a>.")
            )
          )
        ),
        
        # License card
        card(
          class = "license-card",
          style = "margin-bottom: 20px;",
          card_header(
            HTML('<i class="fas fa-copyright" style="margin-right: 8px;"></i>VolleyRotationsApp license')
          ),
          card_body(
            p(
              HTML("VolleyRotationsApp is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License (<a href='https://creativecommons.org/licenses/by-nc-nd/4.0/' target='_blank'>CC BY-NC-ND 4.0</a>).")
            ),
            p("You are free to:"),
            tags$ul(
              tags$li(HTML("<b>🔗 Share</b> — Copy and redistribute the material in any medium or format."))
            ),
            p("Under the following terms:"),
            tags$ul(
              tags$li(HTML("<b>📛 Attribution</b> — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.")),
              tags$li(HTML("<b>🚫 NonCommercial</b> — You may not use the material for commercial purposes.")),
              tags$li(HTML("<b>🚷 NoDerivatives</b> — If you remix, transform, or build upon the material, you may not distribute the modified material.")),
              tags$li(HTML("<b>🔓 No additional restrictions</b> — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits."))
            )
          )
        )
      )
    )

  )
)

# Application server
server <- function(input, output, session) {
  
  # Track current rotation for each team
  team_rotations <- reactiveValues(
    team_A = 1,
    team_B = 1
  )
  
  # Store current player data
  player_data <- reactiveValues(
    team_A_labels = NULL,
    team_B_labels = NULL,
    update_trigger = 0
  )
  
  # Define rotation orders for standard 5-1 system
  get_rotation_orders <- function() {
    return(list(
      c("S", "MB1", "OH2", "OPP", "MB2", "OH1"),      # Rotation 1
      c("OH1", "S", "MB1", "OH2", "OPP", "MB2"),      # Rotation 2
      c("MB2", "OH1", "S", "MB1", "OH2", "OPP"),      # Rotation 3
      c("OPP", "MB2", "OH1", "S", "MB1", "OH2"),      # Rotation 4
      c("OH2", "OPP", "MB2", "OH1", "S", "MB1"),      # Rotation 5
      c("MB1", "OH2", "OPP", "MB2", "OH1", "S")       # Rotation 6
    ))
  }
  
  # Function to generate player labels based on rotation
  generate_player_labels <- function(team, rotation) {
    
    # Define rotation orders for standard 5-1 system
    rotation_orders <- get_rotation_orders()
    
    # Get current rotation order based on the rotation number
    positions_in_rotation <- rotation_orders[[rotation]]
    
    # Define team color for styling
    team_color <- ifelse(team == "A", "blue", "red")
    
    # Generate formatted labels for the court
    labels <- c()
    
    # Team name
    team_name <- paste0("<b><span style='color:", team_color, ";'>Team ", team, "</span></b>")
    labels[1] <- team_name
    
    # Add player positions
    for (i in 1:6) {
      pos <- positions_in_rotation[i]
      number_id <- paste0(pos, "_T", team, "_number")
      name_id <- paste0(pos, "_T", team, "_name")
      number_val <- input[[number_id]]
      name_val <- input[[name_id]]
      line1 <- paste0("<b><span style='color:", team_color, ";'>", pos, "</span></b>")
      line2 <- if (!is.na(number_val)) number_val else ""
      line3 <- if (name_val != "") name_val else ""
      label <- paste0(line1, "<br>", line2, "<br>", line3)
      labels[i+1] <- label
    }
    
    # Add libero as position 7 (doesn't rotate)
    libero_number <- input[[paste0("L_T", team, "_number")]]
    libero_name <- input[[paste0("L_T", team, "_name")]]
    line1 <- paste0("<b><span style='color:", team_color, ";'>L</span></b>")
    line2 <- if (!is.na(libero_number)) libero_number else ""
    line3 <- if (libero_name != "") libero_name else ""
    libero_label <- paste0(line1, "<br>", line2, "<br>", line3)
    labels[8] <- libero_label
    
    return(labels)
    
  }
  
  # Function to create labels for different rotations based on the base rotation
  transform_rotation_labels <- function(base_labels, team, base_rotation, target_rotation) {
    if (base_rotation == target_rotation) return(base_labels)
    
    # Define rotation orders for standard 5-1 system
    rotation_orders <- get_rotation_orders()
    
    # Create new labels array
    result <- c()
    
    # Copy team name
    result[1] <- base_labels[1]
    
    # Copy libero
    result[8] <- base_labels[8]
    
    # Get positions in base and target rotations
    base_pos <- rotation_orders[[base_rotation]]
    target_pos <- rotation_orders[[target_rotation]]
    
    # For each position in target rotation, find corresponding position in base rotation
    for (i in 1:6) {
      target_position <- target_pos[i]
      
      # Find where this position is in the base rotation
      base_index <- which(base_pos == target_position)
      
      # Copy label from base to result (add 1 because labels start at index 2)
      result[i+1] <- base_labels[base_index+1]
    }
    
    return(result)
    
  }
  
  # Add/modify players for Team A
  observeEvent(input$btn_add_modify_players_TA, {
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Add/modify players for Team B
  observeEvent(input$btn_add_modify_players_TB, {
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Initialize player data on app start
  observe({
    if (is.null(player_data$team_A_labels)) {
      player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    }
    if (is.null(player_data$team_B_labels)) {
      player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    }
  })
  
  # Render the first volleyball court plot (current rotation)
  output$rotationPlot1 <- renderPlot({
    player_data$update_trigger
    team_A_labels <- player_data$team_A_labels
    team_B_labels <- player_data$team_B_labels
    f_volley_court(team_A_labels, team_B_labels, team_rotations$team_A, team_rotations$team_B,
                   plot_title = "Starting lineup")
  })
  
  # Render the second volleyball court plot (+1 rotation)
  output$rotationPlot2 <- renderPlot({
    player_data$update_trigger
    next_rotation_A <- (team_rotations$team_A %% 6) + 1
    next_rotation_B <- (team_rotations$team_B %% 6) + 1
    team_A_labels <- transform_rotation_labels(
      player_data$team_A_labels, "A", team_rotations$team_A, next_rotation_A
    )
    team_B_labels <- transform_rotation_labels(
      player_data$team_B_labels, "B", team_rotations$team_B, next_rotation_B
    )
    
    f_volley_court(team_A_labels, team_B_labels, next_rotation_A, next_rotation_B,
                   plot_title = "Starting lineup +1")
  })
  
  # Render the third volleyball court plot (+2 rotations)
  output$rotationPlot3 <- renderPlot({
    player_data$update_trigger
    next_rotation_A <- ((team_rotations$team_A + 1) %% 6) + 1
    next_rotation_B <- ((team_rotations$team_B + 1) %% 6) + 1
    team_A_labels <- transform_rotation_labels(
      player_data$team_A_labels, "A", team_rotations$team_A, next_rotation_A
    )
    team_B_labels <- transform_rotation_labels(
      player_data$team_B_labels, "B", team_rotations$team_B, next_rotation_B
    )
    
    f_volley_court(team_A_labels, team_B_labels, next_rotation_A, next_rotation_B,
                   plot_title = "Starting lineup +2")
  })
  
  # Render the fourth volleyball court plot (+3 rotations)
  output$rotationPlot4 <- renderPlot({
    player_data$update_trigger
    next_rotation_A <- ((team_rotations$team_A + 2) %% 6) + 1
    next_rotation_B <- ((team_rotations$team_B + 2) %% 6) + 1
    team_A_labels <- transform_rotation_labels(
      player_data$team_A_labels, "A", team_rotations$team_A, next_rotation_A
    )
    team_B_labels <- transform_rotation_labels(
      player_data$team_B_labels, "B", team_rotations$team_B, next_rotation_B
    )
    
    f_volley_court(team_A_labels, team_B_labels, next_rotation_A, next_rotation_B,
                   plot_title = "Starting lineup +3")
  })
  
  # Render the fifth volleyball court plot (+4 rotations)
  output$rotationPlot5 <- renderPlot({
    player_data$update_trigger
    next_rotation_A <- ((team_rotations$team_A + 3) %% 6) + 1
    next_rotation_B <- ((team_rotations$team_B + 3) %% 6) + 1
    team_A_labels <- transform_rotation_labels(
      player_data$team_A_labels, "A", team_rotations$team_A, next_rotation_A
    )
    team_B_labels <- transform_rotation_labels(
      player_data$team_B_labels, "B", team_rotations$team_B, next_rotation_B
    )
    
    f_volley_court(team_A_labels, team_B_labels, next_rotation_A, next_rotation_B,
                   plot_title = "Starting lineup +4")
  })
  
  # Render the sixth volleyball court plot (+5 rotations)
  output$rotationPlot6 <- renderPlot({
    player_data$update_trigger
    next_rotation_A <- ((team_rotations$team_A + 4) %% 6) + 1
    next_rotation_B <- ((team_rotations$team_B + 4) %% 6) + 1
    team_A_labels <- transform_rotation_labels(
      player_data$team_A_labels, "A", team_rotations$team_A, next_rotation_A
    )
    team_B_labels <- transform_rotation_labels(
      player_data$team_B_labels, "B", team_rotations$team_B, next_rotation_B
    )
    
    f_volley_court(team_A_labels, team_B_labels, next_rotation_A, next_rotation_B,
                   plot_title = "Starting lineup +5")
  })
  
  # Clockwise rotation for Team A
  observeEvent(input$btn_add_rotate_cw_TA, {
    team_rotations$team_A <- (team_rotations$team_A %% 6) + 1
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Counter-clockwise rotation for Team A
  observeEvent(input$btn_add_rotate_ccw_TA, {
    team_rotations$team_A <- ifelse(team_rotations$team_A == 1, 6, team_rotations$team_A - 1)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Clockwise rotation for Team B
  observeEvent(input$btn_add_rotate_cw_TB, {
    team_rotations$team_B <- (team_rotations$team_B %% 6) + 1
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Counter-clockwise rotation for Team B
  observeEvent(input$btn_add_rotate_ccw_TB, {
    team_rotations$team_B <- ifelse(team_rotations$team_B == 1, 6, team_rotations$team_B - 1)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Function to calculate new rotation number with a given offset
  calculate_rotation <- function(current, offset) {
    # Need to handle both positive and negative rotations
    new_rotation <- ((current - 1 + offset) %% 6) + 1
    if (new_rotation <= 0) new_rotation <- new_rotation + 6
    return(new_rotation)
  }
  
  # Team A multiple rotation modal
  observeEvent(input$btn_multi_rotate_TA, {
    showModal(modalDialog(
      title = "Multiple rotation options",
      div(
        style = "text-align: justify; margin-bottom: 15px;",
        p("Quickly rotate team A multiple positions at once.")
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 10px; justify-content: flex-start;",
        div(
          style = "display: flex; flex-direction: row; gap: 5px; flex-wrap: nowrap; justify-content: flex-start;",
          actionButton("rotate_pos1_TA", "+1", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate +1 position (clockwise)"),
          actionButton("rotate_pos2_TA", "+2", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate +2 positions (clockwise)"),
          actionButton("rotate_pos3_TA", "+3", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate +3 positions (clockwise)"),
          actionButton("rotate_pos4_TA", "+4", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate +4 positions (clockwise)"),
          actionButton("rotate_pos5_TA", "+5", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate +5 positions (clockwise)")
        ),
        div(
          style = "display: flex; flex-direction: row; gap: 5px; flex-wrap: nowrap; justify-content: flex-start;",
          actionButton("rotate_neg1_TA", "-1", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate -1 position (counterclockwise)"),
          actionButton("rotate_neg2_TA", "-2", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate -2 positions (counterclockwise)"),
          actionButton("rotate_neg3_TA", "-3", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate -3 positions (counterclockwise)"),
          actionButton("rotate_neg4_TA", "-4", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate -4 positions (counterclockwise)"),
          actionButton("rotate_neg5_TA", "-5", 
                       style = "background-color: blue; color: white; border: none;",
                       title = "Rotate -5 positions (counterclockwise)")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Team B multiple rotation modal
  observeEvent(input$btn_multi_rotate_TB, {
    showModal(modalDialog(
      title = "Multiple rotation options",
      div(
        style = "text-align: justify; margin-bottom: 15px;",
        p("Quickly rotate team B multiple positions at once.")
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 10px; justify-content: flex-start;",
        div(
          style = "display: flex; flex-direction: row; gap: 5px; flex-wrap: nowrap; justify-content: flex-start;",
          actionButton("rotate_pos1_TB", "+1", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate +1 position (clockwise)"),
          actionButton("rotate_pos2_TB", "+2", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate +2 positions (clockwise)"),
          actionButton("rotate_pos3_TB", "+3", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate +3 positions (clockwise)"),
          actionButton("rotate_pos4_TB", "+4", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate +4 positions (clockwise)"),
          actionButton("rotate_pos5_TB", "+5", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate +5 positions (clockwise)")
        ),
        div(
          style = "display: flex; flex-direction: row; gap: 5px; flex-wrap: nowrap; justify-content: flex-start;",
          actionButton("rotate_neg1_TB", "-1", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate -1 position (counterclockwise)"),
          actionButton("rotate_neg2_TB", "-2", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate -2 positions (counterclockwise)"),
          actionButton("rotate_neg3_TB", "-3", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate -3 positions (counterclockwise)"),
          actionButton("rotate_neg4_TB", "-4", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate -4 positions (counterclockwise)"),
          actionButton("rotate_neg5_TB", "-5", 
                       style = "background-color: red; color: white; border: none;",
                       title = "Rotate -5 positions (counterclockwise)")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Close modal when a rotation is selected (Team A)
  observe({
    lapply(c("rotate_neg5_TA", "rotate_neg4_TA", "rotate_neg3_TA", "rotate_neg2_TA", "rotate_neg1_TA",
             "rotate_pos1_TA", "rotate_pos2_TA", "rotate_pos3_TA", "rotate_pos4_TA", "rotate_pos5_TA"), function(btn) {
               observeEvent(input[[btn]], {
                 removeModal()
               }, ignoreInit = TRUE)
             })
  })
  
  # Close modal when a rotation is selected (Team B)
  observe({
    lapply(c("rotate_neg5_TB", "rotate_neg4_TB", "rotate_neg3_TB", "rotate_neg2_TB", "rotate_neg1_TB",
             "rotate_pos1_TB", "rotate_pos2_TB", "rotate_pos3_TB", "rotate_pos4_TB", "rotate_pos5_TB"), function(btn) {
               observeEvent(input[[btn]], {
                 removeModal()
               }, ignoreInit = TRUE)
             })
  })
  
  # Team A multiple rotation handlers
  observeEvent(input$rotate_neg5_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, -5)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg4_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, -4)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg3_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, -3)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg2_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, -2)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg1_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, -1)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos1_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, 1)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos2_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, 2)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos3_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, 3)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos4_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, 4)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos5_TA, {
    team_rotations$team_A <- calculate_rotation(team_rotations$team_A, 5)
    player_data$team_A_labels <- generate_player_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Team B multiple rotation handlers
  observeEvent(input$rotate_neg5_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, -5)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg4_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, -4)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg3_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, -3)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg2_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, -2)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_neg1_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, -1)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos1_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, 1)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos2_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, 2)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos3_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, 3)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos4_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, 4)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  observeEvent(input$rotate_pos5_TB, {
    team_rotations$team_B <- calculate_rotation(team_rotations$team_B, 5)
    player_data$team_B_labels <- generate_player_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Clear player information for Team A
  observeEvent(input$btn_delete_players_TA, {
    showModal(modalDialog(
      title = "Confirm",
      div(
        style = "text-align: justify;",
        "Are you sure you want to delete all Team A player information?"
      ),
      footer = tagList(
        modalButton("No"),
        actionButton("confirmDeleteTA", "Yes", class = "btn-default")
      )
    ))
  })
  
  # When deletion is confirmed for team A
  observeEvent(input$confirmDeleteTA, {
    
    # Close the modal
    removeModal()
    
    # Clear all inputs
    positions <- c("S", "OPP", "OH1", "OH2", "MB1", "MB2", "L")
    for (pos in positions) {
      updateNumericInput(session, paste0(pos, "_TA_number"), value = NA)
      updateTextInput(session, paste0(pos, "_TA_name"), value = "")
    }
    
    # Function to generate empty labels based on current rotation
    generate_empty_labels <- function(team, rotation) {
      
      # Define team color for styling
      team_color <- ifelse(team == "A", "blue", "red")
      
      # Generate formatted labels for the court
      labels <- c()
      
      # Team name
      team_name <- paste0("<b><span style='color:", team_color, ";'>Team ", team, "</span></b>")
      labels[1] <- team_name
      
      # Get the positions for current rotation
      rotation_orders <- get_rotation_orders()
      positions_in_rotation <- rotation_orders[[rotation]]
      
      # Add empty player positions
      for (i in 1:6) {
        pos <- positions_in_rotation[i]
        line1 <- paste0("<b><span style='color:", team_color, ";'>", pos, "</span></b>")
        labels[i+1] <- paste0(line1, "<br><br>")
      }
      
      # Add empty libero
      line1 <- paste0("<b><span style='color:", team_color, ";'>L</span></b>")
      labels[8] <- paste0(line1, "<br><br>")
      
      return(labels)
    }
    
    # Update labels with current rotation
    player_data$team_A_labels <- generate_empty_labels("A", team_rotations$team_A)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Clear player information for Team B
  observeEvent(input$btn_delete_players_TB, {
    showModal(modalDialog(
      title = "Confirm",
      div(
        style = "text-align: justify;",
        "Are you sure you want to delete all Team B player information?"
      ),
      footer = tagList(
        modalButton("No"),
        actionButton("confirmDeleteTB", "Yes", class = "btn-default")
      )
    ))
  })
  
  # When deletion is confirmed for team B
  observeEvent(input$confirmDeleteTB, {
    
    # Close the modal
    removeModal()
    
    # Clear all inputs
    positions <- c("S", "OPP", "OH1", "OH2", "MB1", "MB2", "L")
    for (pos in positions) {
      updateNumericInput(session, paste0(pos, "_TB_number"), value = NA)
      updateTextInput(session, paste0(pos, "_TB_name"), value = "")
    }
    
    # Function to generate empty labels based on current rotation
    generate_empty_labels <- function(team, rotation) {
      # Define team color for styling
      team_color <- ifelse(team == "A", "blue", "red")
      
      # Generate formatted labels for the court
      labels <- c()
      
      # Team name
      team_name <- paste0("<b><span style='color:", team_color, ";'>Team ", team, "</span></b>")
      labels[1] <- team_name
      
      # Get the positions for current rotation
      rotation_orders <- get_rotation_orders()
      positions_in_rotation <- rotation_orders[[rotation]]
      
      # Add empty player positions
      for (i in 1:6) {
        pos <- positions_in_rotation[i]
        line1 <- paste0("<b><span style='color:", team_color, ";'>", pos, "</span></b>")
        labels[i+1] <- paste0(line1, "<br><br>")
      }
      
      # Add empty libero
      line1 <- paste0("<b><span style='color:", team_color, ";'>L</span></b>")
      labels[8] <- paste0(line1, "<br><br>")
      
      return(labels)
    }
    
    # Update labels with current rotation
    player_data$team_B_labels <- generate_empty_labels("B", team_rotations$team_B)
    player_data$update_trigger <- player_data$update_trigger + 1
  })
  
  # Restart app
  observeEvent(input$restart_app, {
    session$reload()
  })
}

# Run the application
shinyApp(ui = ui, server = server)