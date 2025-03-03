library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(ggsoccer)
library(plotly)
library(tidyr)

# Pitch dimensioner
pitch_length <- 100
pitch_width <- 100

my_theme <- create_theme(
  adminlte_color(
    light_blue = "#00008B",  # Dark Blue (R's "darkblue")
    red = "#E4202C",         # Accent Red
    black = "#000000",       # Black for dark elements
    green = "#FFFFFF"        # Workaround for white
  ),
  adminlte_sidebar(
    dark_bg = "#000000",       # Sidebar background (Black)
    dark_hover_bg = "#00008B", # Sidebar hover (Dark Blue)
    dark_color = "#FFFFFF"     # Sidebar text (White)
  ),
  adminlte_global(
    content_bg = "#f4f6f9",  # Main content background (Light Grey)
    box_bg = "#FFFFFF",      # Box background (White)
    info_box_bg = "#E4202C"  # Info boxes (Red)
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Kombineret Shiny Applikation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mål per spiller", tabName = "mål_per_spiller", icon = icon("chart-bar")),
      menuItem("Spilninger per kamp", tabName = "spilninger_per_kamp", icon = icon("passenger-car")),
      menuItem("Skud Visualisering", tabName = "skud_visualisering", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      # Pane 1: Mål per spiller
      tabItem(tabName = "mål_per_spiller",
              fluidRow(
                box(title = "Mål per spiller", status = "primary", solidHeader = TRUE, width = 12,
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("sæson_valg", label = "Vælg sæson",
                                     choices = c("Alle" = "alle", "21/22" = "21/22", "22/23" = "22/23"),
                                     selected = "alle"),
                        selectInput("land_valg_1", label = "Vælg et land", choices = c("Holland", "Polen")),
                        uiOutput("team_select_ui"),
                        uiOutput("Goal_filter_ui")
                      ),
                      mainPanel(
                        plotOutput("plot_1", height = "600px", width = "1050px")
                      )
                    )
                )
              )
      ),
      
      # Pane 2: Spilninger per kamp
      tabItem(tabName = "spilninger_per_kamp",
              fluidRow(
                box(title = "Spilninger per kamp", status = "primary", solidHeader = TRUE, width = 12,
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("land_valg", label = "Vælg et land", choices = c("Holland", "Polen")),
                        uiOutput("team_filter_ui"),
                        uiOutput("Pass_filter_ui")
                      ),
                      mainPanel(
                        plotlyOutput("plot_2", height = "600px", width = "1050px"),
                        fluidRow(
                          column(12, style = "margin-left: 0px;", tableOutput("spilninger_table"))
                        )
                      )
                    )
                )
              )
      ),
      
      # Pane 3: Skud Visualisering
      tabItem(tabName = "skud_visualisering",
              fluidRow(
                box(title = "Skud Visualisering", status = "primary", solidHeader = TRUE, width = 12,
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("land_valg_2", label = "Vælg et land", choices = c("Holland", "Polen")),
                        uiOutput("hold_filter_ui"),
                        uiOutput("Kamp_filter_ui")
                      ),
                      mainPanel(
                        plotlyOutput("plot_3", height = "600px", width = "1000px"),
                        fluidRow(
                          column(12, style = "margin-left: 200px;", tableOutput("skud_table"))
                        )
                      )
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Pane 1: Mål per spiller server logik
  output$Goal_filter_ui <- renderUI({
    kamp_data <- if (input$land_valg_1 == "Holland") dfmål_holland else dfmål_polen
    
    num_players <- kamp_data %>%
      group_by(player.name.final) %>%
      summarise(Goals = sum(shot.isGoal, na.rm = TRUE)) %>%
      arrange(desc(Goals)) %>%
      nrow()
    
    max_players <- min(num_players, 20)  # Begrænser til maks 20 spillere
    
    sliderInput("Goal_filter", label = "Vælg antal spillere", 
                min = 1, max = max_players, value = 10, step = 1)
  })
  
  
  output$team_select_ui <- renderUI({
    kamp_data <- if (input$land_valg_1 == "Holland") dfmål_holland else dfmål_polen
    teams <- unique(kamp_data$team.name)
    teams <- c("Alle hold", teams)
    
    selectInput("team_valg", label = "Vælg et hold", choices = teams, selected = "Alle hold")
  })
  
  filtered_data_1 <- reactive({
    req(input$Goal_filter)
    kamp_data <- if (input$land_valg_1 == "Holland") dfmål_holland else dfmål_polen
    
    if (input$sæson_valg != "alle" & "sæson" %in% colnames(kamp_data)) {
      kamp_data <- kamp_data %>% filter(sæson == input$sæson_valg)
    }
    
    if (input$team_valg != "Alle hold") {
      kamp_data <- kamp_data %>% filter(team.name == input$team_valg)
    }
    
    result <- kamp_data %>%
      group_by(player.name) %>%
      summarise(
        `Team name` = first(team.name),
        Goals = sum(shot.isGoal, na.rm = TRUE), 
        xG = sum(shot.postShotXg, na.rm = TRUE)
      ) %>%
      arrange(desc(Goals)) %>%
      head(input$Goal_filter)
    
    result$`Team name` <- as.factor(result$`Team name`)  # Konverter til faktor
    print(result$`Team name`)  # Debugging: Tjek hvilke hold der kommer med
    
    return(result)
  })
  
  
  
  output$plot_1 <- renderPlot({
    data <- filtered_data_1()  # Hent de filtrerede data først
    
    ggplot(data, aes(x = reorder(player.name, -Goals), y = Goals, fill = `Team name`)) +
      geom_bar(stat = "identity") +
      
      # xG-prikker med farve, så de får egen legend
      geom_point(aes(y = xG, color = "sum.xG"), size = 3) +
      
      scale_y_continuous(breaks = seq(0, max(data$Goals, na.rm = TRUE), by = 2)) +
      
      # Ændrer farven for xG-punkter og fjerner titel
      scale_color_manual(values = c("sum.xG" = "black"), name = NULL) +
      
      geom_text(aes(label = Goals, y = 0), vjust = -0.5, size = 5) +
      
      labs(title = "Mål per spiller", x = "", y = "Antal mål") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 70, hjust = 1, size = 13),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      
      # Fjerner den sorte prik fra "Team name" ved at angive override.aes
      guides(
        fill = guide_legend(title = "Team name", override.aes = list(shape = NA)),
        color = guide_legend(override.aes = list(fill = NA, shape = 16))  # Sørger for, at xG kun vises som prik
      )
  })
  
  
  # Pane 2: Spilninger per kamp server logik
  output$team_filter_ui <- renderUI({
    req(input$land_valg)
    
    kamp_data <- if (input$land_valg == "Holland") df_holland else df_polen
    teams <- unique(kamp_data$team.name)
    
    selectInput("team_filter", label = "Vælg et hold", choices = c("Alle hold", teams))
  })
  
  output$Pass_filter_ui <- renderUI({
    req(input$team_filter)
    
    kamp_data <- if (input$land_valg == "Holland") df_holland else df_polen
    games <- kamp_data %>%
      filter(team.name == input$team_filter | input$team_filter == "Alle hold") %>%
      pull(label_final) %>%
      unique()
    
    selectInput("Pass_filter", label = "Vælg kamp", choices = games)
  })
  
  filtered_data_2 <- reactive({
    req(input$Pass_filter, input$team_filter)
    kamp_data <- if (input$land_valg == "Holland") df_holland else df_polen
    
    kamp_data %>%
      filter(label_final == input$Pass_filter) %>%
      group_by(team.name) %>%
      summarise(
        good = sum(pass.accurate == TRUE, na.rm = TRUE),  
        inaccurate = sum(pass.accurate == FALSE, na.rm = TRUE)
      ) %>%
      mutate(total = good + inaccurate,
             good_pct = good / total * 100,
             inaccurate_pct = inaccurate / total * 100) %>%
      pivot_longer(cols = c(good, inaccurate), 
                   names_to = "metric", values_to = "value") %>%
      mutate(percentage = ifelse(metric == "good", good_pct, inaccurate_pct))
  })
  
  output$plot_2 <- renderPlotly({
    data_to_plot <- filtered_data_2()
    
    p <- ggplot(data_to_plot, aes(x = team.name, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 8) +
      labs(title = "Statistik over antal spilninger", x = "", y = "Antal afleveringer", fill = "") +
      theme(legend.title = element_blank()) +
      scale_y_continuous(breaks = seq(0, 1000, by = 50)) +
      theme_minimal()+
      theme(
        panel.grid.major.x = element_blank(),  # Fjerner gridlines fra x-aksen
        panel.grid.major.y = element_line(color = "grey80"),  # Beholder gridlines fra y-aksen
        panel.grid.minor = element_blank(),  # Fjerner mindre gridlines
        panel.background = element_blank(),  # Fjerner baggrundsfarven
        plot.title = element_text(size = 14, face = "bold"),  # Gør titlen større og bold
        legend.title = element_blank()
      )
    
    ggplotly(p)
  })
  
  filtered_data_4 <- reactive({
    req(input$Pass_filter, input$team_filter)
    kamp_data <- if (input$land_valg == "Holland") df_holland else df_polen
    
    kamp_data_filtered <- kamp_data %>%
      filter(label_final == input$Pass_filter)
    
    total_afleveringer <- kamp_data_filtered %>%
      summarise(total = n()) %>%
      pull(total)
    
    kamp_data %>%
      filter(label_final == input$Pass_filter) %>%
      group_by(team.name) %>%
      summarise(
        `Spilninger total` = n(),
        `Præcise afleveringer` = sum(pass.accurate == TRUE, na.rm = TRUE),
        `Fejl i afleveringer` = sum(pass.accurate == FALSE, na.rm = TRUE),
        `Præcision (%)` = `Præcise afleveringer` / `Spilninger total` * 100,
        `Andel af total afleveringer (%)` = (`Spilninger total` / total_afleveringer) * 100
      )
  })
  
  output$spilninger_table <- renderTable({
    filtered_data_4()
  })
  
  # Pane 3: Skud Visualisering server logik
  output$hold_filter_ui <- renderUI({
    req(input$land_valg_2)
    
    kamp_data <- if (input$land_valg_2 == "Holland") dfskud_holland else dfskud_polen
    teams <- unique(kamp_data$team.name)
    
    selectInput("Hold_filter", label = "Vælg et hold", choices = teams)
  })
  
  output$Kamp_filter_ui <- renderUI({
    req(input$Hold_filter)
    
    kamp_data <- if (input$land_valg_2 == "Holland") dfskud_holland else dfskud_polen
    games <- kamp_data %>%
      filter(team.name == input$Hold_filter) %>%
      pull(label) %>%
      unique()
    
    selectInput("Kamp_filter", label = "Vælg kamp", choices = games)
  })
  
  filtered_data_3 <- reactive({
    req(input$Kamp_filter, input$Hold_filter)
    kamp_data <- if (input$land_valg_2 == "Holland") dfskud_holland else dfskud_polen
    
    kamp_data %>%
      filter(label == input$Kamp_filter) %>%
      filter(!is.na(location.x) & !is.na(location.y)) %>%
      mutate(
        x_adj = ifelse(team.name != Hjemmebane, pitch_length - location.x, location.x),
        y_adj = ifelse(team.name != Udebane, pitch_width - location.y, location.y),
        shot_type = case_when(
          shot.isGoal == TRUE ~ "Mål",
          shot.onTarget == TRUE ~ "Skud på mål",
          shot.onTarget == FALSE ~ "Forsøg",
          TRUE ~ "Ukendt"
        ),
        color = case_when(
          shot_type == "Mål" ~ "orange",
          shot_type == "Skud på mål" ~ "blue",
          shot_type == "Forsøg" ~ "red",
          TRUE ~ "grey"
        )
      )
  })
  
  output$plot_3 <- renderPlotly({
    data <- filtered_data_3() %>%
      filter(!is.na(player.name), !is.na(x_adj), !is.na(y_adj)) %>%
      mutate(shot_type = factor(shot_type, levels = c("Mål", "Skud på mål", "Forsøg"))) 
    
    p <- ggplot() +
      annotate_pitch(fill = "darkgreen", colour = "white") +
      geom_point(data = data, aes(x = x_adj, y = y_adj, color = shot_type, text = player.name), size = 3) +
      scale_color_manual(
        values = c("Mål" = "orange", "Skud på mål" = "blue", "Forsøg" = "red"),
        labels = c("Mål", "Skud på mål", "Forsøg")
      ) +
      theme_pitch() +
      theme(legend.title = element_blank()) +
      guides(color = guide_legend(title = NULL)) +
      annotate("text", x = (pitch_length - 15), y = -2, label = unique(data$Hjemmebane)[1], color = "black", size = 5, hjust = 1) +
      annotate("text", x = 15, y = -2, label = unique(data$Udebane)[1], color = "black", size = 5, hjust = 0)
    
    p_plotly <- ggplotly(p)
    
    p_plotly %>% layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 0.95,
        yanchor = "bottom"
      )
    )
  })
  
  filtered_data_4_skud <- reactive({
    req(input$Kamp_filter, input$Hold_filter)
    kamp_data <- if (input$land_valg_2 == "Holland") dfskud_holland else dfskud_polen
    
    kamp_data %>%
      filter(label == input$Kamp_filter) %>%
      group_by(team.name) %>%
      summarise(
        `Skud total` = n(),
        `Skud på mål` = sum(shot.onTarget, na.rm = TRUE),
        Mål = sum(shot.isGoal, na.rm = TRUE),
        `Mål i procent af skud` = Mål / `Skud total` * 100
      )
  })
  
  output$skud_table <- renderTable({
    filtered_data_4_skud()
  })
  
}

# Kør appen
shinyApp(ui, server)
