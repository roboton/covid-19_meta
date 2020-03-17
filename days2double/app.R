library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

#' Prepares data.frame for plotting by aggregating, weighting, reordering, 
#' and filtering by location.
#'
#' @param geo_level column name to aggregate (sum) by
#' @param dcr_wts weights to order locations; vector of c(deaths, confirmed,
#'                                                        recovered)
#' @param show_top_n show top n locations ordered by dcr_wts
#'
#' @return an aggregated, reordered, and filtered by location data.frame
#'
#' @examples
#' plot_prep("Country/Region", c(100, 1, -1), show_top_n = 25)
#'
#' @export
plot_prep <- function(df, geo_level = "Country/Region", dcr_wts = c(1, 0.01, 0),
                      show_top = 1:10) {
  df %>%
    mutate(location = !!sym(geo_level)) %>%
    # aggregate by location
    group_by(location, date, stat) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    # location order
    group_by(location) %>%
    mutate(
      total_wt =
        value[which(date == max(date) & stat == "deaths")] * dcr_wts[1] +
        value[which(date == max(date) & stat == "confirmed")] * dcr_wts[2] +
        value[which(date == max(date) & stat == "recovered")] * dcr_wts[3]
    ) %>%
    ungroup() %>%
    mutate(total_rank = dense_rank(desc(total_wt))) %>%
    # filter top n
    filter(total_rank %in% show_top) %>%
    # order by rank
    mutate(location = fct_reorder(location, total_rank)) 
}

plot_deaths_since_first <- function(df, days_since, init_deaths = 3,
                                    show_top = 1:25) {
  xlab <- paste("Days since deaths >=", init_deaths)
  df %>%
    plot_prep("Country/Region", show_top = show_top) %>%
    group_by(location) %>%
    mutate(total_deaths = value[which(date == max(date) &
                                        stat == "deaths")][1]) %>%
    filter(total_deaths > 0) %>%
    mutate(init_death_date = date[which(value >= init_deaths &
                                           stat == "deaths")][1]) %>%
    mutate(
      !!sym(xlab) := date - init_death_date) %>%
    filter(stat == "deaths" & value > 0 &
             between(!!sym(xlab), 0, days_since)) %>%
    ggplot(aes(!!sym(xlab), value, 
               color=location)) + geom_line() +
    theme(strip.background = element_blank(), legend.position = "right",
          legend.title = element_blank()) +
    ylab("Total deaths")
}


# data prep
confirmed_ts <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")) %>%
  gather(date, confirmed, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))
deaths_ts <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")) %>%
  gather(date, deaths, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))
recovered_ts <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")) %>%
  gather(date, recovered, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))

joined <- confirmed_ts %>%
  left_join(deaths_ts) %>%
  left_join(recovered_ts) %>%
  gather(stat, value, confirmed, deaths, recovered) %>%
  arrange(`Country/Region`, `Province/State`, date, stat)

# countries_provinces <- joined %>%
#     select(`Country/Region`, `Province/State`) %>%
#     distinct()

# params

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Days to double deaths"),
  tags$a(
    href = "https://github.com/CSSEGISandData/COVID-19",
    target = "_blank", "data"),
  tags$a(
    href = "https://ond3.com/exploratory.nb.html#days_to_double_deaths_since_10th_death",
    target = "_blank", "analysis"),
  tags$a(
    href = "https://github.com/roboton/covid-19_meta/tree/master/days2double",
    target = "_blank", "git"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_deaths",
                  "initial number of deaths:",
                  min = 2,
                  max = 20,
                  value = 10),
      sliderInput("top_n_countries",
                  "top country range:",
                  min = 1,
                  max = 25,
                  value = c(1, 15)),
      sliderInput("days_since",
                  "days since nth death",
                  min = 1,
                  max = 100,
                  value = 30),
      width = 3
    ),
    mainPanel(
      plotlyOutput("d2dPlot", height = "500px"),
      plotlyOutput("compPlot", height = "500px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$d2dPlot <- renderPlotly({
    geo_level = "Country/Region"
    min_deaths <- input$n_deaths * 2
    trunc_to_n <- 10 - input$trunc_to_n
    top_n_countries <- input$top_n_countries[1]:input$top_n_countries[2]
    ggplotly(joined %>%
      # plot prep at country level
      plot_prep(geo_level, show_top = top_n_countries) %>%
      # limit to deaths
      filter(stat == "deaths") %>%
      # drop countries with less than min_deaths
      group_by(location) %>%
      filter(value[which.max(date)] >= min_deaths) %>%
      ungroup() %>%
      # get the first date with more than or equal to n_deaths
      group_by(location) %>%
      mutate(nth_death_date = date[which(value >= input$n_deaths) - 1][1]) %>%
      mutate(`Days since nth death` = date - nth_death_date) %>%
      # drop days before nth deaths date
      filter(`Days since nth death` > 0) %>%
      # compute Days to Double
      group_by(location) %>%
      mutate(
        # set days to double
        double_idx = sapply(
          value, FUN = function(x) { max(which(value <= x/2)) }),
        `Days to double deaths` = date - date[double_idx]
      ) %>%
      filter(!is.na(`Days to double deaths`)) %>%
      ## plot
      # truncate to trunc_to_n longest time series
      filter(between(`Days since nth death`, 0, input$days_since)) %>%
      # group_by(location) %>%
      # mutate(max_days_since_nth_death = max(`Days since nth death`)) %>%
      # ungroup() %>%
      # mutate(max_days_rank = dense_rank(desc(max_days_since_nth_death))) %>%
      # filter(`Days since nth death` <= max(
      #   `Days since nth death`[max_days_rank == trunc_to_n])) %>%
      group_by(location) %>%
      mutate(time_points = n()) %>%
      ungroup() %>%
      filter(time_points > 2) %>%
      ggplot(aes(`Days since nth death`, `Days to double deaths`,
                 color = location)) +
      geom_point(alpha = 0.2) +
      geom_smooth(se = F) +
      xlab(paste("Days since deaths >=", input$n_deaths)))
  })
    output$compPlot <- renderPlotly({
    top_n_countries <- input$top_n_countries[1]:input$top_n_countries[2]
    
  ggplotly(joined %>%
      plot_deaths_since_first(input$days_since, input$n_deaths,
                              show_top = top_n_countries))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)