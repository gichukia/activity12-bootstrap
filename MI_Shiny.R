r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  selectInput("variable", "Variable:",
                            c(male18to24noHSdip),
  leafletOutput("MImap")
  ##p(),
  ##actionButton("recalc", "New Variables")
))

server = function(input, output) {
  Data3 <- get_acs(geography = "county",
                   variables = Variable,
                   state = "MI",
                   geometry = TRUE) %>%
    st_transform(4326)
  
  Data4 <- get_acs(geography = "tract",
                   variables = Variable,
                   state = "MI",
                   geometry = TRUE) %>%
    st_transform(4326)
  
  output$MImap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = Data3, stroke = FALSE, smoothFactor = 0.2, 
                  color = ~pala(estimate), 
                  label = ~as.character(estimate), 
                  fillOpacity = 0.8, 
                  group = "Counties") %>%
      addPolygons(data = Data4, stroke = FALSE, smoothFactor = 0.2, 
                  color = ~pala(estimate), 
                  label = ~as.character(estimate), 
                  fillOpacity = 0.8, 
                  group = "Tracts") %>%
      addLegend("bottomright", pal = pala, values = Data3$estimate, 
                title = "Females 35 to 44 Without HS Diploma") %>%
      addLayersControl(overlayGroups = c("Tracts", "Counties")) %>% 
      hideGroup("Tracts")
  })
}

shinyApp(ui, server)
