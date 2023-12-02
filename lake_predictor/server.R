
function(input, output, session) {
  
  geo <- reactive({
    
    geo_select <- geo_lakes %>% filter(Lake_name == input$lake)
    geo_select
  })
  
  area <- reactive({
    
    hy_id <- geo()[[1, 'Hylak_id']]
    
    df_wide <- water_area %>%
      filter(Hylak_id == hy_id)
    
    df_area <- df_wide %>%
      select(-Hylak_id) %>%
      pivot_longer(
        cols = everything()
      )
    
    df_area
  })

  
  model_spec <- reactive({
    
    ts_area <- ts(area()$value, 
                  start = c(1985, 1), 
                  end = c(2018, 12), 
                  frequency = 12)
    
    arima_fit <- Arima(ts_area, order = c(0,0,1), seasonal = c(1,1,1),
                       include.drift = T)
  })
  
    
    pred_change <- reactive({
      
      
      
      preds <- forecast(model_spec(), h = 360)
      df_preds <- as_tibble(preds$mean)
      
      df_preds
    })
    
    pred_date_reactive <- reactive({
      pred_date <- ym(str_sub(input$pred_date, 1, -3))
      pred_date
    })
    output$lake_map <- renderLeaflet({
      
      
      pred_date <- pred_date_reactive()
      model_end <- ym("2018-12")
      td <- interval(model_end, pred_date) %/% months(1)
      
      df_geo <- geo() %>%
        st_make_valid()
      
      start_area <- tail(area()$value, n = 1)
      
      target_area <- pred_change()$x[[td]]
      
      perimeter <- sum(st_length(st_cast(df_geo, "LINESTRING")))
      
      buffer_distance <- as.numeric((target_area-start_area) / perimeter)

      longitude <- st_coordinates(st_centroid(df_geo))[[1, 'X']]
      utm_zone <- floor((longitude + 180) / 6) + 1
      utm_crs_code <- ifelse(longitude < 0, 32600, 32700) + utm_zone
      
      df_geo_utm <- st_transform(df_geo, crs = utm_crs_code)
      
      df_buffer <- df_geo_utm %>% st_buffer(dist = buffer_distance) %>%
        st_transform(4326)
        
      
      
      df_geo %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Standard view") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite view") %>%
        addPolygons(
          weight = 1,
          fillOpacity = 0,
          color = "blue"
        ) %>%
        addPolygons(
          data = df_buffer,
          weight = 1,
          fillOpacity = 0,
          color = 'red'
        ) %>%
        addLayersControl(
          baseGroups = c("Standard view", "Satellite view"),
          options = layersControlOptions(collapsed = F)
        )
      
    })
    
    output$forecast_plot <- renderPlot({
      df_plot <- bind_rows(
        area(),
        pred_change() %>% rename(value = x),
        
      ) %>%
        mutate(
          name = seq(
            as.Date("1985-01-01"), as.Date('2048-12-01'), by = "1 month"
          ),
          observed = if_else(name <= as.Date("2018-12-01"), "Observed", "Predicted")
        )
      
      ggplot(
        df_plot, aes(x = name, y = value, group = observed, color = observed)
      ) +
        geom_line() +
        geom_vline(xintercept = as.Date(input$pred_date)) +
        bbplot::bbc_style()
      
    })
    

    

}
