function(input,output,session){
  output$my_dates = renderUI({
    req(input$nm_eta)
    my_flux = test_tout[cod_ror_eg == input$nm_eta]
    dateInput("my_date",label = "Date",min = min( my_flux$date),max=max( my_flux$date),language = "fr",value = sample(my_flux$date,1))
  })
  
  
  output$current_flux = renderPlotly({
    req(input$my_date)
    my_flux = test_tout[date > input$my_date-7 & date <= input$my_date & cod_ror_eg == input$nm_eta][,.(nb=sum(nb)),by=.(date,weekday,cod_ror_eg)]
    my_flux[,month:=month(date)]
    saison = merge(my_flux[,.(month,weekday,cod_ror_eg,date)],unique(moyenne_de_saison_tot),by=c("month","weekday","cod_ror_eg"))
    plot_ly()%>%add_bars(data=my_flux,x=~date,y=~nb,name = "Observée")%>%add_bars(data = saison,x=~date,y=~nb,name="Moyenne de saison")%>%  layout(title="La semaine dernière",showlegend = FALSE,xaxis=list(title="Date"),yaxis=list(title="Nombre d'admissions"))
  })
  
  output$next_flux = renderPlotly({
    req(input$my_date)
    my_flux = test_tout[date > input$my_date & date <= input$my_date+7 & cod_ror_eg == input$nm_eta]
    my_flux= my_flux[,.(pred=sum(pred)),by=.(date,weekday,cod_ror_eg)]
    my_flux[,month:=month(date)]
    saison = merge(my_flux[,.(month,weekday,cod_ror_eg,date)],unique(moyenne_de_saison_tot),by=c("month","weekday","cod_ror_eg"))
    plot_ly(source= "select")%>%add_bars(data=my_flux,x=~date,y=~pred,name = "Prévision")%>%add_bars(data = saison,x=~date,y=~nb,name="Moyenne de saison")%>%  layout(title="La semaine prochaine",showlegend = FALSE,xaxis=list(title="Date"),yaxis=list(title="Nombre d'admissions"))
  })
  
  observe({
    req(c(input$nm_eta,input$my_date))
    ms_eta = moyenne_de_saison_external[cod_ror_eg == input$nm_eta]
    ms_eta[,c("temp_min","temp_max"):=.(temp_min-273.15,temp_max-273.15)]
    setnames(ms_eta,c("temp_min","temp_max","rhinopharyngite","NO2","O3"),c("ms_temp_min","ms_temp_max","ms_rhinopharyngite","ms_NO2","ms_O3"))
    my_data = test_tout[date > input$my_date -7 & date <= input$my_date+7 & cod_ror_eg == input$nm_eta]
    my_data[,week:=week(date)]
    my_data[,c("temp_min","temp_max"):=.(temp_min-273.15,temp_max-273.15)]
    
    min_temp = min(my_data$temp_min)
    max_temp = max(my_data$temp_max)
    
    my_data = merge(my_data,ms_eta,by=c("cod_ror_eg","week"))
    output$variation_meteo = renderPlotly({
    plot_ly()%>%
      add_ribbons(data=my_data,x=~date,ymin=~temp_min,ymax=~temp_max,name = "Observé/prédit")%>%
      add_ribbons(data=my_data,x=~date,ymin=~ms_temp_min,ymax=~ms_temp_max,name = "Moyenne de saison")%>%
      layout(title="Meteo",showlegend = FALSE,xaxis=list(title="Date"),yaxis=list(title="Température"))%>%
      add_segments(x = input$my_date, xend = input$my_date, y = min_temp, yend = max_temp,name="Jour d'observation",line=list(color="black"))
    })
    
    min_polluant = min(min(my_data$NO2polluant_q5),min(my_data$O3polluant_q5))
    max_polluant = max(max(my_data$NO2polluant_q5),max(my_data$O3polluant_q5))
    output$variation_pollution = renderPlotly({
      plot_ly()%>%
        add_lines(data=my_data,x=~date,y=~ms_NO2,name = "MS - NO2")%>%
        add_lines(data=my_data,x=~date,y=~ms_O3,name = "MS - O3")%>%
        add_lines(data=my_data,x=~date,y=~NO2polluant_q5,name = "Obs- NO2")%>%
        add_lines(data=my_data,x=~date,y=~O3polluant_q5,name = "Obs - O3")%>%
        layout(title="Polluants",showlegend = FALSE,xaxis=list(title="Date"),yaxis=list(title="Concentration"))%>%
        add_segments(x = input$my_date, xend = input$my_date, y = min_polluant, yend = max_polluant,name="Jour d'observation",line=list(color="black"))
    })
    
    min_trend = min(min(my_data$rhinopharyngite),min(my_data$ms_rhinopharyngite))
    max_trend = max(max(my_data$rhinopharyngite),max(my_data$ms_rhinopharyngite))
    output$variation_gtrends = renderPlotly({
      plot_ly()%>%
        add_lines(data=my_data,x=~date,y=~ms_rhinopharyngite,name = "MS - rhinopharyngite")%>%
        add_lines(data=my_data,x=~date,y=~rhinopharyngite,name = "Obs - rhinopharyngite")%>%
        layout(title="GTrends",showlegend = FALSE,xaxis=list(title="Date"),yaxis=list(title="Importance"))%>%
        add_segments(x = input$my_date, xend = input$my_date, y = min_trend, yend = max_trend,name="Jour d'observation",line=list(color="black"))
    })
    
  })
  
  
  output$ui_date_focus = renderUI({
    dateInput("date_focus","Jour précis",min = input$my_date+1,max = input$my_date + 7,language = "fr",value = input$my_date+1)
  })
  
  observeEvent(event_data("plotly_click", source = "select"),{
    event.data <- event_data("plotly_click", source = "select")
    updateDateInput(session,"date_focus",value=event.data$x)
    
  })
  
  

  output$flux_hospi = renderPlotly({
    req(input$my_date)
    my_flux = test_hospi[date == input$date_focus & cod_ror_eg == input$nm_eta][,.(nb=sum(nb),pred=sum(pred)),by=.(categorie_age,date,cod_ror_eg)]
    my_flux[,weekday:=weekdays(date)]
    my_flux[,month:=month(date)]
    my_ms = moyenne_de_saison_mode_sortie_age[mode_sortie == "mutation/transfert/deces"]
    my_ms[,mode_sortie:=NULL]
    setnames(my_ms,"nb","ms_nb")
    my_flux = merge(my_flux,my_ms,by=c("month","weekday","cod_ror_eg","categorie_age"))
    
    plot_ly()%>%add_bars(data=my_flux,x=~categorie_age,y=~pred,name = "Prédite")%>%
      add_bars(x=~categorie_age,y=~ms_nb,name="Moyenne de saison")%>%  
      layout(title="Hospitalisation",showlegend = FALSE,xaxis=list(title="Catégorie d'âge"),yaxis=list(title="Admissions"))
  })
  
  output$flux_rad = renderPlotly({
    req(input$my_date)
    my_flux = test_rad[date == input$date_focus & cod_ror_eg == input$nm_eta][,.(nb=sum(nb),pred=sum(pred)),by=.(categorie_age,date,cod_ror_eg)]
    my_flux[,weekday:=weekdays(date)]
    my_flux[,month:=month(date)]
    my_ms = moyenne_de_saison_mode_sortie_age[mode_sortie == "domicile"]
    my_ms[,mode_sortie:=NULL]
    setnames(my_ms,"nb","ms_nb")
    my_flux = merge(my_flux,my_ms,by=c("month","weekday","cod_ror_eg","categorie_age"))
    
    plot_ly()%>%add_bars(data=my_flux,x=~categorie_age,y=~pred,name = "Prédite")%>%
      add_bars(x=~categorie_age,y=~ms_nb,name="Moyenne de saison")%>%  
      layout(title="RaD",showlegend = FALSE,xaxis=list(title="Catégorie d'âge"),yaxis=list(title="Admissions"))
  })
  
  output$erreurs = renderDT({
    print(input$mode_sortie)
    if(input$mode_sortie){
      erreur = erreur_hospi[cod_ror_eg == input$nm_eta]
      
    } else if (!input$mode_sortie){
      erreur = erreur_rad[cod_ror_eg == input$nm_eta]
    }
    erreur = t(erreur)
    colnames(erreur) <- erreur[1,]
    erreur = erreur[-1,]
    erreur = erreur[-1,]
    datatable(erreur)
  })
  
  
  observe({
    req(c(input$date_focus,input$nm_eta))
    my_data = 
    
    
    
    explainer_xgboost <- lime(test_tout[,vars_explicatives,with=F], bst_tout, n_bins = 5)
    
    focus_data = test_tout[date %in% input$date_focus & cod_ror_eg %in% input$nm_eta]
    output$cat_age = renderUI(selectInput("choix_age","Catégorie d'âge",choices = unique(focus_data$categorie_age)))
    req(input$choix_age)
    focus_data = focus_data[categorie_age == input$choix_age]

    explanation_xgboost <- explain(
      x = focus_data[,vars_explicatives,with=F], 
      explainer = explainer_xgboost, 
      n_permutations = 5000,
      dist_fun = "gower",
      kernel_width = .75,
      n_features = 10, 
      feature_select = "highest_weights",
      labels = "Yes"
    )
    
    
    output$lime = renderPlotly(plot_features(explanation_xgboost)%>%ggplotly)
    
  })
  
  
  
  
  
}