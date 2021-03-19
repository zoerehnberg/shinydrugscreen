runuser - rep --command="R -e 'shiny::shinyApp(ui = shinyDrugScreen:::ui, server = shinyDrugScreen:::server, options=list(port=3839))' &> /dev/null &"
/startup/jupyter.sh
/startup/rstudio.sh
/startup/nginx.sh
/startup/console_message.sh
su -s /bin/bash - rep
