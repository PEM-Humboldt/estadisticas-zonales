# Miscelaneo


# presentación de resultados

# Gráfica de barras de las regiones individuales de cada grupo de área especial 

 bar_plot<-function(capa, variable, título){
   capa %>%
     ggplot()+
     #geom_bar(aes(y=!!sym(variable), x= Integrity_inde_x100), stat="identity")+
     geom_bar(aes(y=reorder(!!sym(variable), -Integrity_inde_x100), x= Integrity_inde_x100), stat="identity")+
     labs(title=título, y="", x= "Promedio Integridad")+
     theme(plot.title = element_text(size = 20, hjust = 1),
           axis.title.x = element_text(size=12, margin=margin(t=10)) )
 }


 bar_plot(resguardos_intg,variable="NOMBRE_RES", título="Resguardos Indígenas"  )
 bar_plot(runap_intg,variable="nombre", título="Áreas Protegidas"  )
 bar_plot(negros_intg,variable="CNNOMBRE", título="Comunidades Negras"  )


 # crear gráfica coloreando las celdas deacuerdo la magnitud del valor
 formattable(runap_intg, list(
   Integridad = color_tile("white","darkorange")
 ))
