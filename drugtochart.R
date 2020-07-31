demodrugannot<-function(x){
  df<-demodrugSummary[demodrugSummary$Drug==x,]
  a<-list(x=df$invivoSurvivalSMD,
          y=df$nCellDeathSMD,
          xref= "x",
          yref= "y",
          text =~ x,
          showarrow=TRUE,
          arrowhead=7,
          ax=20,ay=-40)
  a
}

drugtochart<- function(x){
  df<-drugSummary[drugSummary$Drug==x,]
  
  drugrankchart <- plot_ly(
    drugSummary,
    x = ~ invivoSurvivalSMD,
    y = ~ nCellDeathSMD,
    hoverinfo = 'text',
    
    text = ~ paste(
      '</br> Drug:',
      Drug,
      '</br> Clinical Score:',
      round(productScore, digits = 2),
      '</br> n(clinical publications):',
      nPublication,
      '</br> in vivo SMD:',
      round(invivoSurvivalSMD, digits = 2),
      '</br> in vitro SMD:',
      round(nCellDeathSMD),
      digits = 2
    ),
    type = 'scatter',
    mode = 'markers',
    
    marker = list(size = ~ nPublication,
                  opacity = 0.5,
                  color = ~ productScore,
                  colorscale = 'Viridis', 
                  reversescale=TRUE,
                  showscale=T,
                  colorbar=list(title='Product Score'),
                  sizemin= 3
    )
  )%>%
    layout(title="Clinical, in vivo and in vitro scores by drug",
           xaxis=(list(title="In vivo Survival SMD")),
           yaxis=(list(title="In vitro cell death SMD")))
  drugrankchart
  
  a<-list(x=df$invivoSurvivalSMD,
          y=df$nCellDeathSMD,
          xref= "x",
          yref= "y",
          text =~ x,
          showarrow=TRUE,
          arrowhead=7,
          ax=20,ay=-40)
  drugbubble<-drugrankchart%>%add_markers()
  drugbubble<-drugbubble%>%layout(annotations=a)
  drugbubble
}