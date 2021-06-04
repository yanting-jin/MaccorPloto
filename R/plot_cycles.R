plot_cycles <- function(echem,xmax){
  df_cycle <- echem$df_cycle
  max_cycle <-max(df_cycle$cycle)
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(0,max_cycle),
       ylim=c(0,xmax),
       xlab='Cycle',ylab='Specific Capacity (mAh/g)')
  points(df_cycle$cycle, df_cycle$charge, pch=1,cex=0.8)
  points(df_cycle$cycle, df_cycle$discharge, pch=2,cex=0.8)
  legend('bottomright', legend = c("Charge","Discharge"), pch =c(1,2),bg='white')
}
