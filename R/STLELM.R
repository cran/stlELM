#' @importFrom forecast mstl
#' @importFrom nnfor elm
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
STLELM <- function(data, stepahead=10){
  STLcomp <- forecast::mstl(data)
  STLcomp_plots<-plot(STLcomp)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  STLcomp_trn <- STLcomp[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_STLcomp <- NULL
  for (STLcomp in 2:ncol(STLcomp_trn)) {
    Indcomp <- NULL
    Indcomp <- STLcomp_trn[ ,STLcomp]
    stlELMFit <- nnfor::elm(as.ts(Indcomp))
    stlELM_fcast=forecast::forecast(stlELMFit, h=stepahead)
    stlELM_fcast_Mean=stlELM_fcast$mean
    Fcast_STLcomp <- cbind(Fcast_STLcomp, as.matrix(stlELM_fcast_Mean))
  }
  FinalstlELM_fcast <- ts(rowSums(Fcast_STLcomp, na.rm = T))
  MAE_stlELM=mean(abs(data_test - FinalstlELM_fcast))
  MAPE_stlELM=mean(abs(data_test - FinalstlELM_fcast)/data_test)
  rmse_stlELM=sqrt(mean((data_test - FinalstlELM_fcast)^2))
  return(list(data_test=data_test, STLcomp_forecast=Fcast_STLcomp,
              FinalSTLcomp_forecast=FinalstlELM_fcast, MAE_stlELM=MAE_stlELM,
              MAPE_stlELM=MAPE_stlELM, rmse_stlELM=rmse_stlELM,
              STLcomp_plots=STLcomp_plots))
}

