\name{STLELM}
\alias{STLELM}
\title{
STL Based ELM Forecasting Model
}
\description{
The STLELM function forecasts a time series using a hybrid model made of a decomposition technique called seasonal trend decomposition based on loess (STL) and a neural network based forecasting technique called extreme learning machine (ELM). The function further computes the values of different forecasting evaluation criteria.
}
\usage{
STLELM(data, stepahead=10)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
}
\details{
This function decomposes a nonlinear, nonstationary and seasonal time series into trend-cycle, seasonal and remainder component using STL (Cleveland et al., 1990). Extreme learning machine (ELM) is used to forecast these components individually (Huang et al., 2006, Xiong et al. 2018). Finally, the prediction results of all the three components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{STLcomp_forecast }{Forecasted value of all individual components.}
  \item{FinalstlELM_forecast }{Final forecasted value of the stlELM model. It is obtained by combining the forecasted value of all individual components.}
  \item{MAE_stlELM }{Mean Absolute Error (MAE) for stlELM model.}
  \item{SMAPE_stlELM }{Mean Absolute Percentage Error (MAPE) for stlELM model.}
  \item{RMSE_stlELM }{Root Mean Square Error (RMSE) for stlELM model.}
}
\references{
Cleveland, R.B., Cleveland, W.S., McRae, J.E., Terpenning, I. (1990). STL: A seasonal-trend decomposition procedure based on loess, Journal of Official Statistics, 6, 3--73.
Huang, G., Zhu, Q., Siew, C. (2006). Extreme learning machine: theory and applications, Neurocomputing,70, 489--501.
Xiong, T., Li, C., Bao, Y. (2018). Seasonal forecasting of agricultural commodity price using a hybrid STL and ELM method: Evidence from the vegetable market in China. Neurocomputing 275, 2831--2844
}

\examples{
\donttest{
data("Data_potato")
STLELM(Data_potato)
}
}
\keyword{ stl }
