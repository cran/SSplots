\name{SSplots_EPI}
\alias{SSplots_EPI}
\title{Stock-Catch-Status Plot or Catch by Stock Status Graph (Jayasankar et al. (2021))}
\usage{
SSplots_EPI(data,lower.lt,upper.lt, tsplots, MA)
}
\arguments{
\item{data}{dataset}
\item{lower.lt}{lower limit}
\item{upper.lt}{upper limit}
\item{tsplots}{time series plot}
\item{MA}{moving average}}
\description{
This function provides stock status plots to see the typical transition from developing through fully exploited to over exploited for each resources as per the criteria used by Jayasankar et al (2021) to interpret the status of fishery resource.
}
\details{
**Status of fishery** \verb{            }  **Criterion Applied**\cr\cr

Developing: \verb{  }  Year of landings < year of max landings AND landings < 20 percent of max landings \cr\cr
Fully Exploited:\verb{  }   Year of landings is between 20-80 percent of max landings \cr\cr
Over Exploited:\verb{  } Year of landings > year of maximum landings AND landings < 20 percent of \cr \verb{             } maximum landings OR landings > 80 percent of maximum landings\cr\cr
}
\examples{
\dontrun{
library (SSplots)
data("SampleData")
SSplots_EPI(data=SampleData,lower.lt=20,upper.lt=80, tsplots=FALSE, MA=FALSE)
}
}
\note{Note1: tsplots=TRUE for generating the time series plots for each resources. In that case it is advisable to set a working directory and number of graphs generated will be equal to the number of resources
\cr\cr
Note2: MA=TRUE for using the running average of order 3 (a three year running average was used to smooth the curve).
}
\keyword{Time Series Plot}
\keyword{Stock Status plots}
\references{
1) Grainger RJR and Garcia S (1996) Chronicles of marine fisheries landings (1950-1994): trend analysis and fisheries potential. FAO Fisheries Technical Paper 359, 51 p.

2) Kristin Kleisner and Daniel Pauly (2015) Stock-Status Plots (SSPs), https://www.seaaroundus.org/stock-status-plots-method/ (accessed on 28.02.2023)

3) Pauly D, Alder J, Booth S, Cheung WWL, Christensen V, Close C, Sumaila UR, Swartz W, Tavakolie A, Watson R and Zeller D (2008) Fisheries in Large Marine Ecosystems: Descriptions and Diagnoses. pp. 23-40 In: Sherman K and Hempel G (eds.), The UNEP Large Marine Ecosystem Report: a Perspective on Changing Conditions in LMEs of the World’s Regional Seas. UNEP Regional Seas Reports and Studies No. 182, Nairobi.

4) Sherman K and Hempel G, editors (2008) The UNEP Large Marine Ecosystem report: a Perspective on Changing Conditions in LMEs of the World's Regional Seas. UNEP Regional Seas Reports and Studies No. 182, United Nations Environment Programme, Nairobi. 852 p.

5) Jayasankar J, and Eldho Varghese, Shubhadeep Ghosh, Muktha M, Prathibha Rohit,  Abdussamad E M, Dineshbabu A P, Laxmilatha P, Ramachandran C, Shinoj P, Kavitha J, Vivekanandan E and Sreepriya V (2021) Environmental Performance Index (EPI) of Indian Marine Fisheries: A Perspective. ICAR-Central Marine Fisheries Research Institute, Kochi. ISBN 978-9382263-56-2.
}
