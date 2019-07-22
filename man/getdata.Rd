\name{agrin_data}

\Rdversion{1.1}

 
\alias{agrin_data}


\title{
Get example data
}

\description{
Get example data
}

\usage{
agrin_data(name)
}

\arguments{
 \item{name}{data set name}
 \item{files}{filenames to download}
 \item{path}{path (folder) to save the data to}
 \item{url}{url where the data is downloaded from}
}

\value{
A data set. Often a Spatial* object or a data.frame
}



\examples{
marsabit <- agrin_data("marsabit")
}



