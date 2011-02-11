\name{phpSerialize}
\alias{phpSerialize}
\alias{phpSerializeAll}
\title{ R to PHP Serialization }
\description{
  Serializes R objects for PHP import into an associative
  array. Main use is for building web pages with R-support.
}
\usage{
phpSerialize(x, file = NULL, append = FALSE, associative = "1D",
   simplifyMono=TRUE, phpTestCode = FALSE)

phpSerializeAll(include=NULL, exclude=NULL, file = NULL, append = FALSE, 
   associative = "1D",   simplifyMono=TRUE, phpTestCode = FALSE)
}

\arguments{
  \item{x}{ an object }
  \item{file}{ a file name or a connection, or NULL to return
   the output as a string. If the connection is not open it
   will be opened and then closed on exit.}
  \item{append}{ append or overwrite the file? }
  \item{associative}{ a character string of \code{"1D"} (default),
  \code{"2D"} or \code{"no"}. For \code{"1D"}, only scalars
  and vectors are serialized as associative arrays which can
  be retrieved by name, while arrays are exported with numeric indexes.
  For \code{"2D"}, arrays are also exported by name. For \code{"no"},
  objects are serialized with numerical indexes only, and factors are
  serialized as integers. }
  \item{simplifyMono}{ if TRUE (default), unnamed vectors of length 1
  are reduced to scalars, which is easier to understand in PHP. 
  For \code{simplyMono=FALSE}, these vectors are serialized as arrays
  with length 1. We need another level of indexing (\code{[1]}) in PHP, 
  but we are closer to the way R ticks. Named vectors are are always 
  serialized as arrays to preserve the name, even if \code{associative="no"}.
  }
  \item{phpTestCode}{ if TRUE, php test code for direct display of
    the associative PHP-array is added.}
  \item{include}{ a vector of characters strings of the objects in 
   \code{ls(1)} to be serialized. If \code{NULL}, all \code{ls(1)} 
   objects will be serialized.  Partial matching is used, so  
   with \code{include="lm."}, objects like \code{lm.test} and \code{lm.best} 
   are included. } 
  \item{exclude}{a vector of character strings of objects excluded from 
    serialization. Partial matching is used, so \code{exclude="HTML"} will 
    remove objects like \code{HTMLAll} and \code{HTMLlme}, but not \code{myHTML}.
    Items in \code{exclude} have precedence over those in \code{include}.}
}

\details{
See the \code{sample} directory for an extended test case and
the \code{web} directory for an Apache-tested web application.

Matrices with dimensions > 2 and complex numbers are currently not supported.

}
\value{
  Returns a string if file=NULL, or NULL if a file argument was supplied.
  For example, \code{phpSerialize(c("a","b"))} produces the output string
  \code{a:2:\{i:1;s:1:"a";i:2;s:1:"b";\}},
  giving \code{Array([1]=>a,[2]=>b)} after unserializing in PHP.
}
\references{
 Serialize PHP: \url{http://jpspan.sourceforge.net/examples/serialize_php.php}. 
 Manual PHP: \url{http://www.php.net/manual/en/index.php}
}
\author{ Dieter Menne \email{(dieter.menne@menne-biomed.de)}}
\note{ Code was tested mainly with lm, lme and nlme.  }

\seealso{ \code{\link{capture.output}} }

\examples{
# htest object
wc = wilcox.test(rnorm(10)+2,rnorm(10)+5)
phpSerialize(wc)

#lm summary
group = gl(2,10,20, labels=c("Ctl","Trt"))
weight = c(rnorm(10)+10, rnorm(10)+6)
sumlm = summary(lm(weight ~ group))
phpSerialize(sumlm,phpTestCode=TRUE)

phpSerializeAll(include="w",exclude="sum",phpTestCode=TRUE)
}
\keyword{ interface }
\keyword{ connection }
