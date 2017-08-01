import Redirector.MimeTypeFormat
import org.scalatra.Conneg._

/**
  * Created by Markus Ackermann.
  * No rights reserved.
  */
package object Redirector {

  val contentTypeMapping = List(
    (".rdf", "application/rdf+xml"),
    (".owl", "application/rdf+xml"),
    (".owx", "application/owl+xml"),
    (".ttl", "text/turtle"),
    (".nt", "application/n-triples"),
    (".owm", "text/owl-manchester"),
    (".jsonld", "application/ld+json"),
    (".html", "text/html"))

  val supportedMimeTypes = contentTypeMapping.map(_._2).toSet


  implicit object MimeTypeFormat extends Format[String] {

    override def entry: MimeTypeFormat.Parser[Option[String]] = token ^^ { mimeTypeStr =>

      if(supportedMimeTypes contains mimeTypeStr) Some(mimeTypeStr) else None
    }
  }
}
