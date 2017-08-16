package org.mmoon

import javax.activation.MimeType
import javax.servlet.http.HttpServletRequest

import org.scalatra.Conneg
import org.scalatra.Conneg._

/**
  * Created by Markus Ackermann.
  * No rights reserved.
  */
package object scalatra {//

  //mimeTypeMapping isn't complete right now. Missing entries: .ofn .omn
  val mimeTypeMapping = List(
    (".rdf", "application/rdf+xml"),
    (".owl", "application/rdf+xml"),
    (".owx", "application/owl+xml"),
    (".ttl", "text/turtle"),
    (".nt", "application/n-triples"),
    (".owm", "text/owl-manchester"),
    (".jsonld", "application/ld+json"),
    (".html", "text/html"),
    (".rdf", "*/*"))
  //The javax.activation.MimeType.match method matches "*" only in subtype to any supported MimeType
  //(which is the common sense of "*" character). Therefore "*/*" had to be added manually.

  val supportedMimeTypes = mimeTypeMapping.map(_._2).toSet.map((t: String) => new MimeType(t))
  //is there a better solution than using "new" expression?

  implicit object MimeTypeFormat extends Format[MimeType] {
    override def entry: MimeTypeFormat.Parser[Option[MimeType]] = token ^^
      { mimeTypeStr => new MimeType(mimeTypeStr) } ^^
      { mimeType => supportedMimeTypes.find(t => t.`match`(mimeType)) }

    //deleted "/" character, because expressions like "text/turtle" should be accepted
    override def token: MimeTypeFormat.Parser[String] = """[\u0020-\u007E&&[^ \t()<>@,;:\"\[\]?={}]]+""".r
  }

  def acceptedMimeTypes(implicit req: HttpServletRequest): List[Conneg[MimeType]] = values("Accept")


}
