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
    //(".owl", "application/rdf+xml"),
    (".owx", "application/owl+xml"),
    (".ttl", "text/turtle"),
    (".nt", "application/n-triples"),
    (".owm", "text/owl-manchester"),
    (".jsonld", "application/ld+json"),
    (".html", "text/html"))

  val supportedMimeTypes = mimeTypeMapping.map(_._2).toSet.map((t: String) => new MimeType(t))
  //is there a better solution than using "new" expression?

  implicit object MimeTypeFormat extends Format[MimeType] {
    val x = """([a-z]+|\*)/([a-z+-]+|\*)""".r
    override def entry: MimeTypeFormat.Parser[Option[MimeType]] = token ^^
    {t => t match {
      case x(_,_) => Some(new MimeType(t))
      case _ => None
    }
  }


    //deleted "/" character, because expressions like "text/turtle" should be accepted
    override def token: MimeTypeFormat.Parser[String] = """[\u0020-\u007E&&[^ \t()<>@,;:\"\[\]?={}]]+""".r
  }

  val Accept = "Accept"

  def acceptedMimeTypes(implicit req: HttpServletRequest): List[Conneg[MimeType]] = values(Accept)

  //def preferredMimeType(implicit req: HttpServletRequest): Option[MimeType] = preferredValue(Accept)

}
