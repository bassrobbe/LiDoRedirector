package org.mmoon.scalatra

import java.io.{ByteArrayInputStream, InputStream}

import org.scalatest._

import scalaj.http._
import org.eclipse.rdf4j.rio.{RDFFormat, Rio}

/**
  * Created by bassrobbe on 10/27/17.
  */
class RedirectorTest extends FunSuite {

  val supportedAcceptHeaders = List(
    ("application/rdf+xml","application/rdf+xml"),
    ("text/turtle","text/turtle"),
    ("application/n-triples","application/n-triples"),
    ("text/html","text/html"),
    ("fsd/sdf,text/html;q=0.9","text/html"),
    ("image/webp,application/rdf+xml;q=0.9,application/*;q=0.8,*/*;q=0.8","application/rdf+xml"),
    ("text/html;q=0.9,application/rdf+xml;q=0.9,image/webp,application/*;q=0.8,*/*;q=0.8","text/html")
  )

  val supportedFileExtenisons = List(".rdf", ".ttl", ".nt", ".html")

  val supportedUris = List(
    "http://mmoon.org/core",
    "http://mmoon.org/core/AbsoluteAdjective",
    "http://mmoon.org/deu/schema/og",
    "http://mmoon.org/deu/schema/og/Affix",
    "http://mmoon.org/deu/inventory/og",
    "http://mmoon.org/deu/inventory/og/DerivedWord_verkaufen"
  )

  //val unsupported


  test("URIs without file extension -> Content Negotiation") {

    for (acceptHeader <- supportedAcceptHeaders; uri <- "http://mmoon.org/core/" :: supportedUris) {
      println(uri)
      println(acceptHeader)
      val response: HttpResponse[String] = Http(uri).header("Accept", acceptHeader._1).asString

      assert(response.code == 303)
      assert(response.contentType.fold(false) { _.startsWith(acceptHeader._2) })

      println(s"URI: ${uri}     Accept: ${acceptHeader._1}     Identified MimeType: ${acceptHeader._2}     Status: succesful")
    }

  }

  test("URIs with file extension -> MimeType is given via URI") {

    for (fileExt <- supportedFileExtenisons; uri <- supportedUris) {

      val response: HttpResponse[String] = Http(s"${uri}${fileExt}").asString

      assert(response.code == 200)
      assert(response.contentType.fold(false) { _.startsWith(mimeTypeMapping.toMap.get(fileExt).get) })


      val format = fileExt match {

        case ".ttl" => Some(RDFFormat.TURTLE)
        case ".rdf" => Some(RDFFormat.RDFXML)
        case ".nt" => Some(RDFFormat.NTRIPLES)
        case _ => None
      }

      val rdfData : InputStream = new ByteArrayInputStream(response.body.getBytes)


      format.fold() {

        format => {

          try { Rio.parse(rdfData, "", format) }

          catch { case e : Exception => assert(false) }
        }
      }

      println(s"URI: ${uri}${fileExt}     Status: succesful")

    }

  }

}
