package org.mmoon.scalatra

import org.scalatest._
import scalaj.http._

/**
  * Created by bassrobbe on 10/27/17.
  */
class RedirectorTest extends FunSuite {

  val mimeTypes = List(/*"application/rdf+xml",*/ "text/turtle", "application/n-triples", "text/html")

  val fileExtenisons = List(".rdf", ".ttl", ".nt", ".html")

  val testUris = List(
    "http://mmoon.org/core",
    "http://mmoon.org/core/AbsoluteAdjective",
    "http://mmoon.org/deu/schema/og",
    "http://mmoon.org/deu/schema/og/Affix",
    "http://mmoon.org/deu/inventory/og",
    "http://mmoon.org/deu/inventory/og/DerivedWord_verkaufen"
  )


  test("URIs without file extension -> Content Negotiation") {

    for (mimeType <- mimeTypes; uri <- "http://mmoon.org/core/" :: testUris) {

      val response: HttpResponse[String] = Http(uri).header("Accept", mimeType).asString

      assert(response.code == 303)
      assert(response.contentType.fold(false) { _.startsWith(mimeType) })

      println(s"URI: ${uri}     Accept: ${mimeType}     Status: succesful")
    }

  }

  test("URIs with file extension -> MimeType is given via URI") {

    for (fileExt <- fileExtenisons; uri <- testUris) {

      val response: HttpResponse[String] = Http(s"${uri}${fileExt}").asString

      assert(response.code == 200)
      assert(response.contentType.fold(false) { _.startsWith(mimeTypeMapping.toMap.get(fileExt).get) })

      println(s"URI: ${uri}${fileExt}     Status: succesful")
    }

  }

}
