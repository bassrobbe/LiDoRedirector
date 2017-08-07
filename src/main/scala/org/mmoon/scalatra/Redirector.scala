package org.mmoon.scalatra

import org.apache.commons.lang3.CharEncoding

import java.net.URLEncoder

class Redirector extends MmoonredirectorStack {

  // redirect inventory requests to lodview
  get("/:lang/inventory/:schema/:res/?") {

    val lang = params("lang")
    val schema = params("schema")
    val res = params("res")

    val iri = urlEncode(s"http://mmoon.org/$lang/inventory/$schema/$res")
    val sparql = urlEncode("http://mmoon.org/sparql/")

    redirect("http://lodview.it/lodview/?IRI=" + iri + "&sparql=" + sparql)
  }

  def urlEncode(str: String): String = URLEncoder.encode(str, CharEncoding.UTF_8)
}
