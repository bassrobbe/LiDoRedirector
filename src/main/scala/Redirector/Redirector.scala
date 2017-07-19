package Redirector

import org.scalatra._

class Redirector extends MmoonredirectorStack {

  private def repl(str: String) : String = {
    return Map(":" -> "%3A", "/" -> "%2F").foldLeft(str){case (str:String, (x,y)) => str.replaceAll(x, y)}
  }

  get("/lang/:lang/inventory/:schema/:exp") {
    var lang = params("lang")
    var schema = params("schema")
    var exp = params("exp")

    var iri = repl(s"http://mmoon.org/lang/$lang/inventory/$schema/$exp")
    var sparql = repl("http://fusionfactory.de:9988/blazegraph/namespace/mmoon/sparql/")

    redirect("http://lodview.it/lodview/?IRI=" + iri + "&sparql=" + sparql)
  }

}
