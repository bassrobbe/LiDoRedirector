package Redirector

import java.io.File

import org.scalatra._

class Redirector extends MmoonredirectorStack {

  private val documentRoot = "/media/robert/work/test/"

  private val contentTypeMapping = List(
    (".rdf", "application/rdf+xml"),
    (".owl", "application/rdf+xml"),
    (".owx", "application/owl+xml"),
    (".ttl", "text/turtle"),
    (".nt", "application/n-triples"),
    (".owm", "text/owl-manchester"),
    (".jsonld", "application/ld+json"),
    (".html", "text/html"))

  // redirect inventory requests to lodview
  get("/lang/:lang/inventory/:schema/:res/?$") {
    val lang = params("lang")
    val schema = params("schema")
    val res = params("res")

    val iri = repl(s"http://mmoon.org/lang/$lang/inventory/$schema/$res")
    val sparql = repl("http://fusionfactory.de:9988/blazegraph/namespace/mmoon/sparql/")

    redirect("http://lodview.it/lodview/?IRI=" + iri + "&sparql=" + sparql)
  }

  //serve complete core, schema and inventory files
  get("""^((/core|/[a-z]*/(schema|inventory)/[a-z]*)(.ttl|.html|.rdf|.owx|.omn|.ofn|.nt|/?))$""".r) {
    //try content negotiation
    if (multiParams("captures").apply(3).matches("/?")) {
      for (fileExt <- request.getHeader("Accept").split(",").map{x => getFileExtByConType(x)} if !fileExt.isEmpty) {
        val file = findFile(documentRoot + multiParams("captures").apply(1) + fileExt.get)
        if (!file.isEmpty) Ok(file.get, Map("Content-Type" -> getConTypeByFileExt(fileExt).get))
      }
      //NotFound("Sorry, the file could not be found")
    } else { //ignore "Accept" header if file extension is given via URI
      findFile(documentRoot + multiParams("captures").apply(0)) match {
        case Some(file) => Ok(file, Map("Content-Type" -> getConTypeByFileExt(multiParams("captures").lift(3)).getOrElse("")))
        case None => NotFound("Sorry, the file could not be found")
      }
    }
  }

  //sparql query redirects
  get("""^/sparql""") {
    redirect("http://fusionfactory.de:9988/blazegraph/namespace/mmoon/sparql?query=" + params("query"))
  }
  post("""^/sparql""") {
    redirect("http://fusionfactory.de:9988/blazegraph/namespace/mmoon/sparql?query=" + params("query"))
  }


  private def repl(str: String): String = {
    return Map(":" -> "%3A", "/" -> "%2F").foldLeft(str) { case (str: String, (x, y)) => str.replaceAll(x, y) }
  }

  private def findFile(path: String): Option[File] = {
    val file = new File(path)
    if (file.exists && !file.isDirectory) Some(file) else None
  }

  private def getConTypeByFileExt(fileExt: Option[String]): Option[String] = {
    contentTypeMapping.toMap.get(fileExt.getOrElse(""))
  }

  private def getFileExtByConType(contentType: String) : Option[String] = {
    contentTypeMapping.map(x => x.swap).toMap.get(contentType)
  }
}
