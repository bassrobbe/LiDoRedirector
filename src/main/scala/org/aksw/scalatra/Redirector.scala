package org.aksw.scalatra

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.scalatra._
import javax.activation.MimeType

import better.files.File
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import org.scalatra.scalate.ScalateSupport

import scala.io.Source
import scalaj.http.Http

class Redirector extends LidoredirectorStack with LazyLogging with ScalateSupport {

  private lazy val externalConfig = ConfigFactory.load()

  private lazy val documentRoot = externalConfig.getString("redirector.documentRoot")

  private lazy val docRootFile = File(documentRoot)


  //ONTOLOGY
  //serve only static ontology files
  get("""^/ontology(/[^/]+)?/?$""".r) {
    redirectStaticResource("ontology", Option(multiParams("captures").apply(0)));
  }

  get("""^/ontology(/[^/]+)?(\.[a-z]+)$""".r) {
    serveFile("ontology", Option(multiParams("captures").apply(0)), multiParams("captures").apply(1))
  }


  //DATA
  //serve representations of term and concept resources, use LodView
  get("""^/(resource/(Term|Concept)/-?[0-9]+)/?$""".r) {
    redirectDataResource(multiParams("captures").apply(0))
  }

  get("""^/(resource/(Term|Concept)/-?[0-9]+)(\.[a-z]+)$""".r) {
    serveDataResource(multiParams("captures").apply(0), multiParams("captures").apply(2))
  }


  //SPARQL
  //redirect sparql queries to BlazeGraph and website requests to Puma
  get("/sparql") { handleSparqlPath(params.get("query")) }

  post("/sparql") { handleSparqlPath(params.get("query")) }


  //BLAZEGRAPH
  get("/blazegraph/?") {
    contentFromUri("http://127.0.0.1:3000/sparql", Map("Content-Type" -> "text/html"))
  }

  //GRAPHICAL USER INTERFACE
  //redirect graphical user interface to Puma
  get("""/(glossary/.*)""".r) {

    val targetUri = "http://127.0.0.1:3000" / multiParams("captures").apply(0)
    contentFromUri(targetUri, Map("Content-Type" -> "text/html"))
  }

  get("/glossary/?") { contentFromUri("http://127.0.0.1:3000/glossary/glossary/", Map("Content-Type" -> "text/html")) }

  get("/?") { contentFromUri("http://127.0.0.1:3000/glossary/", Map("Content-Type" -> "text/html")) }

  get("""/(assets/.*)""".r) {
    val targetUri = "http://127.0.0.1:3000" / multiParams("captures").apply(0)
    contentFromUri(targetUri, Map("Content-Type" -> "text/html"))
  }


  //necessary to serve .css and .js files for LodView interface
//  get("""^/(lodview/[a-zA-Z/\.-_]+)$""".r) {
//    Ok(Source.fromURL("http://127.0.0.1:8080" / multiParams("captures").apply(0)).mkString)
//  }
//
//  post("""^/(lodview/[a-zA-Z/]+)$""".r) {
//    Ok(Source.fromURL("http://127.0.0.1:8080/" / multiParams("captures").apply(0)).mkString)
//  }


  private def redirectStaticResource(resourcePath : String, resourceName : Option[String]): ActionResult = {

    def checkResourceExistence(resourcePath : String, mimeTypes : List[MimeType]) : Option[MimeType] = {

      def checkFile(t: MimeType) : Boolean = {

        getFileExtension(t).fold(false) { ext => (docRootFile / s"${resourcePath}${ext}").isRegularFile }
      }

      //Don't recompile the same regular expression on each case evaluation, but rather have it on an object.
      val x = """[a-z]+/[a-z+-]+""".r
      val y = """[a-z]+/\*""".r
      val z = """\*/\*""".r

      //Java's MimeType.match method has some strange behaviour concerning */*. So a manual case differentiation is necessary.

      //TODO: use recursion instead of for loop
      for (t <- mimeTypes) t.toString match {

        case x() => if(checkFile(t)) return Some(t)

        case y() => for (s <- mimeTypeMapping.map(k => new MimeType(k._2)) if t.`match`(s)) if (checkFile(s)) return Some(s)

        case z() => if (checkFile(new MimeType("text/html"))) return Some(new MimeType("text/html"))

        case _ =>
      }

      None
    }

    val foundResource = checkResourceExistence(resourcePath, acceptedMimeTypes.sortWith(_.q > _.q).map(_.value))
    foundResource match {

      case Some(mimeType) => {

        val targetUri = "http://lidordf.aksw.org" / s"${resourcePath}${resourceName.getOrElse("")}${getFileExtension(mimeType).get}"

        SeeOther(targetUri, Map("Content-Type" -> mimeType.toString))
      }

      case None => unsupportedMediaType415("http://lidordf.aksw.org" / resourcePath)
    }
  }

  private def serveFile(resourcePath : String, resourceName : Option[String], fileExt : String): ActionResult = {

    val file = docRootFile / s"${resourcePath}${fileExt}"

    if (file.isRegularFile) {

      if (fileExt.equals(".html") && resourceName.isDefined) {

        val targetUri: Uri = "http://lidordf.aksw.org" / s"${resourcePath}.html"

        Found(resourceName.fold(targetUri) { name => targetUri `#` name.substring(1) }, Map("Content-Type" -> "text/html"))

      } else Ok(file.toJava, Map("Content-Type" -> getMimeType(fileExt).getOrElse("").toString))

    } else unsupportedMediaType415("http://lidordf.aksw.org" / resourcePath)
  }

  private def redirectDataResource(resourcePath : String): ActionResult = {

    val supportedMimeTypes = List("application/rdf+xml", "text/html", "text/turtle", "application/n-triples")

    val x = """[a-z]+/[a-z+-]+""".r
    val y = """[a-z]+/\*""".r
    val z = """\*/\*""".r

    def redirect(t: MimeType) = {

      val targetUri = s"http://mmoon.org/${resourcePath}${getFileExtension(t).getOrElse("")}"

      SeeOther(targetUri, Map("Content-Type" -> t.toString))

    }

    def searchForSupportedMimeType(acceptedMimeTypes : List[MimeType]): ActionResult = {

      def handleTypeY(currentMimeType : MimeType, list : List[MimeType]): ActionResult = {

        if (list.isEmpty) searchForSupportedMimeType(list.tail)

        else {

          if (list.head.`match`(currentMimeType)) redirect(list.head)
          else handleTypeY(currentMimeType, list.tail)
        }

      }

      if (acceptedMimeTypes.isEmpty) unsupportedMediaType415("http://mmoon.org" / resourcePath)

      else {
        val currentMimeType = acceptedMimeTypes.head

        currentMimeType.toString match {
          case x() => if(supportedMimeTypes.contains(currentMimeType.toString)) redirect(currentMimeType)
            else searchForSupportedMimeType(acceptedMimeTypes.tail)

          case y() => handleTypeY(currentMimeType, supportedMimeTypes.map(new MimeType(_)))

          case z() => redirect(new MimeType("text/html"))

          case _ => searchForSupportedMimeType(acceptedMimeTypes.tail)
        }
      }
    }

    val testUri = "http://127.0.0.1:8080/lodview" / resourcePath ? ("output" -> "application/n-triples")

    if (Source.fromURL(testUri).mkString.length == 0) notFound404(
      "http://lidordf.aksw.org" / resourcePath,"resource")

    else searchForSupportedMimeType(acceptedMimeTypes.sortWith(_.q > _.q).map(_.value))
  }

  private def serveDataResource(resourcePath : String, fileExt : String): ActionResult = {

    val testUri = "http://127.0.0.1:8080/lodview" / resourcePath ? ("output" -> "application/n-triples")

    if (Source.fromURL(testUri).mkString.length == 0) notFound404(
      "http://lidordf.aksw.org" / resourcePath, "resource")

    else {

      //It seems, there is no ProxyPass functionality included in Scalatra. So a little workaround is necessary.
      fileExt match {

        case ".html" => {

          val targetUri = "http://127.0.0.1:8080/lodview" / resourcePath

          Ok(Source.fromURL(targetUri).mkString, Map("Content-Type" -> "text/html"))
        }

        case ".rdf" | ".nt" | ".ttl" => {

          val t = getMimeType(fileExt).getOrElse(new MimeType)

          val targetUri = "http://127.0.0.1:8080/lodview" / resourcePath ? ("output" -> t.getBaseType)

          Ok(Source.fromURL(targetUri).mkString, Map("Content-Type" -> t.toString))
        }

        case _ => unsupportedMediaType415("http://lidordf.aksw.org" / resourcePath)
      }
    }
  }

  private def handleSparqlPath(query : Option[String]) : ActionResult = {

    query.fold ( contentFromUri("http://127.0.0.1:3000/sparql", Map("Content-Type" -> "text/html")) ) { query =>
      contentFromUri("http://lidordf.aksw.org/sparql", Map("Content-Type" -> "application/sparql-results+json"), Map("query" -> query))
    }
    //"http://127.0.0.1:9999/blazegraph/namespace/lido/sparql" ? (query" -> params("query"))
  }

  private def notFound404(uri : Uri, resourceType : String): ActionResult = {

    NotFound(ssp("NotFound", "resourceType" -> resourceType, "resourceUri" -> uri), Map("Content-Type" -> "text/html"))
  }

  private def unsupportedMediaType415(resourceUri : Uri): ActionResult =

    UnsupportedMediaType(ssp("UnsupportedMediaType", "resourceUri" -> resourceUri), Map("Content-Type" -> "text/html"))

  private def contentFromUri (uri : Uri, httpHeaders : Map[String, String] = Map.empty,
                              parameter : Map[String, String] = Map.empty) : ActionResult = {

    val con = Http(uri).params(parameter).headers(httpHeaders).asString.body
    Ok(con, httpHeaders)
  }


  private def getFileExtension(mimeType: MimeType) : Option[String] =

    mimeTypeMapping.map(_.swap).toMap.get(mimeType.toString)

  private def getMimeType(fileExt: String): Option[MimeType] = mimeTypeMapping.toMap.get(fileExt) match {

    case Some(mimeTypeStr) => Some(new MimeType(mimeTypeStr))

    case None => None
  }
}