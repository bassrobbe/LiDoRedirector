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
//  get("""^/ontology(/[a-zA-Z]+)?/?$""".r) {
//    redirectStaticResource("ontology", Option(multiParams("captures").apply(0)));
//  }
//
//  get("""^/ontology(/[a-zA-Z]+)?(\.[a-z]+)$""".r) {
//    val format = multiParams("capture").apply(1)
//    val targetFile = docRootFile / "ontology" / s"lido${format}"
//    if (mimeTypeMapping.map(_._1).contains(format)) {
//      Ok(targetFile.toJava, Map("Content-Type" -> getMimeType(format).get.toString))
//    } else {
//      val res = multiParams("capture").apply(0)
//      val targetUri = s"http://lidordf.aksw.org/ontology${res}${format}"
//      unsupportedMediaType415(targetUri)
//    }
//    //serveFile("ontology", Option(multiParams("captures").apply(0)), multiParams("captures").apply(1))
//  }

  get("""^/ontology(.*)$""") {
    val targetFile = docRootFile / "ontology/lido.ttl"
    Ok(targetFile.toJava, Map("Content-Type" -> "text/turtle"))
  }

  get("""^/downloads/metadata/lido_dataid(.ttl)?$""".r) {
    val targetFile = docRootFile / "downloads/metadata/lido_dataid.ttl"
    Ok(targetFile.toJava, Map("Content-Type" -> "text/turtle"))
  }

  get("""^/downloads/lido_dataset/(LiDoRDF_v[0-9]+_20[0-9]{2}\-[0-9]{2})(.ttl)?$""".r) {
    val fileName = multiParams("captures").apply(0)
    val targetFile = docRootFile / "downloads/lido_dataset" / s"${fileName}.ttl"
    Ok(targetFile.toJava, Map("Content-Type" -> "text/turtle"))
  }

  //serve representations of term and concept resources, use LodView
  get("""^/resource/([A-Za-z0-9_\-\.]+)/?$""".r) {
    redirectDataResource("resource", multiParams("captures").apply(0))
  }

  get("""^/(resource/[A-Za-z0-9_\-\.]+)(\.[a-z]+)$""".r) {
    if (multiParams("captures").apply(1).equals(".html")) {
      val targetUri = "http://127.0.0.1:3000" / multiParams("captures").apply(0)
      contentFromUri(targetUri, Map("Content-Type" -> "text/html"))
    } else {
      serveDataResource(multiParams("captures").apply(0), multiParams("captures").apply(2))
    }
  }

  get("/resource/?") {
    val targetUri = "http://127.0.0.1:3000/resource"
     contentFromUri(targetUri, Map("Content-Type" -> "text/html"))
  }


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

  private def redirectDataResource(resourceType : String, resourcePath : String): ActionResult = {

    val supportedMimeTypes = List("application/rdf+xml", "text/html", "text/turtle", "application/n-triples")

    val x = """[a-z]+/[a-z+-]+""".r
    val y = """[a-z]+/\*""".r
    val z = """\*/\*""".r

    def redirect(t: MimeType) = {

      val targetUri = s"http://lidordf.aksw.org/${resourceType}/${resourcePath}${getFileExtension(t).getOrElse("")}"

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

      if (acceptedMimeTypes.isEmpty) unsupportedMediaType415("http://lidordf.aksw.org" / resourceType / resourcePath)

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

    val testUri = "http://127.0.0.1:8081/lodview/resource" / resourcePath ? ("output" -> "application/n-triples")

    if (Source.fromURL(testUri).mkString.length == 0) notFound404(
      "http://lidordf.aksw.org" / resourceType / resourcePath, "resource")

    else searchForSupportedMimeType(acceptedMimeTypes.sortWith(_.q > _.q).map(_.value))
  }

  private def serveDataResource(resourcePath : String, fileExt : String): ActionResult = {

    val testUri = "http://127.0.0.1:8081/lodview" / resourcePath ? ("output" -> "application/n-triples")

    if (Source.fromURL(testUri).mkString.length == 0) notFound404(
      "http://lidordf.aksw.org" / resourcePath, "resource")

    else {

      //It seems, there is no ProxyPass functionality included in Scalatra. So a little workaround is necessary.
      fileExt match {

        case ".html" => {

          val targetUri = "http://127.0.0.1:8081/lodview" / resourcePath
          Ok(Source.fromURL(targetUri).mkString, Map("Content-Type" -> "text/html"))
        }

        case ".rdf" | ".nt" | ".ttl" => {

          val t = getMimeType(fileExt).getOrElse(new MimeType)

          val targetUri = "http://127.0.0.1:8081/lodview" / resourcePath ? ("output" -> t.getBaseType)

          Ok(Source.fromURL(targetUri).mkString, Map("Content-Type" -> t.toString))
        }

        case _ => unsupportedMediaType415("http://lidordf.aksw.org" / resourcePath)
      }
    }
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



