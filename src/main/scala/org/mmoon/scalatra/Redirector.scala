package org.mmoon.scalatra

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.scalatra._
import javax.activation.MimeType
import better.files.File
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import org.scalatra.scalate.ScalateSupport
import scala.io.Source

class Redirector extends MmoonredirectorStack with LazyLogging with ScalateSupport {

  private lazy val externalConfig = ConfigFactory.load()

  private lazy val documentRoot = externalConfig.getString("redirector.documentRoot")

  private lazy val docRootFile = File(documentRoot)


  //redirect to http://mmoon.org/ if there's no matching route

  //get("""^.*$""".r) { redirect("http://mmoon.org/") }


  ////CORE
  //serve always full ontology

  get("""^/core(/[a-zA-Z0-9äöüÄÖÜß_]+)?/?$""".r) { redirectStaticResource("core", "") }

  get("""^/core(/[a-zA-Z0-9äöüÄÖÜß_]+)?(\.[a-z]+)$""".r)
    { println("test");serveFile("core", multiParams("captures").apply(1), "") }


  ////SCHEMA
  //serve always full schema file

  get("""^/([a-z]+/schema/[a-z]+)(/[a-zA-Z0-9äöüÄÖÜß_]+)?/?$""".r)
    { redirectStaticResource(multiParams("captures").apply(0), "schema") }

  get("""^/([a-z]+/schema/[a-z]+)(/[a-zA-Z0-9äöüÄÖÜß_]+)?(\.[a-z]+)$""".r)
    { serveFile(multiParams("captures").apply(0), multiParams("captures").apply(2), "schema") }


  ////INVENTORY
  //serve full dataset

  get("""^/([a-z]+/inventory/[a-z]+)/?$""".r)
    { redirectStaticResource(multiParams("captures").apply(0), "dataset") }

  get("""^/([a-z]+/inventory/[a-z]+)(\.[a-z]+)$""".r)
    { serveFile(multiParams("captures").apply(0), multiParams("captures").apply(1), "dataset") }

  //serve just one resource
  get("""^/(([a-z]+/inventory/[a-z]+/)[a-zA-Z0-9äöüÄÖÜß_]+)/?$""".r)
    { redirectInventoryResource(multiParams("captures").apply(0), multiParams("captures").apply(1)) }

  get("""^/(([a-z]+/inventory/[a-z]+/)[a-zA-Z0-9äöüÄÖÜß_]+)(\.[a-z]+)$""".r)
    { serveInventoryResource(multiParams("captures").apply(0), multiParams("captures").apply(1), multiParams("captures").apply(2)) }


  //necessary to serve .css and .js files for lodview interface
  get("""^/(lodview/[a-zA-Z/\.-_]+)$""".r)
    { Ok(Source.fromURL("http://127.0.0.1:8080"/multiParams("captures").apply(0)).mkString) }

  post("""^/(lodview/[a-zA-Z/]+)$""".r)
    { Ok(Source.fromURL("http://127.0.0.1:8080/"/multiParams("captures").apply(0)).mkString) }


  private def redirectStaticResource(resourcePath : String, resourceType : String): ActionResult = {

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

        val targetUri = "http://mmoon.org"/s"${resourcePath}${getFileExtension(mimeType).get}"

        SeeOther(targetUri, Map("Content-Type" -> mimeType.toString))
      }

      case None => {

        val testFile = docRootFile / s"${resourcePath}.html"

        if (testFile.isRegularFile) unsupportedMediaType415("http://mmoon.org" / resourcePath)

        else notFound404("http://mmoon.org" / resourcePath, None, resourceType)
      }
    }
  }

  private def serveFile(resourcePath : String, fileExt : String, resourceType : String): ActionResult = {

    val file = docRootFile / s"${resourcePath}${fileExt}"
    println(file.getClass)

    if (file.isRegularFile) Ok(file.toJava, Map("Content-Type" -> getMimeType(fileExt).getOrElse("").toString))

    else {
      val testFile = docRootFile / s"${resourcePath}.html"

      if (testFile.isRegularFile) unsupportedMediaType415("http://mmoon.org" / resourcePath)

      else notFound404("http://mmoon.org" / resourcePath, None, resourceType)
    }
  }

  private def redirectInventoryResource(resourcePath : String, datasetPath : String): ActionResult = {

    val supportedMimeTypes = List("application/rdf+xml", "text/html", "text/turtle", "application/n-triples")

    val x = """[a-z]+/[a-z+-]+""".r
    val y = """[a-z]+/\*""".r
    val z = """\*/\*""".r

    def redirect(t: MimeType) = {

      val targetUri = s"http://mmoon.org/${resourcePath}${getFileExtension(t).getOrElse("")}"
      //NotFound("zsilutvnei")
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
      "http://mmoon.org" / resourcePath, Some(datasetPath), "resource")

    else searchForSupportedMimeType(acceptedMimeTypes.sortWith(_.q > _.q).map(_.value))
  }

  private def serveInventoryResource(resourcePath : String, datasetPath : String, fileExt : String): ActionResult = {

    val testUri = "http://127.0.0.1:8080/lodview" / resourcePath ? ("output" -> "application/n-triples")

    if (Source.fromURL(testUri).mkString.length == 0) notFound404(
      "http://mmoon.org" / resourcePath, Some(datasetPath), "resource")

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

        case _ => unsupportedMediaType415("http://mmoon.org" / resourcePath)
      }
    }
  }

  private def notFound404(resourceUri : Uri, datasetPath : Option[String], resourceType : String): ActionResult = {

    //datasetPath.fold (None) {...} is not working properly. Why?

    val datasetOption = datasetPath.fold {val tmp : Option[Uri] = None; tmp} { path =>
      Source.fromURL("http://127.0.0.1:8080/lodview" / path ? ("output" -> "application/n-triples"))
        .mkString.length == 0 match {

        case false => Some("http://mmoon.org" / path)
        case true => None
      }
    }

    NotFound(ssp("NotFound", "resourceType" -> resourceType, "resourceUri" -> resourceUri,
      "datasetUri" -> datasetOption), Map("Content-Type" -> "text/html"))

  }

  private def unsupportedMediaType415(resourceUri : Uri): ActionResult =

    UnsupportedMediaType(ssp("UnsupportedMediaType", "resourceUri" -> resourceUri), Map("Content-Type" -> "text/html"))

  private def getFileExtension(mimeType: MimeType) : Option[String] =

    mimeTypeMapping.map(_.swap).toMap.get(mimeType.toString)

  private def getMimeType(fileExt: String): Option[MimeType] = mimeTypeMapping.toMap.get(fileExt) match {

    case Some(mimeTypeStr) => Some(new MimeType(mimeTypeStr))

    case None => None
  }
}
