package org.mmoon.scalatra

import org.apache.commons.lang3.CharEncoding
import java.net.URLEncoder
import javax.activation.MimeType

import org.scalatra.{Conneg, NotFound, Ok}
import java.io.File

import org.mmoon.scalatra

class Redirector extends MmoonredirectorStack {

  private val documentRoot = "/media/robert/work/test/"



  // redirect inventory requests to lodview
  get("/:lang/inventory/:schema/:res/?") {
    val lang = params("lang")
    val schema = params("schema")
    val res = params("res")

    val iri = urlEncode(s"http://mmoon.org/$lang/inventory/$schema/$res")
    val sparql = urlEncode("http://mmoon.org/sparql/")

    redirect("http://lodview.it/lodview/?IRI=" + iri + "&sparql=" + sparql)
  }

  private def urlEncode(str: String): String = URLEncoder.encode(str, CharEncoding.UTF_8)

  //serve full core, schema and inventory files
  get("""^((/core|/[a-z]*/(schema|inventory)/[a-z]*)(.ttl|.html|.rdf|.owx|.omn|.ofn|.owl|.owm|.jsonld|.nt|/?))$""".r) {
    //try content negotiation if no type is given via URI
    if (multiParams("captures").apply(3).matches("/?"))
      serveFile(findFile(documentRoot + multiParams("captures").apply(1), acceptedMimeTypes.sortWith(_.q > _.q))) //better solution than apply?
    //ignore "Accept" header if file extension is given via URI
    else checkFileExistence(documentRoot + multiParams("captures").apply(0)) match {
      case Some(file) => serveFile(Some((file,getMimeType(multiParams("captures").apply(3)).get)))
      case None => serveFile(None)
    }
  }

  private def findFile(basePath: String, mimeTypes: List[Conneg[MimeType]]) : Option[(File, MimeType)] = {
    def findFileRec(mimeTypes: List[Conneg[MimeType]]) : Option[(File, MimeType)] =
      if (mimeTypes.length == 0) None
      else checkFileExistence(basePath + getFileExtension(mimeTypes(0).value).getOrElse("")) match {
          case Some(file) => Some(file, mimeTypes(0).value)
          case None => findFileRec(mimeTypes.tail)
        } //is there a more elegant way?
    findFileRec(mimeTypes)
  }

  private def checkFileExistence(path: String) : Option[File] = {
    val file = new File(path)
    if (file.exists && !file.isDirectory) Some(file) else None
  }

  private def getFileExtension(mimeType: MimeType) : Option[String] =
    mimeTypeMapping.map(_.swap).toMap.get(mimeType.toString)


  private def getMimeType(fileExtension: String): Option[MimeType] = mimeTypeMapping.toMap.get(fileExtension) match {
    case Some(mimeTypeStr) => Some(new MimeType(mimeTypeStr))
    case None => None
  }

    private def serveFile(tuple: Option[(File, MimeType)]) = tuple match {
    case Some((file, mimeType)) => Ok(file, Map("Content-Type" -> mimeType.toString))
    case None => NotFound("Sorry, the file could not be found")
  }

}
