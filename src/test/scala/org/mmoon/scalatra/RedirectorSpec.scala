package org.mmoon.scalatra

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class RedirectorSpec extends ScalatraSpec { def is =
  s2"""GET / on Redirector"
    "should return status 200"                  $root200 """

  addServlet(classOf[Redirector], "/*")

  def root200 = get("/") {
    status must_== 200
  }
}