package amf.javaparser.org.raml

import amf.client.parse.DefaultParserErrorHandler
import amf.core.model.document.{BaseUnit, DeclaresModel, EncodesModel}
import amf.core.model.domain.DomainElement
import amf.core.parser.SyamlParsedDocument
import amf.core.remote.Vendor
import amf.facades.{AMFCompiler, Validation}
import amf.plugins.document.webapi.parser.spec.common.emitters.DomainElementEmitter
import amf.plugins.domain.shapes.models.AnyShape
import amf.plugins.domain.webapi.models.WebApi
import amf.plugins.syntax.SYamlSyntaxPlugin
import org.yaml.model.YDocument

import scala.concurrent.Future

trait DomainElementEmitterTest extends ModelValidationTest {

  override def basePath: String       = path
  override def inputFileName: String  = "input.raml"
  override def outputFileName: String = "output.yaml"

  override def render(model: BaseUnit, d: String, vendor: Vendor): Future[String] = {
    val element = findDomainElement(model)
    Future { renderDomainElement(element, vendor) }
  }

  override def runDirectory(d: String): Future[(String, Boolean)] = {
    for {
      _ <- Validation(platform)
      model <- AMFCompiler(s"file://${d + inputFileName}", platform, hint, eh = DefaultParserErrorHandler.withRun())
        .build()
      output <- {
        val vendor = target(model)
        render(model, d, vendor)
      }
    } yield {
      val usePlatform = false
      (output, usePlatform)
    }
  }

  def renderDomainElement(shape: DomainElement, vendor: Vendor): String = {
    val node     = DomainElementEmitter.emit(shape, vendor)
    val document = SyamlParsedDocument(document = YDocument(node))
    SYamlSyntaxPlugin.unparse("application/yaml", document).getOrElse("").toString
  }

  def findDomainElement(unit: BaseUnit): DomainElement

}

class TypeEmitterTest extends DomainElementEmitterTest {
  override def path: String = "amf-client/shared/src/test/resources/org/raml/cycle/types/"

  def findDomainElement(model: BaseUnit): DomainElement = {
    model match {
      case d: DeclaresModel =>
        d.declares.collectFirst { case s: AnyShape if s.name.is("root") => s } match {
          case Some(anyShape: AnyShape) => anyShape
          case Some(other)              => throw new AssertionError("Wrong type declared $other")
          case None                     => throw new AssertionError("Model with empty declarations")
        }
      case other => throw new AssertionError("Invalid model type $other")
    }
  }
}

class ResponseEmitterTest extends DomainElementEmitterTest {
  override def path: String = "amf-client/shared/src/test/resources/org/raml/cycle/response/"

  def findDomainElement(model: BaseUnit): DomainElement = {
    model match {
      case e: EncodesModel =>
        e.encodes.asInstanceOf[WebApi].endPoints.head.operations.head.responses.head
      case other => throw new AssertionError("Invalid model")
    }
  }
}

class ExampleEmitterTest extends DomainElementEmitterTest {
  override def path: String = "amf-client/shared/src/test/resources/org/raml/cycle/example/"

  def findDomainElement(model: BaseUnit): DomainElement = {
    model match {
      case e: EncodesModel =>
        e.encodes.asInstanceOf[WebApi].endPoints.head.operations.head.responses.head
      case other => throw new AssertionError("Invalid model")
    }
  }
}
