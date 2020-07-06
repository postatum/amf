package amf.plugins.document.webapi.parser.spec.common.emitters

import amf.core.emitter.{PartEmitter, SpecOrdering}
import amf.core.errorhandling.UnhandledErrorHandler
import amf.core.remote.Vendor
import amf.plugins.document.webapi.contexts.emitter.raml.Raml10SpecEmitterContext
import amf.plugins.document.webapi.parser.spec.declaration.emitters.raml.Raml10TypePartEmitter
import amf.plugins.document.webapi.parser.spec.domain.Raml10ResponsePartEmitter
import amf.plugins.domain.shapes.models.{AnyShape, Example}
import amf.plugins.domain.webapi.models.{EndPoint, Response}

trait DomainElementEmitterFactory {
  def typeEmitter(s: AnyShape): Option[PartEmitter]
  def responseEmitter(e: Response): Option[PartEmitter]
}
object DomainElementEmitterFactory {
  def apply(vendor: Vendor): DomainElementEmitterFactory = vendor match {
    case Vendor.RAML10 => Raml10EmitterFactory
    case _             => ???
  }
}

object Raml10EmitterFactory extends DomainElementEmitterFactory {
  implicit val ctx: Raml10SpecEmitterContext = new Raml10SpecEmitterContext(UnhandledErrorHandler)

  override def typeEmitter(s: AnyShape): Option[PartEmitter] =
    Some(Raml10TypePartEmitter(s, SpecOrdering.Lexical, None, references = Nil))

  override def responseEmitter(e: Response): Option[PartEmitter] =
    Some(Raml10ResponsePartEmitter(e, SpecOrdering.Lexical, Nil))
}
