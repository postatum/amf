package amf.domain

import amf.framework.parser.Annotations
import amf.metadata.domain.PayloadModel
import amf.metadata.domain.PayloadModel._
import amf.shape.{ArrayShape, NodeShape, ScalarShape, Shape}
import org.yaml.model.YMap

/**
  * Payload internal model.
  */
case class Payload(fields: Fields, annotations: Annotations) extends DomainElement with Linkable {

  def mediaType: String = fields(MediaType)
  def schema: Shape     = fields(Schema)

  def withMediaType(mediaType: String): this.type = set(MediaType, mediaType)
  def withSchema(schema: Shape): this.type        = set(Schema, schema)

  override def adopted(parent: String): this.type = {
    val mediaType: Option[String] = fields.?(MediaType)
    withId(parent + "/" + mediaType.getOrElse("default"))
  }

  def withObjectSchema(name: String): NodeShape = {
    val node = NodeShape().withName(name)
    set(PayloadModel.Schema, node)
    node
  }

  def withScalarSchema(name: String): ScalarShape = {
    val scalar = ScalarShape().withName(name)
    set(PayloadModel.Schema, scalar)
    scalar
  }

  def withArraySchema(name: String): ArrayShape = {
    val array = ArrayShape().withName(name)
    set(PayloadModel.Schema, array)
    array
  }

  override def linkCopy(): Payload = Payload().withId(id)

  def clonePayload(parent: String): Payload = {
    val cloned = Payload(annotations).withMediaType(mediaType).adopted(parent)

    this.fields.foreach {
      case (f, v) =>
        val clonedValue = v.value match {
          case s: Shape => s.cloneShape()
          case o        => o
        }

        cloned.set(f, clonedValue, v.annotations)
    }

    cloned.asInstanceOf[this.type]
  }
}

object Payload {
  def apply(): Payload = apply(Annotations())

  def apply(ast: YMap): Payload = apply(Annotations(ast))

  def apply(annotations: Annotations): Payload = new Payload(Fields(), annotations)
}
