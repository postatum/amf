package amf.plugins.domain.webapi.annotations

import amf.core.model.domain.{AmfElement, Annotation, AnnotationGraphLoader, SerializableAnnotation}

case class InvalidBinding(value: String) extends SerializableAnnotation {
  override val name: String = "invalid-binding"
}

object InvalidBinding extends AnnotationGraphLoader {
  override def unparse(value: String, objects: Map[String, AmfElement]): Option[Annotation] =
    Some(InvalidBinding(value))
}
