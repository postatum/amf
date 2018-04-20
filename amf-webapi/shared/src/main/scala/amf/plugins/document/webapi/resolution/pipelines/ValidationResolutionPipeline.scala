package amf.plugins.document.webapi.resolution.pipelines

import amf.core.annotations.LexicalInformation
import amf.core.benchmark.ExecutionLog
import amf.core.metamodel.domain.ShapeModel
import amf.core.model.document.BaseUnit
import amf.core.model.domain.Shape
import amf.core.parser.ErrorHandler
import amf.core.resolution.pipelines.ResolutionPipeline
import amf.core.resolution.stages.ReferenceResolutionStage
import amf.plugins.document.webapi.resolution.stages.ExtensionsResolutionStage
import amf.plugins.domain.shapes.resolution.stages.ShapeNormalizationStage
import amf.plugins.domain.shapes.resolution.stages.shape_normalization.InheritanceIncompatibleShapeError
import amf.plugins.features.validation.ParserSideValidations

class ValidationShapeNormalisationStage(profile: String, override val keepEditingInfo: Boolean, errorHandler: ErrorHandler) extends ShapeNormalizationStage(profile, keepEditingInfo, errorHandler) {

  override protected def minShape(baseShapeOrig: Shape, superShape: Shape): Shape = {
    try {
      super.minShape(baseShapeOrig, superShape)
    } catch {
      case e: InheritanceIncompatibleShapeError =>
        errorHandler.violation(
          ParserSideValidations.InvalidTypeInheritanceErrorSpecification.id(),
          baseShapeOrig.id,
          Some(ShapeModel.Inherits.value.iri()),
          e.getMessage,
          baseShapeOrig.annotations.find(classOf[LexicalInformation])
        )
        baseShapeOrig
      case other: Exception => throw other
    }
  }
}

class ValidationResolutionPipeline(profile: String) extends ResolutionPipeline {

  val references = new ReferenceResolutionStage(profile, keepEditingInfo = false)
  val extensions = new ExtensionsResolutionStage(profile, keepEditingInfo = false)

  override def resolve[T <: BaseUnit](model: T): T = {

    val shapes  = new ValidationShapeNormalisationStage(profile, keepEditingInfo = false, errorHandlerForModel(model))

    ExecutionLog.log(s"ValidationResolutionPipeline#resolve: resolving ${model.location}")
    withModel(model) { () =>
      step(references)
      step(extensions)
      step(shapes)
      ExecutionLog.log(s"ValidationResolutionPipeline#resolve: resolution finished ${model.location}")
    }
  }

}
