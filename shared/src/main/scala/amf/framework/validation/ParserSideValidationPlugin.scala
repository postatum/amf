package amf.framework.validation

import amf.document.BaseUnit
import amf.framework.domain.LexicalInformation
import amf.framework.plugins.AMFPlugin
import amf.framework.services.RuntimeValidator
import amf.framework.validation.core.{ValidationReport, ValidationResult}
import amf.validation.model.DefaultAMFValidations

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ParserSideValidationPlugin extends AMFPlugin with RuntimeValidator with ValidationResultProcessor {
  override val ID: String = "Parser side AMF Validation"

  override def dependencies(): Seq[AMFPlugin] = Seq()

  var aggregatedReport: List[AMFValidationResult] = List()

  // The aggregated report
  def reset(): Unit = {
    aggregatedReport = List()
  }

  // disable temporarily the reporting of validations
  var enabled: Boolean = true

  def withEnabledValidation(enabled: Boolean): RuntimeValidator = {
    this.enabled = enabled
    this
  }

  def disableValidations[T]()(f: () => T): T = {
    if (enabled) {
      enabled = false
      try {
        f()
      } finally {
        enabled = true
      }
    } else {
      f()
    }
  }



  /**
    * Loads a validation profile from a URL
    */
  override def loadValidationProfile(validationProfilePath: String): Future[Unit] = Future {}

  /**
    * Low level validation returning a SHACL validation report
    */
  override def shaclValidation(model: BaseUnit, validations: EffectiveValidations, messageStyle: String): Future[ValidationReport] = Future {
    new ValidationReport {
      override def conforms: Boolean = false
      override def results: List[ValidationResult] = Nil
    }
  }

  /**
    * Main validation function returning an AMF validation report linking validation errors
    * for validations in the profile to domain elements in the model
    */
  override def validate(model: BaseUnit, profileName: String, messageStyle: String): Future[AMFValidationReport] = {
    val validations = EffectiveValidations().someEffective(DefaultAMFValidations.parserSideValidationsProfile)
    // aggregating parser-side validations
    var results = aggregatedReport.map(r => processAggregatedResult(r, messageStyle, validations))

    Future {
      AMFValidationReport(
        conforms = !results.exists(_.level == SeverityLevels.VIOLATION),
        model = model.id,
        profile = profileName,
        results = results
      )
    }
  }

  /**
    * Client code can use this function to register a new validation failure
    */
  override def reportConstraintFailure(level: String,
                              validationId: String,
                              targetNode: String,
                              targetProperty: Option[String] = None,
                              message: String = "",
                              position: Option[LexicalInformation] = None): Unit = {
    val validationError = AMFValidationResult(message, level, targetNode, targetProperty, validationId, position)
    if (enabled) {
      aggregatedReport ++= Seq(validationError)
    } else {
      throw new Exception(validationError.toString)
    }
  }

}

object ParserSideValidationPlugin {
  def apply() = new ParserSideValidationPlugin
}
