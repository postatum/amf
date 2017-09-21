package amf.validation.core

import scala.concurrent.Future

/**
  * Created by antoniogarrote on 17/07/2017.
  */


/**
  * Interface for SHACL Validators in the different platforms
  */
trait SHACLValidator {

  /**
    * Validates a data graph against a shapes graph. Graphs are expressed as Strings in a particular format, identified
    * by the media types parameters
    * Returns the raw JSON-LD encoded SHACL validation report
    * @param data string representation of the data graph
    * @param dataMediaType media type for the data graph
    * @param shapes string representation of the shapes graph
    * @param shapesMediaType media type fo the shapes graph
    * @return JSON-LD encoded SHACL validation report
    */
  def validate(data: String, dataMediaType: String, shapes: String, shapesMediaType: String): Future[String]

  /**
    * Validates a data graph against a shapes graph. Graphs are expressed as Strings in a particular format, identified
    * Returns ValidationReport object
    * @param data string representation of the data graph
    * @param dataMediaType media type for the data graph
    * @param shapes string representation of the shapes graph
    * @param shapesMediaType media type fo the shapes graph
    * @return ValidationReport
    */
  def report(data: String, dataMediaType: String, shapes: String, shapesMediaType: String): Future[ValidationReport]

}
