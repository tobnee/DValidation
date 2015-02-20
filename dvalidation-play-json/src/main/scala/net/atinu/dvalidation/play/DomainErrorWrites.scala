package net.atinu.dvalidation.play

import net.atinu.dvalidation.{ DomainErrors, DomainError }
import play.api.libs.json.{ Json, JsArray, JsValue, Writes }

object DomainErrorWrites extends DomainErrorWrites {

  def defaultDomainErrorWrites = customDomainErrorWrites(new JsonWriter())

  def defaultDomainErrorsWrites(implicit w: Writes[DomainError]) = new Writes[DomainErrors] {
    def writes(errors: DomainErrors): JsValue = {
      val array = JsArray(errors.asList.map(error => Json.toJson(error)))
      Json.obj("errors" -> array)
    }
  }

  def customDomainErrorWrites(jw: JsonWriter) = new Writes[DomainError] {
    def writes(v: DomainError): JsValue = jw.renderSingle(v)
  }

}

trait DomainErrorWrites {
  implicit val deWrites = DomainErrorWrites.defaultDomainErrorWrites
  implicit val dexWrites = DomainErrorWrites.defaultDomainErrorsWrites
}
