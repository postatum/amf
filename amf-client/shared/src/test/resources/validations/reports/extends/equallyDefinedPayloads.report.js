Model: file://amf-client/shared/src/test/resources/validations/extends/merging-payloads/media-type-single-request/equallyDefined.raml
Profile: RAML 1.0
Conforms? false
Number of results: 2

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: should be number
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/extends/merging-payloads/media-type-single-request/equallyDefined.raml#/web-api/end-points/%2Fe-mediaType/get/request/application%2Fjson/any/schema/examples/example/default-example
  Property: 
  Position: Some(LexicalInformation([(19,17)-(19,18)]))
  Location: file://amf-client/shared/src/test/resources/validations/extends/merging-payloads/media-type-single-request/equallyDefined.raml

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: should be number
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/extends/merging-payloads/media-type-single-request/equallyDefined.raml#/web-api/end-points/%2Fe-no-mediaType/get/request/any/default/examples/example/default-example
  Property: 
  Position: Some(LexicalInformation([(24,15)-(24,16)]))
  Location: file://amf-client/shared/src/test/resources/validations/extends/merging-payloads/media-type-single-request/equallyDefined.raml
