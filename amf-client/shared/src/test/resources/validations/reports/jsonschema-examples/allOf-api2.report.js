Model: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml
Profile: RAML 1.0
Conforms? false
Number of results: 4

Level: Violation

- Source: http://a.ml/vocabularies/amf/validation#example-validation-error
  Message: should have required property 'bar'
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep2/get/200/application%2Fjson/schema/example/default-example
  Property: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep2/get/200/application%2Fjson/schema/example/default-example
  Position: Some(LexicalInformation([(45,0)-(49,0)]))
  Location: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml

- Source: http://a.ml/vocabularies/amf/validation#example-validation-error
  Message: should have required property 'foo'
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep3/get/200/application%2Fjson/schema/example/default-example
  Property: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep3/get/200/application%2Fjson/schema/example/default-example
  Position: Some(LexicalInformation([(57,0)-(61,0)]))
  Location: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml

- Source: http://a.ml/vocabularies/amf/validation#example-validation-error
  Message: should have required property 'baz'
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep4/get/200/application%2Fjson/schema/example/default-example
  Property: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep4/get/200/application%2Fjson/schema/example/default-example
  Position: Some(LexicalInformation([(69,0)-(72,0)]))
  Location: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml

- Source: http://a.ml/vocabularies/amf/validation#example-validation-error
  Message: should have required property 'baz'
should have required property 'foo'

  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep5/get/200/application%2Fjson/schema/example/default-example
  Property: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml#/web-api/end-points/%2Fep5/get/200/application%2Fjson/schema/example/default-example
  Position: Some(LexicalInformation([(80,0)-(80,20)]))
  Location: file://amf-client/shared/src/test/resources/validations/jsonschema/allOf/api2.raml
