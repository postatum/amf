Model: file://amf-client/shared/src/test/resources/validations/traits/trait1.raml
Profile: RAML 1.0
Conforms? false
Number of results: 1

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#closed-shape
  Message: Property /hi not supported in a RAML 1.0 trait node
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/traits/trait1.raml#/declarations/traits/secured/applied
  Property: 
  Position: Some(LexicalInformation([(13,4)-(13,8)]))
  Location: file://amf-client/shared/src/test/resources/validations/traits/trait1.raml
