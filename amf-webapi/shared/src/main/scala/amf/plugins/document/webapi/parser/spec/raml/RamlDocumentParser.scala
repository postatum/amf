package amf.plugins.document.webapi.parser.spec.raml

import amf.core.Root
import amf.core.annotations._
import amf.core.metamodel.Field
import amf.core.metamodel.document.{BaseUnitModel, ExtensionLikeModel}
import amf.core.metamodel.domain.ShapeModel
import amf.core.metamodel.domain.extensions.CustomDomainPropertyModel
import amf.core.model.document.{BaseUnit, Document}
import amf.core.model.domain.extensions.CustomDomainProperty
import amf.core.model.domain.{AmfArray, AmfScalar}
import amf.core.parser.{Annotations, _}
import amf.core.utils._
import amf.plugins.document.webapi.annotations.DeclaredElement
import amf.plugins.document.webapi.contexts.RamlWebApiContext
import amf.plugins.document.webapi.model.{Extension, Overlay}
import amf.plugins.document.webapi.parser.spec._
import amf.plugins.document.webapi.parser.spec.common._
import amf.plugins.document.webapi.parser.spec.declaration._
import amf.plugins.document.webapi.parser.spec.domain._
import amf.plugins.document.webapi.vocabulary.VocabularyMappings
import amf.plugins.domain.shapes.models.CreativeWork
import amf.plugins.domain.webapi.metamodel.WebApiModel
import amf.plugins.domain.webapi.metamodel.security.SecuritySchemeModel
import amf.plugins.domain.webapi.models._
import amf.plugins.domain.webapi.models.templates.{ResourceType, Trait}
import amf.plugins.features.validation.ParserSideValidations
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Raml 1.0 spec parser
  */
case class Raml10DocumentParser(root: Root)(implicit override val ctx: RamlWebApiContext)
    extends RamlDocumentParser(root)
    with Raml10BaseSpecParser {
  def parseExtension(): Extension = {
    val extension = parseDocument(Extension())

    parseExtension(extension, ExtensionLikeModel.Extends)

    extension
  }

  private def parseExtension(document: Document, field: Field): Unit = {
    val map = root.parsed.document.as[YMap]

    UsageParser(map, document).parse()

    map
      .key("extends")
      .foreach(e => {
        root.references
          .find(_.origin.url == e.value.as[String])
          .foreach(extend =>
            document
              .set(field, AmfScalar(extend.unit.id, Annotations(e.value)), Annotations(e)))
      })
  }

  def parseOverlay(): Overlay = {
    val overlay = parseDocument(Overlay())

    parseExtension(overlay, ExtensionLikeModel.Extends)

    overlay
  }
}

abstract class RamlDocumentParser(root: Root)(implicit val ctx: RamlWebApiContext) extends RamlBaseDocumentParser {

  def parseDocument[T <: Document](document: T): T = {
    document.adopted(root.location).withLocation(root.location)

    val map = root.parsed.document.as[YMap]

    val references = ReferencesParser(document, "uses", map, root.references).parse(root.location)
    parseDeclarations(root, map)

    val api = parseWebApi(map).add(SourceVendor(root.vendor))
    document.withEncodes(api)

    val declarables = ctx.declarations.declarables()
    if (declarables.nonEmpty) document.withDeclares(declarables)
    if (references.references.nonEmpty) document.withReferences(references.solvedReferences())

    ctx.futureDeclarations.resolve()

    document
  }

  def parseDocument(): Document = parseDocument(Document())

  protected def parseWebApi(map: YMap): WebApi = {

    val api = WebApi(root.parsed.document.node).adopted(root.location)

    ctx.closedShape(api.id, map, "webApi")

    map.key("title", (WebApiModel.Name in api).allowingAnnotations)

    map.key("description", (WebApiModel.Description in api).allowingAnnotations)

    map.key(
      "mediaType",
      entry => {
        ctx.globalMediatype = true
        val annotations = Annotations(entry)
        val value = entry.value.tagType match {
          case YType.Seq =>
            ArrayNode(entry.value).text()
          case _ =>
            annotations += SingleValueArray()
            AmfArray(Seq(RamlScalarNode(entry.value).text()))
        }

        api.set(WebApiModel.ContentType, value, annotations)
        api.set(WebApiModel.Accepts, value, annotations)
      }
    )

    map.key("version", (WebApiModel.Version in api).allowingAnnotations)
    map.key("termsOfService".asRamlAnnotation, WebApiModel.TermsOfService in api)
    map.key("protocols", (WebApiModel.Schemes in api).allowingSingleValue)
    map.key("contact".asRamlAnnotation, WebApiModel.Provider in api using OrganizationParser.parse)
    map.key("license".asRamlAnnotation, WebApiModel.License in api using LicenseParser.parse)

    map.key(
      "tags".asRamlAnnotation,
      entry => {
        val tags = entry.value.as[Seq[YMap]].map(tag => TagsParser(tag, (tag: Tag) => tag.adopted(api.id)).parse())
        api.set(WebApiModel.Tags, AmfArray(tags, Annotations(entry.value)), Annotations(entry))
      }
    )

    map.regex(
      "^/.*",
      entries => {
        val endpoints = mutable.ListBuffer[EndPoint]()
        entries.foreach(entry => ctx.factory.endPointParser(entry, api.withEndPoint, None, endpoints, false).parse())
        api.set(WebApiModel.EndPoints, AmfArray(endpoints))
      }
    )

    RamlServersParser(map, api).parse()

    val SchemeParser = RamlParametrizedSecuritySchemeParser.parse(api.withSecurity) _
    map.key("securedBy", (WebApiModel.Security in api using SchemeParser).allowingSingleValue)

    map.key(
      "documentation",
      entry => {
        api.setArray(WebApiModel.Documentations,
                     UserDocumentationsParser(entry.value.as[Seq[YNode]], ctx.declarations, api.id).parse(),
                     Annotations(entry))
      }
    )

    AnnotationParser(api, map).parse()

    api
  }
}

// todo pass to ctx. declaration parser?
trait Raml10BaseSpecParser extends RamlBaseDocumentParser {

  implicit val ctx: RamlWebApiContext

  override def parseParameterDeclarations(key: String, map: YMap, parentPath: String): Unit = {
    map.key(
      key,
      entry => {
        entry.value
          .as[YMap]
          .entries
          .foreach(e => {
            val parameter =
              Raml10ParameterParser(e, (p: Parameter) => p.adopted(parentPath)).parse()
            if (parameter.binding.isNullOrEmpty) {
              ctx.violation(parameter.id, "Missing binding information in declared parameter", entry.value)
            }
            ctx.declarations.registerParameter(parameter.add(DeclaredElement()),
                                               Payload().withSchema(parameter.schema))
          })
      }
    )
  }

  override protected def parseSecuritySchemeDeclarations(map: YMap, parent: String): Unit = {
    map.key(
      "securitySchemes",
      e => {
        e.value.tagType match {
          case YType.Map =>
            e.value.as[YMap].entries.foreach { entry =>
              ctx.declarations += SecuritySchemeParser(
                entry,
                scheme => {
                  scheme.set(SecuritySchemeModel.Name,
                             AmfScalar(entry.key.as[String], Annotations(entry.key.value)),
                             Annotations(entry.key))
                  scheme.adopted(parent)
                }
              ).parse()
                .add(DeclaredElement())
            }
          case YType.Null =>
          case t          => ctx.violation(parent, s"Invalid type $t for 'securitySchemes' node.", e.value)
        }
      }
    )
  }
}

abstract class RamlBaseDocumentParser(implicit ctx: RamlWebApiContext) extends RamlSpecParser with RamlTypeSyntax {
  protected def parseSecuritySchemeDeclarations(map: YMap, parent: String): Unit

  protected def parseDeclarations(root: Root, map: YMap): Unit = {
    val parent = root.location + "#/declarations"
    parseTypeDeclarations(map, parent + "/types")
    parseAnnotationTypeDeclarations(map, parent + "/annotations")
    AbstractDeclarationsParser(
      "traits",
      entry =>
        Trait(entry)
          .withName(entry.key.as[String])
          .withId(parent + s"/traits/${entry.key.as[String].urlComponentEncoded}"),
      map,
      parent + "/traits"
    ).parse()
    AbstractDeclarationsParser(
      "resourceTypes",
      entry =>
        ResourceType(entry)
          .withName(entry.key.as[String])
          .withId(parent + s"/resourceTypes/${entry.key.as[String].urlComponentEncoded}"),
      map,
      parent + "/resourceTypes"
    ).parse()
    parseSecuritySchemeDeclarations(map, parent + "/securitySchemes")
    parseParameterDeclarations("parameters".asRamlAnnotation, map, root.location + "#/parameters")
    parseResponsesDeclarations("responses".asRamlAnnotation, map, root.location + "#/responses")
  }

  def parseResponsesDeclarations(key: String, map: YMap, parentPath: String): Unit = {
    map.key(
      key,
      entry => {
        entry.value
          .as[YMap]
          .entries
          .foreach(e => {
            ctx.declarations +=
              OasResponseParser(e, (r: Response) => r.adopted(parentPath))(toOas(ctx))
                .parse()
                .add(DeclaredElement())

          })
      }
    )
  }

  def parseAnnotationTypeDeclarations(map: YMap, customProperties: String): Unit = {
    map.key(
      "annotationTypes",
      e => {
        e.value.tagType match {
          case YType.Map =>
            e.value
              .as[YMap]
              .entries
              .map { entry =>
                val typeName = entry.key.as[YScalar].text
                val customProperty = AnnotationTypesParser(
                  entry,
                  customProperty => {
                    customProperty.set(CustomDomainPropertyModel.Name,
                                       AmfScalar(typeName, Annotations(entry.key.value)),
                                       Annotations(entry.key))
                    customProperty.adopted(customProperties)
                  }
                )
                ctx.declarations += customProperty.add(DeclaredElement())
              }
          case YType.Null =>
          case t          => ctx.violation(customProperties, s"Invalid type $t for 'annotationTypes' node.", e.value)
        }
      }
    )
  }

  private def parseTypeDeclarations(map: YMap, parent: String): Unit = {
    typeOrSchema(map).foreach { e =>
      e.value.tagType match {
        case YType.Map =>
          e.value.as[YMap].entries.foreach { entry =>
            if (wellKnownType(entry.key.as[String])) {
              ctx.violation(
                ParserSideValidations.ParsingErrorSpecification.id,
                parent,
                "Default type name cannot be used to name a custom type",
                entry.key
              )
            }
            val parser = Raml10TypeParser(entry, shape => {
              shape.set(ShapeModel.Name,
                        AmfScalar(entry.key.as[String], Annotations(entry.key.value)),
                        Annotations(entry.key))
              shape.adopted(parent)
            })
            parser.parse() match {
              case Some(shape) =>
                if (entry.value.tagType == YType.Null) shape.annotations += SynthesizedField()
                ctx.declarations += shape.add(DeclaredElement())
              case None => ctx.violation(parent, s"Error parsing shape '$entry'", entry)
            }

          }
        case YType.Null =>
        case t          => ctx.violation(parent, s"Invalid type $t for 'types' node.", e.value)
      }
    }
  }

  /** Get types or schemas facet. If both are available, default to types facet and throw a validation error. */
  override protected def typeOrSchema(map: YMap): Option[YMapEntry] = {
    val types   = map.key("types")
    val schemas = map.key("schemas")

    for {
      _ <- types
      s <- schemas
    } {
      ctx.violation("'schemas' and 'types' properties are mutually exclusive", s.key)
    }

    schemas.foreach(s =>
      ctx.warning("'schemas' keyword it's deprecated for 1.0 version, should use 'types' instead", s.key))

    types.orElse(schemas)
  }

  def parseParameterDeclarations(key: String, map: YMap, parentPath: String): Unit

}

abstract class RamlSpecParser(implicit ctx: RamlWebApiContext) extends WebApiBaseSpecParser {

  protected def typeOrSchema(map: YMap): Option[YMapEntry] = map.key("type").orElse(map.key("schema"))

  case class UsageParser(map: YMap, baseUnit: BaseUnit) {
    def parse(): Unit = {
      map.key(
        "usage",
        entry => {
          entry.value.tagType match {
            case YType.Str =>
              val value = ScalarNode(entry.value)
              baseUnit.set(BaseUnitModel.Usage, value.string(), Annotations(entry))
            case _ =>
          }
        }
      )
    }
  }

  case class UserDocumentationsParser(seq: Seq[YNode], declarations: WebApiDeclarations, parent: String) {
    def parse(): Seq[CreativeWork] = {
      val results = ListBuffer[CreativeWork]()

      seq.foreach(n =>
        n.tagType match {
          case YType.Map => results += RamlCreativeWorkParser(n).parse()
          case YType.Seq => ctx.violation(parent, s"Unexpected sequence. Options are object or scalar ", n)
          case _ =>
            val scalar = n.as[YScalar]
            declarations.findDocumentations(scalar.text, SearchScope.Fragments) match {
              case Some(doc) =>
                results += doc.link(scalar.text, Annotations()).asInstanceOf[CreativeWork]
              case _ =>
                ctx.violation(parent, s"not supported scalar ${scalar.text} for documentation", scalar)
            }
      })

      results
    }
  }

  object AnnotationTypesParser extends RamlTypeSyntax {
    def apply(ast: YMapEntry, adopt: (CustomDomainProperty) => Unit): CustomDomainProperty =
      ast.value.tagType match {
        case YType.Map =>
          AnnotationTypesParser(ast, ast.key.as[YScalar].text, ast.value.as[YMap], adopt).parse()

        case YType.Seq =>
          val domainProp = CustomDomainProperty()
          adopt(domainProp)
          ctx.violation(domainProp.id,
                        "Invalid value type for annotation types parser, expected map or scalar reference",
                        ast.value)
          domainProp
        case _ =>
          val key             = ast.key.as[YScalar].text
          val scalar: YScalar = ast.value.as[YScalar]
          val domainProp      = CustomDomainProperty()
          adopt(domainProp)

          ctx.declarations.findAnnotation(scalar.text, SearchScope.All) match {
            case Some(a) =>
              val copied: CustomDomainProperty = a.link(scalar.text, Annotations(ast))
              copied.id = null // we reset the ID so ti can be adopted, there's an extra rule where the id is not set
              // because the way they are inserted in the mode later in the parsing
              adopt(copied.withName(key))
              copied
            case _ =>
              Raml10TypeParser(ast, (shape) => shape.adopted(domainProp.id), isAnnotation = true).parse() match {
                case Some(schema) => domainProp.withSchema(schema)
                case _ =>
                  ctx.violation(domainProp.id, "Could not find declared annotation link in references", scalar)
                  domainProp
              }
          }
      }
  }

  case class AnnotationTypesParser(ast: YPart,
                                   annotationName: String,
                                   map: YMap,
                                   adopt: (CustomDomainProperty) => Unit) {
    def parse(): CustomDomainProperty = {

      val custom = CustomDomainProperty(ast)
      custom.withName(annotationName)
      adopt(custom)

      // We parse the node as if it were a data shape, this will also check the closed node condition including the
      // annotation type facets
      val maybeAnnotationType: Option[YMapEntry] = ast match {
        case me: YMapEntry => Some(me)
        case m: YMap       => Some(YMapEntry(YNode("annotationType"), YNode(m)))
        case _             => None
      }

      maybeAnnotationType match {
        case Some(annotationType) =>
          Raml10TypeParser(annotationType, shape => shape.withName("schema").adopted(custom.id), isAnnotation = true)
            .parse()
            .foreach({ shape =>
              custom.set(CustomDomainPropertyModel.Schema, shape, Annotations(ast))
            })

          map.key(
            "allowedTargets",
            entry => {
              val annotations = Annotations(entry)
              val targets: AmfArray = entry.value.tagType match {
                case YType.Seq =>
                  ArrayNode(entry.value).string()
                case YType.Map =>
                  ctx.violation(
                    custom.id,
                    "Property 'allowedTargets' in a RAML annotation can only be a valid scalar or an array of valid scalars",
                    entry.value)
                  AmfArray(Seq())
                case _ =>
                  annotations += SingleValueArray()
                  AmfArray(Seq(ScalarNode(entry.value).string()))
              }

              val targetUris = targets.values.map({
                case s: AmfScalar =>
                  VocabularyMappings.ramlToUri.get(s.toString) match {
                    case Some(uri) => AmfScalar(uri, s.annotations)
                    case None      => s
                  }
                case nodeType => AmfScalar(nodeType.toString, nodeType.annotations)
              })

              custom.set(CustomDomainPropertyModel.Domain, AmfArray(targetUris), annotations)
            }
          )

          map.key("description", entry => {
            val value = ScalarNode(entry.value)
            custom.set(CustomDomainPropertyModel.Description, value.string(), Annotations(entry))
          })

          AnnotationParser(custom, map).parse()

          custom
        case None =>
          ctx.violation(custom.id, "Cannot parse annotation type fragment, cannot find information map", ast)
          custom
      }

    }
  }
}
