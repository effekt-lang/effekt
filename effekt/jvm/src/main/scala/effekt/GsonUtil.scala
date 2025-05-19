package effekt

import com.google.gson._
import com.google.gson.reflect.TypeToken
import java.lang.reflect.{ParameterizedType, Type}
import scala.jdk.CollectionConverters._

class ScalaOptionTypeAdapter extends JsonSerializer[Option[Any]] with JsonDeserializer[Option[Any]] {
  override def serialize(src: Option[Any], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    src match {
      case Some(value) => context.serialize(value)
      case None        => JsonNull.INSTANCE
    }
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Option[Any] = {
    val innerType = typeOfT match {
      case pt: ParameterizedType => pt.getActualTypeArguments.head
      case _                     => classOf[Object]
    }

    if (json.isJsonNull) None
    else Some(context.deserialize(json, innerType))
  }
}

class ScalaListTypeAdapter extends JsonSerializer[scala.collection.immutable.List[Any]] with JsonDeserializer[scala.collection.immutable.List[Any]] {
  override def serialize(src: scala.collection.immutable.List[Any], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val javaList: java.util.List[Any] = src.asJava
    context.serialize(javaList)
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): scala.collection.immutable.List[Any] = {
    val elemType = typeOfT match {
      case pt: ParameterizedType => pt.getActualTypeArguments.head
      case _                     => classOf[Object]
    }

    val listType = TypeToken.getParameterized(classOf[java.util.List[_]], elemType).getType
    val javaList = context.deserialize[java.util.List[Any]](json, listType)

    javaList.asScala.toList
  }
}

implicit class GsonBuilderScalaOps(val builder: GsonBuilder) extends AnyVal {
  /**
   * Make the Scala Option[_] and List[_] types (de)serialize correctly with Gson.
   *
   * As a Java library, Gson does not special case any Scala types. This leads to unexpected (de)serialization by default.
   * By default, Some(x) serializes to {"value":x} and None serializes to {}.
   * List serializes to nested JSON objects.
   *
   * This method adds custom (de)serializers such that
   * - Some(x) serializes to x and None serializes to null
   * - List serializes to a flat JSON array.
   */
  def withScalaSupport: GsonBuilder =
    builder
      .registerTypeHierarchyAdapter(classOf[Option[_]], new ScalaOptionTypeAdapter)
      .registerTypeHierarchyAdapter(classOf[List[_]],   new ScalaListTypeAdapter)
}
