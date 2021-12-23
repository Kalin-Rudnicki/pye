package pye.commands.conf

import klib.Implicits._
import klib.fp.types._

final case class ScaffoldConfig(
    model: ScaffoldConfig.Model,
    query: ScaffoldConfig.Query,
    route: Maybe[ScaffoldConfig.Route],
)
object ScaffoldConfig {

  final case class Model(
      fileName: String,
      tableName: String,
  )

  final case class Query(
      fileName: String,
  )

  final case class Route(
      fileName: String,
      pathName: String,
  )

  def defaults(
      name: String,
      modelFileName: Maybe[String],
      modelTableName: Maybe[String],
      queryFileName: Maybe[String],
      routeFileName: Maybe[String],
      routeName: Maybe[String],
  ): ScaffoldConfig = {
    def deCapitalize(s: String): String =
      if (s == null || s.length == 0 || !s.charAt(0).isUpper) s
      else s.updated(0, s.charAt(0).toLower)

    def defaultTableName: String =
      s"${deCapitalize(name)}s"

    def defaultRouteName: String = {
      val reg = "[a-z](?=[A-Z])|[0-9](?=[a-zA-Z])|[A-Z](?=[A-Z][a-z])|[a-zA-Z](?=[0-9])".r
      reg.replaceAllIn(name, _.group(0) + '-').toLowerCase
    }

    ScaffoldConfig(
      model = Model(
        fileName = modelFileName.getOrElse(name),
        tableName = modelTableName.getOrElse(defaultTableName),
      ),
      query = Query(
        fileName = queryFileName.getOrElse(name),
      ),
      route = Route(
        fileName = routeFileName.getOrElse(name),
        pathName = routeName.getOrElse(defaultRouteName),
      ).some,
    )
  }

}
