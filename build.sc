import mill._, scalalib._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.13.7"
  def scalacOptions = Seq("-Ymacro-annotations")
}

object interface extends CommonModule {
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.3.0"
  )
}

object events extends CommonModule {}

object core extends CommonModule {
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.3.0",
    ivy"org.typelevel::cats-effect:3.3.4",
    ivy"dev.optics::monocle-core:3.1.0",
    ivy"dev.optics::monocle-macro:3.1.0",
    ivy"io.estatico::newtype:0.4.4",
    ivy"co.fs2::fs2-core:3.2.4"
  )

  def moduleDeps = Seq(interface, events)
}

object server extends CommonModule {
  val http4sVersion = "0.23.7"
  def ivyDeps = Agg(
    ivy"org.http4s::http4s-dsl:$http4sVersion",
    ivy"org.http4s::http4s-blaze-server:$http4sVersion",
    ivy"org.typelevel::log4cats-slf4j:2.1.1"
  )
  def moduleDeps = Seq(core)
}
