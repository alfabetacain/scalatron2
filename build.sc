import mill._, scalalib._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.13.7"
}

object interface extends CommonModule {
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.3.0"
  )
}

object events extends CommonModule {}

object core extends CommonModule {
  def scalacOptions = Seq("-Ymacro-annotations")
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
