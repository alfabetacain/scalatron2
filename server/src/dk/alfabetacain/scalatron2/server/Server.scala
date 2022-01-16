package dk.alfabetacain.scalatron2.server

import cats.implicits._
import cats.syntax._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import org.http4s.blaze.server._
import org.http4s.implicits._
import org.http4s.server.Router
import cats.effect.std.Random
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.kernel.Resource

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val resources = for {
      random <- Resource.eval(Random.scalaUtilRandom[IO])
      logger <- Resource.eval(Slf4jLogger.create[IO])
      routes <- Service.routes(random)
      httpApp = Router("/" -> routes).orNotFound
      server <- BlazeServerBuilder[IO]
        .bindHttp(8080, "localhost")
        .withHttpApp(httpApp)
        .resource
    } yield server

    resources.use { _ => IO.never }
  }
}
