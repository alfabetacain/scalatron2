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

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      random <- Random.scalaUtilRandom[IO]
      logger <- Slf4jLogger.create[IO]
      httpApp = Router("/" -> Service.routes(random)).orNotFound
      server <- BlazeServerBuilder[IO]
        .bindHttp(8080, "localhost")
        .withHttpApp(httpApp)
        .resource
        .use(s => IO.never)
    } yield ExitCode.Success
  }
}
