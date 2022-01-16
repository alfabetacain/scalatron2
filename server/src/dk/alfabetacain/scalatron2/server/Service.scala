package dk.alfabetacain.scalatron2.server

import org.http4s.HttpRoutes
import cats.Monad
import cats.implicits._
import cats.syntax._
import cats.effect.IO
import org.http4s.Response
import org.http4s.Status
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl._
import org.http4s.dsl.io._
import cats.effect.kernel.Async
import dk.alfabetacain.scalatron2.core.Game
import cats.effect.std.Random
import dk.alfabetacain.scalatron2.core.GameState
import dk.alfabetacain.scalatron2.core.EntityId

object Service {
  def routes(implicit r: Random[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] { case _ =>
      Game
        .run[IO](
          numberOfFluppets = 5,
          numberOfSnorgs = 0,
          numberOfZugars = 0,
          numberOfToxifera = 0,
          bots = List.empty,
          boardSize = 10,
          steps = 10
        )
        .flatMap { stream =>
          stream.last.compile.toList.flatMap { lastState =>
            lastState.headOption.flatten
              .map(_._1)
              .map { lastState =>
                Ok(visualize(lastState))

              }
              .getOrElse(Ok("Error"))
          }
        }
    }
  }
  private def visualize[F[_]](gameState: GameState[F]): String = {
    val array = gameState.occupancies.visualize(EntityId.generate)

    array.map(_.mkString(" ")).mkString("\n")

  }
}
