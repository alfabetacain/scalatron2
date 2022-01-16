package dk.alfabetacain.scalatron2.core

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.std.Random

object Main extends IOApp {

  type F[A] = IO[A]

  override def run(args: List[String]): IO[ExitCode] = {
    Random
      .scalaUtilRandom[F]
      .flatMap { implicit r =>
        val res = Game
          .run[F](
            numberOfFluppets = 5,
            numberOfSnorgs = 0,
            numberOfZugars = 0,
            numberOfToxifera = 0,
            bots = List.empty,
            boardSize = 10,
            steps = 10
          )
        res
      }
      .flatMap { stream =>
        stream.last.compile.toList.map { lastState =>
          lastState.headOption.flatten
            .map(_._1)
            .map { lastState =>
              println(visualize[F](lastState))
              ExitCode.Success
            }
            .getOrElse(ExitCode.Error)
        }
      }
  }

  private def visualize[F[_]](gameState: GameState[F]): String = {
    println(gameState)

    val array = gameState.occupancies.visualize(EntityId.generate)

    array.map(_.mkString(" ")).mkString("\n")

  }
}
