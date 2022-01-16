package dk.alfabetacain.scalatron2.core

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.std.Random

object Main extends IOApp {

  type F[A] = IO[A]

  override def run(args: List[String]): IO[ExitCode] = {
    Random.scalaUtilRandom[F].flatMap { implicit r =>
      val entities = List(
        Entity.fluppet[F](EntityId.generate),
        Entity.fluppet[F](EntityId.generate),
        Entity.fluppet[F](EntityId.generate)
      )
      val board = Board.create[F](10).set(entities.head, Point(0, 0))
      Game
        .run[F](entities, board, 10)
        .map { endState =>
          println(visualize(endState))
          ExitCode.Success
        }
    }
  }

  private def visualize[F[_]](gameState: GameState[F]): String = {
    println(gameState)

    val points = gameState.occupancies.points
    val array = Array.fill[Array[String]](gameState.occupancies.length)(
      Array.fill[String](gameState.occupancies.length)("_")
    )
    points.foreach { current =>
      array(current._1.y)(current._1.x) = "X"
    }

    array.map(_.mkString(" ")).mkString("\n")

  }
}
