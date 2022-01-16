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
import cats.effect.kernel.Fiber
import fs2.concurrent.Topic
import fs2.Stream
import dk.alfabetacain.scalatron2.events.Event
import cats.effect.kernel.Ref
import cats.effect.std.Supervisor
import cats.effect.std.Semaphore
import cats.effect.kernel.Resource

object Service {

  final case class RunningGame(
      private val fiber: Fiber[IO, Throwable, Unit],
      private val topic: Topic[IO, Event]
  ) {
    def subscribe: Stream[IO, Event] = {
      topic.subscribe(10)
    }
    def cancel: IO[Unit] = fiber.cancel
  }

  def routes(implicit r: Random[IO]): Resource[IO, HttpRoutes[IO]] = {
    for {
      supervisor <- Supervisor[IO]
      lock <- Resource.eval[IO, Semaphore[IO]](Semaphore(1))
      ref <- Resource.eval(Ref.of[IO, Option[RunningGame]](None))
      routes = HttpRoutes.of[IO] { case POST -> Root / "start-game" =>
        for {
          didStartGame <- tryStartGame(supervisor, lock, ref)
          response <-
            if (didStartGame) {
              Ok("Started game!")
            } else {
              Ok("Failed to start game")
            }
        } yield response
      }
    } yield routes
  }
  private def visualize[F[_]](gameState: GameState[F]): String = {
    val array = gameState.occupancies.visualize(EntityId.generate)

    array.map(_.mkString(" ")).mkString("\n")

  }

  private def tryStartGame(
      supervisor: Supervisor[IO],
      lock: Semaphore[IO],
      ref: Ref[IO, Option[RunningGame]]
  )(implicit random: Random[IO]): IO[Boolean] = {
    Resource.make(lock.acquire)(_ => lock.release).use { _ =>
      val action = for {
        currentGame <- ref.get
        didStartGame <- currentGame match {
          case None =>
            for {
              game <- Game
                .run[IO](
                  numberOfFluppets = 5,
                  numberOfSnorgs = 0,
                  numberOfZugars = 0,
                  numberOfToxifera = 0,
                  bots = List.empty,
                  boardSize = 10,
                  steps = 10
                )
              topic <- Topic[IO, Event]
              fiber <- supervisor.supervise(runGame(game, topic))
              runningGame = RunningGame(fiber, topic)
              _ <- ref.set(Some(runningGame))
            } yield true
          case Some(e) => false.pure[IO]
        }
      } yield didStartGame
      action
    }
  }

  private def runGame(
      stream: Stream[IO, (GameState[IO], List[Event])],
      topic: Topic[IO, Event]
  ): IO[Unit] = {
    stream
      .map(_._2.head)
      .through(topic.publish)
      .compile
      .drain
  }
}
