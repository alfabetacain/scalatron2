package dk.alfabetacain.scalatron2.core

import dk.alfabetacain.scalatron2.interface.Command
import dk.alfabetacain.scalatron2.interface.CommandFeedback
import dk.alfabetacain.scalatron2.interface.BotImpl
import cats.Monad
import cats.effect.std.Random
import cats.implicits._
import cats.syntax._

sealed trait Entity[F[_]] {
  def id: EntityId
  type Input
  def narrow(s: GameState[F]): Input
  def act(s: Input): F[Command]
  def triggers(time: Int): Boolean
}

object Entity {
  def fluppet[F[_]: Random: Monad](
      id: EntityId
  ): Entity[F] = {
    Fluppet[F](id)
  }

  final case class Fluppet[F[_]: Random: Monad](id: EntityId)
      extends Entity[F] {

    override type Input = GameState[F]
    override def narrow(s: GameState[F]): Input = s

    override def act(
        s: Input
    ): F[Command] = {

      for {
        deltaX <- Random[F].nextIntBounded(3).map(_ - 1)
        deltaY <- Random[F].nextIntBounded(3).map(_ - 1)
      } yield Command.Move(deltaX, deltaY)
    }

    override def triggers(time: Int): Boolean = time % 4 == 0
  }

  final case class Bot[F[_]: Monad](
      id: EntityId,
      name: String,
      botImpl: BotImpl[F]
  ) extends Entity[F] {
    override type Input = BotImpl.BotInput
    override def narrow(s: GameState[F]): Input = {
      BotImpl.BotInput(
        generation = 0,
        name = name,
        time = s.time,
        view = s.occupancies
          .view(s.occupancies.positionOf(this).get, 15)
          .map(_.map(x => if (x.isDefined) "X" else "_")),
        energy = 100,
        masterDirection = None,
        lastCommandFeedback = s.commandFeedback.get(id),
        slaves = 0,
        state = s.botState.get(id).map(_.state).getOrElse(Map.empty)
      )
    }

    override def triggers(time: Int): Boolean = time % 2 == 0

    override def act(
        s: Input
    ): F[Command] = {
      botImpl.act(s)
    }
  }

  final case class Minibot[F[_]: Monad](
      id: EntityId,
      name: String,
      botImpl: BotImpl[F],
      parent: EntityId,
      generation: Int,
      spawnTime: Int
  ) extends Entity[F] {
    override type Input = BotImpl.BotInput
    override def narrow(s: GameState[F]): BotImpl.BotInput = {
      val state = s.botState(id)
      BotImpl.BotInput(
        generation = generation,
        name = name,
        time = s.time,
        view = s.occupancies
          .view(s.occupancies.positionOf(this).get, 10)
          .map(_.map(x => if (x.isDefined) "X" else "_")),
        energy = state.energy,
        masterDirection = None,
        lastCommandFeedback = s.commandFeedback.get(id),
        slaves = 0,
        state = state.state
      )
    }
    override def act(s: BotImpl.BotInput): F[Command] = {
      botImpl.act(s)
    }

    override def triggers(time: Int): Boolean = true
  }
}
