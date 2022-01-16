package dk.alfabetacain.scalatron2.interface

import cats.Monad

object BotImpl {
  type View = Array[Array[String]]
  final case class BotInput(
      generation: Int,
      name: String,
      time: Int,
      view: View,
      energy: Int,
      masterDirection: Option[(Int, Int)],
      lastCommandFeedback: Option[CommandFeedback],
      slaves: Int,
      state: Map[String, String]
  )
}

abstract class BotImpl[F[_]: Monad] {
  def act(input: BotImpl.BotInput): F[Command]
}

trait BotFactory {

  def create[F[_]: Monad](): BotImpl[F]
}
