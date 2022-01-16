package dk.alfabetacain.scalatron2.interface

sealed trait Command
object Command {
  final case class Move(deltaX: Int, deltaY: Int) extends Command {
    def isValid: Boolean = {
      Math.abs(deltaX) <= 1 && Math.abs(deltaY) <= 1
    }
  }
  final case class Spawn(
      deltaX: Int,
      deltaY: Int,
      name: Option[String],
      energy: Int,
      startingState: Map[String, String]
  ) extends Command
  final case class SetState(updates: Map[String, String]) extends Command
  final case class Explode(size: Int) extends Command {
    def isValid: Boolean = size > 2 && size < 10
  }
  final case object NoOp extends Command
}

sealed trait CommandFeedback {
  def original: Command
}
object CommandFeedback {
  final case class InvalidCommand(msg: String, original: Command)
      extends CommandFeedback
  final case class Collision(original: Command.Move) extends CommandFeedback
}
