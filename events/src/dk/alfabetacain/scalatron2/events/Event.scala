package dk.alfabetacain.scalatron2.events

sealed trait EntityType
object EntityType {
  final case object Fluppet extends EntityType
  final case object Bot extends EntityType
  final case object Minibot extends EntityType
}

sealed trait Faction
object Faction {
  final case class Player(id: String, name: String) extends Faction
  final case object Neutral extends Faction
}
final case class Entity(id: String, kind: EntityType, belongsTo: Faction)

sealed trait Event

object Event {
  final case class Setup(boardSize: Int, players: List[Faction.Player])
      extends Event
  final case class EntityAdded(entity: Entity, position: (Int, Int))
      extends Event
  final case class EntityRemoved(entity: Entity) extends Event
  final case class Moved(entity: Entity, from: (Int, Int), to: (Int, Int))
      extends Event
  final case class Stepped(newTime: Int) extends Event
}
