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

final case class Event(
    state: Array[Array[Option[Entity]]],
    preceedingGameEvents: List[GameEvent]
)

sealed trait GameEvent

object GameEvent {
  final case class Setup(boardSize: Int, players: List[Faction.Player])
      extends GameEvent
  final case class EntityAdded(entity: Entity, position: (Int, Int))
      extends GameEvent
  final case class EntityRemoved(entity: Entity) extends GameEvent
  final case class Moved(entity: Entity, from: (Int, Int), to: (Int, Int))
      extends GameEvent
  final case class Stepped(newTime: Int) extends GameEvent
}
