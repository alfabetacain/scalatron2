package dk.alfabetacain.scalatron2.core

import monocle.syntax.all._
import dk.alfabetacain.scalatron2.core.Entity.Fluppet
import dk.alfabetacain.scalatron2.core.Entity.Bot
import dk.alfabetacain.scalatron2.core.Entity.Minibot

sealed trait Board[F[_]] {
  def length: Int
  def points: List[(Point, Option[Entity[F]])]
  def positionOf(entity: Entity[F]): Option[Point]
  def entityAt(point: Point): Option[Entity[F]]
  def view(point: Point, size: Int): Array[Array[Option[Entity[F]]]]
  def set(entity: Entity[F], point: Point): Board[F]
  def clear(point: Point): Board[F]
  def removeEntity(id: EntityId): Board[F]
  def visualize(owningId: EntityId): Array[Array[String]]
}

object Board {
  def create[F[_]](length: Int): Board[F] = {
    ImmutableMapBoard[F](length, Map.empty)
  }

  def visualize[F[_]](
      owningId: EntityId,
      view: Array[Array[Option[Entity[F]]]]
  ): Array[Array[String]] = {
    view.map(_.map(_.fold[String]("_")(entityToString[F](owningId, _))))
  }

  private def entityToString[F[_]](
      owningId: EntityId,
      entity: Entity[F]
  ): String = {
    entity match {
      case Fluppet(id) => "B"
      case Bot(id, name, botImpl) =>
        if (id == owningId) {
          "M"
        } else {
          "M"
        }
      case Minibot(id, name, botImpl, parent, generation, spawnTime) =>
        if (parent == owningId) {
          "S"
        } else {
          "s"
        }
    }
  }

  private final case class ImmutableMapBoard[F[_]](
      length: Int,
      board: Map[Point, Entity[F]]
  ) extends Board[F] {

    override def points: List[(Point, Option[Entity[F]])] = {
      Range(0, length)
        .flatMap { y =>
          Range(0, length).map { x => Point(x, y) }
        }
        .map { p => (p, entityAt(p)) }
        .toList
    }

    override def positionOf(entity: Entity[F]): Option[Point] =
      board.find(_._2 == entity).map(_._1)

    override def entityAt(point: Point): Option[Entity[F]] = board.get(point)

    override def view(
        point: Point,
        size: Int
    ): Array[Array[Option[Entity[F]]]] = {
      val actualView = Range
        .inclusive(point.y - size, point.y + size)
        .map(_ % length)
        .map { y =>
          Range
            .inclusive(point.x - size, point.x + size)
            .map(_ % length)
            .map { x =>
              entityAt(Point(x, y))
            }
            .toList
        }
        .toList
      actualView.map(_.toArray).toArray
    }

    override def set(entity: Entity[F], point: Point): Board[F] = {
      ImmutableMapBoard[F](length, board + (point -> entity))
    }

    override def clear(point: Point): Board[F] =
      ImmutableMapBoard[F](length, board.removed(point))

    override def removeEntity(id: EntityId): Board[F] = {
      this.focus(_.board).modify(_.filter(_._2.id != id))
    }

    override def visualize(owningId: EntityId): Array[Array[String]] = {
      val array = Range(0, length).map { y =>
        Range(0, length).map { x =>
          entityAt(Point(x, y))
        }.toArray
      }.toArray
      Board.visualize(owningId, array)
    }
  }
}
