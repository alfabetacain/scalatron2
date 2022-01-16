package dk.alfabetacain.scalatron2.core

import dk.alfabetacain.scalatron2.interface.Command
import dk.alfabetacain.scalatron2.interface.CommandFeedback
import dk.alfabetacain.scalatron2.interface.BotImpl
import cats.data.State
import cats.data.StateT
import cats.Monad
import cats.Applicative
import cats.syntax._
import cats.implicits._
import cats.effect.std.Random
import monocle.syntax.all._
import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def move(deltaX: Int, deltaY: Int): Point =
    Point(x + deltaX, y + deltaY)
}

sealed trait Board[F[_]] {
  def length: Int
  def points: List[(Point, Entity[F])]
  def positionOf(entity: Entity[F]): Option[Point]
  def entityAt(point: Point): Option[Entity[F]]
  def view(point: Point, size: Int): Array[Array[Option[Entity[F]]]]
  def set(entity: Entity[F], point: Point): Board[F]
  def clear(point: Point): Board[F]
  def removeEntity(id: EntityId): Board[F]
}

object Board {
  def create[F[_]](length: Int): Board[F] = {
    ImmutableMapBoard[F](length, Map.empty)
  }

  private final case class ImmutableMapBoard[F[_]](
      length: Int,
      board: Map[Point, Entity[F]]
  ) extends Board[F] {

    override def points: List[(Point, Entity[F])] = board.toList

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
  }
}

case class GameState[F[_]](
    entities: Map[EntityId, Entity[F]],
    occupancies: Board[F],
    commandFeedback: Map[EntityId, CommandFeedback],
    time: Int,
    botState: Map[EntityId, Game.BotState]
) {
  def removeEntity(id: EntityId): GameState[F] = {
    this
      .focus(_.entities)
      .modify(_.removed(id))
      .focus(_.botState)
      .modify(_.removed(id))
      .focus(_.occupancies)
      .modify(_.removeEntity(id))
  }
}

object Game {

  final case class BotState(state: Map[String, String], energy: Int)

  def run[F[_]: Random: Monad](
      entities: List[Entity[F]],
      board: Board[F],
      steps: Int
  ): F[GameState[F]] = {
    val gameState =
      GameState[F](
        entities.map(e => (e.id, e)).toMap,
        board,
        Map.empty,
        0,
        Map.empty
      )

    def doRun(state: GameState[F], steps: Int): F[GameState[F]] = {
      if (steps <= 0) {
        state.pure[F]
      } else {
        step(state).flatMap(doRun(_, steps - 1))
      }
    }
    doRun(gameState, steps)
  }

  def step[F[_]: Monad: Applicative: Random](
      input: GameState[F]
  ): F[GameState[F]] = {
    val entities = input.entities

    val runBots = input.time % 2 == 0
    val commands = entities.values.toList
      .filter(entity => runBots || !entity.isInstanceOf[Entity.Bot[F]])
      .map { case entity =>
        val entityInput = entity.narrow(input)
        val feedback = input.commandFeedback.get(entity.id)
        entity.act(entityInput).map(c => (entity, c))
      }
      .sequence

    val newState = commands.map { cmds =>
      cmds.foldLeft[GameState[F]](input) { (acc, current) =>
        resolve(acc, current._1, current._2)
      }
    }

    newState.map(_.focus(_.time).modify(_ + 1))
  }

  private def moveAdjusted(
      length: Int,
      position: Point,
      deltaX: Int,
      deltaY: Int
  ): Point = {
    val newX = position.x + deltaX
    val adjustedX = if (newX < 0) {
      length - 1
    } else if (newX >= length) {
      newX % length
    } else {
      newX
    }

    val newY = position.y + deltaY
    val adjustedY = if (newY < 0) {
      length - 1
    } else if (newY >= length) {
      newY % length
    } else {
      newY
    }
    Point(adjustedX, adjustedY)
  }

  private def removeDeadMinitbots[F[_]](game: GameState[F]): GameState[F] = {
    val forUpkeep = game.entities.values
      .collect { case minibot: Entity.Minibot[F] => minibot }
      .filter { minibot =>
        (game.time - minibot.spawnTime) % 4 == 3
      }

    forUpkeep.foldLeft[GameState[F]](game) { (acc, minibot) =>
      val currentEnergy = acc.botState(minibot.id).energy
      if (currentEnergy <= 0) {
        game.removeEntity(minibot.id)
      } else {
        game
      }
    }
  }

  private def upkeep[F[_]](game: GameState[F]): GameState[F] = {
    val forUpkeep = game.entities.values
      .collect { case minibot: Entity.Minibot[F] => minibot }
      .filter { minibot =>
        (game.time - minibot.spawnTime) % 4 == 3
      }

    val withUpkeep = forUpkeep.foldLeft[GameState[F]](game) { (acc, minibot) =>
      val currentEnergy = acc.botState(minibot.id).energy
      val newEnergy = currentEnergy - 1
      game
        .focus(_.botState)
        .modify(states =>
          states + (minibot.id -> states(minibot.id)
            .focus(_.energy)
            .set(newEnergy))
        )
    }
    removeDeadMinitbots(withUpkeep)
  }

  private def resolve[F[_]: Monad](
      game: GameState[F],
      entity: Entity[F],
      cmd: Command
  ): GameState[F] = {
    val newState = (cmd, entity) match {
      case (Command.NoOp, _) => game
      case (move @ Command.Move(deltaX, deltaY), entity) if move.isValid =>
        val res = for {
          currentPosition <- game.occupancies.positionOf(entity)
          newPosition = moveAdjusted(
            game.occupancies.length,
            currentPosition,
            deltaX,
            deltaY
          )
          updated = game.occupancies.entityAt(newPosition) match {
            case Some(value) =>
              game
                .focus(_.commandFeedback)
                .modify(
                  _ + (entity.id -> CommandFeedback
                    .Collision(move))
                )
            case None =>
              val newBoard =
                game.occupancies
                  .clear(currentPosition)
                  .set(entity, newPosition)
              game.focus(_.occupancies).set(newBoard)
          }
        } yield updated
        res.getOrElse(game)

      case (setState @ Command.SetState(args), _: Entity.Bot[F]) =>
        updateBotState[F](game, entity.id, args)
      case (setState @ Command.SetState(args), _: Entity.Minibot[F]) =>
        updateBotState[F](game, entity.id, args)

      case (
            explosion @ Command.Explode(explosionSize),
            minibot: Entity.Minibot[F]
          ) if explosion.isValid =>
        explode(game, explosionSize, minibot)

      case (spawnCmd: Command.Spawn, bot: Entity.Bot[F]) =>
        spawn[F](game, spawnCmd, 0, bot.botImpl, bot)

      case (spawnCmd: Command.Spawn, bot: Entity.Minibot[F]) =>
        spawn[F](game, spawnCmd, bot.generation, bot.botImpl, bot)

      case (invalid, _) =>
        game
          .focus(_.commandFeedback)
          .modify(
            _ + (entity.id -> CommandFeedback
              .InvalidCommand("Could not apply command", invalid))
          )
    }
    newState
      .focus(_.commandFeedback)
      .modify(_.updatedWith(entity.id) {
        case None                         => None
        case Some(v) if v.original != cmd => None
        case updated                      => updated
      })
  }

  private def spawn[F[_]: Monad](
      game: GameState[F],
      command: Command.Spawn,
      parentGeneration: Int,
      botImpl: BotImpl[F],
      parent: Entity[F]
  ): GameState[F] = {

    val result = for {
      parentState <- game.botState.get(parent.id)
      spawnPoint <- game.occupancies
        .positionOf(parent)
        .map(p =>
          moveAdjusted(
            game.occupancies.length,
            p,
            command.deltaX,
            command.deltaY
          )
        )
      originalId <- parent match {
        case bot: Entity.Bot[F]         => Some(bot.id)
        case minibot: Entity.Minibot[F] => Some(minibot.parent)
        case _                          => None
      }
      startingEnergy <- Option(command.energy).filter(e =>
        e >= 100 && parentState.energy >= e
      )
      spawnPoint <- Option(spawnPoint).filter(p =>
        game.occupancies.entityAt(p).isEmpty
      )
      id = EntityId.generate
      minibot = Entity.Minibot[F](
        id,
        command.name.getOrElse(id.toString),
        botImpl,
        originalId,
        parentGeneration + 1,
        game.time
      )
      updatedGame = game
        .focus(_.botState)
        .modify(_ + (id -> BotState(command.startingState, startingEnergy)))
        .focus(_.occupancies)
        .modify(_.set(minibot, spawnPoint))
    } yield updatedGame

    result.getOrElse(
      game
        .focus(_.commandFeedback)
        .modify(
          _ + (parent.id -> CommandFeedback
            .InvalidCommand("Could not spawn minibot", command))
        )
    )

  }

  private def explode[F[_]](
      game: GameState[F],
      size: Int,
      minibot: Entity.Minibot[F]
  ): GameState[F] = {
    val result = for {
      currentPosition <- game.occupancies.positionOf(minibot)
      immuneParentId = minibot.parent
      energy = game.botState(minibot.id).energy
      blastArea = size.toDouble * size.toDouble * Math.PI
      energyPerArea = energy / blastArea
      damageAtCenter = energyPerArea * 200
      withoutCenterBot = game.removeEntity(minibot.id)
      affectedPoints = Range(-size + 1, size).flatMap { x =>
        Range(-size + 1, size).map { y => Point(x, y) }
      }.toList
      affectedEntities = affectedPoints
        .map(p => withoutCenterBot.occupancies.entityAt(p).map(e => (p, e)))
        .collect { case Some(v) => v }
      afterExplosion = affectedEntities.foldLeft[(Int, GameState[F])](
        (0, withoutCenterBot)
      ) { case ((totalDamage, acc), (p, e)) =>
        e match {
          case entity if entity.id == immuneParentId => (totalDamage, acc)
          case minibot: Entity.Minibot[F] if minibot.parent == immuneParentId =>
            (totalDamage, acc)
          case enemyBot =>
            val resultingGame = for {
              currentState <- acc.botState.get(enemyBot.id)
              dist = Math.min(Math.abs(p.x - size), Math.abs(p.y - size))
              fullDamage =
                damageAtCenter * (1.toDouble - (dist.toDouble / size))
              actualDamage = Math.min(currentState.energy, fullDamage.toInt)
              energyLeft = currentState.energy - actualDamage
              newGame = acc
                .focus(_.botState)
                .modify(
                  _ + (enemyBot.id -> currentState
                    .focus(_.energy)
                    .set(energyLeft))
                )
            } yield (totalDamage + actualDamage, newGame)
            resultingGame.getOrElse((totalDamage, acc))
        }
      }
    } yield afterExplosion

    result
      .map { case (totalDamage, newGame) =>
        newGame
          .focus(_.botState)
          .modify(
            _ + (minibot.parent -> newGame
              .botState(minibot.parent)
              .focus(_.energy)
              .modify(_ + totalDamage))
          )
      }
      .map(removeDeadMinitbots)
      .getOrElse(game)
  }

  private def updateBotState[F[_]](
      game: GameState[F],
      entityId: EntityId,
      newState: Map[String, String]
  ): GameState[F] = {
    game
      .focus(_.botState)
      .modify(_.updatedWith(entityId) {
        case None => ???
        case Some(v) if newState.isEmpty =>
          Some(v.focus(_.state).set(Map.empty))
        case Some(existing) =>
          val toRemove = newState.toList.filter(_._2.isEmpty).map(_._1)
          Some(
            existing
              .focus(_.state)
              .modify(existingArgs =>
                (existingArgs ++ newState).removedAll(toRemove)
              )
          )
      })
  }
}
