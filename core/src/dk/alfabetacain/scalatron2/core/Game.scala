package dk.alfabetacain.scalatron2.core

import dk.alfabetacain.scalatron2.interface.Command
import dk.alfabetacain.scalatron2.interface.CommandFeedback
import dk.alfabetacain.scalatron2.interface.BotImpl
import dk.alfabetacain.scalatron2.events.Event
import cats.data.State
import cats.data.StateT
import cats.Monad
import cats.Applicative
import cats.syntax._
import cats.implicits._
import cats.effect.std.Random
import monocle.syntax.all._
import scala.annotation.tailrec
import cats.Parallel
import fs2.Stream

case class Point(x: Int, y: Int) {
  def move(deltaX: Int, deltaY: Int): Point =
    Point(x + deltaX, y + deltaY)
}

final case class GameState[F[_]](
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

  def run[F[_]: Random: Monad: Parallel](
      numberOfFluppets: Int,
      numberOfSnorgs: Int,
      numberOfZugars: Int,
      numberOfToxifera: Int,
      bots: List[Entity.Bot[F]],
      boardSize: Int,
      steps: Int
  ): F[Stream[F, (GameState[F], List[Event])]] = {
    val fluppets =
      List.fill(numberOfFluppets)(Entity.fluppet[F](EntityId.generate))
    val entities: List[Entity[F]] =
      fluppets ++ bots
    val board = Board.create[F](boardSize)

    def doRun(
        state: GameState[F],
        steps: Int
    ): Stream[F, (GameState[F], List[Event])] = {
      Stream.unfoldEval[F, GameState[F], (GameState[F], List[Event])](state) {
        currentState =>
          if (currentState.time == steps) {
            Option.empty[((GameState[F], List[Event]), GameState[F])].pure[F]
          } else {
            step[F](currentState).map(s => Some(((s, List.empty[Event]), s)))
          }
      }
    }
    for {
      board <- setupBoard(entities, Board.create[F](boardSize))
      initialStates = setupEntities(entities)
      gameState =
        GameState[F](
          entities.map(e => (e.id, e)).toMap,
          board,
          Map.empty,
          0,
          initialStates
        )
      result = doRun(gameState, steps)
    } yield result
  }

  private def setupEntities[F[_]](
      entities: List[Entity[F]]
  ): Map[EntityId, BotState] = {
    entities.foldLeft[Map[EntityId, BotState]](Map.empty) { (acc, entity) =>
      entity match {
        case bot: Entity.Bot[F] => acc + (bot.id -> BotState(Map.empty, 1000))
        case fluppet: Entity.Fluppet[F] =>
          acc + (fluppet.id -> BotState(Map.empty, 200))
        case minibot: Entity.Minibot[F] => ???
      }
    }
  }

  private def setupBoard[F[_]: Random: Monad](
      entities: List[Entity[F]],
      board: Board[F]
  ): F[Board[F]] = {

    for {
      points <- Random[F].shuffleList(board.points.collect { case (p, None) =>
        p
      })
      zipped = points.zip(entities)
      withPlacements = zipped.foldLeft[Board[F]](board) {
        case (acc, (point, entity)) =>
          acc.set(entity, point)
      }
    } yield withPlacements
  }

  def step[F[_]: Monad: Parallel: Random](
      input: GameState[F]
  ): F[GameState[F]] = {
    val entities = input.entities

    val commands = entities.values.toList
      .filter(_.triggers(input.time))
      .map { case entity =>
        val entityInput = entity.narrow(input)
        val feedback = input.commandFeedback.get(entity.id)
        entity.act(entityInput).map(c => (entity, c))
      }
      .parSequence

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

  private def removeDeadEntities[F[_]](game: GameState[F]): GameState[F] = {
    val deadEntities = game.entities.values
      .map(e => game.botState.get(e.id).map(s => (e, s)))
      .collect { case Some(v) => v }
      .filter {
        case (_: Entity.Bot[F], _) => false
        case other                 => other._2.energy <= 0
      }

    deadEntities.foldLeft[GameState[F]](game) { case (acc, (entity, _)) =>
      game.removeEntity(entity.id)
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
    removeDeadEntities(withUpkeep)
  }

  private def addEnergy[F[_]](
      game: GameState[F],
      entityId: EntityId,
      energy: Int
  ): GameState[F] = {
    game.focus(_.botState).modify { states =>
      states + (entityId -> states(entityId).focus(_.energy).modify(_ + energy))
    }
  }

  private def stateOf[F[_]](
      game: GameState[F],
      entityId: EntityId
  ): BotState = {
    game.botState(entityId)
  }

  private def collide[F[_]](
      game: GameState[F],
      move: Command.Move,
      movingEntity: Entity[F],
      stationaryEntity: Entity[F]
  ): GameState[F] = {
    def bonk(game: GameState[F]): GameState[F] = game
      .focus(_.commandFeedback)
      .modify(_ + (movingEntity.id -> CommandFeedback.Collision(move)))
    (movingEntity, stationaryEntity) match {
      case (bot: Entity.Bot[F], target: Entity.Bot[F]) =>
        bonk(game)
      case (bot: Entity.Bot[F], target: Entity.Minibot[F]) =>
        if (target.parent == bot.id) {
          // consume child
          addEnergy(game, bot.id, stateOf(game, target.id).energy)
            .removeEntity(target.id)
        } else {
          addEnergy(game, bot.id, 150).removeEntity(target.id)
        }
      case (bot: Entity.Bot[F], target: Entity.Fluppet[F]) =>
        addEnergy(game, bot.id, 200).removeEntity(target.id)

      case (minibot: Entity.Minibot[F], target: Entity.Bot[F]) =>
        if (minibot.parent == target.id) {
          // consume child
          addEnergy(game, target.id, stateOf(game, minibot.id).energy)
            .removeEntity(minibot.id)
        } else {
          game.removeEntity(minibot.id)
        }
      case (minibot: Entity.Minibot[F], target: Entity.Minibot[F]) =>
        if (minibot.parent == target.parent) {
          // same side
          bonk(game)
        } else {
          game.removeEntity(minibot.id).removeEntity(target.id)
        }
      case (minibot: Entity.Minibot[F], target: Entity.Fluppet[F]) =>
        addEnergy(game, minibot.id, 200).removeEntity(target.id)

      case (fluppet: Entity.Fluppet[F], target: Entity.Bot[F]) =>
        addEnergy(game, target.id, 200).removeEntity(fluppet.id)

      case (fluppet: Entity.Fluppet[F], target: Entity.Minibot[F]) =>
        addEnergy(game, target.id, 200).removeEntity(fluppet.id)
      case (fluppet: Entity.Fluppet[F], other: Entity[F]) =>
        bonk(game)
    }
  }

  private def move[F[_]](
      game: GameState[F],
      entity: Entity[F],
      move: Command.Move
  ): GameState[F] = {
    if (move.isValid) {
      val result = for {
        currentPosition <- game.occupancies.positionOf(entity)
        newPosition = moveAdjusted(
          game.occupancies.length,
          currentPosition,
          move.deltaX,
          move.deltaY
        )
        newGame = game.occupancies.entityAt(newPosition) match {
          case None =>
            game.focus(_.occupancies).modify { board =>
              board.clear(currentPosition).set(entity, newPosition)
            }
          case Some(other) => collide(game, move, entity, other)

        }
      } yield newGame
      result.getOrElse(game)
    } else {
      game
    }
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
      .map(removeDeadEntities)
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
