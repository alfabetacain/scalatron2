package dk.alfabetacain.scalatron2

import io.estatico.newtype.macros.newtype
import cats.effect.std.Random
import java.util.UUID

package object core {

  @newtype class EntityId(value: UUID)

  object EntityId {
    import io.estatico.newtype.ops._
    def generate: EntityId = UUID.randomUUID().coerce
  }
}
