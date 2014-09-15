package ohnosequences.scarph.test

import ohnosequences.scarph._
import ohnosequences.pointless._, AnyTypeSet._

object TwitterSchema {

  case object name extends Property[String]
  case object age  extends Property[Integer]
  case object text extends Property[String]
  case object url  extends Property[String]

  // case class Date(day: Integer, month: Integer, year: Integer)
  case object time extends Property[String]

  case object User  extends VertexType("user", name :~: age :~: ∅)
  case object Tweet extends VertexType("tweet", text :~: ∅)

  case object Posted  extends EdgeType(User, "posted", Tweet, time :~: url :~: ∅) with OneIn with ManyOut
  case object Follows extends EdgeType(User, "follows", User, ∅) with ManyIn with ManyOut

  case object UserNameIx extends CompositeIndex(User, name)
  case object TweetTextIx extends CompositeIndex(Tweet, text)
  // case object PostedTimeIx extends CompositeIndex(Posted, time)

  val schema = GraphSchema("twitter",
    vertexTypes = User :~: Tweet :~: ∅,
    edgeTypes = Posted :~: Follows :~: ∅,
    indexes = UserNameIx :~: TweetTextIx :~: ∅
  )

}
