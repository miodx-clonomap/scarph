package ohnosequences.scarph.titan.test

import ohnosequences.scarph._
import ohnosequences.typesets._

/* 
  ## The Graph of the Gods Schema

  See [Titan tutorial](https://github.com/thinkaurelius/titan/wiki/Getting-Started) and
  [the code](https://github.com/thinkaurelius/titan/blob/master/titan-core/src/main/java/com/thinkaurelius/titan/example/GraphOfTheGodsFactory.java) for the reference.
*/

object GodsSchema {

  /*
    ### Properties
  */
  case object name extends Property[String]
  case object age extends Property[Integer]
  // instead of this property we will create REAL types (because we are cool)
  // case object `type` extends Property[String]
  case object time extends Property[Integer]
  case object reason extends Property[String]
  // This is something advanced:
  import com.thinkaurelius.titan.core.attribute.Geoshape
  case object place extends Property[Geoshape]

  /*
    ### Vertices
  */
  case object Titan extends VertexType("titan")
  implicit val Titan_name = Titan has name
  implicit val Titan_age  = Titan has age

  case object God extends VertexType("god")
  implicit val God_name = God has name
  implicit val God_age  = God has age

  case object Demigod extends VertexType("demigod")
  implicit val Demigod_name = Demigod has name
  implicit val Demigod_age  = Demigod has age

  case object Human extends VertexType("human")
  implicit val Human_name = Human has name
  implicit val Human_age  = Human has age

  case object Monster extends VertexType("monster")
  implicit val Monster_name = Monster has name
  
  case object Location extends VertexType("location")
  implicit val Location_name = Location has name

  /*
    ### Edges
  */

  /* #### Family relationships */

  /* Gods can have a Titan father */
  case object TitanFather extends ManyToOne(God, "father", Titan)

  /* Demigods can have a God father */
  case object GodFather extends ManyToOne(Demigod, "father", God)

  /* Note, that GodFather and TitanFather have the same label, but the two things are needed,
     because of different source/target types */

  /* Demigods can have a Human mother */
  case object HumanMother extends ManyToOne(Demigod, "mother", Human)

  /* Gods can be brothers with Gods */
  case object Brother extends ManyToMany(God, "brother", God)

  // TODO: other combinations are also possible, but are just not needed for the example


  /* #### Other relationships */

  /* a God can have moster pets */
  case object Pet extends OneToMany(God, "pet", Monster)

  /* Demigods battle with Monsters */
  case object Battled extends ManyToMany(Demigod, "battled", Monster)
  implicit val Battled_time  = Battled has time
  implicit val Battled_place = Battled has place
 

  /* Gods live in some Location and they have a reason for that */
  case object GodLives extends ManyToOne(God, "lives", Location)
  implicit val GodLives_reason = GodLives has reason

  /* Monsters live in some Location (without any reason) */
  case object MonsterLives extends ManyToOne(Monster, "lives", Location)


  /*
    ### Fixed Schema
  */
  val godsGraphSchema = Schema("godsGraphSchema",
    propertyTypes = name :~: age :~: time :~: reason :~: place :~: ∅,
    vertexTypes = Titan :~: God :~: Demigod :~: Human :~: Monster :~: Location :~: ∅,
    edgeTypes = TitanFather :~: GodFather :~: HumanMother :~: Brother :~: Pet :~: Battled :~: GodLives :~: MonsterLives :~: ∅
  )

}
