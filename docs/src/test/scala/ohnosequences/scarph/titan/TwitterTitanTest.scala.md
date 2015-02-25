
```scala
package ohnosequences.scarph.test.impl

import com.thinkaurelius.titan.core.{ TitanFactory, TitanGraph, TitanVertex, TitanEdge }
import com.thinkaurelius.titan.core.PropertyKey
import com.thinkaurelius.titan.core.schema.TitanManagement

import ohnosequences.cosas._, types._

import ohnosequences.{ scarph => s }
import s.graphTypes._, s.steps._, s.containers._, s.combinators._, s.indexes._, s.syntax
import s.syntax._, conditions._, predicates._, paths._, graphTypes._
import s.impl, impl.titan.schema._, impl.titan.predicates._, impl.titan.evals._
import s.test.Twitter._


trait AnyTitanTestSuite 
      extends org.scalatest.FunSuite 
      with org.scalatest.BeforeAndAfterAll 
      with ohnosequences.scarph.test.ScalazEquality {

  val g: TitanGraph = TitanFactory.open("inmemory")

  val titanTwitter = twitter := g

  override def beforeAll() {
    titanTwitter.createSchema(twitter)

    // loading data from a prepared GraphSON file
    import com.tinkerpop.blueprints.util.io.graphson._
    GraphSONReader.inputGraph(g, getClass.getResource("/twitter_graph.json").getPath)
  }

  override def afterAll() {
    g.shutdown

    // // NOTE: uncommend if you want to add data to the GraphSON:
    // import com.tinkerpop.blueprints.util.io.graphson._
    // GraphSONWriter.outputGraph(g, "graph_compact.json", GraphSONMode.COMPACT)
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

class TitanTestSuite extends AnyTitanTestSuite {

  // checks existence and arity
  def checkEdgeLabel[ET <: AnyEdge](mgmt: TitanManagement, et: ET)
    (implicit multi: EdgeTypeMultiplicity[ET]) = {

    assert{ mgmt.containsRelationType(et.label) }

    assertResult(multi(et)) {
      mgmt.getEdgeLabel(et.label).getMultiplicity
    }
  }

  // checks existence and dataType
  def checkPropertyKey[P <: AnyGraphProperty](mgmt: TitanManagement, p: P)
    (implicit cc: scala.reflect.ClassTag[P#Raw]) = {

    assert{ mgmt.containsRelationType(p.label) }

    val pkey: PropertyKey = mgmt.getPropertyKey(p.label)

    assertResult( com.thinkaurelius.titan.core.Cardinality.SINGLE ) { 
      pkey.getCardinality 
    }

    assertResult( cc.runtimeClass.asInstanceOf[Class[P#Raw]] ) {
      pkey.getDataType
    }
  }

  import ohnosequences.cosas._, fns._
  import ohnosequences.cosas.ops.typeSets.MapToList
  // checks existence, type and the indexed property
  def checkCompositeIndex[Ix <: AnyCompositeIndex](mgmt: TitanManagement, ix: Ix)
    (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) = {

    assert{ mgmt.containsGraphIndex(ix.label) }

    val index = mgmt.getGraphIndex(ix.label)
    // TODO: check for mixed indexes and any other stuff
    assert{ index.isCompositeIndex }
    assert{ index.isUnique == ix.uniqueness.bool }

    val ixPropertyKeys: Set[PropertyKey] = 
      propLabels(ix.properties).map{ mgmt.getPropertyKey(_) }.toSet

    assert{ index.getFieldKeys.toSet == ixPropertyKeys }
    // println(ixPropertyKeys.mkString(s"[${ix.label}] property keys: {", ", ", "}"))
  }

  // TODO: make it a graph op: checkSchema
  test("check schema keys/labels") {

    val mgmt = g.getManagementSystem

    checkPropertyKey(mgmt, name)
    checkPropertyKey(mgmt, age)
    checkPropertyKey(mgmt, text)
    checkPropertyKey(mgmt, url)
    checkPropertyKey(mgmt, time)

    checkEdgeLabel(mgmt, posted)
    checkEdgeLabel(mgmt, follows)

    assert{ mgmt.containsVertexLabel(user.label) }
    assert{ mgmt.containsVertexLabel(tweet.label) }

    checkCompositeIndex(mgmt, userByName)
    checkCompositeIndex(mgmt, tweetByText)
    checkCompositeIndex(mgmt, postedByTime)
    checkCompositeIndex(mgmt, userByNameAndAge)

    mgmt.commit
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////

  object TestContext {

    // predicates for quering vertices
    val askEdu = user ? (name === "@eparejatobes")
    val askAlexey = user ? (name === "@laughedelic")
    val askKim = user ? (name === "@evdokim")

    val askTweet = tweet ? (text === "back to twitter :)")

    // predicates for quering edges
    val askPost = posted ? (time === "13.11.2012")

    // prepared test queries (they can be reused for different tests)
    val userName = Get(name)
    val postAuthor = Source(posted)
    val postAuthorName = postAuthor >=> userName

    val edu  = user := twitter.query(askEdu).evalOn( titanTwitter ).value.head
    val post = posted := twitter.query(askPost).evalOn( titanTwitter ).value.head
    val twt  = tweet := twitter.query(askTweet).evalOn( titanTwitter ).value.head
  }

  test("check what we got from the index queries") {
    import TestContext._
```

Evaluating steps:

```scala
    assert{ Get(name).evalOn( edu ) == name("@eparejatobes") }
    assert{ Source(posted).evalOn( post ) == edu }
```

Composing steps:

```scala
    val posterName = Source(posted) >=> Get(name)
    assert{ posterName.evalOn( post ) == (name := "@eparejatobes") }

    assertResult( OneOrNone.of(user) := None ){ 
      twitter.query(userByNameAndAge, user ? (name === "@eparejatobes") and (age === 5))
        .evalOn( titanTwitter )
    }
    assertResult( OneOrNone.of(user) := Some(edu.value) ){ 
      twitter.query(userByNameAndAge, user ? (age === 95) and (name === "@eparejatobes"))
        .evalOn( titanTwitter )
    }

    assert{ userName.evalOn( edu ) == name("@eparejatobes") }
    assert{ postAuthor.evalOn( post ) == edu }

    assert{ postAuthorName.evalOn( post ) == name("@eparejatobes") }
  }

  test("cool queries dsl") {
    import TestContext._

    // element op:
    val userName = user.get(name)
    assert{ userName.evalOn( edu ) == name("@eparejatobes") }

    // edge op:
    val posterName = posted.src.get(name)
    assert{ posterName.evalOn( post ) == name("@eparejatobes") }

    // vertex op:
    val friendsPosts =
      user.outE( follows )
          .flatMap( follows.tgt )
          .flatMap( user.outE(posted)
          .flatMap( posted.tgt ) )
    assert{ friendsPosts.out == ManyOrNone.of(tweet) }

    // testing vertex query
    val vertexQuery = user.outE(posted ? (time === "27.10.2013")).map( posted.get(url) )
    // NOTE: scalaz equality doesn understand that these are the same types, so there are just two simple checks:
    implicitly[ vertexQuery.Out ≃ ManyOrNone.Of[url.type] ]
    assert{ vertexQuery.out == ManyOrNone.of(url) }
    assert{ vertexQuery.evalOn( edu ) == (ManyOrNone.of(url) := Stream("https://twitter.com/eparejatobes/status/394430900051927041")) }
  }

  test("evaluating MapOver") {
    import TestContext._

    assertResult( OneOrNone.of(user) := (Option("@eparejatobes")) ){ 
      MapOver(Get(name), OneOrNone).evalOn( 
        OneOrNone.of(user) := (Option(edu.value))
      )
    }

  }

  test("checking combination of Composition and MapOver") {
    import TestContext._

    assertResult( ManyOrNone.of(user) := Stream("@laughedelic", "@evdokim") ){ 
      val q = user.outE(follows)
      (q >=> MapOver(follows.tgt.get(name), q.out.container)).evalOn( edu )
    }

    assertResult( ManyOrNone.of(user) := (Stream("@laughedelic", "@evdokim")) ){ 
      user.outE(follows).map( follows.tgt.get(name) ).evalOn( edu )
    }

    assertResult( ManyOrNone.of(age) := Stream(5, 22) ){ 
      twitter.query(user ? (age < 80)).map( Get(age) ).evalOn( titanTwitter )
    }
    assertResult( ManyOrNone.of(age) := Stream(22) ){ 
      twitter.query(user ? (age < 80) and (age > 10)).map( Get(age) ).evalOn( titanTwitter)
    }
    assertResult( ManyOrNone.of(age) := Stream(5, 22) ){ 
      twitter.query(user ? (age between (3, 25))).map( Get(age) ).evalOn( titanTwitter)
    }

  }

  test("flattening after double map") {
    import TestContext._

    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      Flatten(
        twitter.query(askEdu)
          .map( user.outV(follows) )
      ).map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Same with .flatten syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      twitter.query(askEdu)
        .map( user.outV(follows) )
        .flatten
        .map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Same with .flatMap syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      twitter.query(askEdu)
        .flatMap( user.outV(follows) )
        .map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Flattening with ManyOrNone × ExactlyOne:
    val followersNames = user
      .outV( follows )
      .map( user.get(name) )

    implicitly[ followersNames.Out ≃ ManyOrNone.Of[ExactlyOne.Of[name.type]] ]
    assert{ followersNames.out == ManyOrNone.of(ExactlyOne.of(name)) }

    implicitly[ followersNames.Out ≃ ManyOrNone.Of[name.type] ]
    assert{ followersNames.out == ManyOrNone.of(name) }

    assertResult( ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim") ){ 
      followersNames.flatten.evalOn( edu )
    }

    // Flattening ExactlyOne × ExactlyOne:
    val posterName = posted
      .src
      .map( user.get(name) )

    assert{ posterName.out == name }

    assertResult( name := "@eparejatobes" ){ 
      posterName.evalOn( post )
    }

    assertResult( name := "@eparejatobes" ){ 
      posterName.flatten.evalOn( post )
    }

    // TODO: test all container combinations

  }

  test("type-safe equality for labeled values") {

    assertTypeError("""
      (ManyOrNone.of(user) := "hola") === (user := "hola")
    """)

    assertTypeError("""
      name("hola") === text("hola")
    """)

    assertTypeError("""
      (ManyOrNone.of(user) := "yuhuu") === (ManyOrNone.of(user) := 12)
    """)
  }

  test("parallel combinator") {
    import TestContext._

    // friends' names
    val friendsNames = user
      .outV(follows)
      .map( user.get(name) )

    // friends' ages
    val friendsAges = user
      .outV(follows)
      .map( user.get(age) )

    val result =
      (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ⊗
      (ManyOrNone.of(age) := Stream(5, 22)) 

    // using explicit Par
    assertResult(result) {
      (friendsNames ⊗ friendsAges).evalOn( edu ⊗ edu )
    }

    // now using forkMap:
    assertResult(result) {
      user
        .outV(follows)
        .forkMap( user.get(name) ⊗ user.get(age) )
      .evalOn( edu )
    }

    // now testing just fork:
    assertResult( (name := "@eparejatobes") ⊗ (age := 95) ) {
      tweet
        .inV(posted)  // getting exactly one author
        .fork( user.get(name) ⊗ user.get(age) )
      .evalOn( twt )
    }
  }

  test("choice combinator") {
    import TestContext._
    import scalaz._

    // friends' names
    val friendsNames = user
      .outV(follows)
      .map( user.get(name) )

    // friends' ages
    val friendsAges = user
      .outV(follows)
      .map( user.get(age) )

    // just choosing left or right:
    assertResult( ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim") ) {
      user.left(friendsNames ⊕ friendsAges).evalOn( edu )
    }

    assertResult( ManyOrNone.of(age) := Stream(5, 22) ) {
      user.right(friendsNames ⊕ friendsAges).evalOn( edu )
    }

  }

}

```


------

### Index

+ src
  + test
    + scala
      + ohnosequences
        + scarph
          + [ContainersTest.scala][test/scala/ohnosequences/scarph/ContainersTest.scala]
          + [ScalazEquality.scala][test/scala/ohnosequences/scarph/ScalazEquality.scala]
          + titan
            + [TwitterTitanTest.scala][test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]
          + [TwitterSchema.scala][test/scala/ohnosequences/scarph/TwitterSchema.scala]
    + resources
  + main
    + scala
      + ohnosequences
        + scarph
          + [GraphTypes.scala][main/scala/ohnosequences/scarph/GraphTypes.scala]
          + [Containers.scala][main/scala/ohnosequences/scarph/Containers.scala]
          + impl
            + titan
              + [Schema.scala][main/scala/ohnosequences/scarph/impl/titan/Schema.scala]
              + [Evals.scala][main/scala/ohnosequences/scarph/impl/titan/Evals.scala]
              + [Predicates.scala][main/scala/ohnosequences/scarph/impl/titan/Predicates.scala]
          + [Paths.scala][main/scala/ohnosequences/scarph/Paths.scala]
          + [Indexes.scala][main/scala/ohnosequences/scarph/Indexes.scala]
          + [Evals.scala][main/scala/ohnosequences/scarph/Evals.scala]
          + [Conditions.scala][main/scala/ohnosequences/scarph/Conditions.scala]
          + [Steps.scala][main/scala/ohnosequences/scarph/Steps.scala]
          + [Predicates.scala][main/scala/ohnosequences/scarph/Predicates.scala]
          + [Schemas.scala][main/scala/ohnosequences/scarph/Schemas.scala]
          + [Combinators.scala][main/scala/ohnosequences/scarph/Combinators.scala]
          + syntax
            + [GraphTypes.scala][main/scala/ohnosequences/scarph/syntax/GraphTypes.scala]
            + [Paths.scala][main/scala/ohnosequences/scarph/syntax/Paths.scala]
            + [Conditions.scala][main/scala/ohnosequences/scarph/syntax/Conditions.scala]
            + [Predicates.scala][main/scala/ohnosequences/scarph/syntax/Predicates.scala]

[test/scala/ohnosequences/scarph/ContainersTest.scala]: ../ContainersTest.scala.md
[test/scala/ohnosequences/scarph/ScalazEquality.scala]: ../ScalazEquality.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: TwitterTitanTest.scala.md
[test/scala/ohnosequences/scarph/TwitterSchema.scala]: ../TwitterSchema.scala.md
[main/scala/ohnosequences/scarph/GraphTypes.scala]: ../../../../../main/scala/ohnosequences/scarph/GraphTypes.scala.md
[main/scala/ohnosequences/scarph/Containers.scala]: ../../../../../main/scala/ohnosequences/scarph/Containers.scala.md
[main/scala/ohnosequences/scarph/impl/titan/Schema.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/Schema.scala.md
[main/scala/ohnosequences/scarph/impl/titan/Evals.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/Evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/Predicates.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/Predicates.scala.md
[main/scala/ohnosequences/scarph/Paths.scala]: ../../../../../main/scala/ohnosequences/scarph/Paths.scala.md
[main/scala/ohnosequences/scarph/Indexes.scala]: ../../../../../main/scala/ohnosequences/scarph/Indexes.scala.md
[main/scala/ohnosequences/scarph/Evals.scala]: ../../../../../main/scala/ohnosequences/scarph/Evals.scala.md
[main/scala/ohnosequences/scarph/Conditions.scala]: ../../../../../main/scala/ohnosequences/scarph/Conditions.scala.md
[main/scala/ohnosequences/scarph/Steps.scala]: ../../../../../main/scala/ohnosequences/scarph/Steps.scala.md
[main/scala/ohnosequences/scarph/Predicates.scala]: ../../../../../main/scala/ohnosequences/scarph/Predicates.scala.md
[main/scala/ohnosequences/scarph/Schemas.scala]: ../../../../../main/scala/ohnosequences/scarph/Schemas.scala.md
[main/scala/ohnosequences/scarph/Combinators.scala]: ../../../../../main/scala/ohnosequences/scarph/Combinators.scala.md
[main/scala/ohnosequences/scarph/syntax/GraphTypes.scala]: ../../../../../main/scala/ohnosequences/scarph/syntax/GraphTypes.scala.md
[main/scala/ohnosequences/scarph/syntax/Paths.scala]: ../../../../../main/scala/ohnosequences/scarph/syntax/Paths.scala.md
[main/scala/ohnosequences/scarph/syntax/Conditions.scala]: ../../../../../main/scala/ohnosequences/scarph/syntax/Conditions.scala.md
[main/scala/ohnosequences/scarph/syntax/Predicates.scala]: ../../../../../main/scala/ohnosequences/scarph/syntax/Predicates.scala.md