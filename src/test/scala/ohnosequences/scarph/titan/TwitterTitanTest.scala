package ohnosequences.scarph.test.impl

import com.thinkaurelius.titan.core.{ TitanFactory, TitanGraph, TitanVertex, TitanEdge }
import com.thinkaurelius.titan.core.schema.TitanManagement

import ohnosequences.cosas._, types._

import ohnosequences.{ scarph => s }
import s.graphTypes._, s.steps._, s.containers._, s.combinators._, s.indexes._, s.syntax
import s.impl, impl.titan.schema._, impl.titan.predicates._
import s.test.Twitter, Twitter._


trait AnyTitanTestSuite 
      extends org.scalatest.FunSuite 
      with org.scalatest.BeforeAndAfterAll 
      with ohnosequences.scarph.test.ScalazEquality {

  val g: TitanGraph = TitanFactory.open("inmemory")

  override def beforeAll() {
    g.createSchema(Twitter.schema)

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

// class TitanTestSuite extends org.scalatest.Suites(
//   new TitanSchemaTestSuite,
//   new TitanQueriesTestSuite
// ) with org.scalatest.SequentialNestedSuiteExecution

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

    assertResult(cc.runtimeClass.asInstanceOf[Class[P#Raw]]) {
      mgmt.getPropertyKey(p.label).getDataType
    }
  }

  // checks existence, type and the indexed property
  def checkSimpleIndex[Ix <: AnySimpleIndex](mgmt: TitanManagement, ix: Ix) = {

    assert{ mgmt.containsGraphIndex(ix.label) }

    val index = mgmt.getGraphIndex(ix.label)
    // TODO: check for mixed indexes and any other stuff
    assert{ index.isCompositeIndex }
    assert{ index.getFieldKeys.toSet == Set(mgmt.getPropertyKey(ix.property.label)) }
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

    checkSimpleIndex(mgmt, userByName)
    checkSimpleIndex(mgmt, tweetByText)
    checkSimpleIndex(mgmt, postedByTime)

    mgmt.commit
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////

  import syntax.conditions._
  import syntax.predicates._

  object TestContext {
    val evals = impl.titan.evals(g); import evals._

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

    val edu  = user := Query(user).evalOn(askEdu).value.head
    val post = posted := Query(posted).evalOn(askPost).value.head
    val twt  = tweet := Query(tweet).evalOn(askTweet).value.head
  }

  test("check what we got from the index queries") {
    import TestContext._, evals._

    /* Evaluating steps: */
    assert{ Get(name).evalOn(edu) == name("@eparejatobes") }
    assert{ Source(posted).evalOn(post) == edu }

    /* Composing steps: */
    val posterName = Source(posted) >=> Get(name)
    assert{ posterName.evalOn(post) == (name := "@eparejatobes") }

    assert{ Query(user).evalOn(user ? (name === "@eparejatobes") and (age === 5)).value == Stream() }
    assert{ Query(user).evalOn(user ? (name === "@eparejatobes") and (age === 95)).value == Stream(edu.value) }

    assert{ userName.evalOn(edu) == name("@eparejatobes") }
    assert{ postAuthor.evalOn(post) == edu }

    assert{ postAuthorName.evalOn(post) == name("@eparejatobes") }

    // this query returns a list of 4 Edus, so we comare it as a set
    // assert{ (OutE(any(posted)) >=> postAuthorName).evalOn(edu).toSet == Set(name("@eparejatobes")) }

    // assert{ tweetAuthorName.evalOn(twt) == name("@eparejatobes") }
  }

  test("cool queries dsl") {
    import TestContext._, evals._
    import syntax.paths._

    // element op:
    val userName = user.get(name)
    assert{ userName.evalOn(edu) == name("@eparejatobes") }

    // edge op:
    val posterName = posted.src.get(name)
    assert{ posterName.evalOn(post) == name("@eparejatobes") }

    // vertex op:
    val friendsPosts =
      user.outE( any(follows) )
          .flatMap( follows.tgt )
          .flatMap( user.outE( any(posted) )
          .flatMap( posted.tgt ) )
    assert{ friendsPosts.out == ManyOrNone.of(tweet) }

    import scalaz.Scalaz._
    // testing vertex query
    val vertexQuery = user.outE(posted ? (time === "27.10.2013")).map( posted.get(url) )
    println(vertexQuery.out)
    // NOTE: scalaz equality doesn understand that these are the same types, so there are just two simple checks:
    // implicitly[ vertexQuery.Out ≃ ManyOrNone#Of[url.type] ]
    // assert{ vertexQuery.evalOn(edu) == (ManyOrNone.of(url) := Stream("https://twitter.com/eparejatobes/status/394430900051927041")) }
  }

  test("evaluating MapOver") {
    import TestContext._, evals._
    import scalaz.Scalaz._

    assertResult( OneOrNone.of(user) := (Option("@eparejatobes")) ){ 
      MapOver(Get(name), OneOrNone).evalOn( 
        OneOrNone.of(user) := (Option(edu.value))
      )
    }

  }

  test("checking combination of Composition and MapOver") {
    import TestContext._, evals._
    import scalaz.Scalaz._
    import syntax.paths._

    assertResult( ManyOrNone.of(user) := (Stream("@eparejatobes")) ){ 
      val q = Query(user)
      (q >=> MapOver(Get(name), q.out.container)).evalOn( askEdu )
    }

    assertResult( ManyOrNone.of(user) := (Stream("@eparejatobes")) ){ 
      Query(user).map(Get(name)).evalOn( askEdu )
    }

    val userAges = Query(user).map( Get(age) )
    assert{ userAges.out == ManyOrNone.of(age) }

    assertResult( ManyOrNone.of(age) := Stream(5, 22) ){ 
      userAges.evalOn(user ? (age < 80))
    }
    assertResult( ManyOrNone.of(age) := Stream(22) ){ 
      userAges.evalOn(user ? (age < 80) and (age > 10))
    }

  }

  test("flattening after double map") {
    import TestContext._, evals._
    import scalaz.Scalaz._
    import syntax.paths._

    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      Flatten(
        Query(user).map( user.outE(any(follows)) )
      ).map( Target(follows) >=> Get(name) )
      .evalOn( askEdu )
    }

    // Same with .flatten syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      Query(user)
        .map( user.outE(any(follows)) )
        .flatten
        .map( follows.tgt.get(name) )
      .evalOn( askEdu )
    }

    // Same with .flatMap syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      Query(user)
        .flatMap( user.outE(any(follows)) )
        .map( follows.tgt.get(name) )
      .evalOn( askEdu )
    }

    // with ExactlyOne:
    val followersNames = user
      .outE( any(follows) )
      .map( follows.tgt.get(name) )

    assert{ followersNames.out == ManyOrNone.of(name) }

    // assertResult( ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim") ){ 
    //   followersNames.flatten.evalOn( edu )
    // }

    // NOTE: this doesn't evaluate, because the map is unnecessary
    // therefore there are no sensible situations when you want to flatten Id[Id[]]
    val posterName = posted
      .src
      .map( user.get(name) )

    assert{ posterName.out == name }

    // assertResult( ExactlyOne(name) := Stream("@eparejatobes") ){ 
    //   posterName.evalOn( post )
    // }

    // TODO: test all container combinations

  }

  test("type-safe equality for labeled values") {

    assertTypeError("""
      (ManyOrNone(user) := "hola") === (ExactlyOne(user) := "hola")
    """)

    assertTypeError("""
      name("hola") === text("hola")
    """)

    assertTypeError("""
      (ManyOrNone(user) := "yuhuu") === (ManyOrNone(user) := 12)
    """)
  }

}
