package ohnosequences.scarph.test

import ohnosequences.scarph._, objects._, morphisms._, evals._
import syntax.morphisms._
import asserts._, twitter._, dummy._, dummy.syntax._

class DummyTests extends org.scalatest.FunSuite {

  val I = unit := DummyUnit
  val du = user := DummyVertex
  val dt = tweet := DummyVertex
  val dp = posted := DummyEdge

  val dages = age := new Integer(2)
  val dnames = name := "Paco"
  val dtexts = text := "Hola!"
  val dtimes = time := "24:00"


  test("dummy evals for the basic structure") {
    import dummy.categoryStructure._
    import queries.categoryStructure._

    assertTaggedEq( eval(q_id)(du), du )
    assertTaggedEq(
      eval(q_comp1)(du),
      du
    )
  }

  test("dummy evals for the tensor structure") {
    import dummy.categoryStructure._
    import dummy.tensorStructure._
    import queries.tensorStructure._

    assertTaggedEq( eval(q_symmetry)(du ⊗ dt), dt ⊗ du )
    assertTaggedEq( eval(q_fromUnit)(I), du )
    assertTaggedEq( eval(q_toUnit)(du), I )
    assertTaggedEq( eval(q_tensor)(du ⊗ du ⊗ du), du ⊗ du ⊗ du )
    assertTaggedEq( eval(q_dupl)(du ⊗ du), du ⊗ du ⊗ du )
    assertTaggedEq( eval(q_match)(du ⊗ du), du )
    assertTaggedEq( eval(q_comp)(du ⊗ du), du )

    // assertTaggedEq( eval(q_trace)(du), du )
  }

  test("dummy evals for the biproduct structure") {
    import dummy.categoryStructure._
    import dummy.biproductStructure._
    import queries.biproductStructure._

    assertTaggedEq( eval(q_inj)(dt), du ⊕ du ⊕ dt )
    assertTaggedEq( eval(q_bip)(du ⊕ du ⊕ dt), du ⊕ du ⊕ dt )
    assertTaggedEq( eval(q_fork)(du ⊕ dt), du ⊕ du ⊕ dt )
    assertTaggedEq( eval(q_merge)(du ⊕ du), du )
    assertTaggedEq( eval(q_comp)(du ⊕ dt), dt )
  }

  test("dummy evals for the graph structure") {
    import dummy.categoryStructure._
    import dummy.graphStructure._
    import queries.graphStructure._

    assertTaggedEq( eval(q_outV)(du), dt )
    assertTaggedEq( eval(q_inV)(dt), du )
    assertTaggedEq( eval(q_compV)(du), du )
    //
    assertTaggedEq( eval(q_outE)(du), dt )
    assertTaggedEq( eval(q_inE)(dt), du )
    assertTaggedEq( eval(q_compE)(du), du )
  }

  test("dummy evals for the property structure") {
    import dummy.categoryStructure._
    import dummy.propertyStructure._
    import queries.propertyStructure._

    // TODO these are methods because of lacking implementations
    def p1 = eval(q_getV)(du)
    def p2 = eval(q_lookupV)(dnames)(eval_lookupV)
    def p3 = eval(q_compV)(dnames)

    def p4 = eval(q_getE)(dp)
    def p5 = eval(q_lookupE)(dtimes)
    def p6 = eval(q_compE)(dp)
    //
    // assertTaggedEq( eval(q_getV)(du), dages )
    // assertTaggedEq( eval(q_lookupV)(dnames), du )
    // assertTaggedEq( eval(q_compV)(dnames), dages )
    //
    // assertTaggedEq( eval(q_getE)(dp), dtimes )
    // assertTaggedEq( eval(q_lookupE)(dtimes), dp )
    // assertTaggedEq( eval(q_compE)(dp), dp )
  }

  test("dummy evals for the predicate structure") {
    import dummy.categoryStructure._
    import dummy.predicateStructure._
    import queries.predicateStructure._

    assertTaggedEq( eval(q_quant)(du), pred := du.value )
    assertTaggedEq( eval(q_coerce)(pred := du.value), du )
    assertTaggedEq( eval(q_comp)(du), du )
  }

  import rewrites._

  case object compositionToLeft extends AnyRecursiveRightAssocRewriteStrategy {

    implicit final def left_bias_assoc[
      F <: AnyGraphMorphism,
      G <: AnyGraphMorphism { type In = F#Out },
      H <: AnyGraphMorphism { type In = G#Out }
    ]
    : (F >=> (G >=> H)) rewriteTo ((F >=> G) >=> H)
    = rewriteTo (
      {
        f_gh: F >=> (G >=> H) => {

          val f  = f_gh.first
          val gh = f_gh.second

          val g = gh.first
          val h = gh.second

          ((f >=> g) >=> h): (F >=> G) >=> H
        }
      }
    )
  }

  ignore("rewriting composition") {

    val morph     = outV(follows) >=> ( inV(follows) >=> ( outV(follows) >=> ( inV(follows) >=> outV(follows) )))
    val shouldBe  = ((( outV(follows) >=> inV(follows) ) >=> outV(follows) ) >=> inV(follows) ) >=> outV(follows)

    val rmorph = apply(compositionToLeft).to(morph)

    info(morph.label)
    info(rmorph.label)

    assert{ apply(compositionToLeft).to(morph) === shouldBe }
  }
}
