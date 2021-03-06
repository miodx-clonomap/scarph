package ohnosequences.scarph.test

import ohnosequences.scarph._, syntax._
import ohnosequences.scarph.test.twitter._

case object queries {

  class TestBlock(val label: String)


  object categoryStructure extends TestBlock("Categorical structure"){

    val q_id = id(user)
    val q_comp1 = q_id >=> q_id
    val q_comp2 = q_comp1 >=> q_id >=> q_comp1
  }

  object tensorStructure extends TestBlock("Tensor structure") {

    val q_symmetry = symmetry(user, tweet)
    val q_fromUnit = fromUnit(user)
    val q_toUnit = toUnit(user)
    val q_tensor = id(user) ⊗ id(user) ⊗ id(user)
    val q_dupl = duplicate(user) ⊗ id(user)
    val q_match = matchUp(user)
    val q_comp =
      q_dupl >=>
      q_tensor >=>
      (id(user ⊗ user) ⊗ duplicate(user)) >=>
      (q_match ⊗ q_match) >=>
      q_match
    val q_trace = tensorTrace(toUnit(user) ⊗ fromUnit(user) >=> symmetry(unit, user))
    // val q_trace_Wrong = tensorTrace(q_match)
  }

  object biproductStructure extends TestBlock("Biproduct structure") {

    val q_inj   = rightInj((user ⊕ user) ⊕ tweet)
    val q_bip   = id(user) ⊕ id(user) ⊕ id(tweet)
    val q_fork  = fork(user) ⊕ id(tweet)
    val q_merge = merge(user)
    val q_comp  =
      q_fork >=>
      q_bip >=>
      (id(user ⊕ user) ⊕ fork(tweet)) >=>
      (merge(user) ⊕ merge(tweet)) >=>
      rightProj(user ⊕ tweet)
  }

  object graphStructure extends TestBlock("Graph structure") {

    val q_outV  = outV(posted)
    val q_inV   = inV(posted)
    val q_compV = q_outV >=> q_inV

    val q_outE  = outE(posted) >=> target(posted)
    val q_inE   = inE(posted) >=> source(posted)
    val q_compE = q_outE >=> q_inE
  }

  object propertyStructure extends TestBlock("Property structure") {

    val q_getV = get(user.age)
    val q_lookupV = inV(user.name)
    val q_compV = q_lookupV >=> q_getV

    val q_getE = get(posted.time)
    val q_lookupE = lookup(posted.time)
    val q_compE = q_getE >=> q_lookupE
  }

  object predicateStructure extends TestBlock("Predicate structure") {

    val pred = user ? (user.age > 10) and (user.name =/= "")

    val q_quant = quantify(pred)
    val q_coerce = coerce(pred)
    val q_comp = q_quant >=> q_coerce
  }
}
