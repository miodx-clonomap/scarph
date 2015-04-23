package ohnosequences.scarph

object evals {

  import monoidalStructures._
  import ohnosequences.cosas._, types._, fns._
  import graphTypes._, morphisms._, implementations._, predicates._

  trait AnyMorphismFunctor {

    type Morph <: AnyGraphMorphism
    type OutMorph

    def apply(morph: Morph): OutMorph
  }

  trait AnyEval extends AnyMorphismFunctor {

    type InVal
    type OutVal

    type Input = Morph#In := InVal
    type Output = Morph#Out := OutVal

    type OutMorph = Function1[Input, Output]

    def present(morph: Morph): String
  }

  @annotation.implicitNotFound(msg = "Cannot evaluate morphism ${M} on input ${I}, output ${O}")
  trait EvalOn[I, M <: AnyGraphMorphism, O] extends AnyEval {

    type Morph = M
    type InVal = I
    type OutVal = O
  }

  trait AnyRewrite extends AnyMorphismFunctor {

    type OutMorph <: Morph#In --> Morph#Out
  }

  @annotation.implicitNotFound(msg = "Cannot rewrite morphism ${M} to ${OM}")
  trait Rewrite[M <: AnyGraphMorphism, OM <: M#In --> M#Out] extends AnyRewrite {

    type Morph = M
    type OutMorph = OM
  }


  def rewrite[M <: AnyGraphMorphism, OM <: M#In --> M#Out]
    (m: M)(implicit rewr: Rewrite[M, OM]): OM = rewr(m)


  object evaluate {

    // def apply[I, M <: AnyGraphMorphism, O](m: M)(implicit
    //   ev: EvalOn[I, M, O]
    // ): evaluate[I, M, O] = new evaluate(m, ev)

    def apply[I, M <: AnyGraphMorphism, OM <: M#In --> M#Out, O](m: M)(implicit
      rewrite: Rewrite[M, OM],
      ev: EvalOn[I, OM, O]
    ): evaluate[I, OM, O] = new evaluate(rewrite(m), ev)
  }

  final class evaluate[I, M <: AnyGraphMorphism, O](val f: M, val eval: EvalOn[I, M, O]) {

    final def on(input: M#In := I): M#Out := O = eval(f).apply(input)

    // TODO: this should output the computational behavior of the eval here
    final def evalPlan: String = eval.present(f)
  }


  trait DefaultEvals extends AfterRewritingEvals {

    implicit def id_rewrite[
      M <: AnyGraphMorphism
    ]:  Rewrite[M, M] =
    new Rewrite[M, M] {

      def apply(morph: Morph): OutMorph = morph
    }
  }

  trait AfterRewritingEvals {

    // X = X (does nothing)
    implicit final def eval_id[
      I, X <: AnyGraphObject
    ]:  EvalOn[I, id[X], I] =
    new EvalOn[I, id[X], I] {

      final def apply(morph: Morph): OutMorph = { input: Input => input }

      final def present(morph: Morph): String = morph.label
    }


    // F >=> S
    implicit final def eval_composition[
      I,
      F <: AnyGraphMorphism,
      S <: AnyGraphMorphism { type In = F#Out },
      X, O
    ](implicit
      evalFirst:  EvalOn[I, F, X],
      evalSecond: EvalOn[X, S, O]
    ):  EvalOn[I, F >=> S, O] =
    new EvalOn[I, F >=> S, O] {

      def apply(morph: Morph): OutMorph = { input: Input =>

        val firstResult = evalFirst(morph.first)(input)
        evalSecond(morph.second)(morph.second.in := firstResult.value)
      }

      def present(morph: Morph): String = s"(${evalFirst.present(morph.first)} >=> ${evalSecond.present(morph.second)})"
    }

    // IL ⊗ IR → OL ⊗ OR
    implicit final def eval_tensor[
      IL, IR, I,
      L <: AnyGraphMorphism, R <: AnyGraphMorphism,
      OL, OR, O
    ](implicit
      inTens:  TensorImpl[I, IL, IR],
      outTens: TensorImpl[O, OL, OR],
      evalLeft:  EvalOn[IL, L, OL],
      evalRight: EvalOn[IR, R, OR]
    ):  EvalOn[I, TensorMorph[L, R], O] =
    new EvalOn[I, TensorMorph[L, R], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outTens(
          evalLeft(morph.left)  ( (morph.left.in:  L#In) := inTens.leftProj(input.value) ).value,
          evalRight(morph.right)( (morph.right.in: R#In) := inTens.rightProj(input.value) ).value
        )
      }

      def present(morph: Morph): String = s"(${evalLeft.present(morph.left)} ⊗ ${evalRight.present(morph.right)})"
    }

    // IL ⊕ IR → OL ⊕ OR
    implicit final def eval_biproduct[
      IL, IR, I,
      L <: AnyGraphMorphism, R <: AnyGraphMorphism,
      OL, OR, O
    ](implicit
      inBip:  BiproductImpl[I, IL, IR],
      outBip: BiproductImpl[O, OL, OR],
      evalLeft:  EvalOn[IL, L, OL],
      evalRight: EvalOn[IR, R, OR]
    ):  EvalOn[I, BiproductMorph[L, R], O] =
    new EvalOn[I, BiproductMorph[L, R], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip(
          evalLeft(morph.left)  ( (morph.left.in:  L#In) := inBip.leftProj(input.value) ).value,
          evalRight(morph.right)( (morph.right.in: R#In) := inBip.rightProj(input.value) ).value
        )
      }

      def present(morph: Morph): String = s"(${evalLeft.present(morph.left)} ⊕ ${evalRight.present(morph.right)})"
    }

    // △: X → X ⊗ X
    implicit final def eval_duplicate[
      I, T <: AnyGraphObject, O
    ](implicit
      outTens: TensorImpl[O, I, I]
    ):  EvalOn[I, duplicate[T], O] =
    new EvalOn[I, duplicate[T], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outTens(input.value, input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // ▽: X ⊗ X → X
    implicit final def eval_matchUp[
      I, T <: AnyGraphObject, O
    ](implicit
      tensImpl: TensorImpl[I, O, O],
      matchImpl: MatchUpImpl[O]
    ):  EvalOn[I, matchUp[T], O] =
    new EvalOn[I, matchUp[T], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := matchImpl.matchUp(tensImpl.leftProj(input.value), tensImpl.rightProj(input.value))
      }

      def present(morph: Morph): String = morph.label
    }

    // X → X ⊕ X
    implicit final def eval_fork[
      I, T <: AnyGraphObject, O
    ](implicit
      outBip: BiproductImpl[O, I, I]
    ):  EvalOn[I, fork[T], O] =
    new EvalOn[I, fork[T], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip(input.value, input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // X ⊕ X → X
    implicit final def eval_merge[
      I, T <: AnyGraphObject, O
    ](implicit
      bipImpl: BiproductImpl[I, O, O],
      mergeImpl: MergeImpl[O]
    ):  EvalOn[I, merge[T], O] =
    new EvalOn[I, merge[T], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := mergeImpl.merge(bipImpl.leftProj(input.value), bipImpl.rightProj(input.value))
      }

      def present(morph: Morph): String = morph.label
    }

    // L → L ⊕ R
    implicit final def eval_leftInj[
      L <: AnyGraphObject, R <: AnyGraphObject,
      I, OR, O
    ](implicit
      outBip: BiproductImpl[O, I, OR]
    ):  EvalOn[I, leftInj[L ⊕ R], O] =
    new EvalOn[I, leftInj[L ⊕ R], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip.leftInj(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // R → L ⊕ R
    implicit final def eval_rightInj[
      L <: AnyGraphObject, R <: AnyGraphObject,
      OL, OR, O
    ](implicit
      outBip: BiproductImpl[O, OL, OR]
    ):  EvalOn[OR, rightInj[L ⊕ R], O] =
    new EvalOn[OR, rightInj[L ⊕ R], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip.rightInj(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // L ⊕ R → L
    implicit final def eval_leftProj[
      IL, IR, I,
      L <: AnyGraphObject, R <: AnyGraphObject
    ](implicit
      outBip: BiproductImpl[I, IL, IR]
    ):  EvalOn[I, leftProj[L ⊕ R], IL] =
    new EvalOn[I, leftProj[L ⊕ R], IL] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip.leftProj(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // L ⊕ R → R
    implicit final def eval_rightProj[
      IL, IR, I,
      L <: AnyGraphObject, R <: AnyGraphObject
    ](implicit
      outBip: BiproductImpl[I, IL, IR]
    ):  EvalOn[I, rightProj[L ⊕ R], IR] =
    new EvalOn[I, rightProj[L ⊕ R], IR] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outBip.rightProj(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    // 0 → X
    implicit final def eval_fromZero[
      I, X <: AnyGraphObject, O
    ](implicit
      inZero:  ZeroImpl[I],
      outZero: ZeroImpl[O]
    ):  EvalOn[I, fromZero[X], O] =
    new EvalOn[I, fromZero[X], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outZero()
      }

      def present(morph: Morph): String = morph.label
    }

    // X → 0
    implicit final def eval_toZero[
      I, T, X <: AnyGraphObject, O
    ](implicit
      inZero:  ZeroImpl[I],
      outZero: ZeroImpl[O]
    ):  EvalOn[I, toZero[X], O] =
    new EvalOn[I, toZero[X], O] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := outZero()
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_inE[
      I, E <: AnyEdge, IE, IV
    ](implicit
      vImpl:  VertexInImpl[E, I, IE, IV]
    ):  EvalOn[I, inE[E], IE] =
    new EvalOn[I, inE[E], IE] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := vImpl.inE(input.value, morph.edge)
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_inV[
      I, E <: AnyEdge, IE, IV
    ](implicit
      vImpl:  VertexInImpl[E, I, IE, IV]
    ):  EvalOn[I, inV[E], IV] =
    new EvalOn[I, inV[E], IV] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := vImpl.inV(input.value, morph.edge)
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_outE[
      I, E <: AnyEdge, OE, OV
    ](implicit
      vImpl:  VertexOutImpl[E, I, OE, OV]
    ):  EvalOn[I, outE[E], OE] =
    new EvalOn[I, outE[E], OE] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := vImpl.outE(input.value, morph.edge)
      }

      def present(morph: Morph): String = morph.label
    }



    implicit final def eval_outV[
      I, E <: AnyEdge, OE, OV
    ](implicit
      vImpl:  VertexOutImpl[E, I, OE, OV]
    ):  EvalOn[I, outV[E], OV] =
    new EvalOn[I, outV[E], OV] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := vImpl.outV(input.value, morph.edge)
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_source[
      E <: AnyEdge, I, S, T
    ](implicit
      eImpl: EdgeImpl[I, S, T]
    ):  EvalOn[I, source[E], S] =
    new EvalOn[I, source[E], S] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := eImpl.source(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_target[
      E <: AnyEdge, I, S, T
    ](implicit
      eImpl: EdgeImpl[I, S, T]
    ):  EvalOn[I, target[E], T] =
    new EvalOn[I, target[E], T] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := eImpl.target(input.value)
      }

      def present(morph: Morph): String = morph.label
    }


    // I → X
    implicit final def eval_fromUnit[
      O <: AnyGraphObject, RawObj, RawUnit
    ](implicit
      unitImpl:  UnitImpl[O, RawObj, RawUnit]
    ):  EvalOn[RawUnit, fromUnit[O], RawObj] =
    new EvalOn[RawUnit, fromUnit[O], RawObj] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := unitImpl.fromUnit(input.value, morph.obj)
      }

      def present(morph: Morph): String = morph.label
    }

    // X → I
    implicit final def eval_toUnit[
      O <: AnyGraphObject, RawObj, RawUnit
    ](implicit
      unitImpl:  UnitImpl[O, RawObj, RawUnit]
    ):  EvalOn[RawObj, toUnit[O], RawUnit] =
    new EvalOn[RawObj, toUnit[O], RawUnit] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        morph.out := unitImpl.toUnit(input.value)
      }

      def present(morph: Morph): String = morph.label
    }


    implicit final def eval_get[
      P <: AnyGraphProperty, RawElem, RawValue
    ](implicit
      propImpl: PropertyImpl[P, RawElem, RawValue]
    ):  EvalOn[RawElem, get[P], RawValue] =
    new EvalOn[RawElem, get[P], RawValue] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := propImpl.get(input.value, morph.property)
      }

      def present(morph: Morph): String = morph.label
    }

    implicit final def eval_lookup[
      P <: AnyGraphProperty, RawElem, RawValue
    ](implicit
      propImpl: PropertyImpl[P, RawElem, RawValue]
    ):  EvalOn[RawValue, lookup[P], RawElem] =
    new EvalOn[RawValue, lookup[P], RawElem] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := propImpl.lookup(input.value, morph.property)
      }

      def present(morph: Morph): String = morph.label
    }


    implicit final def eval_quantify[
      P <: AnyPredicate, RawPred, RawElem
    ](implicit
      predImpl: PredicateImpl[P, RawPred, RawElem]
    ):  EvalOn[RawElem, quantify[P], RawPred] =
    new EvalOn[RawElem, quantify[P], RawPred] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := predImpl.quantify(input.value, morph.predicate)
      }

      def present(morph: Morph): String = morph.label
    }


    implicit final def eval_coerce[
      P <: AnyPredicate, RawPred, RawElem
    ](implicit
      predImpl: PredicateImpl[P, RawPred, RawElem]
    ):  EvalOn[RawPred, coerce[P], RawElem] =
    new EvalOn[RawPred, coerce[P], RawElem] {

      def apply(morph: Morph): OutMorph = { input: Input =>
        (morph.out: Morph#Out) := predImpl.coerce(input.value)
      }

      def present(morph: Morph): String = morph.label
    }

  }

}
