package ohnosequences.scarph.test

import ohnosequences.scarph._, objects._, evals._

case object dummy {

  trait Dummy

  case object DummyEdge extends Dummy
  type DummyEdge = DummyEdge.type

  case object DummyVertex extends Dummy
  type DummyVertex = DummyVertex.type


  case object categoryStructure extends CategoryStructure

  case object graphStructure extends GraphStructure {

    type RawEdge = DummyEdge
    type RawSource = DummyVertex
    type RawTarget = DummyVertex

    def outVRaw(edge: AnyEdge)(v: RawSource): RawTarget = DummyVertex
    def inVRaw(edge: AnyEdge)(v: RawTarget): RawSource = DummyVertex

    def outERaw(edge: AnyEdge)(v: RawSource): RawEdge = DummyEdge
    def sourceRaw(edge: AnyEdge)(e: RawEdge): RawSource = DummyVertex

    def inERaw(edge: AnyEdge)(v: RawTarget): RawEdge = DummyEdge
    def targetRaw(edge: AnyEdge)(e: RawEdge): RawTarget = DummyVertex
  }


  case class DummyTensor[L <: Dummy, R <: Dummy](l: L, r: R) extends Dummy

  case object DummyUnit extends Dummy
  type DummyUnit = DummyUnit.type


  implicit def dummyMatch[T <: Dummy]:
      Matchable[T] =
  new Matchable[T] { def matchUp(l: T, r: T): T = l }

  implicit def dummyUnitToEdge[U]:
      FromUnit[U, DummyEdge] =
  new FromUnit[U, DummyEdge] { def fromUnit(u: U, e: AnyGraphObject): T = DummyEdge }

  implicit def dummyUnitToVertex[U]:
      FromUnit[U, DummyVertex] =
  new FromUnit[U, DummyVertex] { def fromUnit(u: U, e: AnyGraphObject): T = DummyVertex }

  implicit def dummyUnitToTensor[U, L <: Dummy, R <: Dummy]
  (implicit
    l: FromUnit[U, L],
    r: FromUnit[U, R]
  ):  FromUnit[U, DummyTensor[L, R]] =
  new FromUnit[U, DummyTensor[L, R]] {

    def fromUnit(u: U, e: AnyGraphObject): T =
      DummyTensor(
        l.fromUnit(u, e),
        r.fromUnit(u, e)
      )
  }

  case object tensorStructure extends TensorStructure {

    type TensorBound = Dummy
    type RawTensor[L <: TensorBound, R <: TensorBound] = DummyTensor[L, R]
    type RawUnit = DummyUnit

    def tensorRaw[L <: TensorBound, R <: TensorBound](l: L, r: R): RawTensor[L, R] = DummyTensor(l, r)
    def leftRaw[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): L = t.l
    def rightRaw[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): R = t.r
    def toUnitRaw[X <: TensorBound](x: X): RawUnit = DummyUnit
  }



  case class DummyBiproduct[L <: Dummy, R <: Dummy](l: L, r: R) extends Dummy

  case object DummyZero extends Dummy
  type DummyZero = DummyZero.type


  implicit def dummyMerge[T <: Dummy]:
      Mergeable[T] =
  new Mergeable[T] { def merge(l: T, r: T): T = r }

  implicit def dummyZeroForEdge:
      ZeroFor[DummyEdge] =
  new ZeroFor[DummyEdge] { def zero(e: AnyGraphObject): T = DummyEdge }

  implicit def dummyZeroForVertex:
      ZeroFor[DummyVertex] =
  new ZeroFor[DummyVertex] { def zero(e: AnyGraphObject): T = DummyVertex }

  case object biproductStructure extends BiproductStructure {

    type BiproductBound = Dummy
    type RawBiproduct[L <: BiproductBound, R <: BiproductBound] = DummyBiproduct[L, R]
    type RawZero = DummyZero

    def biproductRaw[L <: BiproductBound, R <: BiproductBound](l: L, r: R): RawBiproduct[L, R] =
      DummyBiproduct[L, R](l, r)

    def leftProjRaw[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): L = t.l
    def rightProjRaw[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): R = t.r

    def toZeroRaw[X <: BiproductBound](x: X): RawZero = DummyZero
  }


  case object propertyStructure {
    import morphisms._

    implicit def eval_getV[VT, P <: AnyProperty { type Owner <: AnyVertex }]:
        Eval[DummyVertex, get[P], Seq[VT]] =
    new Eval[DummyVertex, get[P], Seq[VT]] {

      def rawApply(morph: InMorph): InVal => OutVal = _ => Seq[VT]()

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    /*
    implicit def eval_getE[P <: AnyProperty { type Owner <: AnyEdge }]:
        Eval[DummyEdge, get[P], Seq[P#Value#Raw]] =
    new Eval[DummyEdge, get[P], Seq[P#Value#Raw]] {

      def rawApply(morph: InMorph): InVal => OutVal = _ => Seq[P#Value#Raw]()

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }
    */


    implicit def eval_lookupV[VT, P <: AnyProperty.withRaw[VT] { type Owner <: AnyVertex }]:
        Eval[Seq[VT], lookup[P], DummyVertex] =
    new Eval[Seq[VT], lookup[P], DummyVertex] {

      def rawApply(morph: InMorph): InVal => OutVal = _ => DummyVertex

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    /*
    implicit def eval_lookupE[VT, P <: AnyProperty.withRaw[VT] { type Owner <: AnyEdge }]:
        Eval[Seq[VT], lookup[P], DummyEdge] =
    new Eval[Seq[VT], lookup[P], DummyEdge] {

      def rawApply(morph: InMorph): InVal => OutVal = _ => DummyEdge

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }
    */

  }

  object syntax {
    import ohnosequences.cosas.types._
    import ohnosequences.scarph.objects._

    implicit def dummyObjectValOps[F <: AnyGraphObject, VF <: Dummy](vf: F := VF):
      DummyObjectValOps[F, VF] =
      DummyObjectValOps[F, VF](vf.value)

    case class DummyObjectValOps[F <: AnyGraphObject, VF <: Dummy](vf: VF) extends AnyVal {

      def ⊗[S <: AnyGraphObject, VS <: Dummy](vs: S := VS): (F ⊗ S) := DummyTensor[VF, VS] =
        new Denotes( DummyTensor(vf, vs.value) )

      def ⊕[S <: AnyGraphObject, VS <: Dummy](vs: S := VS): (F ⊕ S) := DummyBiproduct[VF, VS] =
        new Denotes( DummyBiproduct(vf, vs.value) )
    }
  }

}
