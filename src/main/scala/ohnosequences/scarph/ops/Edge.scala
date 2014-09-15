package ohnosequences.scarph.ops

import ohnosequences.scarph._, AnyEdge._
import ohnosequences.pointless._, AnyWrap._

object edge {

  trait GetSource[E <: AnyEdge] extends Fn1[RawOf[E]] with Out[ValueOf[SourceOf[E]]]

  trait GetTarget[E <: AnyEdge] extends Fn1[RawOf[E]] with Out[ValueOf[TargetOf[E]]]

  trait GetEdgeProperty[E <: AnyEdge, P <: AnyProperty]
    extends Fn2[RawOf[E], P] with Out[ValueOf[P]]

}
