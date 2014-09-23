package ohnosequences.scarph.impl.titan.ops

import ohnosequences.pointless._, AnyTypeSet._, AnyWrap._
import ohnosequences.scarph._, AnyPropertiesHolder._, AnyEdge._
import ohnosequences.scarph.impl.titan._, AnyTitanVertex._

object query {
  // import ohnosequences.scarph.ops.edge._

  import com.tinkerpop.blueprints.Direction

  /* Getting source vertex */
  // implicit def getSource[ET <: AnyEdgeType, Q <: GetSource[ET], E <: AnyTitanEdge { type Tpe = ET },  S <: AnyTitanVertex with AnyVertex.ofType[ET#SourceType]]:
  implicit def getSource[E <: AnyTitanEdge, Q <: GetSource[E#Tpe], S <: AnyTitanVertex with AnyVertex.ofType[E#Tpe#SourceType]]:
        EvalQuery[Q, E, S] =
    new EvalQuery[Q, E, S] {

      def apply(eraw: In1): Out = 
        valueOf[S](eraw.getVertex(Direction.OUT))
    }

  // /* Getting target vertex */
  // implicit def targetGetter[E <: AnyTitanEdge]:
  //       GetTarget[E] =
  //   new GetTarget[E] {
  //     def apply(raw: RawOf[E]): Out = valueOf[TargetOf[E]](raw.getVertex(Direction.IN))
  //   }

}
