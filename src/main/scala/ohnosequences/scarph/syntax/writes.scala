package ohnosequences.scarph

import ohnosequences.cosas.types._

case class AddVertexSyntax[G](u: unit := G) extends AnyVal {

  def add[V <: AnyVertex, RV](v: V)
    (implicit
      adder: CanAddVertices[G, V, RV]
    ): V := RV = {
      adder.addVertex(u.value)(v)
    }

  def removeV[V <: AnyVertex, RV](v: V := RV)(implicit
    adder: CanAddVertices[G, V, RV]
  )
  : TensorUnit.type := G =
    unit := adder.removeVertex(u.value)(v)

  def removeE[E <: AnyEdge, RE, RS, RT](e: E := RE)(implicit
    adder: CanAddEdges[RS, E, RE, RT]
  )
  : unit := G =
    { adder.removeEdge(e); u }
}

case class AddEdgeSyntax[E <: AnyEdge](e: E) {

  def add[RE, RS, RT](
    src: E#Source := RS,
    tgt: E#Target := RT
  )(implicit
    adder: CanAddEdges[RS, E, RE, RT]
  ): E := RE = {
    adder.addEdge(e)(src, tgt)
  }
}

case class SetPropertySyntax[E <: AnyGraphElement, RE](e: E := RE) {

  def set[P <: AnyProperty, V <: P#Target#Val](
    p: P,
    v: V
  )(implicit
    setter: CanSetProperties[E, RE, P, V]
  ): E := RE = {
    setter.setProperty(e, p, v)
  }
}
