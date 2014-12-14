package ohnosequences.scarph

import ohnosequences.scarph.steps._

/* This is any graph type that can have properties, i.e. vertex of edge type */
trait AnyElementType extends AnyLabelType 

/* 
  Property is assigned to one element type and has a raw representation 
  I'm tempted to make this a kind of edge
*/
// TODO: edge-like!
trait AnyProp extends AnyLabelType {

  type Raw

  type Owner <: AnyElementType
  val  owner: Owner
}

abstract class PropertyOf[O <: AnyElementType](val owner: O) extends AnyProp {
  
  type Owner = O

  val label = this.toString
}


/* Vertex type is very simple */
trait AnyVertexType extends AnyElementType

abstract class VertexType extends AnyVertexType {

  val label = this.toString
}

/* Edges connect vertices and have in/out arities */
trait AnyEdgeType extends AnyElementType {

  /* The source vertex for this edge */
  type Source <: AnyVertexType
  val  source: Source
  /* This is the arity for incoming edges */
  type InC <: AnyContainer
  val  inC: InC

  /* The target vertex for this edge */
  type Target <: AnyVertexType
  val  target: Target
  /* This is the arity for outgoing edges */
  type OutC <: AnyContainer
  val  outC: OutC
}

class EdgeType[
  I <: AnyVertexType,
  InC0 <: AnyContainer,
  O <: AnyVertexType,
  OutC0 <: AnyContainer
](val inC: InC0,
  val source: I,
  val outC: OutC0,
  val target: O
) extends AnyEdgeType {

  type Source = I
  type InC = InC0

  type Target = O
  type OutC = OutC0

  val label = this.toString
}


// TODO: HList-like with bound on vertices, another for paths etc

trait AnyParV extends AnyLabelType {

  type First <: AnyLabelType
  val  first: First

  type Second <: AnyLabelType
  val  second: Second
}

case class ParV[F <: AnyLabelType, S <: AnyLabelType](val first: F, val second: S) extends AnyParV {

  type First = F
  type Second = S

  val label = this.toString
}


trait AnyOrV extends AnyLabelType {

  type First <: AnyLabelType
  val  first: First

  type Second <: AnyLabelType
  val  second: Second
}

case class OrV[F <: AnyLabelType, S <: AnyLabelType](val first: F, val second: S) extends AnyOrV {

  type First = F
  type Second = S

  val label = this.toString
}
