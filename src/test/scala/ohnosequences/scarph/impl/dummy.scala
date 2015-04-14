package ohnosequences.scarph.test

import ohnosequences.scarph._, implementations._, graphTypes._, evals._

object dummy extends DefaultEvals {

  case object Dummy
  type Dummy = Dummy.type

  implicit def tensorImpl:
      TensorImpl[Dummy, Dummy, Dummy] =
  new TensorImpl[Dummy, Dummy, Dummy] {

    def leftProj(t: Tensor): Left = Dummy
    def rightProj(t: Tensor): Right = Dummy
    def apply(l: Left, r: Right): Tensor = Dummy
  }


  implicit def matchUpImpl:
      MatchUpImpl[Dummy] =
  new MatchUpImpl[Dummy] {

    def matchUp(l: Impl, r: Impl): Impl = Dummy
  }


  implicit def unitImpl:
      UnitImpl[Dummy, Dummy] =
  new UnitImpl[Dummy, Dummy] {

    def fromUnit(u: UnitImpl): Obj = Dummy
    def toUnit(s: Obj): UnitImpl = Dummy
  }


  implicit def biproductImpl:
      BiproductImpl[Dummy, Dummy, Dummy] =
  new BiproductImpl[Dummy, Dummy, Dummy] {

    def leftProj(b: Biproduct): Left = Dummy
    def rightProj(b: Biproduct): Right = Dummy

    def leftInj(l: Left): Biproduct = l
    def rightInj(r: Right): Biproduct = r

    def apply(l: Left, r: Right): Biproduct = Dummy
  }


  implicit def mergeImpl:
      MergeImpl[Dummy] =
  new MergeImpl[Dummy] {

    def merge(l: Impl, r: Impl): Impl = Dummy
  }


  implicit def zeroImpl:
      ZeroImpl[Dummy] =
  new ZeroImpl[Dummy] { def apply(): Impl = Dummy }


  implicit def edgeImpl:
      EdgeImpl[Dummy, Dummy, Dummy] =
  new EdgeImpl[Dummy, Dummy, Dummy] {

    def source(e: Edge): Source = Dummy
    def target(e: Edge): Target = Dummy
  }


  implicit def vertexInImpl:
      VertexInImpl[Dummy, Dummy, Dummy] =
  new VertexInImpl[Dummy, Dummy, Dummy] {

    def inE(v: Vertex, e: AnyEdge): InEdges = Dummy
    def inV(v: Vertex, e: AnyEdge): InVertices = Dummy
  }


  implicit def vertexOutImpl:
      VertexOutImpl[Dummy, Dummy, Dummy] =
  new VertexOutImpl[Dummy, Dummy, Dummy] {

    def outE(v: Vertex, e: AnyEdge): OutEdges = Dummy
    def outV(v: Vertex, e: AnyEdge): OutVertices = Dummy
  }


  implicit def dummyPropertyImpl:
      PropertyImpl[Dummy, Dummy] =
  new PropertyImpl[Dummy, Dummy] {

    def lookup(p: Property): Element = Dummy
    def get(e: Element, p: AnyGraphProperty): Property = Dummy
  }


}
