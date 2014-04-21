
```scala
package bio4j.model

trait AnyEdge extends Denotation[AnyEdgeType] { edge =>

  // NOTE: if I remove this from here type inference fails in tests. Most likely a bug
  type Tpe <: AnyEdgeType
```

Source-Edge-Target types. Looks like we need to add them here to guide type inference; tests fail otherwise

```scala
  type SourceType = tpe.SourceType
  type TargetType = tpe.TargetType

  type Out[X] = tpe.Out[X]
  type In[X] = tpe.In[X]
```

Get source/target from this representation

```scala
  abstract class GetSource[S <: AnyVertex.ofType[SourceType]](val source: S) {
    def apply(edgeRep: edge.TaggedRep): source.TaggedRep
  }
  abstract class GetTarget[T <: AnyVertex.ofType[TargetType]](val target: T) {
    def apply(edgeRep: edge.TaggedRep): target.TaggedRep
  }

  implicit def edgeOps(edgeRep: edge.TaggedRep) = EdgeOps(edgeRep)
  case class   EdgeOps(edgeRep: edge.TaggedRep) {

    def source[S <: AnyVertex.ofType[SourceType]](implicit getter: GetSource[S]) = getter(edgeRep)

    def target[T <: AnyVertex.ofType[TargetType]](implicit getter: GetTarget[T]) = getter(edgeRep)
  }
}

class Edge[ET <: AnyEdgeType](val tpe: ET) 
  extends AnyEdge { type Tpe = ET }

object AnyEdge {
  type withSourceType[VT <: AnyVertexType] = AnyEdge { type SourceType = VT }
  type withTargetType[VT <: AnyVertexType] = AnyEdge { type TargetType = VT }
}

```


------

### Index

+ src
  + test
    + scala
      + bio4j
        + model
          + [properties.scala][test/scala/bio4j/model/properties.scala]
          + [edges.scala][test/scala/bio4j/model/edges.scala]
          + [vertices.scala][test/scala/bio4j/model/vertices.scala]
          + titan
            + [TitanGodsTest.scala][test/scala/bio4j/model/titan/TitanGodsTest.scala]
            + [TEdge.scala][test/scala/bio4j/model/titan/TEdge.scala]
            + [TVertex.scala][test/scala/bio4j/model/titan/TVertex.scala]
            + [godsImplementation.scala][test/scala/bio4j/model/titan/godsImplementation.scala]
            + [godsSchema.scala][test/scala/bio4j/model/titan/godsSchema.scala]
          + [vertexTypes.scala][test/scala/bio4j/model/vertexTypes.scala]
          + [edgeTypes.scala][test/scala/bio4j/model/edgeTypes.scala]
  + main
    + scala
      + bio4j
        + model
          + [Denotation.scala][main/scala/bio4j/model/Denotation.scala]
          + [EdgeType.scala][main/scala/bio4j/model/EdgeType.scala]
          + [VertexType.scala][main/scala/bio4j/model/VertexType.scala]
          + [Vertex.scala][main/scala/bio4j/model/Vertex.scala]
          + [Edge.scala][main/scala/bio4j/model/Edge.scala]
          + [Property.scala][main/scala/bio4j/model/Property.scala]

[test/scala/bio4j/model/properties.scala]: ../../../../test/scala/bio4j/model/properties.scala.md
[test/scala/bio4j/model/edges.scala]: ../../../../test/scala/bio4j/model/edges.scala.md
[test/scala/bio4j/model/vertices.scala]: ../../../../test/scala/bio4j/model/vertices.scala.md
[test/scala/bio4j/model/titan/TitanGodsTest.scala]: ../../../../test/scala/bio4j/model/titan/TitanGodsTest.scala.md
[test/scala/bio4j/model/titan/TEdge.scala]: ../../../../test/scala/bio4j/model/titan/TEdge.scala.md
[test/scala/bio4j/model/titan/TVertex.scala]: ../../../../test/scala/bio4j/model/titan/TVertex.scala.md
[test/scala/bio4j/model/titan/godsImplementation.scala]: ../../../../test/scala/bio4j/model/titan/godsImplementation.scala.md
[test/scala/bio4j/model/titan/godsSchema.scala]: ../../../../test/scala/bio4j/model/titan/godsSchema.scala.md
[test/scala/bio4j/model/vertexTypes.scala]: ../../../../test/scala/bio4j/model/vertexTypes.scala.md
[test/scala/bio4j/model/edgeTypes.scala]: ../../../../test/scala/bio4j/model/edgeTypes.scala.md
[main/scala/bio4j/model/Denotation.scala]: Denotation.scala.md
[main/scala/bio4j/model/EdgeType.scala]: EdgeType.scala.md
[main/scala/bio4j/model/VertexType.scala]: VertexType.scala.md
[main/scala/bio4j/model/Vertex.scala]: Vertex.scala.md
[main/scala/bio4j/model/Edge.scala]: Edge.scala.md
[main/scala/bio4j/model/Property.scala]: Property.scala.md