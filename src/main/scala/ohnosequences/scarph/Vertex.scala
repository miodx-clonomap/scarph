package ohnosequences.scarph

/*
  `AnyVertex` defines a denotation of the corresponding `VertexType`.

  Instances are modeled as instances of other type tagged with the singleton type of a `Vertex`. For example, an instance of a self of type `User` when stored/represented by a `Neo4jNode` is going to be something of type `FieldType[user.type, Neo4jNode]`  where `user.type <: AnyVertex { type VertexType = User.type; type Raw = Neo4jNode }`.
  
  They are designed to be compatible with shapeless records (maybe, we'll see).
*/

trait AnyVertex extends Denotation[AnyVertexType] with CanHaveProperties { vertex =>

  /* Getters for incoming/outgoing edges */
  abstract class RetrieveOutEdge[E <: Singleton with AnyEdge](val e: E) {
    def apply(rep: vertex.Rep): e.tpe.Out[e.Rep]
  }
  abstract class RetrieveInEdge[E <: Singleton with AnyEdge](val e: E) {
    def apply(rep: vertex.Rep): e.tpe.In[e.Rep]
  }

  /* Additional methods */
  // implicit def vertexOps(rep: vertex.Rep) = VertexOps(rep)
  // case class   VertexOps(rep: vertex.Rep) {

  //   def outT[ET <: From[vertex.Tpe], E <: Singleton with AnyEdge { type Tpe <: ET }]
  //     (et: ET)(implicit e: E, mkRetriever: E => RetrieveOutEdge[E]): E#Tpe#Out[E#Rep] = {
  //       val retriever = mkRetriever(e)
  //       retriever(rep)
  //     }

  //   def out[E <: Singleton with AnyEdge { type Tpe <: From[vertex.Tpe] }]
  //     (e: E)(implicit mkRetriever: E => RetrieveOutEdge[E]): E#Tpe#Out[E#Rep] = {
  //       val retriever = mkRetriever(e)
  //       retriever(rep)
  //     }

  //   def in[E <: Singleton with AnyEdge { type Tpe <: To[vertex.Tpe] }]
  //     (e: E)(implicit mkRetriever: E => RetrieveInEdge[E]): E#Tpe#In[E#Rep] = {
  //       val retriever = mkRetriever(e)
  //       retriever(rep)
  //     }
  // }

}

abstract class Vertex[VT <: AnyVertexType](val tpe: VT) 
    extends AnyVertex { type Tpe = VT }

object AnyVertex {
  type ofType[VT <: AnyVertexType] = AnyVertex { type Tpe = VT }
}

