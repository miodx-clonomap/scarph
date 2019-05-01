package ohnosequences.scarph


trait AnyNaturalIsomorphism extends AnyPrimitiveMorph { iso =>

  type Dagger <: AnyNaturalIsomorphism {
    type Dagger >: iso.type <: AnyNaturalIsomorphism
  }
}

// σ: L ⊗ R → R ⊗ L
case class symmetry[L <: AnyGraphObject, R <: AnyGraphObject](l: L, r: R)
  extends AnyNaturalIsomorphism {

  type     In = L ⊗ R
  lazy val in: In = l ⊗ r

  type     Out = R ⊗ L
  lazy val out: Out = r ⊗ l

  type     Dagger = symmetry[R, L]
  lazy val dagger: Dagger = symmetry(r, l)

  lazy val label: String = s"symmetry(${l.label}, ${r.label})"
}

// A ⊕ (B ⊕ C) → (A ⊕ B) ⊕ C
case class associateBiproductLeft[A <: AnyGraphObject, B <: AnyGraphObject, C <: AnyGraphObject]
  (a: A, b: B, c: C) extends AnyNaturalIsomorphism {

  type     In = A ⊕ (B ⊕ C)
  lazy val in: In = a ⊕ (b ⊕ c)

  type     Out = (A ⊕ B) ⊕ C
  lazy val out: Out = (a ⊕ b) ⊕ c

  type     Dagger = associateBiproductRight[A, B, C]
  lazy val dagger: Dagger = associateBiproductRight(a, b, c)

  lazy val label: String = s"associateBiproductLeft(${a.label} ⊕ (${b.label} ⊕ ${c.label}))"
}

// (A ⊕ B) ⊕ C → A ⊕ (B ⊕ C)
case class associateBiproductRight[A <: AnyGraphObject, B <: AnyGraphObject, C <: AnyGraphObject]
  (a: A, b: B, c: C) extends AnyNaturalIsomorphism {

  type     In = (A ⊕ B) ⊕ C
  lazy val in: In = (a ⊕ b) ⊕ c

  type     Out = A ⊕ (B ⊕ C)
  lazy val out: Out = a ⊕ (b ⊕ c)

  type     Dagger = associateBiproductLeft[A, B, C]
  lazy val dagger: Dagger = associateBiproductLeft(a, b, c)

  lazy val label: String = s"associateBiproductRight((${a.label} ⊕ ${b.label}) ⊕ ${c.label})"
}

// A ⊗ (B ⊗ C) → (A ⊗ B) ⊗ C
case class associateTensorLeft[A <: AnyGraphObject, B <: AnyGraphObject, C <: AnyGraphObject]
  (a: A, b: B, c: C) extends AnyNaturalIsomorphism {

  type     In = A ⊗ (B ⊗ C)
  lazy val in: In = a ⊗ (b ⊗ c)

  type     Out = (A ⊗ B) ⊗ C
  lazy val out: Out = (a ⊗ b) ⊗ c

  type     Dagger = associateTensorRight[A, B, C]
  lazy val dagger: Dagger = associateTensorRight(a, b, c)

  lazy val label: String = s"associateTensorLeft(${a.label} ⊗ (${b.label} ⊗ ${c.label}))"
}

// (A ⊗ B) ⊗ C → A ⊗ (B ⊗ C)
case class associateTensorRight[A <: AnyGraphObject, B <: AnyGraphObject, C <: AnyGraphObject]
  (a: A, b: B, c: C) extends AnyNaturalIsomorphism {

  type     In = (A ⊗ B) ⊗ C
  lazy val in: In = (a ⊗ b) ⊗ c

  type     Out = A ⊗ (B ⊗ C)
  lazy val out: Out = a ⊗ (b ⊗ c)

  type     Dagger = associateTensorLeft[A, B, C]
  lazy val dagger: Dagger = associateTensorLeft(a, b, c)

  lazy val label: String = s"associateTensorRight((${a.label} ⊗ ${b.label}) ⊗ ${c.label})"
}


// U ⊗ (A ⊕ B) → (U ⊗ A) ⊕ (U ⊗ B)
case class distribute[U <: AnyGraphObject, A <: AnyGraphObject, B <: AnyGraphObject]
  (u: U, a: A, b: B) extends AnyNaturalIsomorphism {

  type     In = U ⊗ (A ⊕ B)
  lazy val in: In = u ⊗ (a ⊕ b)

  type     Out = (U ⊗ A) ⊕ (U ⊗ B)
  lazy val out: Out = (u ⊗ a) ⊕ (u ⊗ b)

  type     Dagger = undistribute[U, A, B]
  lazy val dagger: Dagger = undistribute(u, a, b)

  lazy val label: String = s"distribute(${u.label} ⊗ (${a.label} ⊕ ${b.label}))"
}

// (U ⊗ A) ⊕ (U ⊗ B) → U ⊗ (A ⊕ B)
case class undistribute[U <: AnyGraphObject, A <: AnyGraphObject, B <: AnyGraphObject]
  (u: U, a: A, b: B) extends AnyNaturalIsomorphism {

  type     Out = U ⊗ (A ⊕ B)
  lazy val out: Out = u ⊗ (a ⊕ b)

  type     In = (U ⊗ A) ⊕ (U ⊗ B)
  lazy val in: In = (u ⊗ a) ⊕ (u ⊗ b)

  type     Dagger = distribute[U, A, B]
  lazy val dagger: Dagger = distribute(u, a, b)

  lazy val label: String = s"undistribute((${u.label} ⊗ ${a.label}) ⊕ (${u.label} ⊗ ${b.label}))"
}


// I ⊗ X → X
case class leftUnit[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = TensorUnit.type ⊗ X
  lazy val in: In = unit ⊗ x

  type     Out = X
  lazy val out: Out = x

  type     Dagger = leftCounit[X]
  lazy val dagger: Dagger = leftCounit(x)

  lazy val label: String = s"leftUnit(I ⊗ ${x.label})"
}

// X → I ⊗ X
case class leftCounit[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     Out = TensorUnit.type ⊗ X
  lazy val out: Out = unit ⊗ x

  type     In = X
  lazy val in: In = x

  type     Dagger = leftUnit[X]
  lazy val dagger: Dagger = leftUnit(x)

  lazy val label: String = s"leftCounit(${x.label})"

}


// X ⊗ I → X
case class rightUnit[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = X ⊗ unit
  lazy val in: In = x ⊗ unit

  type     Out = X
  lazy val out: Out = x

  type     Dagger = rightCounit[X]
  lazy val dagger: Dagger = rightCounit(x)

  lazy val label: String = s"rightUnit(${x.label} ⊗ I)"
}

// X → I ⊗ X
case class rightCounit[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = X
  lazy val in: In = x

  type     Out = X ⊗ unit
  lazy val out: Out = x ⊗ unit

  type     Dagger = rightUnit[X]
  lazy val dagger: Dagger = rightUnit(x)

  lazy val label: String = s"rightCounit(${x.label})"
}


// 0 ⊕ X → X
case class leftZero[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = BiproductZero.type ⊕ X
  lazy val in: In = zero ⊕ x

  type     Out = X
  lazy val out: Out = x

  type     Dagger = leftCozero[X]
  lazy val dagger: Dagger = leftCozero(x)

  lazy val label: String = s"leftZero(0 ⊕ ${x.label})"
}

// X → 0 ⊕ X
case class leftCozero[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     Out = BiproductZero.type ⊕ X
  lazy val out: Out = zero ⊕ x

  type     In = X
  lazy val in: In = x

  type     Dagger = leftZero[X]
  lazy val dagger: Dagger = leftZero(x)

  lazy val label: String = s"leftCozero(${x.label})"
}


// X ⊕ 0 → X
case class rightZero[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = X ⊕ zero
  lazy val in: In = x ⊕ zero

  type     Out = X
  lazy val out: Out = x

  type     Dagger = rightCozero[X]
  lazy val dagger: Dagger = rightCozero(x)

  lazy val label: String = s"rightZero(${x.label} ⊕ 0)"
}

// X → X ⊕ 0
case class rightCozero[X <: AnyGraphObject](x: X) extends AnyNaturalIsomorphism {

  type     In = X
  lazy val in: In = x

  type     Out = X ⊕ zero
  lazy val out: Out = x ⊕ zero

  type     Dagger = rightZero[X]
  lazy val dagger: Dagger = rightZero(x)

  lazy val label: String = s"rightCozero(${x.label})"
}
