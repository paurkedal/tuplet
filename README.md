## Synopsis

Tuplet implements tuple-like heterogeneous vectors over a type constructor,
providing arity-generic operations.  A tuplet constructor of arity `n`
represents a `(α₁ * ... * αₙ) tuplet` as `α₁ elt * ... * αₙ elt`.  Generic
operations on tuples takes the from of shape-preserving transformations or
shape-agnostic reductions, which accept callbacks acting generically on a
component of type `α elt`.

In practise, the Tuplet library is useful for dealing with representations
of tuples in GADTs, since it eliminates treating each arity as a special
case.  In particular a GADT `α t` may be extended with a single additional
constructor and related code to support cases of the form
`(α₁ * ... * αₙ) t` up to the arity supported by Tuplet.

Recursive type definitions are supported by using recursive modules, where
tuple parts of the signature can be included.  Also a helper module can
provide constructor aliases for the main type.  Thus, no arity-specific code
should be needed outside the Tuplet library.

## Example

See [`test_recursive.ml`](test/test_recursive.ml) for an example.
