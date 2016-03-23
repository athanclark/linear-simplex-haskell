Simplical Optimization
======================

The [Simplex Method](https://en.wikipedia.org/wiki/Simplex_algorithm) is a
canonical algorithm for linear programming and optimization. Usually
implemented with
[Bland's Finite Pivoting Method](https://en.wikipedia.org/wiki/Bland's_rule),
this algorithm has been shown to be pseudo-finite terminating, by showing that
it avoids cycles (see [Bland's original paper](http://www.ohio.edu/people/melkonia/math4620/bland.pdf) for more details).
This is an implementation of the algorithm in pure Haskell - to both show its
feasability, and practicality as obviously correct.


## Usage

### Creating Linear Expressions

We treat linear expressions particularly - specifically in
[standard form](https://en.wikipedia.org/wiki/Linear_equation#General_.28or_standard.29_form), such that sets of variables and their coefficients are _mappings_
between the variable name and coefficient value: `name => coeff`; thus a linear
expression could be designed as the pair `(name => coeff, const)`. This is
exemplified under the [`LinExpr` data type](https://github.com/athanclark/linear-simplex-haskell/blob/master/src/Linear/Grammar.hs#L40).

Likewise, we imagine _inequalities_ as the same structure, but with an
additional tag representing it's constraint type:

```
ineq := equality -- =
      | ltequal  -- <=
      | gtequal  -- >=
```

Such that we imagine the _variables_ on the left, and the _constant_ on the
right. Thus, an _inequality_ is the triple `(ineq, name => coeff, const)`.


### Creating Constraint Sets

There is some machinery we need to account for internally in the algorithm;
particularly in that the simplex method only operates on positive variables,
which is rarely desires. So, we use a state monad to build a tableau
which stores our constraints incrementally, while keeping track of the details.

```haskell
myConstraints :: MonadState Tableau m => m ()
myConstraints = do
  addConstraint ineq1
  addConstraint ineq2
  addConstraint ineq3
```

Then, to get the underlying Tableau, we need to run the state monad,
supplying an initial Tableau via `newTableau` (where we also need an
objective function / cost function):

```haskell
myTableau :: Tableau
myTableau = execState myConstraints (newTableau objectiveIneq)
```

Now we can get to optimization!

### Optimization and Solutions

The tableau contains enough data to start optimizing. To kick the algorithm
into gear, just run `primalSimplex` over the beast:

```haskell
myOptimizedTableau :: Tableau
myOptimizedTableau = primalSimplex myTableau
```

Then you can peek at the supposedly optimal solution via `solution`:

```haskell
mySolution :: Map MainVarName Rational
mySolution = solution myOptimizedTableau
```

This should be optimal such that all other feasible solutions are less than
or equal to this optimal solution.

## How to run tests

```
stack test
```
