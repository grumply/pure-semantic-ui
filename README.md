# Semantic UI Pure

## Usage

Semantic-UI-Pure takes a `Component + Patterns + Properties` approach. Every component has a pattern for construction and inspection and for each property a component supports, an instance of a `Has*Prop` class is declared.

### Construction

Every component has an instance of `Default` as well as a bi-directional pattern for injection into a `View` as well as for pattern matching a `View`.

Here, `Grid` is our pattern declared in `Semantic.Collections.Grid` and `def` will be automatically instantiated to type `def :: Grid ms` to unify with `pattern Grid`.

```haskell
-- construction
myGrid :: View ms
myGrid = Grid def
```

With bidirectional patterns, we can use the same keyword, here `Grid`, to pattern match over a `View ms` to look for a `Grid ms` component.

```haskell
-- pattern matching
getGrid :: View ms -> Maybe (Grid ms)
getGrid (Grid g) = Just g
getGrid _        = Nothing
```

The best way to modify a component is via typeclass-encoded property patterns. For instance, many components support a `Size` property. So there exists a `Size` bidirectional pattern to both pattern match and inject sizes.

```haskell
pattern Size :: HasSizeProp a => Txt -> a -> a
pattern Size s a <- (getSize &&& id -> (s,a)) where
    Size s a = setSize s a
```

With this `Size` pattern, we can modify a default instance of a component with a custom size.

```haskell
myButton :: Button ms
myButton = Size "small" def
```

When chaining properties, it can look better to use `(&)` from `Data.Function` which is automatically exported by `Semantic`.

```haskell
myButton = def & Size "small" & Circular
```

To add `Children` to a component, there is a `Children` property supporting `Semantic` components as well as primitive Pure `View`s.

```haskell
myButton = def & Size "small" & Circular & Children
    [ "My Button"
    , Span [] "And my span" 
    ]
```

There is a shorthand, `!`, for `& Children`.

```haskell
myButton = def & Size "small" & Circular ! [ "My Button" ]
```

And since there is an `IsString` instance for `[View ms]`, we can omit the list syntax.

```haskell
myButton = def & Size "small" & Circular ! "My Button"
```

There is a similar shorthand, `%`, for `& Attributes` which allows adding Pure `Feature`s.

```haskell
infixl 1 %
(%) c as = c & Attributes as
```

There are three other combinators of import to ease view construction.

```haskell
infixr 0 <|
(<|) f = f

infixl 1 |>
(|>) c cs = Children cs c 

infixr 0 <||>
(<||>) c cs = c <| def |> cs
```

With these, we can get a cleaner construction syntax.

```haskell
myView =
  Container <| def & Fluid |>
    [ ButtonGroup <| def & Color Teal |>
        [ Button <| def & Circular |> 
            "Circular Button" 
        , Button <||> "Default Button"
        ]
    ]
```

`(%)` allows the addition of primitive pure-based `Feature`s, like custom event handlers, attributes and properties.

Note that most components support the following properties:

* `As` to modify the embedded element, for instance `Button <| def & As Div |> "Some Div Button"` would construct a `<div>`-based button.
* `Children` to supply children
* `Attributes` to supply features like attributes, properties and event handlers
* `Classes` to supply custom classes