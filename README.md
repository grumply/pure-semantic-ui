# Semantic UI Pure

Semantic-UI-Pure implements the [Semantic-UI](https://semantic-ui.com/) by porting [Semantic-UI-React](https://react.semantic-ui.com/). 

Many thanks to the people over at Semantic-UI-React who did all of the hard work of the react component implementations.

## Usage

Semantic-UI-Pure takes a `Component + Patterns` approach. Every component has a pattern for construction and inspection and for each property a component supports, an instance of a `Has*Prop` class is declared with a bi-directional pattern to get and set it.

### Construction

Each component has a pattern for injection and type unification. For example, `Grid` has:

```haskell
data Grid ms = Grid_ {...}

pattern Grid :: Grid ms -> View ms
pattern Grid g = View g
```

Here, we'll use the `Grid` pattern to inject a grid component into a view. 

```haskell
myGrid :: View ms
myGrid = Grid def
```

Note that `Grid` admits an instance of `Default`, so we can simply create a basic `Grid` with `def`.

With our bidirectional `Grid` pattern, we can use the same keyword to pattern match over a `View ms` to look for a `Grid ms` component.

```haskell
getGrid :: View ms -> Maybe (Grid ms)
getGrid (Grid g) = Just g
getGrid _        = Nothing
```

### Properties

Each component implements a set of properties. For instance, `Grid` has an inverted property. 

```haskell
data Grid ms = Grid_
    { 
    ...
    , inverted :: Bool
    ...
    }

instance HasInvertedProp (Grid ms) where
    getInverted = inverted
    setInverted i grid = grid { inverted = i }
```

And there exists an `Inverted` pattern.

```haskell
pattern Inverted :: HasInvertedProp a => a -> a
pattern Inverted a <- (getInverted &&& id -> (True,a)) where
    Inverted a = setInverted True a
```

With this pattern we can set the inverted prop of a grid to `True`.

```haskell
myInverteGrid = Grid $ def & Inverted
```

And because this is a bi-directional pattern, we can even pattern match for inverted grids.

```haskell
getInvertedGrid :: View ms -> Maybe (Grid ms)
getInvertedGrid (Inverted (Grid g)) = Just g
getInvertedGrid _ = Nothing
```

We can even create a pattern for inverted grids.

```haskell
pattern InvertedGrid :: Grid ms -> View ms
pattern InvertedGrid g = Inverted (Grid g)
```

Note that properties can be easily chained together: 

```haskell
myButton = Button $ def & Size "small" & Circular
```

### Children

Most components implement the `Children` property.

```haskell
myButton = Button $ def & Size "small" & Circular & Children
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

### Attributes/Features

Similarly to `Children`, there is an `Attributes` property for raw features like event listeners and HTML attributes and properties.

There is a shorthand, `%`, for `& Attributes`. 

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

With these, we can get a clean construction syntax.

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

### Augmentation

Most components can be modified to render as other components or HTML elements via the `As` property.

```haskell
divButton = Button <| def & As Div |> "Div-based Button"
```

Note that most components support the following properties:

* `As` to modify rendering 
* `Children` to supply children (children are concatenated)
* `Attributes` to supply features (features are concatenated)
* `Classes` to supply classes (classes are concatenated)