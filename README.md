# Semantic UI Pure

Semantic-UI-Pure implements the [Semantic-UI](https://semantic-ui.com/) by porting [Semantic-UI-React](https://react.semantic-ui.com/). 

Many thanks to the people over at Semantic-UI-React who did all of the hard work of the react component implementations.

## Usage

Every component has a pattern for construction and inspection and for each property a component supports, an instance of a `Prop` class is declared with a pattern for setting the property.

### Construction

Each component has a pattern for type unification. For example, `Grid` has:

```haskell
-- the component
data Grid = Grid_ 
    {...}

-- the type-unifying pattern
pattern Grid :: Grid -> Grid
pattern Grid g = g
```

Because we use `def` to initialize semantic components, we need the, e.g. `Grid`, pattern to fix the type. Otherwise, we can't apply properties without a type signature.

For example, here we initialize a default `Grid`:

```haskell
myGrid :: Grid
myGrid = Grid def
```

Alternatively, with the `TypeApplications` extension, it is possible to avoid the use of the pattern with:

```haskell
def @ Grid ...
```

Either approach should work equally well to inject type information and avoid ambiguity.

### Properties

Each component implements a set of properties. For instance, `Grid` has an `inverted` property. 

```haskell
data Grid = Grid_
    { 
    ...
    , inverted :: Bool
    ...
    }

instance HasProp Inverted Grid where
    type Prop Inverted Grid = Bool
    getProp _ = inverted
    setProp _ i g = g { inverted = i }
```

And there exists an `Inverted` pattern in a shared `Properties` module that contains the type and property pattern:

```haskell
data Inverted = Inverted_
pattern Inverted :: HasProp Inverted a => Prop Inverted a -> a -> a
pattern Inverted p a <- (getProp Inverted_ &&& id -> (p,a)) where
    Inverted p a = setProp Inverted_ p a
```

With this pattern we can set the inverted prop of a grid to `True`.

```haskell
myInverteGrid = Grid def & Inverted True
```

Note that properties can be easily chained together: 

```haskell
myButton = Button def & Size "small" & Circular True
```

### Children

Most components implement the `pure-core` `HasChildren` typeclass and thus inherit the `Children` property.

```haskell
myButton = Button def & Size "small" & Circular True & Children
    [ "My Button" ]
```

### Attributes/Features

Similarly to `HasChildren`, most semantic components implement the `HasFeatures` typeclass and thus inherit the `Features` property and it's associated patterns for events, attributes, properties, like `OnClick`, `Width`, etc....

```haskell
myButton = Button def & Size "large" & OnClick (const (print "Clicked!")) & Children
    [ "Click me" ]
```

### Clean Syntax

There are three other combinators of import to ease view construction that all come from `pure-core`:

```haskell
infixl 8 <|
(<|) :: ToView b => a -> (a -> b) -> View
(<|) a f = toView (f a)

infixr 9 |>
(|>) :: HasChildren a => (a -> a) -> [View] -> a -> a
(|>) f cs = f . setChildren cs

(<||>) :: (ToView a, HasChildren a) => a -> [View] -> View
(<||>) v cs = toView (setChildren cs v)
```

With these, we can get a clean construction syntax.

```haskell
myView =
  Container def <| Fluid True . Styles [(margin,pxs 8)] |>
    [ ButtonGroup def <| Color Teal |>
        [ Button def <| Circular True |> 
            [ "Circular Button" ]
        , Button def <||> 
            [ "Default Button" ]
        ]
    ]
```

### Augmentation

Most components can be modified to render as other components or HTML elements via the `As` property.

```haskell
divButton = let div fs cs = Div <| Features fs |> cs
            in Button <| As div |> [ "Div-based Button" ]
```

### Notes

This is still a beta library as well as an active test of pure for framework development, so expect some bugs. 

A few of the automatically managed components from Semantic-UI-React are left unmanaged in this library. They are:

* Form (refer to #168 and grumply/semantic-ui-pure-forms#1) ** this could be subsumed by a generic form library
* Dropdown
* Search