# reflex-dom-attrs

Enhanced attribute management for Reflex-DOM applications.

## Overview

`reflex-dom-attrs` provides a composable, type-safe system for managing HTML attributes in [Reflex-DOM](https://github.com/reflex-frp/reflex-dom) applications. It offers a clean DSL for working with both static and dynamic attributes while optimizing performance by avoiding unnecessary `Dynamic` creation.

## Features

- **Composable attributes** via `Semigroup`/`Monoid` instances
- **Performance optimized** - uses static elements when possible
- **Type-safe** attribute builders with the `~:` operator
- **Polymorphic** type classes for classes, styles, and attributes
- **Self-referential** attributes that can access the created element
- **Enhanced element functions** with built-in attribute support

## Installation

Add to your `.cabal` file:

```cabal
build-depends: reflex-dom-attrs
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Attrs

-- Simple static attributes
myButton :: Widget t m => m (Event t ())
myButton = elButton_
  [ "class" ~: "btn btn-primary"
  , "style" ~: ["padding" ~:: "10px", "color" ~:: "white"]
  , "data-id" ~: "submit-btn"
  ] $ text "Click me!"

-- Dynamic attributes
dynamicDiv :: Widget t m => Dynamic t Text -> m ()
dynamicDiv dynClass = elDiv
  [ "class" ~: dynClass
  , "style" ~: ["display" ~:: "flex"]
  ] $ text "Dynamic content"

-- Composing attributes
baseAttrs, extraAttrs :: Attrs t m
baseAttrs = "class" ~: "widget" <> "data-role" ~: "container"
extraAttrs = "class" ~: "enhanced" <> "style" ~: ["margin" ~:: "5px"]

combined :: Widget t m => m ()
combined = elDiv [baseAttrs, extraAttrs] $ text "Composed"
-- Results in: class="widget enhanced" data-role="container" style="margin:5px"
```

## Core Concepts

### The `Attrs` Type

The central type that represents a collection of HTML attributes:

```haskell
data Attrs t m = Attrs
  { attrs_class    :: Text                           -- Static classes
  , attrs_style    :: Map Text Text                  -- Static styles
  , attrs_attrs    :: Map Text Text                  -- Static attributes
  , attrs_dynClass :: Maybe (Dynamic t Text)         -- Dynamic classes
  , attrs_dynStyle :: Maybe (Dynamic t (Map Text Text)) -- Dynamic styles
  , attrs_dynAttrs :: Maybe (Dynamic t (Map Text Text)) -- Dynamic attributes
  , attrs_self     :: Maybe (El t m -> m [Attrs t m])   -- Self-referential
  }
```

### Attribute Creation

#### The `~:` Operator

The primary way to create attributes:

```haskell
-- Text attributes
"id" ~: "my-element"
"class" ~: "container"

-- Style attributes (using lists)
"style" ~: ["color" ~:: "red", "padding" ~:: "10px"]

-- Dynamic attributes
"class" ~: dynClass
"data-value" ~: dynValue
```

#### Type Classes

Polymorphic interfaces for specific attribute types:

```haskell
-- Classes
_class_ "my-class"                    -- Static
_class_ dynClass                      -- Dynamic

-- Styles
_style_ $ Map.fromList [("color", "blue")]  -- Static
_style_ dynStyles                           -- Dynamic

-- Generic attributes
_attrs_ $ Map.fromList [("data-id", "123")] -- Static
_attrs_ dynAttrs                            -- Dynamic
```

### Element Creation

All standard HTML elements are supported with attribute-aware variants:

```haskell
-- Basic elements
elDiv  :: [Attrs t m] -> m a -> m a
elSpan :: [Attrs t m] -> m a -> m a
elImg  :: [Attrs t m] -> m a -> m a

-- Elements returning the DOM element
elDiv'  :: [Attrs t m] -> m a -> m (El t m, a)
elSpan' :: [Attrs t m] -> m a -> m (El t m, a)

-- Interactive elements
elButton  :: [Attrs t m] -> m a -> m (Event t (), a)
elButton' :: [Attrs t m] -> m a -> m (El t m, Event t (), a)

-- Form elements
elInput    :: [Attrs t m] -> InputElConfig t m -> m (InputEl t m)
elTextArea :: [Attrs t m] -> TextAreaElConfig t m -> m (TextAreaEl t m)
```

## Advanced Usage

### Self-Referential Attributes

Attributes that depend on the created element:

```haskell
selfRefExample :: Widget t m => m ()
selfRefExample = elDiv
  [ _self_attrs_ $ \el -> do
      -- Access element after creation
      performEvent_ $ domEvent Click el $> liftIO (putStrLn "Clicked!")
      pure []
  ] $ text "Click me"
```

### Dynamic Attribute Lists

Change entire attribute sets dynamically:

```haskell
conditionalAttrs :: Widget t m => Dynamic t Bool -> m ()
conditionalAttrs isActive = elDiv
  [ foldDynAttrs $ isActive <&> \active ->
      if active
        then ["class" ~: "active", "style" ~: ["color" ~:: "green"]]
        else ["class" ~: "inactive", "style" ~: ["color" ~:: "gray"]]
  ] $ text "Conditional styling"
```

### Performance Optimization

The library automatically optimizes element creation:

- When all attributes are static, uses `elAttr` internally
- When any attribute is dynamic, uses `elDynAttr`
- Avoids creating unnecessary `Dynamic` values

```haskell
-- This creates a static element (efficient)
staticEl = elDiv ["class" ~: "static"] $ text "Fast"

-- This creates a dynamic element (when needed)
dynamicEl dynClass = elDiv ["class" ~: dynClass] $ text "Reactive"
```

## API Design Philosophy

1. **Composability First**: Attributes combine naturally via `Semigroup`
2. **Performance Aware**: Static paths optimized automatically
3. **Type Safety**: Leverage Haskell's type system for correctness
4. **Ergonomic**: Clean, intuitive syntax with the `~:` operator
5. **Incremental Adoption**: Works alongside standard reflex-dom

## Examples

### Form with Validation Styling

```haskell
validatedInput :: Widget t m => m (Dynamic t Text)
validatedInput = do
  rec
    input <- elInput
      [ "class" ~: isValid <&> \valid ->
          if valid then "form-control" else "form-control error"
      , "placeholder" ~: "Enter email"
      ] def

    let value = _inputElement_value input
        isValid = value <&> \v -> '@' `T.elem` v

  pure value
```

### Responsive Container

```haskell
responsiveContainer :: Widget t m => Dynamic t Bool -> m () -> m ()
responsiveContainer isMobile content = elDiv
  [ "class" ~: "container"
  , "style" ~: isMobile <&> \mobile ->
      if mobile
        then ["padding" ~:: "5px", "font-size" ~:: "14px"]
        else ["padding" ~:: "20px", "font-size" ~:: "16px"]
  ] content
```

### Button with Loading State

```haskell
loadingButton :: Widget t m => Dynamic t Bool -> m (Event t ())
loadingButton isLoading = elButton
  [ "class" ~: isLoading <&> \loading ->
      if loading then "btn btn-disabled" else "btn btn-primary"
  , "disabled" ~: isLoading <&> \loading ->
      if loading then "true" else ""
  ] $ dynText $ isLoading <&> \loading ->
      if loading then "Loading..." else "Submit"
```

## License

BSD3 - See LICENSE file for details

## Author

Yuri Meister

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.
