{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Reflex.Dom.Attrs.Internal
-- Description : Internal types and utilities for reflex-dom-attrs
-- Copyright   : (c) Yuri Meister
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module containing type aliases and utility functions.
-- These definitions are separated to avoid circular dependencies
-- and to provide a cleaner public API.
--
-- __Note:__ This is an internal module. Its interface may change
-- without notice between minor versions.

module Reflex.Dom.Attrs.Internal where

import Control.Monad.Fix
import Data.String (IsString)
import Reflex.Dom



-- | Simplified type alias for DOM elements in Reflex.
type El t m = Element EventResult (DomBuilderSpace m) t

-- | Configuration type for creating elements.
type ElConfig t m = ElementConfig EventResult t (DomBuilderSpace m)

-- | Simplified type alias for input elements.
type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

-- | Configuration type for input elements.
type InputElConfig t m = InputElementConfig EventResult t (DomBuilderSpace m)

-- | Simplified type alias for textarea elements.
type TextAreaEl t m = TextAreaElement EventResult (DomBuilderSpace m) t

-- | Configuration type for textarea elements.
type TextAreaElConfig t m = TextAreaElementConfig EventResult t (DomBuilderSpace m)

-- | Simplified type alias for select elements.
type SelectEl t m = SelectElement EventResult (DomBuilderSpace m) t



-- | Common constraint bundle for widgets.
--
-- Includes all the constraints typically needed for building
-- reactive DOM elements in Reflex.
type Widget t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

-- | Constraint for prerendering with GHCJS DOM space.
--
-- Used for operations that require direct DOM manipulation,
-- such as setting innerHTML.
type PrerenderJS t m = (Prerender t m, DomBuilderSpace (Client m) ~ GhcjsDomSpace)



-- | Space-separated concatenation operator for CSS classes.
--
-- Concatenates two strings with a space, handling empty strings gracefully:
--
-- * @"" <+> "" = ""@
-- * @"a" <+> "" = "a"@
-- * @"" <+> "b" = "b"@  
-- * @"a" <+> "b" = "a b"@
--
-- Useful for combining CSS class names.
(<+>) :: (IsString s, Eq s, Semigroup s) => s -> s -> s
(<+>) = curry $ \case
  ("", "") -> ""
  (t1, "") -> t1
  ("", t2) -> t2
  (t1, t2) -> t1 <> " " <> t2
