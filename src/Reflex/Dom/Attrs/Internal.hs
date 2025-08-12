{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.Attrs.Internal where

import Control.Monad.Fix
import Data.String (IsString)
import Reflex.Dom



type El t m = Element EventResult (DomBuilderSpace m) t
type ElConfig t m = ElementConfig EventResult t (DomBuilderSpace m)

type InputEl t m = InputElement EventResult (DomBuilderSpace m) t
type InputElConfig t m = InputElementConfig EventResult t (DomBuilderSpace m)

type TextAreaEl t m = TextAreaElement EventResult (DomBuilderSpace m) t
type TextAreaElConfig t m = TextAreaElementConfig EventResult t (DomBuilderSpace m)

type SelectEl t m = SelectElement EventResult (DomBuilderSpace m) t



type Widget t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

type PrerenderJS t m = (Prerender t m, DomBuilderSpace (Client m) ~ GhcjsDomSpace)



dynSequence
  :: ( Adjustable t m
     , MonadHold t m
     )
  => Dynamic t (m a) -> m (Dynamic t a)
dynSequence x = do
  initial <- sample $ current x
  widgetHold initial $ updated x



(<+>) :: (IsString s, Eq s, Semigroup s) => s -> s -> s
(<+>) = curry $ \case
  ("", "") -> ""
  (t1, "") -> t1
  ("", t2) -> t2
  (t1, t2) -> t1 <> " " <> t2
