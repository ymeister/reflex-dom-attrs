{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Reflex.Dom.Attrs
-- Description : Enhanced attribute management for Reflex-DOM
-- Copyright   : (c) Yuri Meister
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides an enhanced attribute management system for Reflex-DOM applications.
-- It offers a composable, type-safe way to handle both static and dynamic HTML attributes,
-- including classes, styles, and custom attributes.
--
-- = Key Features
--
-- * Composable 'Attrs' type with 'Semigroup' and 'Monoid' instances
-- * Support for both static and dynamic attributes with performance optimization
-- * Type classes for convenient attribute creation ('Has_Class', 'Has_Style', 'Has_Attrs')
-- * Builder pattern for creating attributes with the '~:' operator
-- * Enhanced element creation functions with attribute support
--
-- = Basic Usage
--
-- @
-- -- Static attributes
-- myDiv :: Widget t m => m ()
-- myDiv = elDiv
--   [ "class" ~: "container"
--   , "style" ~: ["color" ~:: "red", "padding" ~:: "10px"]
--   ] $ text "Hello"
--
-- -- Dynamic attributes
-- myDynamicDiv :: Widget t m => Dynamic t Text -> m ()
-- myDynamicDiv dynClass = elDiv
--   [ "class" ~: dynClass
--   , "data-id" ~: "123"
--   ] $ text "Dynamic content"
-- @

module Reflex.Dom.Attrs where

import Control.Arrow ((***))
import Control.Lens ((%~))
import Control.Monad
import Data.Default
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Proxy
import Data.Semialign
import Data.Text (Text)
import Data.Text qualified as T
import Data.These
import Data.Traversable (for)
import GHCJS.DOM.Element (setInnerHTML)
import Reflex.Dom hiding (Attrs, El, ElConfig, Widget)
import Text.Read (readMaybe)

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

import Reflex.Dom.Attrs.Internal



-- | The core attribute type that encapsulates both static and dynamic HTML attributes.
--
-- 'Attrs' supports composition through its 'Semigroup' and 'Monoid' instances, allowing
-- attributes to be combined from multiple sources. The type distinguishes between static
-- and dynamic attributes for performance optimization.
--
-- When combining attributes:
--
-- * Classes are concatenated with spaces
-- * Styles follow CSS cascade rules (later values override earlier ones)
-- * Other attributes use custom merging logic via 'unionAttrs'
-- * Self-referential attributes allow access to the created element
data Attrs t m = Attrs
  { attrs_class :: Text
  -- ^ Static CSS class names
  , attrs_style :: Map Text Text
  -- ^ Static inline CSS styles as property-value pairs
  , attrs_attrs :: Map Text Text
  -- ^ Static HTML attributes (excluding class and style)
  , attrs_dynClass :: Maybe (Dynamic t Text)
  -- ^ Dynamic CSS class names that can change over time
  , attrs_dynStyle :: Maybe (Dynamic t (Map Text Text))
  -- ^ Dynamic inline CSS styles that can change over time
  , attrs_dynAttrs :: Maybe (Dynamic t (Map Text Text))
  -- ^ Dynamic HTML attributes that can change over time
  , attrs_self :: Maybe (El t m -> m [Attrs t m])
  -- ^ Self-referential attributes that depend on the created element
  }

instance (Applicative (Dynamic t), Applicative m) => Semigroup (Attrs t m) where
  x <> y = Attrs
    { attrs_class = ((<+>) `on` attrs_class) x y
    , attrs_style = (flip Map.union `on` attrs_style) x y
    , attrs_attrs = (unionAttrs `on` attrs_attrs) x y
    , attrs_dynClass = (alignWith (mergeThese $ liftA2 (<+>)) `on` attrs_dynClass) x y
    , attrs_dynStyle = (alignWith (mergeThese $ liftA2 (flip Map.union)) `on` attrs_dynStyle) x y
    , attrs_dynAttrs = (alignWith (mergeThese $ liftA2 (unionAttrs)) `on` attrs_dynAttrs) x y
    , attrs_self = (alignWith (mergeThese $ (liftA2 . liftA2) (<>)) `on` attrs_self) x y
    }

instance (Reflex t, Applicative m) => Monoid (Attrs t m) where
  mempty = Attrs
    { attrs_class = mempty
    , attrs_style = mempty
    , attrs_attrs = mempty
    , attrs_dynClass = Nothing
    , attrs_dynStyle = Nothing
    , attrs_dynAttrs = Nothing
    , attrs_self = Nothing
    }

instance (Reflex t, Applicative m) => Default (Attrs t m) where
  def = mempty

-- | Merge two attribute maps with special handling for @class@ and @style@ attributes.
--
-- * @class@ attributes are concatenated with a space separator
-- * @style@ attributes are concatenated with semicolon separation
-- * Other attributes: the first (left) value takes precedence
unionAttrs :: Map Text Text -> Map Text Text -> Map Text Text
unionAttrs attrs1 attrs2 = Map.unionWithKey go attrs1 attrs2
  where
    go "class" a1 a2 = a1 <+> a2
    go "style" a1 a2 = case T.takeEnd 1 a1 of
      "" -> a2
      ";" -> a1 <> a2
      _ -> a1 <> ";" <> a2
    go _ a1 _ = a1

-- | Fold a list of 'Attrs' into a final attribute map.
--
-- Returns 'Left' with static attributes if all attributes are static,
-- or 'Right' with a 'Dynamic' if any attributes are dynamic.
-- This optimization allows elements to use 'elAttr' instead of 'elDynAttr'
-- when possible, improving performance.
--
-- The optional element parameter enables self-referential attributes.
foldAttrs :: (Reflex t, Monad m) => Maybe (El t m) -> [Attrs t m] -> m (Either (Map Text Text) (Dynamic t (Map Text Text)))
foldAttrs e attrss = do
  let attrs = mconcat attrss

  attrsSelfMEDyn <- for ((,) <$> e <*> attrs_self attrs) $ \(e', selfAttrs) ->
    foldAttrs Nothing =<< selfAttrs e'
  let (attrsSelfMStatic, attrsSelfMDyn) = case attrsSelfMEDyn of
        Just (Left attrsSelfStatic) -> (Just attrsSelfStatic, Nothing)
        Just (Right attrsSelfDyn) -> (Nothing, Just attrsSelfDyn)
        Nothing -> (Nothing, Nothing)

  let toStyle = T.intercalate ";" . fmap (\(k, v) -> k <> ":" <> v) . Map.toList
      --
      attrsStatic =
          Map.filter (not . T.null)
        $ foldr (unionAttrs) mempty
            [ "class" =: attrs_class attrs
            , "style" =: toStyle (attrs_style attrs)
            , attrs_attrs attrs
            , fromMaybe mempty attrsSelfMStatic
            ]
      --
      attrsMDyn =
          (fmap . fmap) (Map.filter (not . T.null))
        $ foldr (alignWith $ mergeThese $ liftA2 unionAttrs) Nothing
            [ (fmap . fmap) ("class" =:) $ attrs_dynClass attrs
            , (fmap . fmap) (("style" =:) . toStyle) $ attrs_dynStyle attrs
            , attrs_dynAttrs attrs
            , attrsSelfMDyn
            ]

  pure $ case attrsMDyn of
    Nothing -> Left attrsStatic
    Just attrsDyn -> Right $ unionAttrs <$> constDyn attrsStatic <*> attrsDyn

-- | Like 'foldAttrs' but always returns a 'Dynamic', even for static attributes.
--
-- Useful when you need a 'Dynamic' result regardless of whether the input
-- attributes are static or dynamic.
foldAttrsDyn :: (Reflex t, Monad m) => Maybe (El t m) -> [Attrs t m] -> m (Dynamic t (Map Text Text))
foldAttrsDyn e attrss = fmap (either constDyn id) $ foldAttrs e attrss

-- | Create an 'Attrs' from a 'Dynamic' list of 'Attrs'.
--
-- This allows attribute lists to change over time, useful for conditional
-- attributes or attributes that depend on application state.
foldDynAttrs :: forall t m. (Adjustable t m, MonadHold t m) => Dynamic t [Attrs t m] -> Attrs t m
foldDynAttrs attrsDyn = (def :: Attrs t m)
  { attrs_self = Just $ \e -> do
      dynAttrs <- widgetHold (pure $ constDyn mempty) $ updated (foldAttrsDyn (Just e) <$> attrsDyn)
      pure $ [ def { attrs_dynAttrs = Just $ join dynAttrs } ]
  }

--
-- | Has_Class
--

-- | Type class for values that can be converted to or from CSS class attributes.
--
-- This type class provides a polymorphic interface for setting and getting
-- class attributes on 'Attrs'. Supports both static and dynamic classes.
--
-- ==== __Examples__
--
-- @
-- -- Static class
-- staticClass :: Attrs t m
-- staticClass = _class_ "my-class"
--
-- -- Dynamic class
-- dynamicClass :: Dynamic t Text -> Attrs t m
-- dynamicClass dyn = _class_ dyn
-- @
class Has_Class x y where
  _class_ :: x -> y

instance (Reflex t, Applicative m) => Has_Class Text (Attrs t m) where
  _class_ x = def { attrs_class = x }
instance (Reflex t, Applicative m) => Has_Class (Maybe (Dynamic t Text)) (Attrs t m) where
  _class_ x = def { attrs_dynClass = x }
instance (Reflex t, Applicative m) => Has_Class (Dynamic t Text) (Attrs t m) where
  _class_ x = def { attrs_dynClass = Just x }

instance Reflex t => Has_Class (Attrs t m) Text where
  _class_ = attrs_class
instance Reflex t => Has_Class (Attrs t m) (Maybe (Dynamic t Text)) where
  _class_ = attrs_dynClass

instance (Reflex t, Applicative m) => Has_Class String (Attrs t m) where
  _class_ x = def { attrs_class = T.pack x }
instance (Reflex t, Applicative m) => Has_Class (Maybe (Dynamic t String)) (Attrs t m) where
  _class_ x = def { attrs_dynClass = (fmap . fmap) T.pack x }
instance (Reflex t, Applicative m) => Has_Class (Dynamic t String) (Attrs t m) where
  _class_ x = def { attrs_dynClass = Just $ T.pack <$> x }

--
-- | Has_Style
--

-- | Type class for values that can be converted to or from CSS style attributes.
--
-- This type class provides a polymorphic interface for setting and getting
-- inline CSS styles on 'Attrs'. Supports both static and dynamic styles.
--
-- ==== __Examples__
--
-- @
-- -- Static styles
-- staticStyle :: Attrs t m
-- staticStyle = _style_ $ Map.fromList [("color", "red"), ("padding", "10px")]
--
-- -- Dynamic styles
-- dynamicStyle :: Dynamic t (Map Text Text) -> Attrs t m
-- dynamicStyle dyn = _style_ dyn
-- @
class Has_Style x y where
  _style_ :: x -> y

instance (Reflex t, Applicative m) => Has_Style (Map Text Text) (Attrs t m) where
  _style_ x = def { attrs_style = x }
instance (Reflex t, Applicative m) => Has_Style (Maybe (Dynamic t (Map Text Text))) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = x }
instance (Reflex t, Applicative m) => Has_Style (Dynamic t (Map Text Text)) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = Just x }

instance Reflex t => Has_Style (Attrs t m) (Map Text Text) where
  _style_ = attrs_style
instance Reflex t => Has_Style (Attrs t m) (Maybe (Dynamic t (Map Text Text))) where
  _style_ = attrs_dynStyle

instance (Reflex t, Applicative m) => Has_Style (Map String String) (Attrs t m) where
  _style_ x = def { attrs_style = Map.fromDistinctAscList $ join (***) T.pack <$> Map.toAscList x }
instance (Reflex t, Applicative m) => Has_Style (Maybe (Dynamic t (Map String String))) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = (fmap . fmap) (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) x }
instance (Reflex t, Applicative m) => Has_Style (Dynamic t (Map String String)) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = Just $ (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) <$> x }

--
-- | Has_Attrs
--

-- | Type class for values that can be converted to or from HTML attributes.
--
-- This type class provides a polymorphic interface for setting and getting
-- arbitrary HTML attributes (excluding class and style) on 'Attrs'.
-- Supports both static and dynamic attributes.
--
-- ==== __Examples__
--
-- @
-- -- Static attributes
-- staticAttrs :: Attrs t m
-- staticAttrs = _attrs_ $ Map.fromList [("data-id", "123"), ("disabled", "")]
--
-- -- Dynamic attributes
-- dynamicAttrs :: Dynamic t (Map Text Text) -> Attrs t m
-- dynamicAttrs dyn = _attrs_ dyn
-- @
class Has_Attrs x y where
  _attrs_ :: x -> y

instance (Reflex t, Applicative m) => Has_Attrs (Map Text Text) (Attrs t m) where
  _attrs_ x = def { attrs_attrs = x }
instance (Reflex t, Applicative m) => Has_Attrs (Maybe (Dynamic t (Map Text Text))) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = x }
instance (Reflex t, Applicative m) => Has_Attrs (Dynamic t (Map Text Text)) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = Just x }

instance Reflex t => Has_Attrs (Attrs t m) (Map Text Text) where
  _attrs_ = attrs_attrs
instance Reflex t => Has_Attrs (Attrs t m) (Maybe (Dynamic t (Map Text Text))) where
  _attrs_ = attrs_dynAttrs

instance (Reflex t, Applicative m) => Has_Attrs (Map String String) (Attrs t m) where
  _attrs_ x = def { attrs_attrs = Map.fromDistinctAscList $ join (***) T.pack <$> Map.toAscList x }
instance (Reflex t, Applicative m) => Has_Attrs (Maybe (Dynamic t (Map String String))) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = (fmap . fmap) (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) x }
instance (Reflex t, Applicative m) => Has_Attrs (Dynamic t (Map String String)) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = Just $ (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) <$> x }

--
-- | Self Attrs
--

-- | Create attributes that depend on the element they're attached to.
--
-- This enables self-referential patterns where attributes can access
-- the DOM element after it's created. Useful for setting up event handlers
-- or attributes that need element-specific information.
--
-- ==== __Example__
--
-- @
-- selfRefAttrs :: Attrs t m
-- selfRefAttrs = _self_attrs_ $ \el -> do
--   -- Access the element after creation
--   performEvent_ $ domEvent Click el $> liftIO (putStrLn "Clicked!")
--   pure []
-- @
_self_attrs_ :: forall t m. (Reflex t, Applicative m) => (El t m -> m [Attrs t m]) -> Attrs t m
_self_attrs_ x = (def :: Attrs t m) { attrs_self = Just x }

--
-- | MkAttrs
--

-- | Type class for creating 'Attrs' from various value types using the '~:' operator.
--
-- This provides a convenient builder syntax for creating attributes.
-- The first argument is the attribute name, and the second is the value.
--
-- ==== __Examples__
--
-- @
-- -- Simple text attribute
-- idAttr :: Attrs t m
-- idAttr = "id" ~: "my-element"
--
-- -- Class attribute
-- classAttr :: Attrs t m
-- classAttr = "class" ~: "btn btn-primary"
--
-- -- Style attribute with list syntax
-- styleAttr :: Attrs t m
-- styleAttr = "style" ~: ["color" ~:: "red", "padding" ~:: "10px"]
--
-- -- Dynamic attribute
-- dynAttr :: Dynamic t Text -> Attrs t m
-- dynAttr dyn = "data-value" ~: dyn
-- @
class MkAttrs t m a where
  (~:) :: Text -> a -> Attrs t m

instance (Reflex t, Applicative m) => MkAttrs t m Text where
  k ~: v = case k of
    "class" -> _class_ v
    _ -> _attrs_ (Map.singleton k v)

instance (Reflex t, Applicative m) => MkAttrs t m (Dynamic t Text) where
  k ~: v = case k of
    "class" -> _class_ v
    _ -> _attrs_ (Map.singleton k <$> v)

instance (Reflex t, Applicative m) => MkAttrs t m (Map Text Text) where
  k ~: v = case k of
    "style" -> _style_ v
    _ -> _attrs_ v

instance (Reflex t, Applicative m) => MkAttrs t m (Dynamic t (Map Text Text)) where
  k ~: v = case k of
    "style" -> _style_ v
    _ -> _attrs_ v

instance (Reflex t, Applicative m) => MkAttrs t m [Map Text Text] where
  k ~: v = case k of
    "style" -> _style_ (Map.unions $ reverse v)
    _ -> _attrs_ (Map.unions $ reverse v)

instance (Reflex t, Applicative m) => MkAttrs t m (Dynamic t [Map Text Text]) where
  k ~: v = case k of
    "style" -> _style_ (Map.unions . reverse <$> v)
    _ -> _attrs_ (Map.unions . reverse <$> v)

instance (Reflex t, Applicative m) => MkAttrs t m String where
  k ~: v = k ~: (T.pack v)
instance (Reflex t, Applicative m) => MkAttrs t m (Dynamic t String) where
  k ~: v = k ~: (T.pack <$> v)
instance (Reflex t, Applicative m) => MkAttrs t m (Map String String) where
  k ~: v = k ~: (Map.fromDistinctAscList $ join (***) T.pack <$> Map.toAscList v)
instance (Reflex t, Applicative m) => MkAttrs t m (Dynamic t (Map String String)) where
  k ~: v = k ~: ((Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) <$> v)

-- | Convenience operator for creating style property maps.
--
-- Used in conjunction with '~:' to create inline styles:
--
-- @
-- "style" ~: ["color" ~:: "red", "padding" ~:: "10px"]
-- @
(~::) :: Text -> Text -> Map Text Text
(~::) = (=:)

--
-- | Basic
--

elAttrs
  :: Widget t m
  => Text -> [Attrs t m] -> m a -> m a
elAttrs elTag attrs child = snd <$> elAttrs' elTag attrs child

elAttrs'
  :: Widget t m
  => Text -> [Attrs t m] -> m a -> m (El t m, a)
elAttrs' elTag attrs child = mdo
  attrsEDyn <- foldAttrs (Just e) attrs
  attrsEl@(e, _) <- case attrsEDyn of
    Left attrsStatic -> elAttr' elTag attrsStatic child
    Right attrsDyn -> elDynAttr' elTag attrsDyn child
  pure attrsEl

--
-- | HTML
--

elHTML
  :: ( Widget t m
     , PrerenderJS t m
     )
  => Text -> [Attrs t (Client m)] -> Text -> m ()
elHTML htmlTag attrs html = do
  prerender_ blank $ do
    (e, _) <- elAttrs' htmlTag attrs blank
    setInnerHTML (_element_raw e) html

--
-- | Div
--

elDiv
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elDiv attrs child = snd <$> elDiv' attrs child

elDiv'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elDiv' attrs child = elAttrs' "div" attrs child

--
-- | Span
--

elSpan
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elSpan attrs child = snd <$> elSpan' attrs child

elSpan'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elSpan' attrs child = elAttrs' "span" attrs child

--
-- | Img
--

elImg
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elImg attrs child = snd <$> elImg' attrs child

elImg'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elImg' attrs child = elAttrs' "img" attrs child

--
-- | Button
--

elButton_ :: Widget t m => [Attrs t m] -> m a -> m (Event t ())
elButton_ attrs child = do
  (clickEv, _) <- elButton attrs child
  pure clickEv

elButton :: Widget t m => [Attrs t m] -> m a -> m (Event t (), a)
elButton attrs child = do
  (_, clickEv, result) <- elButton' attrs child
  pure (clickEv, result)

elButton' :: forall t m a. Widget t m => [Attrs t m] -> m a -> m (El t m, Event t (), a)
elButton' attrs child = mdo
  attrsEDyn <- foldAttrs (Just e) attrs

  (initMAttrs, modMAttrs) <- case attrsEDyn of
    Left attrsStatic -> pure (Just attrsStatic, Nothing)
    Right attrsDyn -> (Nothing,) . Just <$> dynamicAttributesToModifyAttributes attrsDyn

  let cfg = (def :: ElConfig t m)
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & (maybe id ((elementConfig_initialAttributes .~) . mapKeysToAttributeName) initMAttrs)
        & (maybe id ((elementConfig_modifyAttributes .~) . fmap mapKeysToAttributeName) modMAttrs)

  (e, result) <- element "button" cfg child

  pure (e, domEvent Click e, result)

--
-- | Input
--

elInput :: forall t m. Widget t m => [Attrs t m] -> InputElConfig t m -> m (InputEl t m)
elInput attrs cfg' = mdo
  attrsEDyn <- foldAttrs (Just $ _inputElement_element inputEl) attrs

  (initMAttrs, modMAttrs) <- case attrsEDyn of
    Left attrsStatic -> pure (Just attrsStatic, Nothing)
    Right attrsDyn -> (Nothing,) . Just <$> dynamicAttributesToModifyAttributes attrsDyn

  let cfg = cfg'
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & (maybe id ((inputElementConfig_elementConfig . elementConfig_initialAttributes .~) . mapKeysToAttributeName) initMAttrs)
        & (maybe id ((inputElementConfig_elementConfig . elementConfig_modifyAttributes .~) . fmap mapKeysToAttributeName) modMAttrs)

  inputEl <- inputElement cfg

  pure inputEl

-- | Create an input that parses its value using 'Read'.
--
-- Returns 'Nothing' when the input cannot be parsed.
elRead :: (Widget t m, Read a) => [Attrs t m] -> InputElConfig t m -> m (Dynamic t (Maybe a))
elRead attrs cfg = snd <$> elRead' attrs cfg

-- | Like 'elRead' but also returns the input element.
elRead' :: (Widget t m, Read a) => [Attrs t m] -> InputElConfig t m -> m (InputEl t m, Dynamic t (Maybe a))
elRead' attrs cfg = do
  e <- elInput attrs cfg
  pure (e, readMaybe . T.unpack <$> _inputElement_value e)

-- | Create a number input element.
--
-- Automatically sets @type="number"@ and returns the parsed numeric value.
-- Uses 'improvingMaybe' to retain the last valid value when parsing fails.
elNumber :: (Widget t m, Read a, Num a) => [Attrs t m] -> InputElConfig t m -> m (Dynamic t a)
elNumber attrs cfg = snd <$> elNumber' attrs cfg

-- | Like 'elNumber' but also returns the input element.
elNumber' :: forall t m a. (Widget t m, Read a, Num a) => [Attrs t m] -> InputElConfig t m -> m (InputEl t m, Dynamic t a)
elNumber' attrs cfg = do
  let _ = fromInteger 0 :: a
  (e, v') <- elRead' (["type" ~: "number"] <> attrs) cfg
  v <- improvingMaybe v'
  pure (e, fromJust <$> v)

--
-- | Text Area
--

elTextArea :: forall t m. Widget t m => [Attrs t m] -> TextAreaElConfig t m -> m (TextAreaEl t m)
elTextArea attrs cfg' = mdo
  attrsEDyn <- foldAttrs (Just $ _textAreaElement_element textAreaEl) attrs

  (initMAttrs, modMAttrs) <- case attrsEDyn of
    Left attrsStatic -> pure (Just attrsStatic, Nothing)
    Right attrsDyn -> (Nothing,) . Just <$> dynamicAttributesToModifyAttributes attrsDyn

  let cfg = cfg'
        & textAreaElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & (maybe id ((textAreaElementConfig_elementConfig. elementConfig_initialAttributes .~) . mapKeysToAttributeName) initMAttrs)
        & (maybe id ((textAreaElementConfig_elementConfig. elementConfig_modifyAttributes .~) . fmap mapKeysToAttributeName) modMAttrs)

  textAreaEl <- textAreaElement cfg

  pure textAreaEl
