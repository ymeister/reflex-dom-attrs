{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.Attrs where

import Control.Applicative
import Control.Arrow ((***))
import Control.Lens ((%~))
import Control.Monad
import Data.Default
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHCJS.DOM.Element (setInnerHTML)
import Reflex.Dom hiding (Attrs, El, ElConfig, Widget)
import Text.Read (readMaybe)

import Reflex.Dom.Attrs.Internal



data Attrs t m = Attrs
  { attrs_class :: Text
  , attrs_style :: Map Text Text
  , attrs_attrs :: Map Text Text
  , attrs_dynClass :: Dynamic t Text
  , attrs_dynStyle :: Dynamic t (Map Text Text)
  , attrs_dynAttrs :: Dynamic t (Map Text Text)
  , attrs_self :: El t m -> m [Attrs t m]
  }

instance (Applicative (Dynamic t), Applicative m) => Semigroup (Attrs t m) where
  x <> y = Attrs
    { attrs_class = ((<+>) `on` attrs_class) x y
    -- We use flip union in order to be right associative, so that using foldr (fold, mconcat) makes the last to set a property wins
    , attrs_style = (flip Map.union `on` attrs_style) x y
    , attrs_attrs = (unionAttrs `on` attrs_attrs) x y
    , attrs_dynClass = (liftA2 (<+>) `on` attrs_dynClass) x y
    , attrs_dynStyle = (liftA2 (flip Map.union) `on` attrs_dynStyle) x y
    , attrs_dynAttrs = (liftA2 (unionAttrs) `on` attrs_dynAttrs) x y
    , attrs_self = \selfEl -> liftA2 (<>) (attrs_self x selfEl) (attrs_self y selfEl)
    }

instance (Reflex t, Applicative m) => Monoid (Attrs t m) where
  mempty = Attrs
    { attrs_class = mempty
    , attrs_style = mempty
    , attrs_attrs = mempty
    , attrs_dynClass = constDyn mempty
    , attrs_dynStyle = constDyn mempty
    , attrs_dynAttrs = constDyn mempty
    , attrs_self = const $ pure mempty
    }

instance (Reflex t, Applicative m) => Default (Attrs t m) where
  def = mempty

unionAttrs :: Map Text Text -> Map Text Text -> Map Text Text
unionAttrs attrs1 attrs2 = Map.unionWithKey go attrs1 attrs2
  where
    go "class" a1 a2 = a1 <+> a2
    go "style" a1 a2 = case T.takeEnd 1 a1 of
      "" -> a2
      ";" -> a1 <> a2
      _ -> a1 <> ";" <> a2
    go _ a1 _ = a1

foldAttrs :: (Reflex t, Monad m) => Maybe (El t m) -> [Attrs t m] -> m (Dynamic t (Map Text Text))
foldAttrs e attrss = do
  let attrs = mconcat attrss
  selfDynAttrs <- maybe (pure mempty) (foldAttrs Nothing <=< attrs_self attrs) e

  let style = T.intercalate ";" . fmap (\(k, v) -> k <> ":" <> v) . Map.toList
      foldAttrs' class_ style_ attrs_ = foldr (unionAttrs) mempty
        [ "class" =: class_
        , "style" =: style style_
        , attrs_
        ]
      --
      staticAttrs = foldAttrs' (attrs_class attrs) (attrs_style attrs) (attrs_attrs attrs)
      dynAttrs = unionAttrs <$> selfDynAttrs <*> liftA3 (foldAttrs') (attrs_dynClass attrs) (attrs_dynStyle attrs) (attrs_dynAttrs attrs)

  pure $ Map.filter (not . T.null) <$> unionAttrs staticAttrs <$> dynAttrs

foldDynAttrs :: forall t m. (Adjustable t m, MonadHold t m) => Dynamic t [Attrs t m] -> Attrs t m
foldDynAttrs attrsDyn = (def :: Attrs t m)
  { attrs_self = \e -> do
      dynAttrs <- dynSequence $ foldAttrs (Just e) <$> attrsDyn
      pure $ [ def { attrs_dynAttrs = join dynAttrs } ]
  }



class Has_Class x y where
  _class_ :: x -> y

instance (Reflex t, Applicative m) => Has_Class Text (Attrs t m) where
  _class_ x = def { attrs_class = x }
instance (Reflex t, Applicative m) => Has_Class (Dynamic t Text) (Attrs t m) where
  _class_ x = def { attrs_dynClass = x }

instance Reflex t => Has_Class (Attrs t m) Text where
  _class_ = attrs_class
instance Reflex t => Has_Class (Attrs t m) (Dynamic t Text) where
  _class_ = attrs_dynClass

instance (Reflex t, Applicative m) => Has_Class String (Attrs t m) where
  _class_ x = def { attrs_class = T.pack x }
instance (Reflex t, Applicative m) => Has_Class (Dynamic t String) (Attrs t m) where
  _class_ x = def { attrs_dynClass = T.pack <$> x }


class Has_Style x y where
  _style_ :: x -> y

instance (Reflex t, Applicative m) => Has_Style (Map Text Text) (Attrs t m) where
  _style_ x = def { attrs_style = x }
instance (Reflex t, Applicative m) => Has_Style (Dynamic t (Map Text Text)) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = x }

instance Reflex t => Has_Style (Attrs t m) (Map Text Text) where
  _style_ = attrs_style
instance Reflex t => Has_Style (Attrs t m) (Dynamic t (Map Text Text)) where
  _style_ = attrs_dynStyle

instance (Reflex t, Applicative m) => Has_Style (Map String String) (Attrs t m) where
  _style_ x = def { attrs_style = Map.fromDistinctAscList $ join (***) T.pack <$> Map.toAscList x }
instance (Reflex t, Applicative m) => Has_Style (Dynamic t (Map String String)) (Attrs t m) where
  _style_ x = def { attrs_dynStyle = (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) <$> x }


class Has_Attrs x y where
  _attrs_ :: x -> y

instance (Reflex t, Applicative m) => Has_Attrs (Map Text Text) (Attrs t m) where
  _attrs_ x = def { attrs_attrs = x }
instance (Reflex t, Applicative m) => Has_Attrs (Dynamic t (Map Text Text)) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = x }

instance Reflex t => Has_Attrs (Attrs t m) (Map Text Text) where
  _attrs_ = attrs_attrs
instance Reflex t => Has_Attrs (Attrs t m) (Dynamic t (Map Text Text)) where
  _attrs_ = attrs_dynAttrs

instance (Reflex t, Applicative m) => Has_Attrs (Map String String) (Attrs t m) where
  _attrs_ x = def { attrs_attrs = Map.fromDistinctAscList $ join (***) T.pack <$> Map.toAscList x }
instance (Reflex t, Applicative m) => Has_Attrs (Dynamic t (Map String String)) (Attrs t m) where
  _attrs_ x = def { attrs_dynAttrs = (Map.fromDistinctAscList . fmap (join (***) T.pack) . Map.toAscList) <$> x }


_self_attrs_ :: forall t m. (Reflex t, Applicative m) => (El t m -> m [Attrs t m]) -> Attrs t m
_self_attrs_ x = (def :: Attrs t m) { attrs_self = x }



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

(~::) :: Text -> Text -> Map Text Text
(~::) = (=:)




elAttrs
  :: Widget t m
  => Text -> [Attrs t m] -> m a -> m a
elAttrs elTag attrs child = snd <$> elAttrs' elTag attrs child

elAttrs'
  :: Widget t m
  => Text -> [Attrs t m] -> m a -> m (El t m, a)
elAttrs' elTag attrs child = mdo
  attrsDyn <- foldAttrs (Just e) attrs
  attrsEl@(e, _) <- elDynAttr' elTag attrsDyn child
  pure attrsEl


elHTML
  :: ( Widget t m
     , PrerenderJS t m
     )
  => Text -> [Attrs t (Client m)] -> Text -> m ()
elHTML htmlTag attrs html = do
  prerender_ blank $ do
    (e, _) <- elAttrs' htmlTag attrs blank
    setInnerHTML (_element_raw e) html


elDiv
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elDiv attrs child = snd <$> elDiv' attrs child

elDiv'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elDiv' attrs child = elAttrs' "div" attrs child


elSpan
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elSpan attrs child = snd <$> elSpan' attrs child

elSpan'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elSpan' attrs child = elAttrs' "span" attrs child


elImg
  :: Widget t m
  => [Attrs t m] -> m a -> m a
elImg attrs child = snd <$> elImg' attrs child

elImg'
  :: Widget t m
  => [Attrs t m] -> m a -> m (El t m, a)
elImg' attrs child = elAttrs' "img" attrs child


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
  attrsDyn <- foldAttrs (Just e) attrs
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = (def :: ElConfig t m)
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & elementConfig_modifyAttributes .~ (mapKeysToAttributeName <$> modAttrs)
  (e, result) <- element "button" cfg child
  pure (e, domEvent Click e, result)


elInput :: forall t m. Widget t m => [Attrs t m] -> InputElConfig t m -> m (InputEl t m)
elInput attrs cfg' = mdo
  attrsDyn <- foldAttrs (Just $ _inputElement_element inputEl) attrs
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = cfg'
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ (mapKeysToAttributeName <$> modAttrs)
  inputEl <- inputElement cfg
  pure inputEl

elRead :: (Widget t m, Read a) => [Attrs t m] -> InputElConfig t m -> m (Dynamic t (Maybe a))
elRead attrs cfg = snd <$> elRead' attrs cfg

elRead' :: (Widget t m, Read a) => [Attrs t m] -> InputElConfig t m -> m (InputEl t m, Dynamic t (Maybe a))
elRead' attrs cfg = do
  e <- elInput attrs cfg
  pure (e, readMaybe . T.unpack <$> _inputElement_value e)

elNumber :: (Widget t m, Read a, Num a) => [Attrs t m] -> InputElConfig t m -> m (Dynamic t a)
elNumber attrs cfg = snd <$> elNumber' attrs cfg

elNumber' :: forall t m a. (Widget t m, Read a, Num a) => [Attrs t m] -> InputElConfig t m -> m (InputEl t m, Dynamic t a)
elNumber' attrs cfg = do
  let _ = fromInteger 0 :: a
  (e, v') <- elRead' (["type" ~: "number"] <> attrs) cfg
  v <- improvingMaybe v'
  pure (e, fromJust <$> v)


elTextArea :: forall t m. Widget t m => [Attrs t m] -> TextAreaElConfig t m -> m (TextAreaEl t m)
elTextArea attrs cfg' = mdo
  attrsDyn <- foldAttrs (Just $ _textAreaElement_element textAreaEl) attrs
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = cfg'
        & textAreaElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const $ preventDefault <> stopPropagation)
        & textAreaElementConfig_elementConfig . elementConfig_modifyAttributes .~ (mapKeysToAttributeName <$> modAttrs)
  textAreaEl <- textAreaElement cfg
  pure textAreaEl
