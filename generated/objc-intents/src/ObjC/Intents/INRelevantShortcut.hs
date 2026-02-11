{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lets you provide relevant shortcut to Siri, for display on the Siri Watch Face.
--
-- Including relevance information allows Siri to make suggestions for shortcuts that the user might be interested in but has not previously performed.
--
-- Generated bindings for @INRelevantShortcut@.
module ObjC.Intents.INRelevantShortcut
  ( INRelevantShortcut
  , IsINRelevantShortcut(..)
  , initWithShortcut
  , relevanceProviders
  , setRelevanceProviders
  , watchTemplate
  , setWatchTemplate
  , widgetKind
  , setWidgetKind
  , shortcutRole
  , setShortcutRole
  , shortcut
  , initWithShortcutSelector
  , relevanceProvidersSelector
  , setRelevanceProvidersSelector
  , watchTemplateSelector
  , setWatchTemplateSelector
  , widgetKindSelector
  , setWidgetKindSelector
  , shortcutRoleSelector
  , setShortcutRoleSelector
  , shortcutSelector

  -- * Enum types
  , INRelevantShortcutRole(INRelevantShortcutRole)
  , pattern INRelevantShortcutRoleAction
  , pattern INRelevantShortcutRoleInformation

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a relevant shortcut for the given shortcut.
--
-- ObjC selector: @- initWithShortcut:@
initWithShortcut :: (IsINRelevantShortcut inRelevantShortcut, IsINShortcut shortcut) => inRelevantShortcut -> shortcut -> IO (Id INRelevantShortcut)
initWithShortcut inRelevantShortcut  shortcut =
withObjCPtr shortcut $ \raw_shortcut ->
    sendMsg inRelevantShortcut (mkSelector "initWithShortcut:") (retPtr retVoid) [argPtr (castPtr raw_shortcut :: Ptr ())] >>= ownedObject . castPtr

-- | A collection of relevance information that is attached to the relevant shortcuts.
--
-- Providing additional relevance information allows Siri to suggest a shortcut that the user is interested in but has not previously performed.
--
-- INRelevanceProvider
--
-- ObjC selector: @- relevanceProviders@
relevanceProviders :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id NSArray)
relevanceProviders inRelevantShortcut  =
  sendMsg inRelevantShortcut (mkSelector "relevanceProviders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A collection of relevance information that is attached to the relevant shortcuts.
--
-- Providing additional relevance information allows Siri to suggest a shortcut that the user is interested in but has not previously performed.
--
-- INRelevanceProvider
--
-- ObjC selector: @- setRelevanceProviders:@
setRelevanceProviders :: (IsINRelevantShortcut inRelevantShortcut, IsNSArray value) => inRelevantShortcut -> value -> IO ()
setRelevanceProviders inRelevantShortcut  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRelevantShortcut (mkSelector "setRelevanceProviders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Customizes the display of the relevant shortcut on the Siri watch face.
--
-- By default, the UI for the relevant shortcut can be derivied from the information provided in the @INShortcut.@ In certain situations, it may be desirable to override this behavior and provide a custom template.
--
-- INDefaultCardTemplate
--
-- ObjC selector: @- watchTemplate@
watchTemplate :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id INDefaultCardTemplate)
watchTemplate inRelevantShortcut  =
  sendMsg inRelevantShortcut (mkSelector "watchTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Customizes the display of the relevant shortcut on the Siri watch face.
--
-- By default, the UI for the relevant shortcut can be derivied from the information provided in the @INShortcut.@ In certain situations, it may be desirable to override this behavior and provide a custom template.
--
-- INDefaultCardTemplate
--
-- ObjC selector: @- setWatchTemplate:@
setWatchTemplate :: (IsINRelevantShortcut inRelevantShortcut, IsINDefaultCardTemplate value) => inRelevantShortcut -> value -> IO ()
setWatchTemplate inRelevantShortcut  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRelevantShortcut (mkSelector "setWatchTemplate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Links the relevant shortcut to a specific WidgetKit widget kind.
--
-- When a relevant shortcut is linked to a WidgetKit widget, it hints to the system when to show the widget in a stack.
--
-- ObjC selector: @- widgetKind@
widgetKind :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id NSString)
widgetKind inRelevantShortcut  =
  sendMsg inRelevantShortcut (mkSelector "widgetKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Links the relevant shortcut to a specific WidgetKit widget kind.
--
-- When a relevant shortcut is linked to a WidgetKit widget, it hints to the system when to show the widget in a stack.
--
-- ObjC selector: @- setWidgetKind:@
setWidgetKind :: (IsINRelevantShortcut inRelevantShortcut, IsNSString value) => inRelevantShortcut -> value -> IO ()
setWidgetKind inRelevantShortcut  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRelevantShortcut (mkSelector "setWidgetKind:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The role of the relevant shortcut.
--
-- Provides a hint to Siri about the expected user experience. The default is @INRelevantShortcutRoleAction.@
--
-- INRelevantShortcutRole
--
-- ObjC selector: @- shortcutRole@
shortcutRole :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO INRelevantShortcutRole
shortcutRole inRelevantShortcut  =
  fmap (coerce :: CLong -> INRelevantShortcutRole) $ sendMsg inRelevantShortcut (mkSelector "shortcutRole") retCLong []

-- | The role of the relevant shortcut.
--
-- Provides a hint to Siri about the expected user experience. The default is @INRelevantShortcutRoleAction.@
--
-- INRelevantShortcutRole
--
-- ObjC selector: @- setShortcutRole:@
setShortcutRole :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> INRelevantShortcutRole -> IO ()
setShortcutRole inRelevantShortcut  value =
  sendMsg inRelevantShortcut (mkSelector "setShortcutRole:") retVoid [argCLong (coerce value)]

-- | The shortcut that will be performed when this relevant shortcut is invoked.
--
-- INShortcut
--
-- ObjC selector: @- shortcut@
shortcut :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id INShortcut)
shortcut inRelevantShortcut  =
  sendMsg inRelevantShortcut (mkSelector "shortcut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithShortcut:@
initWithShortcutSelector :: Selector
initWithShortcutSelector = mkSelector "initWithShortcut:"

-- | @Selector@ for @relevanceProviders@
relevanceProvidersSelector :: Selector
relevanceProvidersSelector = mkSelector "relevanceProviders"

-- | @Selector@ for @setRelevanceProviders:@
setRelevanceProvidersSelector :: Selector
setRelevanceProvidersSelector = mkSelector "setRelevanceProviders:"

-- | @Selector@ for @watchTemplate@
watchTemplateSelector :: Selector
watchTemplateSelector = mkSelector "watchTemplate"

-- | @Selector@ for @setWatchTemplate:@
setWatchTemplateSelector :: Selector
setWatchTemplateSelector = mkSelector "setWatchTemplate:"

-- | @Selector@ for @widgetKind@
widgetKindSelector :: Selector
widgetKindSelector = mkSelector "widgetKind"

-- | @Selector@ for @setWidgetKind:@
setWidgetKindSelector :: Selector
setWidgetKindSelector = mkSelector "setWidgetKind:"

-- | @Selector@ for @shortcutRole@
shortcutRoleSelector :: Selector
shortcutRoleSelector = mkSelector "shortcutRole"

-- | @Selector@ for @setShortcutRole:@
setShortcutRoleSelector :: Selector
setShortcutRoleSelector = mkSelector "setShortcutRole:"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector
shortcutSelector = mkSelector "shortcut"

