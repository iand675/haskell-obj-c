{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setShortcutRoleSelector
  , setWatchTemplateSelector
  , setWidgetKindSelector
  , shortcutRoleSelector
  , shortcutSelector
  , watchTemplateSelector
  , widgetKindSelector

  -- * Enum types
  , INRelevantShortcutRole(INRelevantShortcutRole)
  , pattern INRelevantShortcutRoleAction
  , pattern INRelevantShortcutRoleInformation

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a relevant shortcut for the given shortcut.
--
-- ObjC selector: @- initWithShortcut:@
initWithShortcut :: (IsINRelevantShortcut inRelevantShortcut, IsINShortcut shortcut) => inRelevantShortcut -> shortcut -> IO (Id INRelevantShortcut)
initWithShortcut inRelevantShortcut shortcut =
  sendOwnedMessage inRelevantShortcut initWithShortcutSelector (toINShortcut shortcut)

-- | A collection of relevance information that is attached to the relevant shortcuts.
--
-- Providing additional relevance information allows Siri to suggest a shortcut that the user is interested in but has not previously performed.
--
-- INRelevanceProvider
--
-- ObjC selector: @- relevanceProviders@
relevanceProviders :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id NSArray)
relevanceProviders inRelevantShortcut =
  sendMessage inRelevantShortcut relevanceProvidersSelector

-- | A collection of relevance information that is attached to the relevant shortcuts.
--
-- Providing additional relevance information allows Siri to suggest a shortcut that the user is interested in but has not previously performed.
--
-- INRelevanceProvider
--
-- ObjC selector: @- setRelevanceProviders:@
setRelevanceProviders :: (IsINRelevantShortcut inRelevantShortcut, IsNSArray value) => inRelevantShortcut -> value -> IO ()
setRelevanceProviders inRelevantShortcut value =
  sendMessage inRelevantShortcut setRelevanceProvidersSelector (toNSArray value)

-- | Customizes the display of the relevant shortcut on the Siri watch face.
--
-- By default, the UI for the relevant shortcut can be derivied from the information provided in the @INShortcut.@ In certain situations, it may be desirable to override this behavior and provide a custom template.
--
-- INDefaultCardTemplate
--
-- ObjC selector: @- watchTemplate@
watchTemplate :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id INDefaultCardTemplate)
watchTemplate inRelevantShortcut =
  sendMessage inRelevantShortcut watchTemplateSelector

-- | Customizes the display of the relevant shortcut on the Siri watch face.
--
-- By default, the UI for the relevant shortcut can be derivied from the information provided in the @INShortcut.@ In certain situations, it may be desirable to override this behavior and provide a custom template.
--
-- INDefaultCardTemplate
--
-- ObjC selector: @- setWatchTemplate:@
setWatchTemplate :: (IsINRelevantShortcut inRelevantShortcut, IsINDefaultCardTemplate value) => inRelevantShortcut -> value -> IO ()
setWatchTemplate inRelevantShortcut value =
  sendMessage inRelevantShortcut setWatchTemplateSelector (toINDefaultCardTemplate value)

-- | Links the relevant shortcut to a specific WidgetKit widget kind.
--
-- When a relevant shortcut is linked to a WidgetKit widget, it hints to the system when to show the widget in a stack.
--
-- ObjC selector: @- widgetKind@
widgetKind :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id NSString)
widgetKind inRelevantShortcut =
  sendMessage inRelevantShortcut widgetKindSelector

-- | Links the relevant shortcut to a specific WidgetKit widget kind.
--
-- When a relevant shortcut is linked to a WidgetKit widget, it hints to the system when to show the widget in a stack.
--
-- ObjC selector: @- setWidgetKind:@
setWidgetKind :: (IsINRelevantShortcut inRelevantShortcut, IsNSString value) => inRelevantShortcut -> value -> IO ()
setWidgetKind inRelevantShortcut value =
  sendMessage inRelevantShortcut setWidgetKindSelector (toNSString value)

-- | The role of the relevant shortcut.
--
-- Provides a hint to Siri about the expected user experience. The default is @INRelevantShortcutRoleAction.@
--
-- INRelevantShortcutRole
--
-- ObjC selector: @- shortcutRole@
shortcutRole :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO INRelevantShortcutRole
shortcutRole inRelevantShortcut =
  sendMessage inRelevantShortcut shortcutRoleSelector

-- | The role of the relevant shortcut.
--
-- Provides a hint to Siri about the expected user experience. The default is @INRelevantShortcutRoleAction.@
--
-- INRelevantShortcutRole
--
-- ObjC selector: @- setShortcutRole:@
setShortcutRole :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> INRelevantShortcutRole -> IO ()
setShortcutRole inRelevantShortcut value =
  sendMessage inRelevantShortcut setShortcutRoleSelector value

-- | The shortcut that will be performed when this relevant shortcut is invoked.
--
-- INShortcut
--
-- ObjC selector: @- shortcut@
shortcut :: IsINRelevantShortcut inRelevantShortcut => inRelevantShortcut -> IO (Id INShortcut)
shortcut inRelevantShortcut =
  sendMessage inRelevantShortcut shortcutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithShortcut:@
initWithShortcutSelector :: Selector '[Id INShortcut] (Id INRelevantShortcut)
initWithShortcutSelector = mkSelector "initWithShortcut:"

-- | @Selector@ for @relevanceProviders@
relevanceProvidersSelector :: Selector '[] (Id NSArray)
relevanceProvidersSelector = mkSelector "relevanceProviders"

-- | @Selector@ for @setRelevanceProviders:@
setRelevanceProvidersSelector :: Selector '[Id NSArray] ()
setRelevanceProvidersSelector = mkSelector "setRelevanceProviders:"

-- | @Selector@ for @watchTemplate@
watchTemplateSelector :: Selector '[] (Id INDefaultCardTemplate)
watchTemplateSelector = mkSelector "watchTemplate"

-- | @Selector@ for @setWatchTemplate:@
setWatchTemplateSelector :: Selector '[Id INDefaultCardTemplate] ()
setWatchTemplateSelector = mkSelector "setWatchTemplate:"

-- | @Selector@ for @widgetKind@
widgetKindSelector :: Selector '[] (Id NSString)
widgetKindSelector = mkSelector "widgetKind"

-- | @Selector@ for @setWidgetKind:@
setWidgetKindSelector :: Selector '[Id NSString] ()
setWidgetKindSelector = mkSelector "setWidgetKind:"

-- | @Selector@ for @shortcutRole@
shortcutRoleSelector :: Selector '[] INRelevantShortcutRole
shortcutRoleSelector = mkSelector "shortcutRole"

-- | @Selector@ for @setShortcutRole:@
setShortcutRoleSelector :: Selector '[INRelevantShortcutRole] ()
setShortcutRoleSelector = mkSelector "setShortcutRole:"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector '[] (Id INShortcut)
shortcutSelector = mkSelector "shortcut"

