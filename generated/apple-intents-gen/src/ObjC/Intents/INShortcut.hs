{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A shortcut is an action that can be suggested by the system or added to Siri.
--
-- Generated bindings for @INShortcut@.
module ObjC.Intents.INShortcut
  ( INShortcut
  , IsINShortcut(..)
  , new
  , init_
  , initWithIntent
  , initWithUserActivity
  , intent
  , userActivity
  , initSelector
  , initWithIntentSelector
  , initWithUserActivitySelector
  , intentSelector
  , newSelector
  , userActivitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Note: Must be initilaized with either an intent or user activity, using those initializers.
--
-- ObjC selector: @+ new@
new :: IO (Id INShortcut)
new  =
  do
    cls' <- getRequiredClass "INShortcut"
    sendOwnedClassMessage cls' newSelector

-- | Note: Must be initilaized with either an intent or user activity, using those initializers.
--
-- ObjC selector: @- init@
init_ :: IsINShortcut inShortcut => inShortcut -> IO (Id INShortcut)
init_ inShortcut =
  sendOwnedMessage inShortcut initSelector

-- | Creates a shortcut with the given intent.
--
-- @intent@ — Unless user configurable, must have a title and have valid shortcut types.
--
-- Returns: Will return @nil@ (and log an error) if the intent isn't valid.
--
-- ObjC selector: @- initWithIntent:@
initWithIntent :: (IsINShortcut inShortcut, IsINIntent intent) => inShortcut -> intent -> IO (Id INShortcut)
initWithIntent inShortcut intent =
  sendOwnedMessage inShortcut initWithIntentSelector (toINIntent intent)

-- | Creates a shortcut with the given user activity.
--
-- ObjC selector: @- initWithUserActivity:@
initWithUserActivity :: (IsINShortcut inShortcut, IsNSUserActivity userActivity) => inShortcut -> userActivity -> IO (Id INShortcut)
initWithUserActivity inShortcut userActivity =
  sendOwnedMessage inShortcut initWithUserActivitySelector (toNSUserActivity userActivity)

-- | The intent that will be performed when this shortcut is invoked.
--
-- Is @nil@ if the shortcut was created with a @NSUserActivity.@
--
-- ObjC selector: @- intent@
intent :: IsINShortcut inShortcut => inShortcut -> IO (Id INIntent)
intent inShortcut =
  sendMessage inShortcut intentSelector

-- | The user activity that will be performed when this shortcut is invoked.
--
-- Is @nil@ if the shortcut was created with an @INIntent.@
--
-- ObjC selector: @- userActivity@
userActivity :: IsINShortcut inShortcut => inShortcut -> IO (Id NSUserActivity)
userActivity inShortcut =
  sendMessage inShortcut userActivitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id INShortcut)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INShortcut)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIntent:@
initWithIntentSelector :: Selector '[Id INIntent] (Id INShortcut)
initWithIntentSelector = mkSelector "initWithIntent:"

-- | @Selector@ for @initWithUserActivity:@
initWithUserActivitySelector :: Selector '[Id NSUserActivity] (Id INShortcut)
initWithUserActivitySelector = mkSelector "initWithUserActivity:"

-- | @Selector@ for @intent@
intentSelector :: Selector '[] (Id INIntent)
intentSelector = mkSelector "intent"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector '[] (Id NSUserActivity)
userActivitySelector = mkSelector "userActivity"

