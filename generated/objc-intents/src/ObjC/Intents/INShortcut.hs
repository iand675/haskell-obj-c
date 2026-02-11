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
  , newSelector
  , initSelector
  , initWithIntentSelector
  , initWithUserActivitySelector
  , intentSelector
  , userActivitySelector


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
import ObjC.Foundation.Internal.Classes

-- | Note: Must be initilaized with either an intent or user activity, using those initializers.
--
-- ObjC selector: @+ new@
new :: IO (Id INShortcut)
new  =
  do
    cls' <- getRequiredClass "INShortcut"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Note: Must be initilaized with either an intent or user activity, using those initializers.
--
-- ObjC selector: @- init@
init_ :: IsINShortcut inShortcut => inShortcut -> IO (Id INShortcut)
init_ inShortcut  =
  sendMsg inShortcut (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a shortcut with the given intent.
--
-- @intent@ — Unless user configurable, must have a title and have valid shortcut types.
--
-- Returns: Will return @nil@ (and log an error) if the intent isn't valid.
--
-- ObjC selector: @- initWithIntent:@
initWithIntent :: (IsINShortcut inShortcut, IsINIntent intent) => inShortcut -> intent -> IO (Id INShortcut)
initWithIntent inShortcut  intent =
withObjCPtr intent $ \raw_intent ->
    sendMsg inShortcut (mkSelector "initWithIntent:") (retPtr retVoid) [argPtr (castPtr raw_intent :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a shortcut with the given user activity.
--
-- ObjC selector: @- initWithUserActivity:@
initWithUserActivity :: (IsINShortcut inShortcut, IsNSUserActivity userActivity) => inShortcut -> userActivity -> IO (Id INShortcut)
initWithUserActivity inShortcut  userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inShortcut (mkSelector "initWithUserActivity:") (retPtr retVoid) [argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | The intent that will be performed when this shortcut is invoked.
--
-- Is @nil@ if the shortcut was created with a @NSUserActivity.@
--
-- ObjC selector: @- intent@
intent :: IsINShortcut inShortcut => inShortcut -> IO (Id INIntent)
intent inShortcut  =
  sendMsg inShortcut (mkSelector "intent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user activity that will be performed when this shortcut is invoked.
--
-- Is @nil@ if the shortcut was created with an @INIntent.@
--
-- ObjC selector: @- userActivity@
userActivity :: IsINShortcut inShortcut => inShortcut -> IO (Id NSUserActivity)
userActivity inShortcut  =
  sendMsg inShortcut (mkSelector "userActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIntent:@
initWithIntentSelector :: Selector
initWithIntentSelector = mkSelector "initWithIntent:"

-- | @Selector@ for @initWithUserActivity:@
initWithUserActivitySelector :: Selector
initWithUserActivitySelector = mkSelector "initWithUserActivity:"

-- | @Selector@ for @intent@
intentSelector :: Selector
intentSelector = mkSelector "intent"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector
userActivitySelector = mkSelector "userActivity"

