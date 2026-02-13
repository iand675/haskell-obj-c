{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKDialogController@.
module ObjC.GameKit.GKDialogController
  ( GKDialogController
  , IsGKDialogController(..)
  , presentViewController
  , dismiss
  , sharedDialogController
  , parentWindow
  , setParentWindow
  , dismissSelector
  , parentWindowSelector
  , presentViewControllerSelector
  , setParentWindowSelector
  , sharedDialogControllerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentViewController:@
presentViewController :: (IsGKDialogController gkDialogController, IsNSViewController viewController) => gkDialogController -> viewController -> IO Bool
presentViewController gkDialogController viewController =
  sendMessage gkDialogController presentViewControllerSelector (toNSViewController viewController)

-- | @- dismiss:@
dismiss :: IsGKDialogController gkDialogController => gkDialogController -> RawId -> IO ()
dismiss gkDialogController sender =
  sendMessage gkDialogController dismissSelector sender

-- | @+ sharedDialogController@
sharedDialogController :: IO (Id GKDialogController)
sharedDialogController  =
  do
    cls' <- getRequiredClass "GKDialogController"
    sendClassMessage cls' sharedDialogControllerSelector

-- | @- parentWindow@
parentWindow :: IsGKDialogController gkDialogController => gkDialogController -> IO (Id NSWindow)
parentWindow gkDialogController =
  sendMessage gkDialogController parentWindowSelector

-- | @- setParentWindow:@
setParentWindow :: (IsGKDialogController gkDialogController, IsNSWindow value) => gkDialogController -> value -> IO ()
setParentWindow gkDialogController value =
  sendMessage gkDialogController setParentWindowSelector (toNSWindow value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentViewController:@
presentViewControllerSelector :: Selector '[Id NSViewController] Bool
presentViewControllerSelector = mkSelector "presentViewController:"

-- | @Selector@ for @dismiss:@
dismissSelector :: Selector '[RawId] ()
dismissSelector = mkSelector "dismiss:"

-- | @Selector@ for @sharedDialogController@
sharedDialogControllerSelector :: Selector '[] (Id GKDialogController)
sharedDialogControllerSelector = mkSelector "sharedDialogController"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector '[] (Id NSWindow)
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector '[Id NSWindow] ()
setParentWindowSelector = mkSelector "setParentWindow:"

