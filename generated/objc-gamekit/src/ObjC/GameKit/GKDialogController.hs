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
  , presentViewControllerSelector
  , dismissSelector
  , sharedDialogControllerSelector
  , parentWindowSelector
  , setParentWindowSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentViewController:@
presentViewController :: (IsGKDialogController gkDialogController, IsNSViewController viewController) => gkDialogController -> viewController -> IO Bool
presentViewController gkDialogController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkDialogController (mkSelector "presentViewController:") retCULong [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- dismiss:@
dismiss :: IsGKDialogController gkDialogController => gkDialogController -> RawId -> IO ()
dismiss gkDialogController  sender =
  sendMsg gkDialogController (mkSelector "dismiss:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ sharedDialogController@
sharedDialogController :: IO (Id GKDialogController)
sharedDialogController  =
  do
    cls' <- getRequiredClass "GKDialogController"
    sendClassMsg cls' (mkSelector "sharedDialogController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentWindow@
parentWindow :: IsGKDialogController gkDialogController => gkDialogController -> IO (Id NSWindow)
parentWindow gkDialogController  =
  sendMsg gkDialogController (mkSelector "parentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParentWindow:@
setParentWindow :: (IsGKDialogController gkDialogController, IsNSWindow value) => gkDialogController -> value -> IO ()
setParentWindow gkDialogController  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkDialogController (mkSelector "setParentWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentViewController:@
presentViewControllerSelector :: Selector
presentViewControllerSelector = mkSelector "presentViewController:"

-- | @Selector@ for @dismiss:@
dismissSelector :: Selector
dismissSelector = mkSelector "dismiss:"

-- | @Selector@ for @sharedDialogController@
sharedDialogControllerSelector :: Selector
sharedDialogControllerSelector = mkSelector "sharedDialogController"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector
setParentWindowSelector = mkSelector "setParentWindow:"

