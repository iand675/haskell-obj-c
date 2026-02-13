{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CANetworkBrowserWindowController
--
-- A window controller object that can present a window that displays available network audio devices (including AVB). The user can connect to one or more of those devices to use exclusively with his mac.
--
-- To use this class, create an instance of the CANetworkBrowserWindowController, initialize it, and call showWindow: to display the UI.
--
-- Generated bindings for @CANetworkBrowserWindowController@.
module ObjC.CoreAudioKit.CANetworkBrowserWindowController
  ( CANetworkBrowserWindowController
  , IsCANetworkBrowserWindowController(..)
  , isAVBSupported
  , init_
  , initSelector
  , isAVBSupportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides a check for determining if the current machine hardware supports AVB.
--
-- Returns: True if AVB is supported.
--
-- ObjC selector: @+ isAVBSupported@
isAVBSupported :: IO Bool
isAVBSupported  =
  do
    cls' <- getRequiredClass "CANetworkBrowserWindowController"
    sendClassMessage cls' isAVBSupportedSelector

-- | Designated initializer
--
-- Returns: initialized instance of CANetworkBrowerWindowController
--
-- ObjC selector: @- init@
init_ :: IsCANetworkBrowserWindowController caNetworkBrowserWindowController => caNetworkBrowserWindowController -> IO (Id CANetworkBrowserWindowController)
init_ caNetworkBrowserWindowController =
  sendOwnedMessage caNetworkBrowserWindowController initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAVBSupported@
isAVBSupportedSelector :: Selector '[] Bool
isAVBSupportedSelector = mkSelector "isAVBSupported"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CANetworkBrowserWindowController)
initSelector = mkSelector "init"

