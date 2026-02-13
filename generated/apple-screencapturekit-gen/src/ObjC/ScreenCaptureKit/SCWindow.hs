{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCWindow@.
module ObjC.ScreenCaptureKit.SCWindow
  ( SCWindow
  , IsSCWindow(..)
  , init_
  , new
  , windowID
  , title
  , windowLayer
  , owningApplication
  , onScreen
  , active
  , activeSelector
  , initSelector
  , newSelector
  , onScreenSelector
  , owningApplicationSelector
  , titleSelector
  , windowIDSelector
  , windowLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCWindow scWindow => scWindow -> IO (Id SCWindow)
init_ scWindow =
  sendOwnedMessage scWindow initSelector

-- | @+ new@
new :: IO (Id SCWindow)
new  =
  do
    cls' <- getRequiredClass "SCWindow"
    sendOwnedClassMessage cls' newSelector

-- | windowID the CGWindowID for the SCWindow
--
-- ObjC selector: @- windowID@
windowID :: IsSCWindow scWindow => scWindow -> IO CUInt
windowID scWindow =
  sendMessage scWindow windowIDSelector

-- | title the window title for the SCWindow
--
-- ObjC selector: @- title@
title :: IsSCWindow scWindow => scWindow -> IO (Id NSString)
title scWindow =
  sendMessage scWindow titleSelector

-- | windowLayer the window layer for the SCWindow
--
-- ObjC selector: @- windowLayer@
windowLayer :: IsSCWindow scWindow => scWindow -> IO CLong
windowLayer scWindow =
  sendMessage scWindow windowLayerSelector

-- | owningApplication is the SCRunningApplication that owns this SCWindow
--
-- ObjC selector: @- owningApplication@
owningApplication :: IsSCWindow scWindow => scWindow -> IO (Id SCRunningApplication)
owningApplication scWindow =
  sendMessage scWindow owningApplicationSelector

-- | onScreen the bool property denoting of the SCWindow is on the screen
--
-- ObjC selector: @- onScreen@
onScreen :: IsSCWindow scWindow => scWindow -> IO Bool
onScreen scWindow =
  sendMessage scWindow onScreenSelector

-- | active the bool property denoting of the SCWindow is active. with Stage Manager, SCWindow can be offScreen and active
--
-- ObjC selector: @- active@
active :: IsSCWindow scWindow => scWindow -> IO Bool
active scWindow =
  sendMessage scWindow activeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCWindow)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCWindow)
newSelector = mkSelector "new"

-- | @Selector@ for @windowID@
windowIDSelector :: Selector '[] CUInt
windowIDSelector = mkSelector "windowID"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @windowLayer@
windowLayerSelector :: Selector '[] CLong
windowLayerSelector = mkSelector "windowLayer"

-- | @Selector@ for @owningApplication@
owningApplicationSelector :: Selector '[] (Id SCRunningApplication)
owningApplicationSelector = mkSelector "owningApplication"

-- | @Selector@ for @onScreen@
onScreenSelector :: Selector '[] Bool
onScreenSelector = mkSelector "onScreen"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

