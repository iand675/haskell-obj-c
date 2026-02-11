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
  , initSelector
  , newSelector
  , windowIDSelector
  , titleSelector
  , windowLayerSelector
  , owningApplicationSelector
  , onScreenSelector
  , activeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCWindow scWindow => scWindow -> IO (Id SCWindow)
init_ scWindow  =
  sendMsg scWindow (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCWindow)
new  =
  do
    cls' <- getRequiredClass "SCWindow"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | windowID the CGWindowID for the SCWindow
--
-- ObjC selector: @- windowID@
windowID :: IsSCWindow scWindow => scWindow -> IO CUInt
windowID scWindow  =
  sendMsg scWindow (mkSelector "windowID") retCUInt []

-- | title the window title for the SCWindow
--
-- ObjC selector: @- title@
title :: IsSCWindow scWindow => scWindow -> IO (Id NSString)
title scWindow  =
  sendMsg scWindow (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | windowLayer the window layer for the SCWindow
--
-- ObjC selector: @- windowLayer@
windowLayer :: IsSCWindow scWindow => scWindow -> IO CLong
windowLayer scWindow  =
  sendMsg scWindow (mkSelector "windowLayer") retCLong []

-- | owningApplication is the SCRunningApplication that owns this SCWindow
--
-- ObjC selector: @- owningApplication@
owningApplication :: IsSCWindow scWindow => scWindow -> IO (Id SCRunningApplication)
owningApplication scWindow  =
  sendMsg scWindow (mkSelector "owningApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | onScreen the bool property denoting of the SCWindow is on the screen
--
-- ObjC selector: @- onScreen@
onScreen :: IsSCWindow scWindow => scWindow -> IO Bool
onScreen scWindow  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scWindow (mkSelector "onScreen") retCULong []

-- | active the bool property denoting of the SCWindow is active. with Stage Manager, SCWindow can be offScreen and active
--
-- ObjC selector: @- active@
active :: IsSCWindow scWindow => scWindow -> IO Bool
active scWindow  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scWindow (mkSelector "active") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @windowID@
windowIDSelector :: Selector
windowIDSelector = mkSelector "windowID"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @windowLayer@
windowLayerSelector :: Selector
windowLayerSelector = mkSelector "windowLayer"

-- | @Selector@ for @owningApplication@
owningApplicationSelector :: Selector
owningApplicationSelector = mkSelector "owningApplication"

-- | @Selector@ for @onScreen@
onScreenSelector :: Selector
onScreenSelector = mkSelector "onScreen"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

