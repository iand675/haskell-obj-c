{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCDisplay@.
module ObjC.ScreenCaptureKit.SCDisplay
  ( SCDisplay
  , IsSCDisplay(..)
  , init_
  , new
  , displayID
  , width
  , height
  , initSelector
  , newSelector
  , displayIDSelector
  , widthSelector
  , heightSelector


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
init_ :: IsSCDisplay scDisplay => scDisplay -> IO (Id SCDisplay)
init_ scDisplay  =
  sendMsg scDisplay (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCDisplay)
new  =
  do
    cls' <- getRequiredClass "SCDisplay"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | displayId the CGDirectDisplayID for the SCDisplay
--
-- ObjC selector: @- displayID@
displayID :: IsSCDisplay scDisplay => scDisplay -> IO CUInt
displayID scDisplay  =
  sendMsg scDisplay (mkSelector "displayID") retCUInt []

-- | width the width, in points, for the SCDisplay
--
-- ObjC selector: @- width@
width :: IsSCDisplay scDisplay => scDisplay -> IO CLong
width scDisplay  =
  sendMsg scDisplay (mkSelector "width") retCLong []

-- | height the height, in points, for the SCDisplay
--
-- ObjC selector: @- height@
height :: IsSCDisplay scDisplay => scDisplay -> IO CLong
height scDisplay  =
  sendMsg scDisplay (mkSelector "height") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @displayID@
displayIDSelector :: Selector
displayIDSelector = mkSelector "displayID"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

