{-# LANGUAGE DataKinds #-}
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
  , displayIDSelector
  , heightSelector
  , initSelector
  , newSelector
  , widthSelector


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
init_ :: IsSCDisplay scDisplay => scDisplay -> IO (Id SCDisplay)
init_ scDisplay =
  sendOwnedMessage scDisplay initSelector

-- | @+ new@
new :: IO (Id SCDisplay)
new  =
  do
    cls' <- getRequiredClass "SCDisplay"
    sendOwnedClassMessage cls' newSelector

-- | displayId the CGDirectDisplayID for the SCDisplay
--
-- ObjC selector: @- displayID@
displayID :: IsSCDisplay scDisplay => scDisplay -> IO CUInt
displayID scDisplay =
  sendMessage scDisplay displayIDSelector

-- | width the width, in points, for the SCDisplay
--
-- ObjC selector: @- width@
width :: IsSCDisplay scDisplay => scDisplay -> IO CLong
width scDisplay =
  sendMessage scDisplay widthSelector

-- | height the height, in points, for the SCDisplay
--
-- ObjC selector: @- height@
height :: IsSCDisplay scDisplay => scDisplay -> IO CLong
height scDisplay =
  sendMessage scDisplay heightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCDisplay)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCDisplay)
newSelector = mkSelector "new"

-- | @Selector@ for @displayID@
displayIDSelector :: Selector '[] CUInt
displayIDSelector = mkSelector "displayID"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CLong
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CLong
heightSelector = mkSelector "height"

