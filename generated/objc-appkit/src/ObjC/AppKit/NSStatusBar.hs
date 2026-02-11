{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStatusBar@.
module ObjC.AppKit.NSStatusBar
  ( NSStatusBar
  , IsNSStatusBar(..)
  , statusItemWithLength
  , removeStatusItem
  , systemStatusBar
  , vertical
  , thickness
  , statusItemWithLengthSelector
  , removeStatusItemSelector
  , systemStatusBarSelector
  , verticalSelector
  , thicknessSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- statusItemWithLength:@
statusItemWithLength :: IsNSStatusBar nsStatusBar => nsStatusBar -> CDouble -> IO (Id NSStatusItem)
statusItemWithLength nsStatusBar  length_ =
  sendMsg nsStatusBar (mkSelector "statusItemWithLength:") (retPtr retVoid) [argCDouble (fromIntegral length_)] >>= retainedObject . castPtr

-- | @- removeStatusItem:@
removeStatusItem :: (IsNSStatusBar nsStatusBar, IsNSStatusItem item) => nsStatusBar -> item -> IO ()
removeStatusItem nsStatusBar  item =
withObjCPtr item $ \raw_item ->
    sendMsg nsStatusBar (mkSelector "removeStatusItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | @+ systemStatusBar@
systemStatusBar :: IO (Id NSStatusBar)
systemStatusBar  =
  do
    cls' <- getRequiredClass "NSStatusBar"
    sendClassMsg cls' (mkSelector "systemStatusBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- vertical@
vertical :: IsNSStatusBar nsStatusBar => nsStatusBar -> IO Bool
vertical nsStatusBar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStatusBar (mkSelector "vertical") retCULong []

-- | @- thickness@
thickness :: IsNSStatusBar nsStatusBar => nsStatusBar -> IO CDouble
thickness nsStatusBar  =
  sendMsg nsStatusBar (mkSelector "thickness") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statusItemWithLength:@
statusItemWithLengthSelector :: Selector
statusItemWithLengthSelector = mkSelector "statusItemWithLength:"

-- | @Selector@ for @removeStatusItem:@
removeStatusItemSelector :: Selector
removeStatusItemSelector = mkSelector "removeStatusItem:"

-- | @Selector@ for @systemStatusBar@
systemStatusBarSelector :: Selector
systemStatusBarSelector = mkSelector "systemStatusBar"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @thickness@
thicknessSelector :: Selector
thicknessSelector = mkSelector "thickness"

