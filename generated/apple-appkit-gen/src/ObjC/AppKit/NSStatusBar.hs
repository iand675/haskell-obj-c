{-# LANGUAGE DataKinds #-}
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
  , removeStatusItemSelector
  , statusItemWithLengthSelector
  , systemStatusBarSelector
  , thicknessSelector
  , verticalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- statusItemWithLength:@
statusItemWithLength :: IsNSStatusBar nsStatusBar => nsStatusBar -> CDouble -> IO (Id NSStatusItem)
statusItemWithLength nsStatusBar length_ =
  sendMessage nsStatusBar statusItemWithLengthSelector length_

-- | @- removeStatusItem:@
removeStatusItem :: (IsNSStatusBar nsStatusBar, IsNSStatusItem item) => nsStatusBar -> item -> IO ()
removeStatusItem nsStatusBar item =
  sendMessage nsStatusBar removeStatusItemSelector (toNSStatusItem item)

-- | @+ systemStatusBar@
systemStatusBar :: IO (Id NSStatusBar)
systemStatusBar  =
  do
    cls' <- getRequiredClass "NSStatusBar"
    sendClassMessage cls' systemStatusBarSelector

-- | @- vertical@
vertical :: IsNSStatusBar nsStatusBar => nsStatusBar -> IO Bool
vertical nsStatusBar =
  sendMessage nsStatusBar verticalSelector

-- | @- thickness@
thickness :: IsNSStatusBar nsStatusBar => nsStatusBar -> IO CDouble
thickness nsStatusBar =
  sendMessage nsStatusBar thicknessSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statusItemWithLength:@
statusItemWithLengthSelector :: Selector '[CDouble] (Id NSStatusItem)
statusItemWithLengthSelector = mkSelector "statusItemWithLength:"

-- | @Selector@ for @removeStatusItem:@
removeStatusItemSelector :: Selector '[Id NSStatusItem] ()
removeStatusItemSelector = mkSelector "removeStatusItem:"

-- | @Selector@ for @systemStatusBar@
systemStatusBarSelector :: Selector '[] (Id NSStatusBar)
systemStatusBarSelector = mkSelector "systemStatusBar"

-- | @Selector@ for @vertical@
verticalSelector :: Selector '[] Bool
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @thickness@
thicknessSelector :: Selector '[] CDouble
thicknessSelector = mkSelector "thickness"

