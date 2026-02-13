{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAScrollLayer@.
module ObjC.QuartzCore.CAScrollLayer
  ( CAScrollLayer
  , IsCAScrollLayer(..)
  , scrollMode
  , setScrollMode
  , scrollModeSelector
  , setScrollModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- scrollMode@
scrollMode :: IsCAScrollLayer caScrollLayer => caScrollLayer -> IO (Id NSString)
scrollMode caScrollLayer =
  sendMessage caScrollLayer scrollModeSelector

-- | @- setScrollMode:@
setScrollMode :: (IsCAScrollLayer caScrollLayer, IsNSString value) => caScrollLayer -> value -> IO ()
setScrollMode caScrollLayer value =
  sendMessage caScrollLayer setScrollModeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollMode@
scrollModeSelector :: Selector '[] (Id NSString)
scrollModeSelector = mkSelector "scrollMode"

-- | @Selector@ for @setScrollMode:@
setScrollModeSelector :: Selector '[Id NSString] ()
setScrollModeSelector = mkSelector "setScrollMode:"

