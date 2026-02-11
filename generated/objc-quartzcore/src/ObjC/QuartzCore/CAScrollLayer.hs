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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- scrollMode@
scrollMode :: IsCAScrollLayer caScrollLayer => caScrollLayer -> IO (Id NSString)
scrollMode caScrollLayer  =
  sendMsg caScrollLayer (mkSelector "scrollMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScrollMode:@
setScrollMode :: (IsCAScrollLayer caScrollLayer, IsNSString value) => caScrollLayer -> value -> IO ()
setScrollMode caScrollLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caScrollLayer (mkSelector "setScrollMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollMode@
scrollModeSelector :: Selector
scrollModeSelector = mkSelector "scrollMode"

-- | @Selector@ for @setScrollMode:@
setScrollModeSelector :: Selector
setScrollModeSelector = mkSelector "setScrollMode:"

