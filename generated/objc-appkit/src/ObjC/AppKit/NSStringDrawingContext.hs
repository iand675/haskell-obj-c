{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStringDrawingContext@.
module ObjC.AppKit.NSStringDrawingContext
  ( NSStringDrawingContext
  , IsNSStringDrawingContext(..)
  , minimumScaleFactor
  , setMinimumScaleFactor
  , actualScaleFactor
  , minimumScaleFactorSelector
  , setMinimumScaleFactorSelector
  , actualScaleFactorSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minimumScaleFactor@
minimumScaleFactor :: IsNSStringDrawingContext nsStringDrawingContext => nsStringDrawingContext -> IO CDouble
minimumScaleFactor nsStringDrawingContext  =
  sendMsg nsStringDrawingContext (mkSelector "minimumScaleFactor") retCDouble []

-- | @- setMinimumScaleFactor:@
setMinimumScaleFactor :: IsNSStringDrawingContext nsStringDrawingContext => nsStringDrawingContext -> CDouble -> IO ()
setMinimumScaleFactor nsStringDrawingContext  value =
  sendMsg nsStringDrawingContext (mkSelector "setMinimumScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- actualScaleFactor@
actualScaleFactor :: IsNSStringDrawingContext nsStringDrawingContext => nsStringDrawingContext -> IO CDouble
actualScaleFactor nsStringDrawingContext  =
  sendMsg nsStringDrawingContext (mkSelector "actualScaleFactor") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumScaleFactor@
minimumScaleFactorSelector :: Selector
minimumScaleFactorSelector = mkSelector "minimumScaleFactor"

-- | @Selector@ for @setMinimumScaleFactor:@
setMinimumScaleFactorSelector :: Selector
setMinimumScaleFactorSelector = mkSelector "setMinimumScaleFactor:"

-- | @Selector@ for @actualScaleFactor@
actualScaleFactorSelector :: Selector
actualScaleFactorSelector = mkSelector "actualScaleFactor"

