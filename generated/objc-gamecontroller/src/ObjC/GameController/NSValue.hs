{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.GameController.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithGCPoint2
  , gcPoint2Value
  , valueWithGCPoint2Selector
  , gcPoint2ValueSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithGCPoint2:@
valueWithGCPoint2 :: GCPoint2 -> IO (Id NSValue)
valueWithGCPoint2 point =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithGCPoint2:") (retPtr retVoid) [argGCPoint2 point] >>= retainedObject . castPtr

-- | @- GCPoint2Value@
gcPoint2Value :: IsNSValue nsValue => nsValue -> IO GCPoint2
gcPoint2Value nsValue  =
  sendMsgStret nsValue (mkSelector "GCPoint2Value") retGCPoint2 []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithGCPoint2:@
valueWithGCPoint2Selector :: Selector
valueWithGCPoint2Selector = mkSelector "valueWithGCPoint2:"

-- | @Selector@ for @GCPoint2Value@
gcPoint2ValueSelector :: Selector
gcPoint2ValueSelector = mkSelector "GCPoint2Value"

