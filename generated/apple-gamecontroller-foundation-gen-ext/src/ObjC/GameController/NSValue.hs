{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.GameController.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithGCPoint2
  , gcPoint2Value
  , gcPoint2ValueSelector
  , valueWithGCPoint2Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' valueWithGCPoint2Selector point

-- | @- GCPoint2Value@
gcPoint2Value :: IsNSValue nsValue => nsValue -> IO GCPoint2
gcPoint2Value nsValue =
  sendMessage nsValue gcPoint2ValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithGCPoint2:@
valueWithGCPoint2Selector :: Selector '[GCPoint2] (Id NSValue)
valueWithGCPoint2Selector = mkSelector "valueWithGCPoint2:"

-- | @Selector@ for @GCPoint2Value@
gcPoint2ValueSelector :: Selector '[] GCPoint2
gcPoint2ValueSelector = mkSelector "GCPoint2Value"

