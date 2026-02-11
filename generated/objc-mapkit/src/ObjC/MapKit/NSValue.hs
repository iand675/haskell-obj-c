{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.MapKit.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithMKCoordinate
  , mkCoordinateValue
  , valueWithMKCoordinateSelector
  , mkCoordinateValueSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithMKCoordinate:@
valueWithMKCoordinate :: CLLocationCoordinate2D -> IO (Id NSValue)
valueWithMKCoordinate coordinate =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithMKCoordinate:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate] >>= retainedObject . castPtr

-- | @- MKCoordinateValue@
mkCoordinateValue :: IsNSValue nsValue => nsValue -> IO CLLocationCoordinate2D
mkCoordinateValue nsValue  =
  sendMsgStret nsValue (mkSelector "MKCoordinateValue") retCLLocationCoordinate2D []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithMKCoordinate:@
valueWithMKCoordinateSelector :: Selector
valueWithMKCoordinateSelector = mkSelector "valueWithMKCoordinate:"

-- | @Selector@ for @MKCoordinateValue@
mkCoordinateValueSelector :: Selector
mkCoordinateValueSelector = mkSelector "MKCoordinateValue"

