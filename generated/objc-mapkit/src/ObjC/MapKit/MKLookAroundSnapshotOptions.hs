{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSnapshotOptions@.
module ObjC.MapKit.MKLookAroundSnapshotOptions
  ( MKLookAroundSnapshotOptions
  , IsMKLookAroundSnapshotOptions(..)
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLookAroundSnapshotOptions mkLookAroundSnapshotOptions => mkLookAroundSnapshotOptions -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLookAroundSnapshotOptions  =
  sendMsg mkLookAroundSnapshotOptions (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLookAroundSnapshotOptions mkLookAroundSnapshotOptions, IsMKPointOfInterestFilter value) => mkLookAroundSnapshotOptions -> value -> IO ()
setPointOfInterestFilter mkLookAroundSnapshotOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLookAroundSnapshotOptions (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

