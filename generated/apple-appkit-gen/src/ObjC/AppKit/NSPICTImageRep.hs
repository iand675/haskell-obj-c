{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPICTImageRep@.
module ObjC.AppKit.NSPICTImageRep
  ( NSPICTImageRep
  , IsNSPICTImageRep(..)
  , imageRepWithData
  , initWithData
  , pictRepresentation
  , boundingBox
  , boundingBoxSelector
  , imageRepWithDataSelector
  , initWithDataSelector
  , pictRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData pictData => pictData -> IO (Id NSPICTImageRep)
imageRepWithData pictData =
  do
    cls' <- getRequiredClass "NSPICTImageRep"
    sendClassMessage cls' imageRepWithDataSelector (toNSData pictData)

-- | @- initWithData:@
initWithData :: (IsNSPICTImageRep nspictImageRep, IsNSData pictData) => nspictImageRep -> pictData -> IO (Id NSPICTImageRep)
initWithData nspictImageRep pictData =
  sendOwnedMessage nspictImageRep initWithDataSelector (toNSData pictData)

-- | @- PICTRepresentation@
pictRepresentation :: IsNSPICTImageRep nspictImageRep => nspictImageRep -> IO (Id NSData)
pictRepresentation nspictImageRep =
  sendMessage nspictImageRep pictRepresentationSelector

-- | @- boundingBox@
boundingBox :: IsNSPICTImageRep nspictImageRep => nspictImageRep -> IO NSRect
boundingBox nspictImageRep =
  sendMessage nspictImageRep boundingBoxSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector '[Id NSData] (Id NSPICTImageRep)
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSPICTImageRep)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @PICTRepresentation@
pictRepresentationSelector :: Selector '[] (Id NSData)
pictRepresentationSelector = mkSelector "PICTRepresentation"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector '[] NSRect
boundingBoxSelector = mkSelector "boundingBox"

