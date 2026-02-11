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
  , imageRepWithDataSelector
  , initWithDataSelector
  , pictRepresentationSelector
  , boundingBoxSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData pictData => pictData -> IO (Id NSPICTImageRep)
imageRepWithData pictData =
  do
    cls' <- getRequiredClass "NSPICTImageRep"
    withObjCPtr pictData $ \raw_pictData ->
      sendClassMsg cls' (mkSelector "imageRepWithData:") (retPtr retVoid) [argPtr (castPtr raw_pictData :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSPICTImageRep nspictImageRep, IsNSData pictData) => nspictImageRep -> pictData -> IO (Id NSPICTImageRep)
initWithData nspictImageRep  pictData =
withObjCPtr pictData $ \raw_pictData ->
    sendMsg nspictImageRep (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_pictData :: Ptr ())] >>= ownedObject . castPtr

-- | @- PICTRepresentation@
pictRepresentation :: IsNSPICTImageRep nspictImageRep => nspictImageRep -> IO (Id NSData)
pictRepresentation nspictImageRep  =
  sendMsg nspictImageRep (mkSelector "PICTRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boundingBox@
boundingBox :: IsNSPICTImageRep nspictImageRep => nspictImageRep -> IO NSRect
boundingBox nspictImageRep  =
  sendMsgStret nspictImageRep (mkSelector "boundingBox") retNSRect []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @PICTRepresentation@
pictRepresentationSelector :: Selector
pictRepresentationSelector = mkSelector "PICTRepresentation"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector
boundingBoxSelector = mkSelector "boundingBox"

