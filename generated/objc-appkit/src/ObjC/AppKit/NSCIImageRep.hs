{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCIImageRep@.
module ObjC.AppKit.NSCIImageRep
  ( NSCIImageRep
  , IsNSCIImageRep(..)
  , imageRepWithCIImage
  , initWithCIImage
  , ciImage
  , imageRepWithCIImageSelector
  , initWithCIImageSelector
  , ciImageSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ imageRepWithCIImage:@
imageRepWithCIImage :: IsCIImage image => image -> IO (Id NSCIImageRep)
imageRepWithCIImage image =
  do
    cls' <- getRequiredClass "NSCIImageRep"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "imageRepWithCIImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithCIImage:@
initWithCIImage :: (IsNSCIImageRep nsciImageRep, IsCIImage image) => nsciImageRep -> image -> IO (Id NSCIImageRep)
initWithCIImage nsciImageRep  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsciImageRep (mkSelector "initWithCIImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- CIImage@
ciImage :: IsNSCIImageRep nsciImageRep => nsciImageRep -> IO (Id CIImage)
ciImage nsciImageRep  =
  sendMsg nsciImageRep (mkSelector "CIImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithCIImage:@
imageRepWithCIImageSelector :: Selector
imageRepWithCIImageSelector = mkSelector "imageRepWithCIImage:"

-- | @Selector@ for @initWithCIImage:@
initWithCIImageSelector :: Selector
initWithCIImageSelector = mkSelector "initWithCIImage:"

-- | @Selector@ for @CIImage@
ciImageSelector :: Selector
ciImageSelector = mkSelector "CIImage"

