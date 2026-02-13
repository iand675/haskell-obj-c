{-# LANGUAGE DataKinds #-}
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
  , ciImageSelector
  , imageRepWithCIImageSelector
  , initWithCIImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' imageRepWithCIImageSelector (toCIImage image)

-- | @- initWithCIImage:@
initWithCIImage :: (IsNSCIImageRep nsciImageRep, IsCIImage image) => nsciImageRep -> image -> IO (Id NSCIImageRep)
initWithCIImage nsciImageRep image =
  sendOwnedMessage nsciImageRep initWithCIImageSelector (toCIImage image)

-- | @- CIImage@
ciImage :: IsNSCIImageRep nsciImageRep => nsciImageRep -> IO (Id CIImage)
ciImage nsciImageRep =
  sendMessage nsciImageRep ciImageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithCIImage:@
imageRepWithCIImageSelector :: Selector '[Id CIImage] (Id NSCIImageRep)
imageRepWithCIImageSelector = mkSelector "imageRepWithCIImage:"

-- | @Selector@ for @initWithCIImage:@
initWithCIImageSelector :: Selector '[Id CIImage] (Id NSCIImageRep)
initWithCIImageSelector = mkSelector "initWithCIImage:"

-- | @Selector@ for @CIImage@
ciImageSelector :: Selector '[] (Id CIImage)
ciImageSelector = mkSelector "CIImage"

