{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIImageAccumulator@.
module ObjC.CoreImage.CIImageAccumulator
  ( CIImageAccumulator
  , IsCIImageAccumulator(..)
  , image
  , setImage
  , clear
  , format
  , clearSelector
  , formatSelector
  , imageSelector
  , setImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO (Id CIImage)
image ciImageAccumulator =
  sendMessage ciImageAccumulator imageSelector

-- | @- setImage:@
setImage :: (IsCIImageAccumulator ciImageAccumulator, IsCIImage image) => ciImageAccumulator -> image -> IO ()
setImage ciImageAccumulator image =
  sendMessage ciImageAccumulator setImageSelector (toCIImage image)

-- | @- clear@
clear :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO ()
clear ciImageAccumulator =
  sendMessage ciImageAccumulator clearSelector

-- | @- format@
format :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO CInt
format ciImageAccumulator =
  sendMessage ciImageAccumulator formatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id CIImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id CIImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @clear@
clearSelector :: Selector '[] ()
clearSelector = mkSelector "clear"

-- | @Selector@ for @format@
formatSelector :: Selector '[] CInt
formatSelector = mkSelector "format"

