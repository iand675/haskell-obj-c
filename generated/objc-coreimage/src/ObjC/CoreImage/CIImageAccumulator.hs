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
  , imageSelector
  , setImageSelector
  , clearSelector
  , formatSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO (Id CIImage)
image ciImageAccumulator  =
  sendMsg ciImageAccumulator (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsCIImageAccumulator ciImageAccumulator, IsCIImage image) => ciImageAccumulator -> image -> IO ()
setImage ciImageAccumulator  image =
withObjCPtr image $ \raw_image ->
    sendMsg ciImageAccumulator (mkSelector "setImage:") retVoid [argPtr (castPtr raw_image :: Ptr ())]

-- | @- clear@
clear :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO ()
clear ciImageAccumulator  =
  sendMsg ciImageAccumulator (mkSelector "clear") retVoid []

-- | @- format@
format :: IsCIImageAccumulator ciImageAccumulator => ciImageAccumulator -> IO CInt
format ciImageAccumulator  =
  sendMsg ciImageAccumulator (mkSelector "format") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

