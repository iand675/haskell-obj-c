{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIImage@.
module ObjC.AppKit.CIImage
  ( CIImage
  , IsCIImage(..)
  , initWithBitmapImageRep
  , drawInRect_fromRect_operation_fraction
  , drawAtPoint_fromRect_operation_fraction
  , initWithBitmapImageRepSelector
  , drawInRect_fromRect_operation_fractionSelector
  , drawAtPoint_fromRect_operation_fractionSelector

  -- * Enum types
  , NSCompositingOperation(NSCompositingOperation)
  , pattern NSCompositingOperationClear
  , pattern NSCompositingOperationCopy
  , pattern NSCompositingOperationSourceOver
  , pattern NSCompositingOperationSourceIn
  , pattern NSCompositingOperationSourceOut
  , pattern NSCompositingOperationSourceAtop
  , pattern NSCompositingOperationDestinationOver
  , pattern NSCompositingOperationDestinationIn
  , pattern NSCompositingOperationDestinationOut
  , pattern NSCompositingOperationDestinationAtop
  , pattern NSCompositingOperationXOR
  , pattern NSCompositingOperationPlusDarker
  , pattern NSCompositingOperationHighlight
  , pattern NSCompositingOperationPlusLighter
  , pattern NSCompositingOperationMultiply
  , pattern NSCompositingOperationScreen
  , pattern NSCompositingOperationOverlay
  , pattern NSCompositingOperationDarken
  , pattern NSCompositingOperationLighten
  , pattern NSCompositingOperationColorDodge
  , pattern NSCompositingOperationColorBurn
  , pattern NSCompositingOperationSoftLight
  , pattern NSCompositingOperationHardLight
  , pattern NSCompositingOperationDifference
  , pattern NSCompositingOperationExclusion
  , pattern NSCompositingOperationHue
  , pattern NSCompositingOperationSaturation
  , pattern NSCompositingOperationColor
  , pattern NSCompositingOperationLuminosity

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBitmapImageRep:@
initWithBitmapImageRep :: (IsCIImage ciImage, IsNSBitmapImageRep bitmapImageRep) => ciImage -> bitmapImageRep -> IO (Id CIImage)
initWithBitmapImageRep ciImage  bitmapImageRep =
withObjCPtr bitmapImageRep $ \raw_bitmapImageRep ->
    sendMsg ciImage (mkSelector "initWithBitmapImageRep:") (retPtr retVoid) [argPtr (castPtr raw_bitmapImageRep :: Ptr ())] >>= ownedObject . castPtr

-- | @- drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fraction :: IsCIImage ciImage => ciImage -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawInRect_fromRect_operation_fraction ciImage  rect fromRect op delta =
  sendMsg ciImage (mkSelector "drawInRect:fromRect:operation:fraction:") retVoid [argNSRect rect, argNSRect fromRect, argCULong (coerce op), argCDouble (fromIntegral delta)]

-- | @- drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fraction :: IsCIImage ciImage => ciImage -> NSPoint -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawAtPoint_fromRect_operation_fraction ciImage  point fromRect op delta =
  sendMsg ciImage (mkSelector "drawAtPoint:fromRect:operation:fraction:") retVoid [argNSPoint point, argNSRect fromRect, argCULong (coerce op), argCDouble (fromIntegral delta)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBitmapImageRep:@
initWithBitmapImageRepSelector :: Selector
initWithBitmapImageRepSelector = mkSelector "initWithBitmapImageRep:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fractionSelector :: Selector
drawInRect_fromRect_operation_fractionSelector = mkSelector "drawInRect:fromRect:operation:fraction:"

-- | @Selector@ for @drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fractionSelector :: Selector
drawAtPoint_fromRect_operation_fractionSelector = mkSelector "drawAtPoint:fromRect:operation:fraction:"

