{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFeatureBoolean
--
-- ICScannerFeatureBoolean object is used to represent a property of a scanner functional unit whose value can be YES or NO.
--
-- Generated bindings for @ICScannerFeatureBoolean@.
module ObjC.ImageCaptureCore.ICScannerFeatureBoolean
  ( ICScannerFeatureBoolean
  , IsICScannerFeatureBoolean(..)
  , value
  , setValue
  , valueSelector
  , setValueSelector


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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | value
--
-- ￼The value of this feature.
--
-- ObjC selector: @- value@
value :: IsICScannerFeatureBoolean icScannerFeatureBoolean => icScannerFeatureBoolean -> IO Bool
value icScannerFeatureBoolean  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFeatureBoolean (mkSelector "value") retCULong []

-- | value
--
-- ￼The value of this feature.
--
-- ObjC selector: @- setValue:@
setValue :: IsICScannerFeatureBoolean icScannerFeatureBoolean => icScannerFeatureBoolean -> Bool -> IO ()
setValue icScannerFeatureBoolean  value =
  sendMsg icScannerFeatureBoolean (mkSelector "setValue:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

