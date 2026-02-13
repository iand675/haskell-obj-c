{-# LANGUAGE DataKinds #-}
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
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
value icScannerFeatureBoolean =
  sendMessage icScannerFeatureBoolean valueSelector

-- | value
--
-- ￼The value of this feature.
--
-- ObjC selector: @- setValue:@
setValue :: IsICScannerFeatureBoolean icScannerFeatureBoolean => icScannerFeatureBoolean -> Bool -> IO ()
setValue icScannerFeatureBoolean value =
  sendMessage icScannerFeatureBoolean setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @value@
valueSelector :: Selector '[] Bool
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Bool] ()
setValueSelector = mkSelector "setValue:"

