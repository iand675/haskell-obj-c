{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.CoreImage.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , detectedBarcodeDescriptor
  , detectedBarcodeDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The scanned code in the user activity passed in by system scanner.
--
-- This property is optional. This value is present if the user activity was created from a source that detected a QR code or other code symbol.
--
-- ObjC selector: @- detectedBarcodeDescriptor@
detectedBarcodeDescriptor :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id CIBarcodeDescriptor)
detectedBarcodeDescriptor nsUserActivity =
  sendMessage nsUserActivity detectedBarcodeDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @detectedBarcodeDescriptor@
detectedBarcodeDescriptorSelector :: Selector '[] (Id CIBarcodeDescriptor)
detectedBarcodeDescriptorSelector = mkSelector "detectedBarcodeDescriptor"

