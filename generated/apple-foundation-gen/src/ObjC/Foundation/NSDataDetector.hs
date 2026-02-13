{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDataDetector@.
module ObjC.Foundation.NSDataDetector
  ( NSDataDetector
  , IsNSDataDetector(..)
  , dataDetectorWithTypes_error
  , initWithTypes_error
  , checkingTypes
  , checkingTypesSelector
  , dataDetectorWithTypes_errorSelector
  , initWithTypes_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ dataDetectorWithTypes:error:@
dataDetectorWithTypes_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSDataDetector)
dataDetectorWithTypes_error checkingTypes error_ =
  do
    cls' <- getRequiredClass "NSDataDetector"
    sendClassMessage cls' dataDetectorWithTypes_errorSelector checkingTypes (toNSError error_)

-- | @- initWithTypes:error:@
initWithTypes_error :: (IsNSDataDetector nsDataDetector, IsNSError error_) => nsDataDetector -> CULong -> error_ -> IO (Id NSDataDetector)
initWithTypes_error nsDataDetector checkingTypes error_ =
  sendOwnedMessage nsDataDetector initWithTypes_errorSelector checkingTypes (toNSError error_)

-- | @- checkingTypes@
checkingTypes :: IsNSDataDetector nsDataDetector => nsDataDetector -> IO CULong
checkingTypes nsDataDetector =
  sendMessage nsDataDetector checkingTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dataDetectorWithTypes:error:@
dataDetectorWithTypes_errorSelector :: Selector '[CULong, Id NSError] (Id NSDataDetector)
dataDetectorWithTypes_errorSelector = mkSelector "dataDetectorWithTypes:error:"

-- | @Selector@ for @initWithTypes:error:@
initWithTypes_errorSelector :: Selector '[CULong, Id NSError] (Id NSDataDetector)
initWithTypes_errorSelector = mkSelector "initWithTypes:error:"

-- | @Selector@ for @checkingTypes@
checkingTypesSelector :: Selector '[] CULong
checkingTypesSelector = mkSelector "checkingTypes"

