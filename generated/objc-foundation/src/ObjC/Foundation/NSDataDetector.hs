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
  , dataDetectorWithTypes_errorSelector
  , initWithTypes_errorSelector
  , checkingTypesSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ dataDetectorWithTypes:error:@
dataDetectorWithTypes_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSDataDetector)
dataDetectorWithTypes_error checkingTypes error_ =
  do
    cls' <- getRequiredClass "NSDataDetector"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "dataDetectorWithTypes:error:") (retPtr retVoid) [argCULong (fromIntegral checkingTypes), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithTypes:error:@
initWithTypes_error :: (IsNSDataDetector nsDataDetector, IsNSError error_) => nsDataDetector -> CULong -> error_ -> IO (Id NSDataDetector)
initWithTypes_error nsDataDetector  checkingTypes error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsDataDetector (mkSelector "initWithTypes:error:") (retPtr retVoid) [argCULong (fromIntegral checkingTypes), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- checkingTypes@
checkingTypes :: IsNSDataDetector nsDataDetector => nsDataDetector -> IO CULong
checkingTypes nsDataDetector  =
  sendMsg nsDataDetector (mkSelector "checkingTypes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dataDetectorWithTypes:error:@
dataDetectorWithTypes_errorSelector :: Selector
dataDetectorWithTypes_errorSelector = mkSelector "dataDetectorWithTypes:error:"

-- | @Selector@ for @initWithTypes:error:@
initWithTypes_errorSelector :: Selector
initWithTypes_errorSelector = mkSelector "initWithTypes:error:"

-- | @Selector@ for @checkingTypes@
checkingTypesSelector :: Selector
checkingTypesSelector = mkSelector "checkingTypes"

