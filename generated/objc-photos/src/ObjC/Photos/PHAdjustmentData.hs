{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAdjustmentData@.
module ObjC.Photos.PHAdjustmentData
  ( PHAdjustmentData
  , IsPHAdjustmentData(..)
  , initWithFormatIdentifier_formatVersion_data
  , formatIdentifier
  , formatVersion
  , data_
  , initWithFormatIdentifier_formatVersion_dataSelector
  , formatIdentifierSelector
  , formatVersionSelector
  , dataSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFormatIdentifier:formatVersion:data:@
initWithFormatIdentifier_formatVersion_data :: (IsPHAdjustmentData phAdjustmentData, IsNSString formatIdentifier, IsNSString formatVersion, IsNSData data_) => phAdjustmentData -> formatIdentifier -> formatVersion -> data_ -> IO (Id PHAdjustmentData)
initWithFormatIdentifier_formatVersion_data phAdjustmentData  formatIdentifier formatVersion data_ =
withObjCPtr formatIdentifier $ \raw_formatIdentifier ->
  withObjCPtr formatVersion $ \raw_formatVersion ->
    withObjCPtr data_ $ \raw_data_ ->
        sendMsg phAdjustmentData (mkSelector "initWithFormatIdentifier:formatVersion:data:") (retPtr retVoid) [argPtr (castPtr raw_formatIdentifier :: Ptr ()), argPtr (castPtr raw_formatVersion :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- formatIdentifier@
formatIdentifier :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSString)
formatIdentifier phAdjustmentData  =
  sendMsg phAdjustmentData (mkSelector "formatIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- formatVersion@
formatVersion :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSString)
formatVersion phAdjustmentData  =
  sendMsg phAdjustmentData (mkSelector "formatVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSData)
data_ phAdjustmentData  =
  sendMsg phAdjustmentData (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormatIdentifier:formatVersion:data:@
initWithFormatIdentifier_formatVersion_dataSelector :: Selector
initWithFormatIdentifier_formatVersion_dataSelector = mkSelector "initWithFormatIdentifier:formatVersion:data:"

-- | @Selector@ for @formatIdentifier@
formatIdentifierSelector :: Selector
formatIdentifierSelector = mkSelector "formatIdentifier"

-- | @Selector@ for @formatVersion@
formatVersionSelector :: Selector
formatVersionSelector = mkSelector "formatVersion"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

