{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , formatIdentifierSelector
  , formatVersionSelector
  , initWithFormatIdentifier_formatVersion_dataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFormatIdentifier:formatVersion:data:@
initWithFormatIdentifier_formatVersion_data :: (IsPHAdjustmentData phAdjustmentData, IsNSString formatIdentifier, IsNSString formatVersion, IsNSData data_) => phAdjustmentData -> formatIdentifier -> formatVersion -> data_ -> IO (Id PHAdjustmentData)
initWithFormatIdentifier_formatVersion_data phAdjustmentData formatIdentifier formatVersion data_ =
  sendOwnedMessage phAdjustmentData initWithFormatIdentifier_formatVersion_dataSelector (toNSString formatIdentifier) (toNSString formatVersion) (toNSData data_)

-- | @- formatIdentifier@
formatIdentifier :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSString)
formatIdentifier phAdjustmentData =
  sendMessage phAdjustmentData formatIdentifierSelector

-- | @- formatVersion@
formatVersion :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSString)
formatVersion phAdjustmentData =
  sendMessage phAdjustmentData formatVersionSelector

-- | @- data@
data_ :: IsPHAdjustmentData phAdjustmentData => phAdjustmentData -> IO (Id NSData)
data_ phAdjustmentData =
  sendMessage phAdjustmentData dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormatIdentifier:formatVersion:data:@
initWithFormatIdentifier_formatVersion_dataSelector :: Selector '[Id NSString, Id NSString, Id NSData] (Id PHAdjustmentData)
initWithFormatIdentifier_formatVersion_dataSelector = mkSelector "initWithFormatIdentifier:formatVersion:data:"

-- | @Selector@ for @formatIdentifier@
formatIdentifierSelector :: Selector '[] (Id NSString)
formatIdentifierSelector = mkSelector "formatIdentifier"

-- | @Selector@ for @formatVersion@
formatVersionSelector :: Selector '[] (Id NSString)
formatVersionSelector = mkSelector "formatVersion"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

