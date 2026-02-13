{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMXPathResult@.
module ObjC.WebKit.DOMXPathResult
  ( DOMXPathResult
  , IsDOMXPathResult(..)
  , iterateNext
  , snapshotItem
  , resultType
  , numberValue
  , stringValue
  , booleanValue
  , singleNodeValue
  , invalidIteratorState
  , snapshotLength
  , booleanValueSelector
  , invalidIteratorStateSelector
  , iterateNextSelector
  , numberValueSelector
  , resultTypeSelector
  , singleNodeValueSelector
  , snapshotItemSelector
  , snapshotLengthSelector
  , stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- iterateNext@
iterateNext :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id DOMNode)
iterateNext domxPathResult =
  sendMessage domxPathResult iterateNextSelector

-- | @- snapshotItem:@
snapshotItem :: IsDOMXPathResult domxPathResult => domxPathResult -> CUInt -> IO (Id DOMNode)
snapshotItem domxPathResult index =
  sendMessage domxPathResult snapshotItemSelector index

-- | @- resultType@
resultType :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CUShort
resultType domxPathResult =
  sendMessage domxPathResult resultTypeSelector

-- | @- numberValue@
numberValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CDouble
numberValue domxPathResult =
  sendMessage domxPathResult numberValueSelector

-- | @- stringValue@
stringValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id NSString)
stringValue domxPathResult =
  sendMessage domxPathResult stringValueSelector

-- | @- booleanValue@
booleanValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO Bool
booleanValue domxPathResult =
  sendMessage domxPathResult booleanValueSelector

-- | @- singleNodeValue@
singleNodeValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id DOMNode)
singleNodeValue domxPathResult =
  sendMessage domxPathResult singleNodeValueSelector

-- | @- invalidIteratorState@
invalidIteratorState :: IsDOMXPathResult domxPathResult => domxPathResult -> IO Bool
invalidIteratorState domxPathResult =
  sendMessage domxPathResult invalidIteratorStateSelector

-- | @- snapshotLength@
snapshotLength :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CUInt
snapshotLength domxPathResult =
  sendMessage domxPathResult snapshotLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iterateNext@
iterateNextSelector :: Selector '[] (Id DOMNode)
iterateNextSelector = mkSelector "iterateNext"

-- | @Selector@ for @snapshotItem:@
snapshotItemSelector :: Selector '[CUInt] (Id DOMNode)
snapshotItemSelector = mkSelector "snapshotItem:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] CUShort
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @numberValue@
numberValueSelector :: Selector '[] CDouble
numberValueSelector = mkSelector "numberValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @booleanValue@
booleanValueSelector :: Selector '[] Bool
booleanValueSelector = mkSelector "booleanValue"

-- | @Selector@ for @singleNodeValue@
singleNodeValueSelector :: Selector '[] (Id DOMNode)
singleNodeValueSelector = mkSelector "singleNodeValue"

-- | @Selector@ for @invalidIteratorState@
invalidIteratorStateSelector :: Selector '[] Bool
invalidIteratorStateSelector = mkSelector "invalidIteratorState"

-- | @Selector@ for @snapshotLength@
snapshotLengthSelector :: Selector '[] CUInt
snapshotLengthSelector = mkSelector "snapshotLength"

