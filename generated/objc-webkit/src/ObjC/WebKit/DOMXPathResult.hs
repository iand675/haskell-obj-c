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
  , iterateNextSelector
  , snapshotItemSelector
  , resultTypeSelector
  , numberValueSelector
  , stringValueSelector
  , booleanValueSelector
  , singleNodeValueSelector
  , invalidIteratorStateSelector
  , snapshotLengthSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- iterateNext@
iterateNext :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id DOMNode)
iterateNext domxPathResult  =
  sendMsg domxPathResult (mkSelector "iterateNext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- snapshotItem:@
snapshotItem :: IsDOMXPathResult domxPathResult => domxPathResult -> CUInt -> IO (Id DOMNode)
snapshotItem domxPathResult  index =
  sendMsg domxPathResult (mkSelector "snapshotItem:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- resultType@
resultType :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CUShort
resultType domxPathResult  =
  fmap fromIntegral $ sendMsg domxPathResult (mkSelector "resultType") retCUInt []

-- | @- numberValue@
numberValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CDouble
numberValue domxPathResult  =
  sendMsg domxPathResult (mkSelector "numberValue") retCDouble []

-- | @- stringValue@
stringValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id NSString)
stringValue domxPathResult  =
  sendMsg domxPathResult (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- booleanValue@
booleanValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO Bool
booleanValue domxPathResult  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domxPathResult (mkSelector "booleanValue") retCULong []

-- | @- singleNodeValue@
singleNodeValue :: IsDOMXPathResult domxPathResult => domxPathResult -> IO (Id DOMNode)
singleNodeValue domxPathResult  =
  sendMsg domxPathResult (mkSelector "singleNodeValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invalidIteratorState@
invalidIteratorState :: IsDOMXPathResult domxPathResult => domxPathResult -> IO Bool
invalidIteratorState domxPathResult  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domxPathResult (mkSelector "invalidIteratorState") retCULong []

-- | @- snapshotLength@
snapshotLength :: IsDOMXPathResult domxPathResult => domxPathResult -> IO CUInt
snapshotLength domxPathResult  =
  sendMsg domxPathResult (mkSelector "snapshotLength") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iterateNext@
iterateNextSelector :: Selector
iterateNextSelector = mkSelector "iterateNext"

-- | @Selector@ for @snapshotItem:@
snapshotItemSelector :: Selector
snapshotItemSelector = mkSelector "snapshotItem:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @numberValue@
numberValueSelector :: Selector
numberValueSelector = mkSelector "numberValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @booleanValue@
booleanValueSelector :: Selector
booleanValueSelector = mkSelector "booleanValue"

-- | @Selector@ for @singleNodeValue@
singleNodeValueSelector :: Selector
singleNodeValueSelector = mkSelector "singleNodeValue"

-- | @Selector@ for @invalidIteratorState@
invalidIteratorStateSelector :: Selector
invalidIteratorStateSelector = mkSelector "invalidIteratorState"

-- | @Selector@ for @snapshotLength@
snapshotLengthSelector :: Selector
snapshotLengthSelector = mkSelector "snapshotLength"

