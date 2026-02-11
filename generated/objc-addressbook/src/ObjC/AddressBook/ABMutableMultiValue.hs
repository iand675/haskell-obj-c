{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABMutableMultiValue@.
module ObjC.AddressBook.ABMutableMultiValue
  ( ABMutableMultiValue
  , IsABMutableMultiValue(..)
  , addValue_withLabel
  , insertValue_withLabel_atIndex
  , removeValueAndLabelAtIndex
  , replaceValueAtIndex_withValue
  , replaceLabelAtIndex_withLabel
  , setPrimaryIdentifier
  , addValue_withLabelSelector
  , insertValue_withLabel_atIndexSelector
  , removeValueAndLabelAtIndexSelector
  , replaceValueAtIndex_withValueSelector
  , replaceLabelAtIndex_withLabelSelector
  , setPrimaryIdentifierSelector


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

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addValue:withLabel:@
addValue_withLabel :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> RawId -> label -> IO (Id NSString)
addValue_withLabel abMutableMultiValue  value label =
withObjCPtr label $ \raw_label ->
    sendMsg abMutableMultiValue (mkSelector "addValue:withLabel:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_label :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertValue:withLabel:atIndex:@
insertValue_withLabel_atIndex :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> RawId -> label -> CULong -> IO (Id NSString)
insertValue_withLabel_atIndex abMutableMultiValue  value label index =
withObjCPtr label $ \raw_label ->
    sendMsg abMutableMultiValue (mkSelector "insertValue:withLabel:atIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_label :: Ptr ()), argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- removeValueAndLabelAtIndex:@
removeValueAndLabelAtIndex :: IsABMutableMultiValue abMutableMultiValue => abMutableMultiValue -> CULong -> IO Bool
removeValueAndLabelAtIndex abMutableMultiValue  index =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg abMutableMultiValue (mkSelector "removeValueAndLabelAtIndex:") retCULong [argCULong (fromIntegral index)]

-- | @- replaceValueAtIndex:withValue:@
replaceValueAtIndex_withValue :: IsABMutableMultiValue abMutableMultiValue => abMutableMultiValue -> CULong -> RawId -> IO Bool
replaceValueAtIndex_withValue abMutableMultiValue  index value =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg abMutableMultiValue (mkSelector "replaceValueAtIndex:withValue:") retCULong [argCULong (fromIntegral index), argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- replaceLabelAtIndex:withLabel:@
replaceLabelAtIndex_withLabel :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> CULong -> label -> IO Bool
replaceLabelAtIndex_withLabel abMutableMultiValue  index label =
withObjCPtr label $ \raw_label ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abMutableMultiValue (mkSelector "replaceLabelAtIndex:withLabel:") retCULong [argCULong (fromIntegral index), argPtr (castPtr raw_label :: Ptr ())]

-- | @- setPrimaryIdentifier:@
setPrimaryIdentifier :: (IsABMutableMultiValue abMutableMultiValue, IsNSString identifier) => abMutableMultiValue -> identifier -> IO Bool
setPrimaryIdentifier abMutableMultiValue  identifier =
withObjCPtr identifier $ \raw_identifier ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abMutableMultiValue (mkSelector "setPrimaryIdentifier:") retCULong [argPtr (castPtr raw_identifier :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addValue:withLabel:@
addValue_withLabelSelector :: Selector
addValue_withLabelSelector = mkSelector "addValue:withLabel:"

-- | @Selector@ for @insertValue:withLabel:atIndex:@
insertValue_withLabel_atIndexSelector :: Selector
insertValue_withLabel_atIndexSelector = mkSelector "insertValue:withLabel:atIndex:"

-- | @Selector@ for @removeValueAndLabelAtIndex:@
removeValueAndLabelAtIndexSelector :: Selector
removeValueAndLabelAtIndexSelector = mkSelector "removeValueAndLabelAtIndex:"

-- | @Selector@ for @replaceValueAtIndex:withValue:@
replaceValueAtIndex_withValueSelector :: Selector
replaceValueAtIndex_withValueSelector = mkSelector "replaceValueAtIndex:withValue:"

-- | @Selector@ for @replaceLabelAtIndex:withLabel:@
replaceLabelAtIndex_withLabelSelector :: Selector
replaceLabelAtIndex_withLabelSelector = mkSelector "replaceLabelAtIndex:withLabel:"

-- | @Selector@ for @setPrimaryIdentifier:@
setPrimaryIdentifierSelector :: Selector
setPrimaryIdentifierSelector = mkSelector "setPrimaryIdentifier:"

