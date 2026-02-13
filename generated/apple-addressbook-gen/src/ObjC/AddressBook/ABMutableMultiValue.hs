{-# LANGUAGE DataKinds #-}
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
  , replaceLabelAtIndex_withLabelSelector
  , replaceValueAtIndex_withValueSelector
  , setPrimaryIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addValue:withLabel:@
addValue_withLabel :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> RawId -> label -> IO (Id NSString)
addValue_withLabel abMutableMultiValue value label =
  sendMessage abMutableMultiValue addValue_withLabelSelector value (toNSString label)

-- | @- insertValue:withLabel:atIndex:@
insertValue_withLabel_atIndex :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> RawId -> label -> CULong -> IO (Id NSString)
insertValue_withLabel_atIndex abMutableMultiValue value label index =
  sendMessage abMutableMultiValue insertValue_withLabel_atIndexSelector value (toNSString label) index

-- | @- removeValueAndLabelAtIndex:@
removeValueAndLabelAtIndex :: IsABMutableMultiValue abMutableMultiValue => abMutableMultiValue -> CULong -> IO Bool
removeValueAndLabelAtIndex abMutableMultiValue index =
  sendMessage abMutableMultiValue removeValueAndLabelAtIndexSelector index

-- | @- replaceValueAtIndex:withValue:@
replaceValueAtIndex_withValue :: IsABMutableMultiValue abMutableMultiValue => abMutableMultiValue -> CULong -> RawId -> IO Bool
replaceValueAtIndex_withValue abMutableMultiValue index value =
  sendMessage abMutableMultiValue replaceValueAtIndex_withValueSelector index value

-- | @- replaceLabelAtIndex:withLabel:@
replaceLabelAtIndex_withLabel :: (IsABMutableMultiValue abMutableMultiValue, IsNSString label) => abMutableMultiValue -> CULong -> label -> IO Bool
replaceLabelAtIndex_withLabel abMutableMultiValue index label =
  sendMessage abMutableMultiValue replaceLabelAtIndex_withLabelSelector index (toNSString label)

-- | @- setPrimaryIdentifier:@
setPrimaryIdentifier :: (IsABMutableMultiValue abMutableMultiValue, IsNSString identifier) => abMutableMultiValue -> identifier -> IO Bool
setPrimaryIdentifier abMutableMultiValue identifier =
  sendMessage abMutableMultiValue setPrimaryIdentifierSelector (toNSString identifier)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addValue:withLabel:@
addValue_withLabelSelector :: Selector '[RawId, Id NSString] (Id NSString)
addValue_withLabelSelector = mkSelector "addValue:withLabel:"

-- | @Selector@ for @insertValue:withLabel:atIndex:@
insertValue_withLabel_atIndexSelector :: Selector '[RawId, Id NSString, CULong] (Id NSString)
insertValue_withLabel_atIndexSelector = mkSelector "insertValue:withLabel:atIndex:"

-- | @Selector@ for @removeValueAndLabelAtIndex:@
removeValueAndLabelAtIndexSelector :: Selector '[CULong] Bool
removeValueAndLabelAtIndexSelector = mkSelector "removeValueAndLabelAtIndex:"

-- | @Selector@ for @replaceValueAtIndex:withValue:@
replaceValueAtIndex_withValueSelector :: Selector '[CULong, RawId] Bool
replaceValueAtIndex_withValueSelector = mkSelector "replaceValueAtIndex:withValue:"

-- | @Selector@ for @replaceLabelAtIndex:withLabel:@
replaceLabelAtIndex_withLabelSelector :: Selector '[CULong, Id NSString] Bool
replaceLabelAtIndex_withLabelSelector = mkSelector "replaceLabelAtIndex:withLabel:"

-- | @Selector@ for @setPrimaryIdentifier:@
setPrimaryIdentifierSelector :: Selector '[Id NSString] Bool
setPrimaryIdentifierSelector = mkSelector "setPrimaryIdentifier:"

