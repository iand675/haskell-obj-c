{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.Foundation.NSValue
  ( NSValue
  , IsNSValue(..)
  , getValue_size
  , initWithBytes_objCType
  , initWithCoder
  , valueWithPoint
  , valueWithSize
  , valueWithRect
  , valueWithEdgeInsets
  , valueWithRange
  , getValue
  , valueWithNonretainedObject
  , valueWithPointer
  , isEqualToValue
  , valueWithBytes_objCType
  , value_withObjCType
  , objCType
  , pointValue
  , sizeValue
  , rectValue
  , edgeInsetsValue
  , rangeValue
  , nonretainedObjectValue
  , pointerValue
  , edgeInsetsValueSelector
  , getValueSelector
  , getValue_sizeSelector
  , initWithBytes_objCTypeSelector
  , initWithCoderSelector
  , isEqualToValueSelector
  , nonretainedObjectValueSelector
  , objCTypeSelector
  , pointValueSelector
  , pointerValueSelector
  , rangeValueSelector
  , rectValueSelector
  , sizeValueSelector
  , valueWithBytes_objCTypeSelector
  , valueWithEdgeInsetsSelector
  , valueWithNonretainedObjectSelector
  , valueWithPointSelector
  , valueWithPointerSelector
  , valueWithRangeSelector
  , valueWithRectSelector
  , valueWithSizeSelector
  , value_withObjCTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- getValue:size:@
getValue_size :: IsNSValue nsValue => nsValue -> Ptr () -> CULong -> IO ()
getValue_size nsValue value size =
  sendMessage nsValue getValue_sizeSelector value size

-- | @- initWithBytes:objCType:@
initWithBytes_objCType :: IsNSValue nsValue => nsValue -> Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
initWithBytes_objCType nsValue value type_ =
  sendOwnedMessage nsValue initWithBytes_objCTypeSelector value type_

-- | @- initWithCoder:@
initWithCoder :: (IsNSValue nsValue, IsNSCoder coder) => nsValue -> coder -> IO (Id NSValue)
initWithCoder nsValue coder =
  sendOwnedMessage nsValue initWithCoderSelector (toNSCoder coder)

-- | @+ valueWithPoint:@
valueWithPoint :: NSPoint -> IO (Id NSValue)
valueWithPoint point =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithPointSelector point

-- | @+ valueWithSize:@
valueWithSize :: NSSize -> IO (Id NSValue)
valueWithSize size =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithSizeSelector size

-- | @+ valueWithRect:@
valueWithRect :: NSRect -> IO (Id NSValue)
valueWithRect rect =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithRectSelector rect

-- | @+ valueWithEdgeInsets:@
valueWithEdgeInsets :: NSEdgeInsets -> IO (Id NSValue)
valueWithEdgeInsets insets =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithEdgeInsetsSelector insets

-- | @+ valueWithRange:@
valueWithRange :: NSRange -> IO (Id NSValue)
valueWithRange range =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithRangeSelector range

-- | @- getValue:@
getValue :: IsNSValue nsValue => nsValue -> Ptr () -> IO ()
getValue nsValue value =
  sendMessage nsValue getValueSelector value

-- | @+ valueWithNonretainedObject:@
valueWithNonretainedObject :: RawId -> IO (Id NSValue)
valueWithNonretainedObject anObject =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithNonretainedObjectSelector anObject

-- | @+ valueWithPointer:@
valueWithPointer :: Const (Ptr ()) -> IO (Id NSValue)
valueWithPointer pointer =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithPointerSelector pointer

-- | @- isEqualToValue:@
isEqualToValue :: (IsNSValue nsValue, IsNSValue value) => nsValue -> value -> IO Bool
isEqualToValue nsValue value =
  sendMessage nsValue isEqualToValueSelector (toNSValue value)

-- | @+ valueWithBytes:objCType:@
valueWithBytes_objCType :: Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
valueWithBytes_objCType value type_ =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithBytes_objCTypeSelector value type_

-- | @+ value:withObjCType:@
value_withObjCType :: Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
value_withObjCType value type_ =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' value_withObjCTypeSelector value type_

-- | @- objCType@
objCType :: IsNSValue nsValue => nsValue -> IO (Ptr CChar)
objCType nsValue =
  sendMessage nsValue objCTypeSelector

-- | @- pointValue@
pointValue :: IsNSValue nsValue => nsValue -> IO NSPoint
pointValue nsValue =
  sendMessage nsValue pointValueSelector

-- | @- sizeValue@
sizeValue :: IsNSValue nsValue => nsValue -> IO NSSize
sizeValue nsValue =
  sendMessage nsValue sizeValueSelector

-- | @- rectValue@
rectValue :: IsNSValue nsValue => nsValue -> IO NSRect
rectValue nsValue =
  sendMessage nsValue rectValueSelector

-- | @- edgeInsetsValue@
edgeInsetsValue :: IsNSValue nsValue => nsValue -> IO NSEdgeInsets
edgeInsetsValue nsValue =
  sendMessage nsValue edgeInsetsValueSelector

-- | @- rangeValue@
rangeValue :: IsNSValue nsValue => nsValue -> IO NSRange
rangeValue nsValue =
  sendMessage nsValue rangeValueSelector

-- | @- nonretainedObjectValue@
nonretainedObjectValue :: IsNSValue nsValue => nsValue -> IO RawId
nonretainedObjectValue nsValue =
  sendMessage nsValue nonretainedObjectValueSelector

-- | @- pointerValue@
pointerValue :: IsNSValue nsValue => nsValue -> IO (Ptr ())
pointerValue nsValue =
  sendMessage nsValue pointerValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getValue:size:@
getValue_sizeSelector :: Selector '[Ptr (), CULong] ()
getValue_sizeSelector = mkSelector "getValue:size:"

-- | @Selector@ for @initWithBytes:objCType:@
initWithBytes_objCTypeSelector :: Selector '[Const (Ptr ()), Const (Ptr CChar)] (Id NSValue)
initWithBytes_objCTypeSelector = mkSelector "initWithBytes:objCType:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSValue)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueWithPoint:@
valueWithPointSelector :: Selector '[NSPoint] (Id NSValue)
valueWithPointSelector = mkSelector "valueWithPoint:"

-- | @Selector@ for @valueWithSize:@
valueWithSizeSelector :: Selector '[NSSize] (Id NSValue)
valueWithSizeSelector = mkSelector "valueWithSize:"

-- | @Selector@ for @valueWithRect:@
valueWithRectSelector :: Selector '[NSRect] (Id NSValue)
valueWithRectSelector = mkSelector "valueWithRect:"

-- | @Selector@ for @valueWithEdgeInsets:@
valueWithEdgeInsetsSelector :: Selector '[NSEdgeInsets] (Id NSValue)
valueWithEdgeInsetsSelector = mkSelector "valueWithEdgeInsets:"

-- | @Selector@ for @valueWithRange:@
valueWithRangeSelector :: Selector '[NSRange] (Id NSValue)
valueWithRangeSelector = mkSelector "valueWithRange:"

-- | @Selector@ for @getValue:@
getValueSelector :: Selector '[Ptr ()] ()
getValueSelector = mkSelector "getValue:"

-- | @Selector@ for @valueWithNonretainedObject:@
valueWithNonretainedObjectSelector :: Selector '[RawId] (Id NSValue)
valueWithNonretainedObjectSelector = mkSelector "valueWithNonretainedObject:"

-- | @Selector@ for @valueWithPointer:@
valueWithPointerSelector :: Selector '[Const (Ptr ())] (Id NSValue)
valueWithPointerSelector = mkSelector "valueWithPointer:"

-- | @Selector@ for @isEqualToValue:@
isEqualToValueSelector :: Selector '[Id NSValue] Bool
isEqualToValueSelector = mkSelector "isEqualToValue:"

-- | @Selector@ for @valueWithBytes:objCType:@
valueWithBytes_objCTypeSelector :: Selector '[Const (Ptr ()), Const (Ptr CChar)] (Id NSValue)
valueWithBytes_objCTypeSelector = mkSelector "valueWithBytes:objCType:"

-- | @Selector@ for @value:withObjCType:@
value_withObjCTypeSelector :: Selector '[Const (Ptr ()), Const (Ptr CChar)] (Id NSValue)
value_withObjCTypeSelector = mkSelector "value:withObjCType:"

-- | @Selector@ for @objCType@
objCTypeSelector :: Selector '[] (Ptr CChar)
objCTypeSelector = mkSelector "objCType"

-- | @Selector@ for @pointValue@
pointValueSelector :: Selector '[] NSPoint
pointValueSelector = mkSelector "pointValue"

-- | @Selector@ for @sizeValue@
sizeValueSelector :: Selector '[] NSSize
sizeValueSelector = mkSelector "sizeValue"

-- | @Selector@ for @rectValue@
rectValueSelector :: Selector '[] NSRect
rectValueSelector = mkSelector "rectValue"

-- | @Selector@ for @edgeInsetsValue@
edgeInsetsValueSelector :: Selector '[] NSEdgeInsets
edgeInsetsValueSelector = mkSelector "edgeInsetsValue"

-- | @Selector@ for @rangeValue@
rangeValueSelector :: Selector '[] NSRange
rangeValueSelector = mkSelector "rangeValue"

-- | @Selector@ for @nonretainedObjectValue@
nonretainedObjectValueSelector :: Selector '[] RawId
nonretainedObjectValueSelector = mkSelector "nonretainedObjectValue"

-- | @Selector@ for @pointerValue@
pointerValueSelector :: Selector '[] (Ptr ())
pointerValueSelector = mkSelector "pointerValue"

