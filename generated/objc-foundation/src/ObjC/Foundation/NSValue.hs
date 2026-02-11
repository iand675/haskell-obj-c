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
  , pointValue
  , sizeValue
  , rectValue
  , edgeInsetsValue
  , rangeValue
  , nonretainedObjectValue
  , pointerValue
  , getValue_sizeSelector
  , initWithBytes_objCTypeSelector
  , initWithCoderSelector
  , valueWithPointSelector
  , valueWithSizeSelector
  , valueWithRectSelector
  , valueWithEdgeInsetsSelector
  , valueWithRangeSelector
  , getValueSelector
  , valueWithNonretainedObjectSelector
  , valueWithPointerSelector
  , isEqualToValueSelector
  , valueWithBytes_objCTypeSelector
  , value_withObjCTypeSelector
  , pointValueSelector
  , sizeValueSelector
  , rectValueSelector
  , edgeInsetsValueSelector
  , rangeValueSelector
  , nonretainedObjectValueSelector
  , pointerValueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- getValue:size:@
getValue_size :: IsNSValue nsValue => nsValue -> Ptr () -> CULong -> IO ()
getValue_size nsValue  value size =
  sendMsg nsValue (mkSelector "getValue:size:") retVoid [argPtr value, argCULong (fromIntegral size)]

-- | @- initWithBytes:objCType:@
initWithBytes_objCType :: IsNSValue nsValue => nsValue -> Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
initWithBytes_objCType nsValue  value type_ =
  sendMsg nsValue (mkSelector "initWithBytes:objCType:") (retPtr retVoid) [argPtr (unConst value), argPtr (unConst type_)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSValue nsValue, IsNSCoder coder) => nsValue -> coder -> IO (Id NSValue)
initWithCoder nsValue  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsValue (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ valueWithPoint:@
valueWithPoint :: NSPoint -> IO (Id NSValue)
valueWithPoint point =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @+ valueWithSize:@
valueWithSize :: NSSize -> IO (Id NSValue)
valueWithSize size =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithSize:") (retPtr retVoid) [argNSSize size] >>= retainedObject . castPtr

-- | @+ valueWithRect:@
valueWithRect :: NSRect -> IO (Id NSValue)
valueWithRect rect =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @+ valueWithEdgeInsets:@
valueWithEdgeInsets :: NSEdgeInsets -> IO (Id NSValue)
valueWithEdgeInsets insets =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithEdgeInsets:") (retPtr retVoid) [argNSEdgeInsets insets] >>= retainedObject . castPtr

-- | @+ valueWithRange:@
valueWithRange :: NSRange -> IO (Id NSValue)
valueWithRange range =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- getValue:@
getValue :: IsNSValue nsValue => nsValue -> Ptr () -> IO ()
getValue nsValue  value =
  sendMsg nsValue (mkSelector "getValue:") retVoid [argPtr value]

-- | @+ valueWithNonretainedObject:@
valueWithNonretainedObject :: RawId -> IO (Id NSValue)
valueWithNonretainedObject anObject =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithNonretainedObject:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ valueWithPointer:@
valueWithPointer :: Const (Ptr ()) -> IO (Id NSValue)
valueWithPointer pointer =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithPointer:") (retPtr retVoid) [argPtr (unConst pointer)] >>= retainedObject . castPtr

-- | @- isEqualToValue:@
isEqualToValue :: (IsNSValue nsValue, IsNSValue value) => nsValue -> value -> IO Bool
isEqualToValue nsValue  value =
withObjCPtr value $ \raw_value ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsValue (mkSelector "isEqualToValue:") retCULong [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ valueWithBytes:objCType:@
valueWithBytes_objCType :: Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
valueWithBytes_objCType value type_ =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithBytes:objCType:") (retPtr retVoid) [argPtr (unConst value), argPtr (unConst type_)] >>= retainedObject . castPtr

-- | @+ value:withObjCType:@
value_withObjCType :: Const (Ptr ()) -> Const (Ptr CChar) -> IO (Id NSValue)
value_withObjCType value type_ =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "value:withObjCType:") (retPtr retVoid) [argPtr (unConst value), argPtr (unConst type_)] >>= retainedObject . castPtr

-- | @- pointValue@
pointValue :: IsNSValue nsValue => nsValue -> IO NSPoint
pointValue nsValue  =
  sendMsgStret nsValue (mkSelector "pointValue") retNSPoint []

-- | @- sizeValue@
sizeValue :: IsNSValue nsValue => nsValue -> IO NSSize
sizeValue nsValue  =
  sendMsgStret nsValue (mkSelector "sizeValue") retNSSize []

-- | @- rectValue@
rectValue :: IsNSValue nsValue => nsValue -> IO NSRect
rectValue nsValue  =
  sendMsgStret nsValue (mkSelector "rectValue") retNSRect []

-- | @- edgeInsetsValue@
edgeInsetsValue :: IsNSValue nsValue => nsValue -> IO NSEdgeInsets
edgeInsetsValue nsValue  =
  sendMsgStret nsValue (mkSelector "edgeInsetsValue") retNSEdgeInsets []

-- | @- rangeValue@
rangeValue :: IsNSValue nsValue => nsValue -> IO NSRange
rangeValue nsValue  =
  sendMsgStret nsValue (mkSelector "rangeValue") retNSRange []

-- | @- nonretainedObjectValue@
nonretainedObjectValue :: IsNSValue nsValue => nsValue -> IO RawId
nonretainedObjectValue nsValue  =
  fmap (RawId . castPtr) $ sendMsg nsValue (mkSelector "nonretainedObjectValue") (retPtr retVoid) []

-- | @- pointerValue@
pointerValue :: IsNSValue nsValue => nsValue -> IO (Ptr ())
pointerValue nsValue  =
  fmap castPtr $ sendMsg nsValue (mkSelector "pointerValue") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getValue:size:@
getValue_sizeSelector :: Selector
getValue_sizeSelector = mkSelector "getValue:size:"

-- | @Selector@ for @initWithBytes:objCType:@
initWithBytes_objCTypeSelector :: Selector
initWithBytes_objCTypeSelector = mkSelector "initWithBytes:objCType:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueWithPoint:@
valueWithPointSelector :: Selector
valueWithPointSelector = mkSelector "valueWithPoint:"

-- | @Selector@ for @valueWithSize:@
valueWithSizeSelector :: Selector
valueWithSizeSelector = mkSelector "valueWithSize:"

-- | @Selector@ for @valueWithRect:@
valueWithRectSelector :: Selector
valueWithRectSelector = mkSelector "valueWithRect:"

-- | @Selector@ for @valueWithEdgeInsets:@
valueWithEdgeInsetsSelector :: Selector
valueWithEdgeInsetsSelector = mkSelector "valueWithEdgeInsets:"

-- | @Selector@ for @valueWithRange:@
valueWithRangeSelector :: Selector
valueWithRangeSelector = mkSelector "valueWithRange:"

-- | @Selector@ for @getValue:@
getValueSelector :: Selector
getValueSelector = mkSelector "getValue:"

-- | @Selector@ for @valueWithNonretainedObject:@
valueWithNonretainedObjectSelector :: Selector
valueWithNonretainedObjectSelector = mkSelector "valueWithNonretainedObject:"

-- | @Selector@ for @valueWithPointer:@
valueWithPointerSelector :: Selector
valueWithPointerSelector = mkSelector "valueWithPointer:"

-- | @Selector@ for @isEqualToValue:@
isEqualToValueSelector :: Selector
isEqualToValueSelector = mkSelector "isEqualToValue:"

-- | @Selector@ for @valueWithBytes:objCType:@
valueWithBytes_objCTypeSelector :: Selector
valueWithBytes_objCTypeSelector = mkSelector "valueWithBytes:objCType:"

-- | @Selector@ for @value:withObjCType:@
value_withObjCTypeSelector :: Selector
value_withObjCTypeSelector = mkSelector "value:withObjCType:"

-- | @Selector@ for @pointValue@
pointValueSelector :: Selector
pointValueSelector = mkSelector "pointValue"

-- | @Selector@ for @sizeValue@
sizeValueSelector :: Selector
sizeValueSelector = mkSelector "sizeValue"

-- | @Selector@ for @rectValue@
rectValueSelector :: Selector
rectValueSelector = mkSelector "rectValue"

-- | @Selector@ for @edgeInsetsValue@
edgeInsetsValueSelector :: Selector
edgeInsetsValueSelector = mkSelector "edgeInsetsValue"

-- | @Selector@ for @rangeValue@
rangeValueSelector :: Selector
rangeValueSelector = mkSelector "rangeValue"

-- | @Selector@ for @nonretainedObjectValue@
nonretainedObjectValueSelector :: Selector
nonretainedObjectValueSelector = mkSelector "nonretainedObjectValue"

-- | @Selector@ for @pointerValue@
pointerValueSelector :: Selector
pointerValueSelector = mkSelector "pointerValue"

