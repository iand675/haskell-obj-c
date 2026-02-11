{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPasteboardItem@.
module ObjC.AppKit.NSPasteboardItem
  ( NSPasteboardItem
  , IsNSPasteboardItem(..)
  , availableTypeFromArray
  , setDataProvider_forTypes
  , setData_forType
  , setString_forType
  , setPropertyList_forType
  , dataForType
  , stringForType
  , propertyListForType
  , types
  , availableTypeFromArraySelector
  , setDataProvider_forTypesSelector
  , setData_forTypeSelector
  , setString_forTypeSelector
  , setPropertyList_forTypeSelector
  , dataForTypeSelector
  , stringForTypeSelector
  , propertyListForTypeSelector
  , typesSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- availableTypeFromArray:@
availableTypeFromArray :: (IsNSPasteboardItem nsPasteboardItem, IsNSArray types) => nsPasteboardItem -> types -> IO (Id NSString)
availableTypeFromArray nsPasteboardItem  types =
withObjCPtr types $ \raw_types ->
    sendMsg nsPasteboardItem (mkSelector "availableTypeFromArray:") (retPtr retVoid) [argPtr (castPtr raw_types :: Ptr ())] >>= retainedObject . castPtr

-- | @- setDataProvider:forTypes:@
setDataProvider_forTypes :: (IsNSPasteboardItem nsPasteboardItem, IsNSArray types) => nsPasteboardItem -> RawId -> types -> IO Bool
setDataProvider_forTypes nsPasteboardItem  dataProvider types =
withObjCPtr types $ \raw_types ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboardItem (mkSelector "setDataProvider:forTypes:") retCULong [argPtr (castPtr (unRawId dataProvider) :: Ptr ()), argPtr (castPtr raw_types :: Ptr ())]

-- | @- setData:forType:@
setData_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSData data_, IsNSString type_) => nsPasteboardItem -> data_ -> type_ -> IO Bool
setData_forType nsPasteboardItem  data_ type_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboardItem (mkSelector "setData:forType:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- setString:forType:@
setString_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString string, IsNSString type_) => nsPasteboardItem -> string -> type_ -> IO Bool
setString_forType nsPasteboardItem  string type_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboardItem (mkSelector "setString:forType:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- setPropertyList:forType:@
setPropertyList_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> RawId -> type_ -> IO Bool
setPropertyList_forType nsPasteboardItem  propertyList type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboardItem (mkSelector "setPropertyList:forType:") retCULong [argPtr (castPtr (unRawId propertyList) :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- dataForType:@
dataForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO (Id NSData)
dataForType nsPasteboardItem  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsPasteboardItem (mkSelector "dataForType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringForType:@
stringForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO (Id NSString)
stringForType nsPasteboardItem  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsPasteboardItem (mkSelector "stringForType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- propertyListForType:@
propertyListForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO RawId
propertyListForType nsPasteboardItem  type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap (RawId . castPtr) $ sendMsg nsPasteboardItem (mkSelector "propertyListForType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- types@
types :: IsNSPasteboardItem nsPasteboardItem => nsPasteboardItem -> IO (Id NSArray)
types nsPasteboardItem  =
  sendMsg nsPasteboardItem (mkSelector "types") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableTypeFromArray:@
availableTypeFromArraySelector :: Selector
availableTypeFromArraySelector = mkSelector "availableTypeFromArray:"

-- | @Selector@ for @setDataProvider:forTypes:@
setDataProvider_forTypesSelector :: Selector
setDataProvider_forTypesSelector = mkSelector "setDataProvider:forTypes:"

-- | @Selector@ for @setData:forType:@
setData_forTypeSelector :: Selector
setData_forTypeSelector = mkSelector "setData:forType:"

-- | @Selector@ for @setString:forType:@
setString_forTypeSelector :: Selector
setString_forTypeSelector = mkSelector "setString:forType:"

-- | @Selector@ for @setPropertyList:forType:@
setPropertyList_forTypeSelector :: Selector
setPropertyList_forTypeSelector = mkSelector "setPropertyList:forType:"

-- | @Selector@ for @dataForType:@
dataForTypeSelector :: Selector
dataForTypeSelector = mkSelector "dataForType:"

-- | @Selector@ for @stringForType:@
stringForTypeSelector :: Selector
stringForTypeSelector = mkSelector "stringForType:"

-- | @Selector@ for @propertyListForType:@
propertyListForTypeSelector :: Selector
propertyListForTypeSelector = mkSelector "propertyListForType:"

-- | @Selector@ for @types@
typesSelector :: Selector
typesSelector = mkSelector "types"

