{-# LANGUAGE DataKinds #-}
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
  , dataForTypeSelector
  , propertyListForTypeSelector
  , setDataProvider_forTypesSelector
  , setData_forTypeSelector
  , setPropertyList_forTypeSelector
  , setString_forTypeSelector
  , stringForTypeSelector
  , typesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- availableTypeFromArray:@
availableTypeFromArray :: (IsNSPasteboardItem nsPasteboardItem, IsNSArray types) => nsPasteboardItem -> types -> IO (Id NSString)
availableTypeFromArray nsPasteboardItem types =
  sendMessage nsPasteboardItem availableTypeFromArraySelector (toNSArray types)

-- | @- setDataProvider:forTypes:@
setDataProvider_forTypes :: (IsNSPasteboardItem nsPasteboardItem, IsNSArray types) => nsPasteboardItem -> RawId -> types -> IO Bool
setDataProvider_forTypes nsPasteboardItem dataProvider types =
  sendMessage nsPasteboardItem setDataProvider_forTypesSelector dataProvider (toNSArray types)

-- | @- setData:forType:@
setData_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSData data_, IsNSString type_) => nsPasteboardItem -> data_ -> type_ -> IO Bool
setData_forType nsPasteboardItem data_ type_ =
  sendMessage nsPasteboardItem setData_forTypeSelector (toNSData data_) (toNSString type_)

-- | @- setString:forType:@
setString_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString string, IsNSString type_) => nsPasteboardItem -> string -> type_ -> IO Bool
setString_forType nsPasteboardItem string type_ =
  sendMessage nsPasteboardItem setString_forTypeSelector (toNSString string) (toNSString type_)

-- | @- setPropertyList:forType:@
setPropertyList_forType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> RawId -> type_ -> IO Bool
setPropertyList_forType nsPasteboardItem propertyList type_ =
  sendMessage nsPasteboardItem setPropertyList_forTypeSelector propertyList (toNSString type_)

-- | @- dataForType:@
dataForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO (Id NSData)
dataForType nsPasteboardItem type_ =
  sendMessage nsPasteboardItem dataForTypeSelector (toNSString type_)

-- | @- stringForType:@
stringForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO (Id NSString)
stringForType nsPasteboardItem type_ =
  sendMessage nsPasteboardItem stringForTypeSelector (toNSString type_)

-- | @- propertyListForType:@
propertyListForType :: (IsNSPasteboardItem nsPasteboardItem, IsNSString type_) => nsPasteboardItem -> type_ -> IO RawId
propertyListForType nsPasteboardItem type_ =
  sendMessage nsPasteboardItem propertyListForTypeSelector (toNSString type_)

-- | @- types@
types :: IsNSPasteboardItem nsPasteboardItem => nsPasteboardItem -> IO (Id NSArray)
types nsPasteboardItem =
  sendMessage nsPasteboardItem typesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableTypeFromArray:@
availableTypeFromArraySelector :: Selector '[Id NSArray] (Id NSString)
availableTypeFromArraySelector = mkSelector "availableTypeFromArray:"

-- | @Selector@ for @setDataProvider:forTypes:@
setDataProvider_forTypesSelector :: Selector '[RawId, Id NSArray] Bool
setDataProvider_forTypesSelector = mkSelector "setDataProvider:forTypes:"

-- | @Selector@ for @setData:forType:@
setData_forTypeSelector :: Selector '[Id NSData, Id NSString] Bool
setData_forTypeSelector = mkSelector "setData:forType:"

-- | @Selector@ for @setString:forType:@
setString_forTypeSelector :: Selector '[Id NSString, Id NSString] Bool
setString_forTypeSelector = mkSelector "setString:forType:"

-- | @Selector@ for @setPropertyList:forType:@
setPropertyList_forTypeSelector :: Selector '[RawId, Id NSString] Bool
setPropertyList_forTypeSelector = mkSelector "setPropertyList:forType:"

-- | @Selector@ for @dataForType:@
dataForTypeSelector :: Selector '[Id NSString] (Id NSData)
dataForTypeSelector = mkSelector "dataForType:"

-- | @Selector@ for @stringForType:@
stringForTypeSelector :: Selector '[Id NSString] (Id NSString)
stringForTypeSelector = mkSelector "stringForType:"

-- | @Selector@ for @propertyListForType:@
propertyListForTypeSelector :: Selector '[Id NSString] RawId
propertyListForTypeSelector = mkSelector "propertyListForType:"

-- | @Selector@ for @types@
typesSelector :: Selector '[] (Id NSArray)
typesSelector = mkSelector "types"

