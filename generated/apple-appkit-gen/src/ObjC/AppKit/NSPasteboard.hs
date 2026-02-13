{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPasteboard@.
module ObjC.AppKit.NSPasteboard
  ( NSPasteboard
  , IsNSPasteboard(..)
  , pasteboardWithName
  , pasteboardWithUniqueName
  , releaseGlobally
  , prepareForNewContentsWithOptions
  , clearContents
  , writeObjects
  , readObjectsForClasses_options
  , indexOfPasteboardItem
  , canReadItemWithDataConformingToTypes
  , canReadObjectForClasses_options
  , declareTypes_owner
  , addTypes_owner
  , availableTypeFromArray
  , setData_forType
  , setPropertyList_forType
  , setString_forType
  , dataForType
  , propertyListForType
  , stringForType
  , writeFileContents
  , readFileContentsType_toFile
  , writeFileWrapper
  , readFileWrapper
  , typesFilterableTo
  , pasteboardByFilteringFile
  , pasteboardByFilteringData_ofType
  , pasteboardByFilteringTypesInPasteboard
  , generalPasteboard
  , name
  , changeCount
  , accessBehavior
  , pasteboardItems
  , types
  , accessBehaviorSelector
  , addTypes_ownerSelector
  , availableTypeFromArraySelector
  , canReadItemWithDataConformingToTypesSelector
  , canReadObjectForClasses_optionsSelector
  , changeCountSelector
  , clearContentsSelector
  , dataForTypeSelector
  , declareTypes_ownerSelector
  , generalPasteboardSelector
  , indexOfPasteboardItemSelector
  , nameSelector
  , pasteboardByFilteringData_ofTypeSelector
  , pasteboardByFilteringFileSelector
  , pasteboardByFilteringTypesInPasteboardSelector
  , pasteboardItemsSelector
  , pasteboardWithNameSelector
  , pasteboardWithUniqueNameSelector
  , prepareForNewContentsWithOptionsSelector
  , propertyListForTypeSelector
  , readFileContentsType_toFileSelector
  , readFileWrapperSelector
  , readObjectsForClasses_optionsSelector
  , releaseGloballySelector
  , setData_forTypeSelector
  , setPropertyList_forTypeSelector
  , setString_forTypeSelector
  , stringForTypeSelector
  , typesFilterableToSelector
  , typesSelector
  , writeFileContentsSelector
  , writeFileWrapperSelector
  , writeObjectsSelector

  -- * Enum types
  , NSPasteboardAccessBehavior(NSPasteboardAccessBehavior)
  , pattern NSPasteboardAccessBehaviorDefault
  , pattern NSPasteboardAccessBehaviorAsk
  , pattern NSPasteboardAccessBehaviorAlwaysAllow
  , pattern NSPasteboardAccessBehaviorAlwaysDeny
  , NSPasteboardContentsOptions(NSPasteboardContentsOptions)
  , pattern NSPasteboardContentsCurrentHostOnly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ pasteboardWithName:@
pasteboardWithName :: IsNSString name => name -> IO (Id NSPasteboard)
pasteboardWithName name =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' pasteboardWithNameSelector (toNSString name)

-- | @+ pasteboardWithUniqueName@
pasteboardWithUniqueName :: IO (Id NSPasteboard)
pasteboardWithUniqueName  =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' pasteboardWithUniqueNameSelector

-- | @- releaseGlobally@
releaseGlobally :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO ()
releaseGlobally nsPasteboard =
  sendMessage nsPasteboard releaseGloballySelector

-- | @- prepareForNewContentsWithOptions:@
prepareForNewContentsWithOptions :: IsNSPasteboard nsPasteboard => nsPasteboard -> NSPasteboardContentsOptions -> IO CLong
prepareForNewContentsWithOptions nsPasteboard options =
  sendMessage nsPasteboard prepareForNewContentsWithOptionsSelector options

-- | @- clearContents@
clearContents :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO CLong
clearContents nsPasteboard =
  sendMessage nsPasteboard clearContentsSelector

-- | @- writeObjects:@
writeObjects :: (IsNSPasteboard nsPasteboard, IsNSArray objects) => nsPasteboard -> objects -> IO Bool
writeObjects nsPasteboard objects =
  sendMessage nsPasteboard writeObjectsSelector (toNSArray objects)

-- | @- readObjectsForClasses:options:@
readObjectsForClasses_options :: (IsNSPasteboard nsPasteboard, IsNSArray classArray, IsNSDictionary options) => nsPasteboard -> classArray -> options -> IO (Id NSArray)
readObjectsForClasses_options nsPasteboard classArray options =
  sendMessage nsPasteboard readObjectsForClasses_optionsSelector (toNSArray classArray) (toNSDictionary options)

-- | @- indexOfPasteboardItem:@
indexOfPasteboardItem :: (IsNSPasteboard nsPasteboard, IsNSPasteboardItem pasteboardItem) => nsPasteboard -> pasteboardItem -> IO CULong
indexOfPasteboardItem nsPasteboard pasteboardItem =
  sendMessage nsPasteboard indexOfPasteboardItemSelector (toNSPasteboardItem pasteboardItem)

-- | @- canReadItemWithDataConformingToTypes:@
canReadItemWithDataConformingToTypes :: (IsNSPasteboard nsPasteboard, IsNSArray types) => nsPasteboard -> types -> IO Bool
canReadItemWithDataConformingToTypes nsPasteboard types =
  sendMessage nsPasteboard canReadItemWithDataConformingToTypesSelector (toNSArray types)

-- | @- canReadObjectForClasses:options:@
canReadObjectForClasses_options :: (IsNSPasteboard nsPasteboard, IsNSArray classArray, IsNSDictionary options) => nsPasteboard -> classArray -> options -> IO Bool
canReadObjectForClasses_options nsPasteboard classArray options =
  sendMessage nsPasteboard canReadObjectForClasses_optionsSelector (toNSArray classArray) (toNSDictionary options)

-- | @- declareTypes:owner:@
declareTypes_owner :: (IsNSPasteboard nsPasteboard, IsNSArray newTypes) => nsPasteboard -> newTypes -> RawId -> IO CLong
declareTypes_owner nsPasteboard newTypes newOwner =
  sendMessage nsPasteboard declareTypes_ownerSelector (toNSArray newTypes) newOwner

-- | @- addTypes:owner:@
addTypes_owner :: (IsNSPasteboard nsPasteboard, IsNSArray newTypes) => nsPasteboard -> newTypes -> RawId -> IO CLong
addTypes_owner nsPasteboard newTypes newOwner =
  sendMessage nsPasteboard addTypes_ownerSelector (toNSArray newTypes) newOwner

-- | @- availableTypeFromArray:@
availableTypeFromArray :: (IsNSPasteboard nsPasteboard, IsNSArray types) => nsPasteboard -> types -> IO (Id NSString)
availableTypeFromArray nsPasteboard types =
  sendMessage nsPasteboard availableTypeFromArraySelector (toNSArray types)

-- | @- setData:forType:@
setData_forType :: (IsNSPasteboard nsPasteboard, IsNSData data_, IsNSString dataType) => nsPasteboard -> data_ -> dataType -> IO Bool
setData_forType nsPasteboard data_ dataType =
  sendMessage nsPasteboard setData_forTypeSelector (toNSData data_) (toNSString dataType)

-- | @- setPropertyList:forType:@
setPropertyList_forType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> RawId -> dataType -> IO Bool
setPropertyList_forType nsPasteboard plist dataType =
  sendMessage nsPasteboard setPropertyList_forTypeSelector plist (toNSString dataType)

-- | @- setString:forType:@
setString_forType :: (IsNSPasteboard nsPasteboard, IsNSString string, IsNSString dataType) => nsPasteboard -> string -> dataType -> IO Bool
setString_forType nsPasteboard string dataType =
  sendMessage nsPasteboard setString_forTypeSelector (toNSString string) (toNSString dataType)

-- | @- dataForType:@
dataForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO (Id NSData)
dataForType nsPasteboard dataType =
  sendMessage nsPasteboard dataForTypeSelector (toNSString dataType)

-- | @- propertyListForType:@
propertyListForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO RawId
propertyListForType nsPasteboard dataType =
  sendMessage nsPasteboard propertyListForTypeSelector (toNSString dataType)

-- | @- stringForType:@
stringForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO (Id NSString)
stringForType nsPasteboard dataType =
  sendMessage nsPasteboard stringForTypeSelector (toNSString dataType)

-- | @- writeFileContents:@
writeFileContents :: (IsNSPasteboard nsPasteboard, IsNSString filename) => nsPasteboard -> filename -> IO Bool
writeFileContents nsPasteboard filename =
  sendMessage nsPasteboard writeFileContentsSelector (toNSString filename)

-- | @- readFileContentsType:toFile:@
readFileContentsType_toFile :: (IsNSPasteboard nsPasteboard, IsNSString type_, IsNSString filename) => nsPasteboard -> type_ -> filename -> IO (Id NSString)
readFileContentsType_toFile nsPasteboard type_ filename =
  sendMessage nsPasteboard readFileContentsType_toFileSelector (toNSString type_) (toNSString filename)

-- | @- writeFileWrapper:@
writeFileWrapper :: (IsNSPasteboard nsPasteboard, IsNSFileWrapper wrapper) => nsPasteboard -> wrapper -> IO Bool
writeFileWrapper nsPasteboard wrapper =
  sendMessage nsPasteboard writeFileWrapperSelector (toNSFileWrapper wrapper)

-- | @- readFileWrapper@
readFileWrapper :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSFileWrapper)
readFileWrapper nsPasteboard =
  sendMessage nsPasteboard readFileWrapperSelector

-- | @+ typesFilterableTo:@
typesFilterableTo :: IsNSString type_ => type_ -> IO (Id NSArray)
typesFilterableTo type_ =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' typesFilterableToSelector (toNSString type_)

-- | @+ pasteboardByFilteringFile:@
pasteboardByFilteringFile :: IsNSString filename => filename -> IO (Id NSPasteboard)
pasteboardByFilteringFile filename =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' pasteboardByFilteringFileSelector (toNSString filename)

-- | @+ pasteboardByFilteringData:ofType:@
pasteboardByFilteringData_ofType :: (IsNSData data_, IsNSString type_) => data_ -> type_ -> IO (Id NSPasteboard)
pasteboardByFilteringData_ofType data_ type_ =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' pasteboardByFilteringData_ofTypeSelector (toNSData data_) (toNSString type_)

-- | @+ pasteboardByFilteringTypesInPasteboard:@
pasteboardByFilteringTypesInPasteboard :: IsNSPasteboard pboard => pboard -> IO (Id NSPasteboard)
pasteboardByFilteringTypesInPasteboard pboard =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' pasteboardByFilteringTypesInPasteboardSelector (toNSPasteboard pboard)

-- | @+ generalPasteboard@
generalPasteboard :: IO (Id NSPasteboard)
generalPasteboard  =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMessage cls' generalPasteboardSelector

-- | @- name@
name :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSString)
name nsPasteboard =
  sendMessage nsPasteboard nameSelector

-- | @- changeCount@
changeCount :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO CLong
changeCount nsPasteboard =
  sendMessage nsPasteboard changeCountSelector

-- | The current pasteboard access behavior. The user can customize this behavior per-app in System Settings for any app that has triggered a pasteboard access alert in the past.
--
-- ObjC selector: @- accessBehavior@
accessBehavior :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO NSPasteboardAccessBehavior
accessBehavior nsPasteboard =
  sendMessage nsPasteboard accessBehaviorSelector

-- | @- pasteboardItems@
pasteboardItems :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSArray)
pasteboardItems nsPasteboard =
  sendMessage nsPasteboard pasteboardItemsSelector

-- | @- types@
types :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSArray)
types nsPasteboard =
  sendMessage nsPasteboard typesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pasteboardWithName:@
pasteboardWithNameSelector :: Selector '[Id NSString] (Id NSPasteboard)
pasteboardWithNameSelector = mkSelector "pasteboardWithName:"

-- | @Selector@ for @pasteboardWithUniqueName@
pasteboardWithUniqueNameSelector :: Selector '[] (Id NSPasteboard)
pasteboardWithUniqueNameSelector = mkSelector "pasteboardWithUniqueName"

-- | @Selector@ for @releaseGlobally@
releaseGloballySelector :: Selector '[] ()
releaseGloballySelector = mkSelector "releaseGlobally"

-- | @Selector@ for @prepareForNewContentsWithOptions:@
prepareForNewContentsWithOptionsSelector :: Selector '[NSPasteboardContentsOptions] CLong
prepareForNewContentsWithOptionsSelector = mkSelector "prepareForNewContentsWithOptions:"

-- | @Selector@ for @clearContents@
clearContentsSelector :: Selector '[] CLong
clearContentsSelector = mkSelector "clearContents"

-- | @Selector@ for @writeObjects:@
writeObjectsSelector :: Selector '[Id NSArray] Bool
writeObjectsSelector = mkSelector "writeObjects:"

-- | @Selector@ for @readObjectsForClasses:options:@
readObjectsForClasses_optionsSelector :: Selector '[Id NSArray, Id NSDictionary] (Id NSArray)
readObjectsForClasses_optionsSelector = mkSelector "readObjectsForClasses:options:"

-- | @Selector@ for @indexOfPasteboardItem:@
indexOfPasteboardItemSelector :: Selector '[Id NSPasteboardItem] CULong
indexOfPasteboardItemSelector = mkSelector "indexOfPasteboardItem:"

-- | @Selector@ for @canReadItemWithDataConformingToTypes:@
canReadItemWithDataConformingToTypesSelector :: Selector '[Id NSArray] Bool
canReadItemWithDataConformingToTypesSelector = mkSelector "canReadItemWithDataConformingToTypes:"

-- | @Selector@ for @canReadObjectForClasses:options:@
canReadObjectForClasses_optionsSelector :: Selector '[Id NSArray, Id NSDictionary] Bool
canReadObjectForClasses_optionsSelector = mkSelector "canReadObjectForClasses:options:"

-- | @Selector@ for @declareTypes:owner:@
declareTypes_ownerSelector :: Selector '[Id NSArray, RawId] CLong
declareTypes_ownerSelector = mkSelector "declareTypes:owner:"

-- | @Selector@ for @addTypes:owner:@
addTypes_ownerSelector :: Selector '[Id NSArray, RawId] CLong
addTypes_ownerSelector = mkSelector "addTypes:owner:"

-- | @Selector@ for @availableTypeFromArray:@
availableTypeFromArraySelector :: Selector '[Id NSArray] (Id NSString)
availableTypeFromArraySelector = mkSelector "availableTypeFromArray:"

-- | @Selector@ for @setData:forType:@
setData_forTypeSelector :: Selector '[Id NSData, Id NSString] Bool
setData_forTypeSelector = mkSelector "setData:forType:"

-- | @Selector@ for @setPropertyList:forType:@
setPropertyList_forTypeSelector :: Selector '[RawId, Id NSString] Bool
setPropertyList_forTypeSelector = mkSelector "setPropertyList:forType:"

-- | @Selector@ for @setString:forType:@
setString_forTypeSelector :: Selector '[Id NSString, Id NSString] Bool
setString_forTypeSelector = mkSelector "setString:forType:"

-- | @Selector@ for @dataForType:@
dataForTypeSelector :: Selector '[Id NSString] (Id NSData)
dataForTypeSelector = mkSelector "dataForType:"

-- | @Selector@ for @propertyListForType:@
propertyListForTypeSelector :: Selector '[Id NSString] RawId
propertyListForTypeSelector = mkSelector "propertyListForType:"

-- | @Selector@ for @stringForType:@
stringForTypeSelector :: Selector '[Id NSString] (Id NSString)
stringForTypeSelector = mkSelector "stringForType:"

-- | @Selector@ for @writeFileContents:@
writeFileContentsSelector :: Selector '[Id NSString] Bool
writeFileContentsSelector = mkSelector "writeFileContents:"

-- | @Selector@ for @readFileContentsType:toFile:@
readFileContentsType_toFileSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
readFileContentsType_toFileSelector = mkSelector "readFileContentsType:toFile:"

-- | @Selector@ for @writeFileWrapper:@
writeFileWrapperSelector :: Selector '[Id NSFileWrapper] Bool
writeFileWrapperSelector = mkSelector "writeFileWrapper:"

-- | @Selector@ for @readFileWrapper@
readFileWrapperSelector :: Selector '[] (Id NSFileWrapper)
readFileWrapperSelector = mkSelector "readFileWrapper"

-- | @Selector@ for @typesFilterableTo:@
typesFilterableToSelector :: Selector '[Id NSString] (Id NSArray)
typesFilterableToSelector = mkSelector "typesFilterableTo:"

-- | @Selector@ for @pasteboardByFilteringFile:@
pasteboardByFilteringFileSelector :: Selector '[Id NSString] (Id NSPasteboard)
pasteboardByFilteringFileSelector = mkSelector "pasteboardByFilteringFile:"

-- | @Selector@ for @pasteboardByFilteringData:ofType:@
pasteboardByFilteringData_ofTypeSelector :: Selector '[Id NSData, Id NSString] (Id NSPasteboard)
pasteboardByFilteringData_ofTypeSelector = mkSelector "pasteboardByFilteringData:ofType:"

-- | @Selector@ for @pasteboardByFilteringTypesInPasteboard:@
pasteboardByFilteringTypesInPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSPasteboard)
pasteboardByFilteringTypesInPasteboardSelector = mkSelector "pasteboardByFilteringTypesInPasteboard:"

-- | @Selector@ for @generalPasteboard@
generalPasteboardSelector :: Selector '[] (Id NSPasteboard)
generalPasteboardSelector = mkSelector "generalPasteboard"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @changeCount@
changeCountSelector :: Selector '[] CLong
changeCountSelector = mkSelector "changeCount"

-- | @Selector@ for @accessBehavior@
accessBehaviorSelector :: Selector '[] NSPasteboardAccessBehavior
accessBehaviorSelector = mkSelector "accessBehavior"

-- | @Selector@ for @pasteboardItems@
pasteboardItemsSelector :: Selector '[] (Id NSArray)
pasteboardItemsSelector = mkSelector "pasteboardItems"

-- | @Selector@ for @types@
typesSelector :: Selector '[] (Id NSArray)
typesSelector = mkSelector "types"

