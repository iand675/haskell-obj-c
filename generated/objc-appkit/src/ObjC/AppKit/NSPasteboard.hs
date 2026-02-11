{-# LANGUAGE PatternSynonyms #-}
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
  , types
  , pasteboardWithNameSelector
  , pasteboardWithUniqueNameSelector
  , releaseGloballySelector
  , prepareForNewContentsWithOptionsSelector
  , clearContentsSelector
  , writeObjectsSelector
  , readObjectsForClasses_optionsSelector
  , indexOfPasteboardItemSelector
  , canReadItemWithDataConformingToTypesSelector
  , canReadObjectForClasses_optionsSelector
  , declareTypes_ownerSelector
  , addTypes_ownerSelector
  , availableTypeFromArraySelector
  , setData_forTypeSelector
  , setPropertyList_forTypeSelector
  , setString_forTypeSelector
  , dataForTypeSelector
  , propertyListForTypeSelector
  , stringForTypeSelector
  , writeFileContentsSelector
  , readFileContentsType_toFileSelector
  , writeFileWrapperSelector
  , readFileWrapperSelector
  , typesFilterableToSelector
  , pasteboardByFilteringFileSelector
  , pasteboardByFilteringData_ofTypeSelector
  , pasteboardByFilteringTypesInPasteboardSelector
  , generalPasteboardSelector
  , nameSelector
  , changeCountSelector
  , accessBehaviorSelector
  , typesSelector

  -- * Enum types
  , NSPasteboardAccessBehavior(NSPasteboardAccessBehavior)
  , pattern NSPasteboardAccessBehaviorDefault
  , pattern NSPasteboardAccessBehaviorAsk
  , pattern NSPasteboardAccessBehaviorAlwaysAllow
  , pattern NSPasteboardAccessBehaviorAlwaysDeny
  , NSPasteboardContentsOptions(NSPasteboardContentsOptions)
  , pattern NSPasteboardContentsCurrentHostOnly

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ pasteboardWithName:@
pasteboardWithName :: IsNSString name => name -> IO (Id NSPasteboard)
pasteboardWithName name =
  do
    cls' <- getRequiredClass "NSPasteboard"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "pasteboardWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pasteboardWithUniqueName@
pasteboardWithUniqueName :: IO (Id NSPasteboard)
pasteboardWithUniqueName  =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMsg cls' (mkSelector "pasteboardWithUniqueName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- releaseGlobally@
releaseGlobally :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO ()
releaseGlobally nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "releaseGlobally") retVoid []

-- | @- prepareForNewContentsWithOptions:@
prepareForNewContentsWithOptions :: IsNSPasteboard nsPasteboard => nsPasteboard -> NSPasteboardContentsOptions -> IO CLong
prepareForNewContentsWithOptions nsPasteboard  options =
  sendMsg nsPasteboard (mkSelector "prepareForNewContentsWithOptions:") retCLong [argCULong (coerce options)]

-- | @- clearContents@
clearContents :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO CLong
clearContents nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "clearContents") retCLong []

-- | @- writeObjects:@
writeObjects :: (IsNSPasteboard nsPasteboard, IsNSArray objects) => nsPasteboard -> objects -> IO Bool
writeObjects nsPasteboard  objects =
withObjCPtr objects $ \raw_objects ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "writeObjects:") retCULong [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- readObjectsForClasses:options:@
readObjectsForClasses_options :: (IsNSPasteboard nsPasteboard, IsNSArray classArray, IsNSDictionary options) => nsPasteboard -> classArray -> options -> IO (Id NSArray)
readObjectsForClasses_options nsPasteboard  classArray options =
withObjCPtr classArray $ \raw_classArray ->
  withObjCPtr options $ \raw_options ->
      sendMsg nsPasteboard (mkSelector "readObjectsForClasses:options:") (retPtr retVoid) [argPtr (castPtr raw_classArray :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexOfPasteboardItem:@
indexOfPasteboardItem :: (IsNSPasteboard nsPasteboard, IsNSPasteboardItem pasteboardItem) => nsPasteboard -> pasteboardItem -> IO CULong
indexOfPasteboardItem nsPasteboard  pasteboardItem =
withObjCPtr pasteboardItem $ \raw_pasteboardItem ->
    sendMsg nsPasteboard (mkSelector "indexOfPasteboardItem:") retCULong [argPtr (castPtr raw_pasteboardItem :: Ptr ())]

-- | @- canReadItemWithDataConformingToTypes:@
canReadItemWithDataConformingToTypes :: (IsNSPasteboard nsPasteboard, IsNSArray types) => nsPasteboard -> types -> IO Bool
canReadItemWithDataConformingToTypes nsPasteboard  types =
withObjCPtr types $ \raw_types ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "canReadItemWithDataConformingToTypes:") retCULong [argPtr (castPtr raw_types :: Ptr ())]

-- | @- canReadObjectForClasses:options:@
canReadObjectForClasses_options :: (IsNSPasteboard nsPasteboard, IsNSArray classArray, IsNSDictionary options) => nsPasteboard -> classArray -> options -> IO Bool
canReadObjectForClasses_options nsPasteboard  classArray options =
withObjCPtr classArray $ \raw_classArray ->
  withObjCPtr options $ \raw_options ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "canReadObjectForClasses:options:") retCULong [argPtr (castPtr raw_classArray :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- declareTypes:owner:@
declareTypes_owner :: (IsNSPasteboard nsPasteboard, IsNSArray newTypes) => nsPasteboard -> newTypes -> RawId -> IO CLong
declareTypes_owner nsPasteboard  newTypes newOwner =
withObjCPtr newTypes $ \raw_newTypes ->
    sendMsg nsPasteboard (mkSelector "declareTypes:owner:") retCLong [argPtr (castPtr raw_newTypes :: Ptr ()), argPtr (castPtr (unRawId newOwner) :: Ptr ())]

-- | @- addTypes:owner:@
addTypes_owner :: (IsNSPasteboard nsPasteboard, IsNSArray newTypes) => nsPasteboard -> newTypes -> RawId -> IO CLong
addTypes_owner nsPasteboard  newTypes newOwner =
withObjCPtr newTypes $ \raw_newTypes ->
    sendMsg nsPasteboard (mkSelector "addTypes:owner:") retCLong [argPtr (castPtr raw_newTypes :: Ptr ()), argPtr (castPtr (unRawId newOwner) :: Ptr ())]

-- | @- availableTypeFromArray:@
availableTypeFromArray :: (IsNSPasteboard nsPasteboard, IsNSArray types) => nsPasteboard -> types -> IO (Id NSString)
availableTypeFromArray nsPasteboard  types =
withObjCPtr types $ \raw_types ->
    sendMsg nsPasteboard (mkSelector "availableTypeFromArray:") (retPtr retVoid) [argPtr (castPtr raw_types :: Ptr ())] >>= retainedObject . castPtr

-- | @- setData:forType:@
setData_forType :: (IsNSPasteboard nsPasteboard, IsNSData data_, IsNSString dataType) => nsPasteboard -> data_ -> dataType -> IO Bool
setData_forType nsPasteboard  data_ dataType =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr dataType $ \raw_dataType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "setData:forType:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_dataType :: Ptr ())]

-- | @- setPropertyList:forType:@
setPropertyList_forType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> RawId -> dataType -> IO Bool
setPropertyList_forType nsPasteboard  plist dataType =
withObjCPtr dataType $ \raw_dataType ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "setPropertyList:forType:") retCULong [argPtr (castPtr (unRawId plist) :: Ptr ()), argPtr (castPtr raw_dataType :: Ptr ())]

-- | @- setString:forType:@
setString_forType :: (IsNSPasteboard nsPasteboard, IsNSString string, IsNSString dataType) => nsPasteboard -> string -> dataType -> IO Bool
setString_forType nsPasteboard  string dataType =
withObjCPtr string $ \raw_string ->
  withObjCPtr dataType $ \raw_dataType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "setString:forType:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_dataType :: Ptr ())]

-- | @- dataForType:@
dataForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO (Id NSData)
dataForType nsPasteboard  dataType =
withObjCPtr dataType $ \raw_dataType ->
    sendMsg nsPasteboard (mkSelector "dataForType:") (retPtr retVoid) [argPtr (castPtr raw_dataType :: Ptr ())] >>= retainedObject . castPtr

-- | @- propertyListForType:@
propertyListForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO RawId
propertyListForType nsPasteboard  dataType =
withObjCPtr dataType $ \raw_dataType ->
    fmap (RawId . castPtr) $ sendMsg nsPasteboard (mkSelector "propertyListForType:") (retPtr retVoid) [argPtr (castPtr raw_dataType :: Ptr ())]

-- | @- stringForType:@
stringForType :: (IsNSPasteboard nsPasteboard, IsNSString dataType) => nsPasteboard -> dataType -> IO (Id NSString)
stringForType nsPasteboard  dataType =
withObjCPtr dataType $ \raw_dataType ->
    sendMsg nsPasteboard (mkSelector "stringForType:") (retPtr retVoid) [argPtr (castPtr raw_dataType :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeFileContents:@
writeFileContents :: (IsNSPasteboard nsPasteboard, IsNSString filename) => nsPasteboard -> filename -> IO Bool
writeFileContents nsPasteboard  filename =
withObjCPtr filename $ \raw_filename ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "writeFileContents:") retCULong [argPtr (castPtr raw_filename :: Ptr ())]

-- | @- readFileContentsType:toFile:@
readFileContentsType_toFile :: (IsNSPasteboard nsPasteboard, IsNSString type_, IsNSString filename) => nsPasteboard -> type_ -> filename -> IO (Id NSString)
readFileContentsType_toFile nsPasteboard  type_ filename =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr filename $ \raw_filename ->
      sendMsg nsPasteboard (mkSelector "readFileContentsType:toFile:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeFileWrapper:@
writeFileWrapper :: (IsNSPasteboard nsPasteboard, IsNSFileWrapper wrapper) => nsPasteboard -> wrapper -> IO Bool
writeFileWrapper nsPasteboard  wrapper =
withObjCPtr wrapper $ \raw_wrapper ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPasteboard (mkSelector "writeFileWrapper:") retCULong [argPtr (castPtr raw_wrapper :: Ptr ())]

-- | @- readFileWrapper@
readFileWrapper :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSFileWrapper)
readFileWrapper nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "readFileWrapper") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ typesFilterableTo:@
typesFilterableTo :: IsNSString type_ => type_ -> IO (Id NSArray)
typesFilterableTo type_ =
  do
    cls' <- getRequiredClass "NSPasteboard"
    withObjCPtr type_ $ \raw_type_ ->
      sendClassMsg cls' (mkSelector "typesFilterableTo:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pasteboardByFilteringFile:@
pasteboardByFilteringFile :: IsNSString filename => filename -> IO (Id NSPasteboard)
pasteboardByFilteringFile filename =
  do
    cls' <- getRequiredClass "NSPasteboard"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "pasteboardByFilteringFile:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pasteboardByFilteringData:ofType:@
pasteboardByFilteringData_ofType :: (IsNSData data_, IsNSString type_) => data_ -> type_ -> IO (Id NSPasteboard)
pasteboardByFilteringData_ofType data_ type_ =
  do
    cls' <- getRequiredClass "NSPasteboard"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr type_ $ \raw_type_ ->
        sendClassMsg cls' (mkSelector "pasteboardByFilteringData:ofType:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pasteboardByFilteringTypesInPasteboard:@
pasteboardByFilteringTypesInPasteboard :: IsNSPasteboard pboard => pboard -> IO (Id NSPasteboard)
pasteboardByFilteringTypesInPasteboard pboard =
  do
    cls' <- getRequiredClass "NSPasteboard"
    withObjCPtr pboard $ \raw_pboard ->
      sendClassMsg cls' (mkSelector "pasteboardByFilteringTypesInPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pboard :: Ptr ())] >>= retainedObject . castPtr

-- | @+ generalPasteboard@
generalPasteboard :: IO (Id NSPasteboard)
generalPasteboard  =
  do
    cls' <- getRequiredClass "NSPasteboard"
    sendClassMsg cls' (mkSelector "generalPasteboard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSString)
name nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changeCount@
changeCount :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO CLong
changeCount nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "changeCount") retCLong []

-- | The current pasteboard access behavior. The user can customize this behavior per-app in System Settings for any app that has triggered a pasteboard access alert in the past.
--
-- ObjC selector: @- accessBehavior@
accessBehavior :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO NSPasteboardAccessBehavior
accessBehavior nsPasteboard  =
  fmap (coerce :: CLong -> NSPasteboardAccessBehavior) $ sendMsg nsPasteboard (mkSelector "accessBehavior") retCLong []

-- | @- types@
types :: IsNSPasteboard nsPasteboard => nsPasteboard -> IO (Id NSArray)
types nsPasteboard  =
  sendMsg nsPasteboard (mkSelector "types") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pasteboardWithName:@
pasteboardWithNameSelector :: Selector
pasteboardWithNameSelector = mkSelector "pasteboardWithName:"

-- | @Selector@ for @pasteboardWithUniqueName@
pasteboardWithUniqueNameSelector :: Selector
pasteboardWithUniqueNameSelector = mkSelector "pasteboardWithUniqueName"

-- | @Selector@ for @releaseGlobally@
releaseGloballySelector :: Selector
releaseGloballySelector = mkSelector "releaseGlobally"

-- | @Selector@ for @prepareForNewContentsWithOptions:@
prepareForNewContentsWithOptionsSelector :: Selector
prepareForNewContentsWithOptionsSelector = mkSelector "prepareForNewContentsWithOptions:"

-- | @Selector@ for @clearContents@
clearContentsSelector :: Selector
clearContentsSelector = mkSelector "clearContents"

-- | @Selector@ for @writeObjects:@
writeObjectsSelector :: Selector
writeObjectsSelector = mkSelector "writeObjects:"

-- | @Selector@ for @readObjectsForClasses:options:@
readObjectsForClasses_optionsSelector :: Selector
readObjectsForClasses_optionsSelector = mkSelector "readObjectsForClasses:options:"

-- | @Selector@ for @indexOfPasteboardItem:@
indexOfPasteboardItemSelector :: Selector
indexOfPasteboardItemSelector = mkSelector "indexOfPasteboardItem:"

-- | @Selector@ for @canReadItemWithDataConformingToTypes:@
canReadItemWithDataConformingToTypesSelector :: Selector
canReadItemWithDataConformingToTypesSelector = mkSelector "canReadItemWithDataConformingToTypes:"

-- | @Selector@ for @canReadObjectForClasses:options:@
canReadObjectForClasses_optionsSelector :: Selector
canReadObjectForClasses_optionsSelector = mkSelector "canReadObjectForClasses:options:"

-- | @Selector@ for @declareTypes:owner:@
declareTypes_ownerSelector :: Selector
declareTypes_ownerSelector = mkSelector "declareTypes:owner:"

-- | @Selector@ for @addTypes:owner:@
addTypes_ownerSelector :: Selector
addTypes_ownerSelector = mkSelector "addTypes:owner:"

-- | @Selector@ for @availableTypeFromArray:@
availableTypeFromArraySelector :: Selector
availableTypeFromArraySelector = mkSelector "availableTypeFromArray:"

-- | @Selector@ for @setData:forType:@
setData_forTypeSelector :: Selector
setData_forTypeSelector = mkSelector "setData:forType:"

-- | @Selector@ for @setPropertyList:forType:@
setPropertyList_forTypeSelector :: Selector
setPropertyList_forTypeSelector = mkSelector "setPropertyList:forType:"

-- | @Selector@ for @setString:forType:@
setString_forTypeSelector :: Selector
setString_forTypeSelector = mkSelector "setString:forType:"

-- | @Selector@ for @dataForType:@
dataForTypeSelector :: Selector
dataForTypeSelector = mkSelector "dataForType:"

-- | @Selector@ for @propertyListForType:@
propertyListForTypeSelector :: Selector
propertyListForTypeSelector = mkSelector "propertyListForType:"

-- | @Selector@ for @stringForType:@
stringForTypeSelector :: Selector
stringForTypeSelector = mkSelector "stringForType:"

-- | @Selector@ for @writeFileContents:@
writeFileContentsSelector :: Selector
writeFileContentsSelector = mkSelector "writeFileContents:"

-- | @Selector@ for @readFileContentsType:toFile:@
readFileContentsType_toFileSelector :: Selector
readFileContentsType_toFileSelector = mkSelector "readFileContentsType:toFile:"

-- | @Selector@ for @writeFileWrapper:@
writeFileWrapperSelector :: Selector
writeFileWrapperSelector = mkSelector "writeFileWrapper:"

-- | @Selector@ for @readFileWrapper@
readFileWrapperSelector :: Selector
readFileWrapperSelector = mkSelector "readFileWrapper"

-- | @Selector@ for @typesFilterableTo:@
typesFilterableToSelector :: Selector
typesFilterableToSelector = mkSelector "typesFilterableTo:"

-- | @Selector@ for @pasteboardByFilteringFile:@
pasteboardByFilteringFileSelector :: Selector
pasteboardByFilteringFileSelector = mkSelector "pasteboardByFilteringFile:"

-- | @Selector@ for @pasteboardByFilteringData:ofType:@
pasteboardByFilteringData_ofTypeSelector :: Selector
pasteboardByFilteringData_ofTypeSelector = mkSelector "pasteboardByFilteringData:ofType:"

-- | @Selector@ for @pasteboardByFilteringTypesInPasteboard:@
pasteboardByFilteringTypesInPasteboardSelector :: Selector
pasteboardByFilteringTypesInPasteboardSelector = mkSelector "pasteboardByFilteringTypesInPasteboard:"

-- | @Selector@ for @generalPasteboard@
generalPasteboardSelector :: Selector
generalPasteboardSelector = mkSelector "generalPasteboard"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @changeCount@
changeCountSelector :: Selector
changeCountSelector = mkSelector "changeCount"

-- | @Selector@ for @accessBehavior@
accessBehaviorSelector :: Selector
accessBehaviorSelector = mkSelector "accessBehavior"

-- | @Selector@ for @types@
typesSelector :: Selector
typesSelector = mkSelector "types"

