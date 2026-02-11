{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ODMappings@.
module ObjC.OpenDirectory.ODMappings
  ( ODMappings
  , IsODMappings(..)
  , mappings
  , recordMapForStandardRecordType
  , setRecordMap_forStandardRecordType
  , comment
  , setComment
  , templateName
  , setTemplateName
  , identifier
  , setIdentifier
  , recordTypes
  , function
  , setFunction
  , functionAttributes
  , setFunctionAttributes
  , mappingsSelector
  , recordMapForStandardRecordTypeSelector
  , setRecordMap_forStandardRecordTypeSelector
  , commentSelector
  , setCommentSelector
  , templateNameSelector
  , setTemplateNameSelector
  , identifierSelector
  , setIdentifierSelector
  , recordTypesSelector
  , functionSelector
  , setFunctionSelector
  , functionAttributesSelector
  , setFunctionAttributesSelector


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

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | mappings
--
-- Returns an initialized and autoreleased ODMappings object.
--
-- Returns an initialized and autoreleased ODMappings object.
--
-- ObjC selector: @+ mappings@
mappings :: IO (Id ODMappings)
mappings  =
  do
    cls' <- getRequiredClass "ODMappings"
    sendClassMsg cls' (mkSelector "mappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | recordType:
--
-- Returns an ODRecordMap associated with the provided recordtype.
--
-- Returns an ODRecordMap associated with the provided recordtype.
--
-- ObjC selector: @- recordMapForStandardRecordType:@
recordMapForStandardRecordType :: (IsODMappings odMappings, IsNSString stdType) => odMappings -> stdType -> IO (Id ODRecordMap)
recordMapForStandardRecordType odMappings  stdType =
withObjCPtr stdType $ \raw_stdType ->
    sendMsg odMappings (mkSelector "recordMapForStandardRecordType:") (retPtr retVoid) [argPtr (castPtr raw_stdType :: Ptr ())] >>= retainedObject . castPtr

-- | setRecordMap:forRecordType:
--
-- Sets a particular ODRecordMap for a given standard record type.
--
-- Sets a particular ODRecordMap for a given standard record type.
--
-- ObjC selector: @- setRecordMap:forStandardRecordType:@
setRecordMap_forStandardRecordType :: (IsODMappings odMappings, IsODRecordMap map_, IsNSString stdType) => odMappings -> map_ -> stdType -> IO ()
setRecordMap_forStandardRecordType odMappings  map_ stdType =
withObjCPtr map_ $ \raw_map_ ->
  withObjCPtr stdType $ \raw_stdType ->
      sendMsg odMappings (mkSelector "setRecordMap:forStandardRecordType:") retVoid [argPtr (castPtr raw_map_ :: Ptr ()), argPtr (castPtr raw_stdType :: Ptr ())]

-- | @- comment@
comment :: IsODMappings odMappings => odMappings -> IO (Id NSString)
comment odMappings  =
  sendMsg odMappings (mkSelector "comment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setComment:@
setComment :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setComment odMappings  value =
withObjCPtr value $ \raw_value ->
    sendMsg odMappings (mkSelector "setComment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- templateName@
templateName :: IsODMappings odMappings => odMappings -> IO (Id NSString)
templateName odMappings  =
  sendMsg odMappings (mkSelector "templateName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTemplateName:@
setTemplateName :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setTemplateName odMappings  value =
withObjCPtr value $ \raw_value ->
    sendMsg odMappings (mkSelector "setTemplateName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsODMappings odMappings => odMappings -> IO (Id NSString)
identifier odMappings  =
  sendMsg odMappings (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setIdentifier odMappings  value =
withObjCPtr value $ \raw_value ->
    sendMsg odMappings (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordTypes@
recordTypes :: IsODMappings odMappings => odMappings -> IO (Id NSArray)
recordTypes odMappings  =
  sendMsg odMappings (mkSelector "recordTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- function@
function :: IsODMappings odMappings => odMappings -> IO (Id NSString)
function odMappings  =
  sendMsg odMappings (mkSelector "function") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFunction:@
setFunction :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setFunction odMappings  value =
withObjCPtr value $ \raw_value ->
    sendMsg odMappings (mkSelector "setFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- functionAttributes@
functionAttributes :: IsODMappings odMappings => odMappings -> IO (Id NSArray)
functionAttributes odMappings  =
  sendMsg odMappings (mkSelector "functionAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFunctionAttributes:@
setFunctionAttributes :: (IsODMappings odMappings, IsNSArray value) => odMappings -> value -> IO ()
setFunctionAttributes odMappings  value =
withObjCPtr value $ \raw_value ->
    sendMsg odMappings (mkSelector "setFunctionAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mappings@
mappingsSelector :: Selector
mappingsSelector = mkSelector "mappings"

-- | @Selector@ for @recordMapForStandardRecordType:@
recordMapForStandardRecordTypeSelector :: Selector
recordMapForStandardRecordTypeSelector = mkSelector "recordMapForStandardRecordType:"

-- | @Selector@ for @setRecordMap:forStandardRecordType:@
setRecordMap_forStandardRecordTypeSelector :: Selector
setRecordMap_forStandardRecordTypeSelector = mkSelector "setRecordMap:forStandardRecordType:"

-- | @Selector@ for @comment@
commentSelector :: Selector
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @templateName@
templateNameSelector :: Selector
templateNameSelector = mkSelector "templateName"

-- | @Selector@ for @setTemplateName:@
setTemplateNameSelector :: Selector
setTemplateNameSelector = mkSelector "setTemplateName:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @function@
functionSelector :: Selector
functionSelector = mkSelector "function"

-- | @Selector@ for @setFunction:@
setFunctionSelector :: Selector
setFunctionSelector = mkSelector "setFunction:"

-- | @Selector@ for @functionAttributes@
functionAttributesSelector :: Selector
functionAttributesSelector = mkSelector "functionAttributes"

-- | @Selector@ for @setFunctionAttributes:@
setFunctionAttributesSelector :: Selector
setFunctionAttributesSelector = mkSelector "setFunctionAttributes:"

