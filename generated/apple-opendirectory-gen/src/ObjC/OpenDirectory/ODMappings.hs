{-# LANGUAGE DataKinds #-}
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
  , commentSelector
  , functionAttributesSelector
  , functionSelector
  , identifierSelector
  , mappingsSelector
  , recordMapForStandardRecordTypeSelector
  , recordTypesSelector
  , setCommentSelector
  , setFunctionAttributesSelector
  , setFunctionSelector
  , setIdentifierSelector
  , setRecordMap_forStandardRecordTypeSelector
  , setTemplateNameSelector
  , templateNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' mappingsSelector

-- | recordType:
--
-- Returns an ODRecordMap associated with the provided recordtype.
--
-- Returns an ODRecordMap associated with the provided recordtype.
--
-- ObjC selector: @- recordMapForStandardRecordType:@
recordMapForStandardRecordType :: (IsODMappings odMappings, IsNSString stdType) => odMappings -> stdType -> IO (Id ODRecordMap)
recordMapForStandardRecordType odMappings stdType =
  sendMessage odMappings recordMapForStandardRecordTypeSelector (toNSString stdType)

-- | setRecordMap:forRecordType:
--
-- Sets a particular ODRecordMap for a given standard record type.
--
-- Sets a particular ODRecordMap for a given standard record type.
--
-- ObjC selector: @- setRecordMap:forStandardRecordType:@
setRecordMap_forStandardRecordType :: (IsODMappings odMappings, IsODRecordMap map_, IsNSString stdType) => odMappings -> map_ -> stdType -> IO ()
setRecordMap_forStandardRecordType odMappings map_ stdType =
  sendMessage odMappings setRecordMap_forStandardRecordTypeSelector (toODRecordMap map_) (toNSString stdType)

-- | @- comment@
comment :: IsODMappings odMappings => odMappings -> IO (Id NSString)
comment odMappings =
  sendMessage odMappings commentSelector

-- | @- setComment:@
setComment :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setComment odMappings value =
  sendMessage odMappings setCommentSelector (toNSString value)

-- | @- templateName@
templateName :: IsODMappings odMappings => odMappings -> IO (Id NSString)
templateName odMappings =
  sendMessage odMappings templateNameSelector

-- | @- setTemplateName:@
setTemplateName :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setTemplateName odMappings value =
  sendMessage odMappings setTemplateNameSelector (toNSString value)

-- | @- identifier@
identifier :: IsODMappings odMappings => odMappings -> IO (Id NSString)
identifier odMappings =
  sendMessage odMappings identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setIdentifier odMappings value =
  sendMessage odMappings setIdentifierSelector (toNSString value)

-- | @- recordTypes@
recordTypes :: IsODMappings odMappings => odMappings -> IO (Id NSArray)
recordTypes odMappings =
  sendMessage odMappings recordTypesSelector

-- | @- function@
function :: IsODMappings odMappings => odMappings -> IO (Id NSString)
function odMappings =
  sendMessage odMappings functionSelector

-- | @- setFunction:@
setFunction :: (IsODMappings odMappings, IsNSString value) => odMappings -> value -> IO ()
setFunction odMappings value =
  sendMessage odMappings setFunctionSelector (toNSString value)

-- | @- functionAttributes@
functionAttributes :: IsODMappings odMappings => odMappings -> IO (Id NSArray)
functionAttributes odMappings =
  sendMessage odMappings functionAttributesSelector

-- | @- setFunctionAttributes:@
setFunctionAttributes :: (IsODMappings odMappings, IsNSArray value) => odMappings -> value -> IO ()
setFunctionAttributes odMappings value =
  sendMessage odMappings setFunctionAttributesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mappings@
mappingsSelector :: Selector '[] (Id ODMappings)
mappingsSelector = mkSelector "mappings"

-- | @Selector@ for @recordMapForStandardRecordType:@
recordMapForStandardRecordTypeSelector :: Selector '[Id NSString] (Id ODRecordMap)
recordMapForStandardRecordTypeSelector = mkSelector "recordMapForStandardRecordType:"

-- | @Selector@ for @setRecordMap:forStandardRecordType:@
setRecordMap_forStandardRecordTypeSelector :: Selector '[Id ODRecordMap, Id NSString] ()
setRecordMap_forStandardRecordTypeSelector = mkSelector "setRecordMap:forStandardRecordType:"

-- | @Selector@ for @comment@
commentSelector :: Selector '[] (Id NSString)
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector '[Id NSString] ()
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @templateName@
templateNameSelector :: Selector '[] (Id NSString)
templateNameSelector = mkSelector "templateName"

-- | @Selector@ for @setTemplateName:@
setTemplateNameSelector :: Selector '[Id NSString] ()
setTemplateNameSelector = mkSelector "setTemplateName:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector '[] (Id NSArray)
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @function@
functionSelector :: Selector '[] (Id NSString)
functionSelector = mkSelector "function"

-- | @Selector@ for @setFunction:@
setFunctionSelector :: Selector '[Id NSString] ()
setFunctionSelector = mkSelector "setFunction:"

-- | @Selector@ for @functionAttributes@
functionAttributesSelector :: Selector '[] (Id NSArray)
functionAttributesSelector = mkSelector "functionAttributes"

-- | @Selector@ for @setFunctionAttributes:@
setFunctionAttributesSelector :: Selector '[Id NSArray] ()
setFunctionAttributesSelector = mkSelector "setFunctionAttributes:"

