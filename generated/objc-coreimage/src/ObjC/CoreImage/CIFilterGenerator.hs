{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The goal is to CIFilters to be connected and form a single CIFilter for ease of reusability.
--
-- The CIFilterGenerator allows developers to create complex effects built out of one or more CIFilter and reuse them without changing code. The resulting CIFilterGenerator can be written into a file for which we introduce a new file type (extension). A CIFilterGenerator can be created from the API or more conveniently through an editor view that we provide. CIFilterGenerator files can be put into the Image Units folders on the system and they will be loaded when the user invokes one of the loadPlugIns methods. They will be registered by their filename or if present by an attribute in its description.
--
-- Generated bindings for @CIFilterGenerator@.
module ObjC.CoreImage.CIFilterGenerator
  ( CIFilterGenerator
  , IsCIFilterGenerator(..)
  , filterGenerator
  , filterGeneratorWithContentsOfURL
  , initWithContentsOfURL
  , connectObject_withKey_toObject_withKey
  , disconnectObject_withKey_toObject_withKey
  , exportKey_fromObject_withName
  , removeExportedKey
  , setAttributes_forExportedKey
  , filter_
  , registerFilterName
  , writeToURL_atomically
  , exportedKeys
  , classAttributes
  , setClassAttributes
  , filterGeneratorSelector
  , filterGeneratorWithContentsOfURLSelector
  , initWithContentsOfURLSelector
  , connectObject_withKey_toObject_withKeySelector
  , disconnectObject_withKey_toObject_withKeySelector
  , exportKey_fromObject_withNameSelector
  , removeExportedKeySelector
  , setAttributes_forExportedKeySelector
  , filterSelector
  , registerFilterNameSelector
  , writeToURL_atomicallySelector
  , exportedKeysSelector
  , classAttributesSelector
  , setClassAttributesSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This creates an empty CIFilterGenerator in which you connect filters and images.
--
-- ObjC selector: @+ filterGenerator@
filterGenerator :: IO (Id CIFilterGenerator)
filterGenerator  =
  do
    cls' <- getRequiredClass "CIFilterGenerator"
    sendClassMsg cls' (mkSelector "filterGenerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a CIFilterGenerator with the contents of the file.
--
-- Returns: CIFilterGenerator object. If the file could not be read it returns nil.
--
-- ObjC selector: @+ filterGeneratorWithContentsOfURL:@
filterGeneratorWithContentsOfURL :: IsNSURL aURL => aURL -> IO (Id CIFilterGenerator)
filterGeneratorWithContentsOfURL aURL =
  do
    cls' <- getRequiredClass "CIFilterGenerator"
    withObjCPtr aURL $ \raw_aURL ->
      sendClassMsg cls' (mkSelector "filterGeneratorWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_aURL :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a CIFilterGenerator with the contents of the file.
--
-- Returns: CIFilterGenerator object. If the file could not be read it returns nil.
--
-- ObjC selector: @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsCIFilterGenerator ciFilterGenerator, IsNSURL aURL) => ciFilterGenerator -> aURL -> IO RawId
initWithContentsOfURL ciFilterGenerator  aURL =
withObjCPtr aURL $ \raw_aURL ->
    fmap (RawId . castPtr) $ sendMsg ciFilterGenerator (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_aURL :: Ptr ())]

-- | Connect two objects into the filter chain.
--
-- This method connects two object in the filter chain. For instance you can connect the outputImage key of a CISepiaTone filter object to the inputImage key of another CIFilter.
--
-- @sourceObject@ — A CIFilter, CIImage, NSString, or NSURL describing the path to the image
--
-- @sourceKey@ — For KVC access to the source object. Can be nil which means that the source object will be used directly.
--
-- @targetObject@ — The object that you link the source object to.
--
-- @targetKey@ — The key that you assign the source object to.
--
-- ObjC selector: @- connectObject:withKey:toObject:withKey:@
connectObject_withKey_toObject_withKey :: (IsCIFilterGenerator ciFilterGenerator, IsNSString sourceKey, IsNSString targetKey) => ciFilterGenerator -> RawId -> sourceKey -> RawId -> targetKey -> IO ()
connectObject_withKey_toObject_withKey ciFilterGenerator  sourceObject sourceKey targetObject targetKey =
withObjCPtr sourceKey $ \raw_sourceKey ->
  withObjCPtr targetKey $ \raw_targetKey ->
      sendMsg ciFilterGenerator (mkSelector "connectObject:withKey:toObject:withKey:") retVoid [argPtr (castPtr (unRawId sourceObject) :: Ptr ()), argPtr (castPtr raw_sourceKey :: Ptr ()), argPtr (castPtr (unRawId targetObject) :: Ptr ()), argPtr (castPtr raw_targetKey :: Ptr ())]

-- | Removes the connection between two objects in the filter chain.
--
-- Use this method to disconnect two objects that you connected using the connectObject:withKey:toObject:withKey: method.
--
-- @sourceObject@ — A CIFilter or CIImage or an NSString or an NSURL describing the path to the image
--
-- @sourceKey@ — For KVC access to the source object. Can be nil which means that the source object will be used directly.
--
-- @targetObject@ — The object that you linked the source object to.
--
-- @targetKey@ — The key that you assigned the source object to.
--
-- ObjC selector: @- disconnectObject:withKey:toObject:withKey:@
disconnectObject_withKey_toObject_withKey :: (IsCIFilterGenerator ciFilterGenerator, IsNSString sourceKey, IsNSString targetKey) => ciFilterGenerator -> RawId -> sourceKey -> RawId -> targetKey -> IO ()
disconnectObject_withKey_toObject_withKey ciFilterGenerator  sourceObject sourceKey targetObject targetKey =
withObjCPtr sourceKey $ \raw_sourceKey ->
  withObjCPtr targetKey $ \raw_targetKey ->
      sendMsg ciFilterGenerator (mkSelector "disconnectObject:withKey:toObject:withKey:") retVoid [argPtr (castPtr (unRawId sourceObject) :: Ptr ()), argPtr (castPtr raw_sourceKey :: Ptr ()), argPtr (castPtr (unRawId targetObject) :: Ptr ()), argPtr (castPtr raw_targetKey :: Ptr ())]

-- | This methods allows you to export an input or output key of an object in the filter chain to be available through the inputKeys or outputKeys API when converted into a CIFilter
--
-- When you create a CIFilter from the CIFilterGenerator, you might want the client of the filter being able to set some of the paramters of the filter chain. To do so these parameters have to be exported as keys much like the inputKeys and outputKeys of all CIFilters.
--
-- @key@ — The key path that is to be exported from the target object (eg. inputImage)
--
-- @targetObject@ — The object of which the key is to be exported (eg the filter).
--
-- @exportedKeyName@ — The name under which you want the new key to be available. This parameter can be nil in which case the original key name will be used. This name has to be unique. If a key being exported is an inputKey of the filter it will be exported as an input key and the other way around for output keys.
--
-- ObjC selector: @- exportKey:fromObject:withName:@
exportKey_fromObject_withName :: (IsCIFilterGenerator ciFilterGenerator, IsNSString key, IsNSString exportedKeyName) => ciFilterGenerator -> key -> RawId -> exportedKeyName -> IO ()
exportKey_fromObject_withName ciFilterGenerator  key targetObject exportedKeyName =
withObjCPtr key $ \raw_key ->
  withObjCPtr exportedKeyName $ \raw_exportedKeyName ->
      sendMsg ciFilterGenerator (mkSelector "exportKey:fromObject:withName:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId targetObject) :: Ptr ()), argPtr (castPtr raw_exportedKeyName :: Ptr ())]

-- | Removes a key that was exported before using exportKey:fromObject:withName:
--
-- Use this method when you want to remove a prior exported key. It will not show up under inputKeys or outputKeys anymore.
--
-- @exportedKeyName@ — Name of the key that was exported.
--
-- ObjC selector: @- removeExportedKey:@
removeExportedKey :: (IsCIFilterGenerator ciFilterGenerator, IsNSString exportedKeyName) => ciFilterGenerator -> exportedKeyName -> IO ()
removeExportedKey ciFilterGenerator  exportedKeyName =
withObjCPtr exportedKeyName $ \raw_exportedKeyName ->
    sendMsg ciFilterGenerator (mkSelector "removeExportedKey:") retVoid [argPtr (castPtr raw_exportedKeyName :: Ptr ())]

-- | Set a new dictionary of attributes for an exported key.
--
-- By default, the exported key inherits the attributes from its original key and target object. Use this method to for instance change the default value or lower the maximum allowed value.
--
-- ObjC selector: @- setAttributes:forExportedKey:@
setAttributes_forExportedKey :: (IsCIFilterGenerator ciFilterGenerator, IsNSDictionary attributes, IsNSString key) => ciFilterGenerator -> attributes -> key -> IO ()
setAttributes_forExportedKey ciFilterGenerator  attributes key =
withObjCPtr attributes $ \raw_attributes ->
  withObjCPtr key $ \raw_key ->
      sendMsg ciFilterGenerator (mkSelector "setAttributes:forExportedKey:") retVoid [argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | Create a CIFilter object based on this filter chain.
--
-- This method creates a CIFilter from the filter chain where the topology of the chain is immutable, meaning that changes to the filter chain will not be reflected in the filter. The filter will have the input and output keys that were exported as described above.
--
-- ObjC selector: @- filter@
filter_ :: IsCIFilterGenerator ciFilterGenerator => ciFilterGenerator -> IO (Id CIFilter)
filter_ ciFilterGenerator  =
  sendMsg ciFilterGenerator (mkSelector "filter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Register the resulting filter of the chain in the CIFilter repository.
--
-- This method allows you to register the filter chain as a named filter in the filter repository. You can then create a CIFilter object from it using the filterWithName: method. Make sure you set the class attributes first - see CIFilter for a description of the classAttributes that are needed to register a filter. When registering Core Image automatically adds the kCIFilterGeneratorCategory to the filters categories. The kCIFilterGeneratorCategory is purely for identification purpose and will not be exposed in the filter browser as a seperate category.
--
-- @name@ — The name under which the filter will be registered. This name has to be unique.
--
-- ObjC selector: @- registerFilterName:@
registerFilterName :: (IsCIFilterGenerator ciFilterGenerator, IsNSString name) => ciFilterGenerator -> name -> IO ()
registerFilterName ciFilterGenerator  name =
withObjCPtr name $ \raw_name ->
    sendMsg ciFilterGenerator (mkSelector "registerFilterName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | Write the CIFilterGenerator into a file
--
-- Returns: Returns true when the chain with written our succesfully
--
-- ObjC selector: @- writeToURL:atomically:@
writeToURL_atomically :: (IsCIFilterGenerator ciFilterGenerator, IsNSURL aURL) => ciFilterGenerator -> aURL -> Bool -> IO Bool
writeToURL_atomically ciFilterGenerator  aURL flag =
withObjCPtr aURL $ \raw_aURL ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFilterGenerator (mkSelector "writeToURL:atomically:") retCULong [argPtr (castPtr raw_aURL :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | An array of the exported keys.
--
-- Use this method to get an NSArray of all the keys that you have exported using exportKey:fromObject:withName: or that were exported before written to a file from which you read the filter chain.
--
-- Returns: An array of dictionaries that describe the exported key and target object. See CIExportedKey, CIExportedKeyTargetObject and CIExportedKeyName for keys used in the dictionary.
--
-- ObjC selector: @- exportedKeys@
exportedKeys :: IsCIFilterGenerator ciFilterGenerator => ciFilterGenerator -> IO (Id NSDictionary)
exportedKeys ciFilterGenerator  =
  sendMsg ciFilterGenerator (mkSelector "exportedKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Retrieve or Set the class attributes that will be used to register the filter using the registerFilterName method. Make sure you set the class attributes before using the registerFilterName method. See CIFilter for a description of the classAttributes that are needed to register a filter.
--
-- ObjC selector: @- classAttributes@
classAttributes :: IsCIFilterGenerator ciFilterGenerator => ciFilterGenerator -> IO (Id NSDictionary)
classAttributes ciFilterGenerator  =
  sendMsg ciFilterGenerator (mkSelector "classAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Retrieve or Set the class attributes that will be used to register the filter using the registerFilterName method. Make sure you set the class attributes before using the registerFilterName method. See CIFilter for a description of the classAttributes that are needed to register a filter.
--
-- ObjC selector: @- setClassAttributes:@
setClassAttributes :: (IsCIFilterGenerator ciFilterGenerator, IsNSDictionary value) => ciFilterGenerator -> value -> IO ()
setClassAttributes ciFilterGenerator  value =
withObjCPtr value $ \raw_value ->
    sendMsg ciFilterGenerator (mkSelector "setClassAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterGenerator@
filterGeneratorSelector :: Selector
filterGeneratorSelector = mkSelector "filterGenerator"

-- | @Selector@ for @filterGeneratorWithContentsOfURL:@
filterGeneratorWithContentsOfURLSelector :: Selector
filterGeneratorWithContentsOfURLSelector = mkSelector "filterGeneratorWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @connectObject:withKey:toObject:withKey:@
connectObject_withKey_toObject_withKeySelector :: Selector
connectObject_withKey_toObject_withKeySelector = mkSelector "connectObject:withKey:toObject:withKey:"

-- | @Selector@ for @disconnectObject:withKey:toObject:withKey:@
disconnectObject_withKey_toObject_withKeySelector :: Selector
disconnectObject_withKey_toObject_withKeySelector = mkSelector "disconnectObject:withKey:toObject:withKey:"

-- | @Selector@ for @exportKey:fromObject:withName:@
exportKey_fromObject_withNameSelector :: Selector
exportKey_fromObject_withNameSelector = mkSelector "exportKey:fromObject:withName:"

-- | @Selector@ for @removeExportedKey:@
removeExportedKeySelector :: Selector
removeExportedKeySelector = mkSelector "removeExportedKey:"

-- | @Selector@ for @setAttributes:forExportedKey:@
setAttributes_forExportedKeySelector :: Selector
setAttributes_forExportedKeySelector = mkSelector "setAttributes:forExportedKey:"

-- | @Selector@ for @filter@
filterSelector :: Selector
filterSelector = mkSelector "filter"

-- | @Selector@ for @registerFilterName:@
registerFilterNameSelector :: Selector
registerFilterNameSelector = mkSelector "registerFilterName:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @exportedKeys@
exportedKeysSelector :: Selector
exportedKeysSelector = mkSelector "exportedKeys"

-- | @Selector@ for @classAttributes@
classAttributesSelector :: Selector
classAttributesSelector = mkSelector "classAttributes"

-- | @Selector@ for @setClassAttributes:@
setClassAttributesSelector :: Selector
setClassAttributesSelector = mkSelector "setClassAttributes:"

