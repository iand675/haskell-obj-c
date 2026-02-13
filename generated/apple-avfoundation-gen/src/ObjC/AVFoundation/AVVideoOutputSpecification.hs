{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVVideoOutputSpecification
--
-- AVVideoOutputSpecification offers a way to package CMTagCollections together with output settings. Allowing for direct association between output settings and specific tag collections, as well as default output settings which can be associated with all tag collections which do not have a specified mapping.
--
-- For more information about working with CMTagCollections and CMTags first look at <CoreMedia/CMTagCollection.h>
--
-- Generated bindings for @AVVideoOutputSpecification@.
module ObjC.AVFoundation.AVVideoOutputSpecification
  ( AVVideoOutputSpecification
  , IsAVVideoOutputSpecification(..)
  , init_
  , new
  , initWithTagCollections
  , setOutputPixelBufferAttributes_forTagCollection
  , setOutputSettings_forTagCollection
  , preferredTagCollections
  , defaultPixelBufferAttributes
  , setDefaultPixelBufferAttributes
  , defaultOutputSettings
  , setDefaultOutputSettings
  , defaultOutputSettingsSelector
  , defaultPixelBufferAttributesSelector
  , initSelector
  , initWithTagCollectionsSelector
  , newSelector
  , preferredTagCollectionsSelector
  , setDefaultOutputSettingsSelector
  , setDefaultPixelBufferAttributesSelector
  , setOutputPixelBufferAttributes_forTagCollectionSelector
  , setOutputSettings_forTagCollectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVVideoOutputSpecification avVideoOutputSpecification => avVideoOutputSpecification -> IO (Id AVVideoOutputSpecification)
init_ avVideoOutputSpecification =
  sendOwnedMessage avVideoOutputSpecification initSelector

-- | @+ new@
new :: IO (Id AVVideoOutputSpecification)
new  =
  do
    cls' <- getRequiredClass "AVVideoOutputSpecification"
    sendOwnedClassMessage cls' newSelector

-- | initWithTagCollections:
--
-- Creates an instance of AVVideoOutputSpecification initialized with the specified tag collections.
--
-- @tagCollections@ — Expects a non-empty array of CMTagCollections.  Tag collections are given priority based on their position in the array, where position i take priority over position i+1.
--
-- This method throws an exception for the following reasons:			 		- tagCollections is nil or has a count of 0.					- tagCollections contains elements that are not of the type CMTagCollection.
--
-- ObjC selector: @- initWithTagCollections:@
initWithTagCollections :: (IsAVVideoOutputSpecification avVideoOutputSpecification, IsNSArray tagCollections) => avVideoOutputSpecification -> tagCollections -> IO (Id AVVideoOutputSpecification)
initWithTagCollections avVideoOutputSpecification tagCollections =
  sendOwnedMessage avVideoOutputSpecification initWithTagCollectionsSelector (toNSArray tagCollections)

-- | setOutputPixelBufferAttributes:forTagCollection:
--
-- Specifies a mapping between a tag collection and a set of pixel buffer attributes.
--
-- @pixelBufferAttributes@ — The client requirements for CVPixelBuffers related to the tags in tagCollection, expressed using the constants in <CoreVideo/CVPixelBuffer.h>.
--
-- @tagCollection@ — A single tag collection for which these pixel buffer attributes should map to.
--
-- If this method is called twice on the same tag collection, the first requested pixel buffer attributes will be overridden.
--
-- Note: Pixel buffer attributes are translated into output settings, therefore, the rules of @-setOutputSettings:forTagCollection@ apply to this method as well. 					Namely, if you set pixel buffer attributes for a tag collection and then output settings for that same tag collection, your pixel buffer attributes will be overridden and vice-versa.
--
-- ObjC selector: @- setOutputPixelBufferAttributes:forTagCollection:@
setOutputPixelBufferAttributes_forTagCollection :: (IsAVVideoOutputSpecification avVideoOutputSpecification, IsNSDictionary pixelBufferAttributes) => avVideoOutputSpecification -> pixelBufferAttributes -> RawId -> IO ()
setOutputPixelBufferAttributes_forTagCollection avVideoOutputSpecification pixelBufferAttributes tagCollection =
  sendMessage avVideoOutputSpecification setOutputPixelBufferAttributes_forTagCollectionSelector (toNSDictionary pixelBufferAttributes) tagCollection

-- | setOutputSettings:forTagCollection
--
-- Specifies a mapping between a tag collection and a set of output settings.
--
-- @outputSettings@ — The client requirements for output CVPixelBuffers related to the tags in tagCollection, expressed using the constants in AVVideoSettings.h. 					For uncompressed video output, start with kCVPixelBuffer* keys in <CoreVideo/CVPixelBuffer.h>. 					In addition to the keys in CVPixelBuffer.h, uncompressed video settings dictionaries may also contain the following keys: 						- AVVideoAllowWideColorKey
--
-- @tagCollection@ — A single tag collection for which these output settings should map to.
--
-- If this method is called twice on the same tag collection, the first requested output settings will be overridden.
--
-- Note: This method throws an exception for any of the following reasons: 						- The settings will yield compressed output 						- The settings do not honor the requirements list above for outputSettings. 						- tagCollection does not match with any tag collection in -preferredTagCollections.
--
-- ObjC selector: @- setOutputSettings:forTagCollection:@
setOutputSettings_forTagCollection :: (IsAVVideoOutputSpecification avVideoOutputSpecification, IsNSDictionary outputSettings) => avVideoOutputSpecification -> outputSettings -> RawId -> IO ()
setOutputSettings_forTagCollection avVideoOutputSpecification outputSettings tagCollection =
  sendMessage avVideoOutputSpecification setOutputSettings_forTagCollectionSelector (toNSDictionary outputSettings) tagCollection

-- | preferredTagCollections
--
-- Tag collections held by AVVideoOutputSpecification.
--
-- Returns an array of CMTagCollections.
--
-- ObjC selector: @- preferredTagCollections@
preferredTagCollections :: IsAVVideoOutputSpecification avVideoOutputSpecification => avVideoOutputSpecification -> IO (Id NSArray)
preferredTagCollections avVideoOutputSpecification =
  sendMessage avVideoOutputSpecification preferredTagCollectionsSelector

-- | defaultPixelBufferAttributes
--
-- The default client requirements for CVPixelBuffers related to all tag collections not explicitly set with setOutputPixelBufferAttributes:forTagCollection:, expressed using the constants in <CoreVideo/CVPixelBuffer.h>.
--
-- NSDictionary where keys are of type NSString, values should match the type specified by the corresponding keys documentation in <CoreVideo/CVPixelBuffer.h>
--
-- Note: Pixel buffer attributes are translated into output settings, therefore, the rules of defaultOutputSettings apply to defaultPixelBufferAttributes as well.  If defaultPixelBufferAttributes are set after setting defaultOutputSettings, the set output settings will be overridden and vice-versa.
--
-- ObjC selector: @- defaultPixelBufferAttributes@
defaultPixelBufferAttributes :: IsAVVideoOutputSpecification avVideoOutputSpecification => avVideoOutputSpecification -> IO (Id NSDictionary)
defaultPixelBufferAttributes avVideoOutputSpecification =
  sendMessage avVideoOutputSpecification defaultPixelBufferAttributesSelector

-- | defaultPixelBufferAttributes
--
-- The default client requirements for CVPixelBuffers related to all tag collections not explicitly set with setOutputPixelBufferAttributes:forTagCollection:, expressed using the constants in <CoreVideo/CVPixelBuffer.h>.
--
-- NSDictionary where keys are of type NSString, values should match the type specified by the corresponding keys documentation in <CoreVideo/CVPixelBuffer.h>
--
-- Note: Pixel buffer attributes are translated into output settings, therefore, the rules of defaultOutputSettings apply to defaultPixelBufferAttributes as well.  If defaultPixelBufferAttributes are set after setting defaultOutputSettings, the set output settings will be overridden and vice-versa.
--
-- ObjC selector: @- setDefaultPixelBufferAttributes:@
setDefaultPixelBufferAttributes :: (IsAVVideoOutputSpecification avVideoOutputSpecification, IsNSDictionary value) => avVideoOutputSpecification -> value -> IO ()
setDefaultPixelBufferAttributes avVideoOutputSpecification value =
  sendMessage avVideoOutputSpecification setDefaultPixelBufferAttributesSelector (toNSDictionary value)

-- | defaultOutputSettings
--
-- The default client requirements for output CVPixelBuffers related to all tag collections not explicitly set with -setOutputSettings:forTagCollection, expressed using the constants in AVVideoSettings.h. 					For uncompressed video output, start with kCVPixelBuffer* keys in <CoreVideo/CVPixelBuffer.h>.					In addition to the keys in CVPixelBuffer.h, uncompressed video settings dictionaries may also contain the following keys:						- AVVideoAllowWideColorKey
--
-- NSDictionary where keys are of type NSString, values should match the type specified by the corresponding keys documentation in <AVFoundation/AVVideoSettings.h> and <CoreVideo/CVPixelBuffer.h>.
--
-- Note: The setter for this property throws an exception for any of the following reasons:						 - The settings will yield compressed output						 - The settings do not honor the requirements list above for outputSettings.
--
-- ObjC selector: @- defaultOutputSettings@
defaultOutputSettings :: IsAVVideoOutputSpecification avVideoOutputSpecification => avVideoOutputSpecification -> IO (Id NSDictionary)
defaultOutputSettings avVideoOutputSpecification =
  sendMessage avVideoOutputSpecification defaultOutputSettingsSelector

-- | defaultOutputSettings
--
-- The default client requirements for output CVPixelBuffers related to all tag collections not explicitly set with -setOutputSettings:forTagCollection, expressed using the constants in AVVideoSettings.h. 					For uncompressed video output, start with kCVPixelBuffer* keys in <CoreVideo/CVPixelBuffer.h>.					In addition to the keys in CVPixelBuffer.h, uncompressed video settings dictionaries may also contain the following keys:						- AVVideoAllowWideColorKey
--
-- NSDictionary where keys are of type NSString, values should match the type specified by the corresponding keys documentation in <AVFoundation/AVVideoSettings.h> and <CoreVideo/CVPixelBuffer.h>.
--
-- Note: The setter for this property throws an exception for any of the following reasons:						 - The settings will yield compressed output						 - The settings do not honor the requirements list above for outputSettings.
--
-- ObjC selector: @- setDefaultOutputSettings:@
setDefaultOutputSettings :: (IsAVVideoOutputSpecification avVideoOutputSpecification, IsNSDictionary value) => avVideoOutputSpecification -> value -> IO ()
setDefaultOutputSettings avVideoOutputSpecification value =
  sendMessage avVideoOutputSpecification setDefaultOutputSettingsSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVVideoOutputSpecification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVVideoOutputSpecification)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithTagCollections:@
initWithTagCollectionsSelector :: Selector '[Id NSArray] (Id AVVideoOutputSpecification)
initWithTagCollectionsSelector = mkSelector "initWithTagCollections:"

-- | @Selector@ for @setOutputPixelBufferAttributes:forTagCollection:@
setOutputPixelBufferAttributes_forTagCollectionSelector :: Selector '[Id NSDictionary, RawId] ()
setOutputPixelBufferAttributes_forTagCollectionSelector = mkSelector "setOutputPixelBufferAttributes:forTagCollection:"

-- | @Selector@ for @setOutputSettings:forTagCollection:@
setOutputSettings_forTagCollectionSelector :: Selector '[Id NSDictionary, RawId] ()
setOutputSettings_forTagCollectionSelector = mkSelector "setOutputSettings:forTagCollection:"

-- | @Selector@ for @preferredTagCollections@
preferredTagCollectionsSelector :: Selector '[] (Id NSArray)
preferredTagCollectionsSelector = mkSelector "preferredTagCollections"

-- | @Selector@ for @defaultPixelBufferAttributes@
defaultPixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
defaultPixelBufferAttributesSelector = mkSelector "defaultPixelBufferAttributes"

-- | @Selector@ for @setDefaultPixelBufferAttributes:@
setDefaultPixelBufferAttributesSelector :: Selector '[Id NSDictionary] ()
setDefaultPixelBufferAttributesSelector = mkSelector "setDefaultPixelBufferAttributes:"

-- | @Selector@ for @defaultOutputSettings@
defaultOutputSettingsSelector :: Selector '[] (Id NSDictionary)
defaultOutputSettingsSelector = mkSelector "defaultOutputSettings"

-- | @Selector@ for @setDefaultOutputSettings:@
setDefaultOutputSettingsSelector :: Selector '[Id NSDictionary] ()
setDefaultOutputSettingsSelector = mkSelector "setDefaultOutputSettings:"

