{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRCDTextBlock
--
-- Defines a CD-Text block, which holds the CD-Text strings				for the entire disc in one language.
--
-- Generated bindings for @DRCDTextBlock@.
module ObjC.DiscRecording.DRCDTextBlock
  ( DRCDTextBlock
  , IsDRCDTextBlock(..)
  , arrayOfCDTextBlocksFromPacks
  , cdTextBlockWithLanguage_encoding
  , initWithLanguage_encoding
  , properties
  , setProperties
  , trackDictionaries
  , setTrackDictionaries
  , objectForKey_ofTrack
  , setObject_forKey_ofTrack
  , flatten
  , language
  , encoding
  , arrayOfCDTextBlocksFromPacksSelector
  , cdTextBlockWithLanguage_encodingSelector
  , initWithLanguage_encodingSelector
  , propertiesSelector
  , setPropertiesSelector
  , trackDictionariesSelector
  , setTrackDictionariesSelector
  , objectForKey_ofTrackSelector
  , setObject_forKey_ofTrackSelector
  , flattenSelector
  , languageSelector
  , encodingSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | arrayOfCDTextBlocksFromPacks:
--
-- Parses raw CD-Text data from a disc into DRCDTextBlock objects.
--
-- This method can be used to parse any data blob containing CD-Text PACKs,				such as the result of +[DRDevice readCDText], or the data returned by the				IOKit ioctl DKIOCCDREADTOC with format=5.
--
-- The NSData should be sized to fit the exact number of PACKs.  Each PACK				occupies 18 bytes, and the 4-byte header from a READ TOC command may				optionally be included.
--
-- @packs@ — NSData containing raw CD-Text PACKs.
--
-- Returns: An autoreleased array of DRCDTextBlock objects describing the information				in the raw PACKs, or nil if the data could not be parsed.
--
-- ObjC selector: @+ arrayOfCDTextBlocksFromPacks:@
arrayOfCDTextBlocksFromPacks :: IsNSData packs => packs -> IO (Id NSArray)
arrayOfCDTextBlocksFromPacks packs =
  do
    cls' <- getRequiredClass "DRCDTextBlock"
    withObjCPtr packs $ \raw_packs ->
      sendClassMsg cls' (mkSelector "arrayOfCDTextBlocksFromPacks:") (retPtr retVoid) [argPtr (castPtr raw_packs :: Ptr ())] >>= retainedObject . castPtr

-- | cdTextBlockWithLanguage:encoding:
--
-- Creates a CD-Text block.
--
-- @lang@ — ISO 639 language code describing the language in which the strings							are provided.  CD-Text allows the concept of an unknown language,							which can be represented here by an empty string.
--
-- @enc@ — Character encoding into which the strings should be converted.
--
-- Returns: An autoreleased DRCDTextBlock object.
--
-- ObjC selector: @+ cdTextBlockWithLanguage:encoding:@
cdTextBlockWithLanguage_encoding :: IsNSString lang => lang -> CULong -> IO (Id DRCDTextBlock)
cdTextBlockWithLanguage_encoding lang enc =
  do
    cls' <- getRequiredClass "DRCDTextBlock"
    withObjCPtr lang $ \raw_lang ->
      sendClassMsg cls' (mkSelector "cdTextBlockWithLanguage:encoding:") (retPtr retVoid) [argPtr (castPtr raw_lang :: Ptr ()), argCULong (fromIntegral enc)] >>= retainedObject . castPtr

-- | initWithLanguage:encoding:
--
-- Initializes an empty CD-Text block.
--
-- @lang@ — ISO 639 language code describing the language which this block will hold.							CD-Text allows the concept of an unknown language, which can be represented							here by an empty string.
--
-- @enc@ — Character encoding into which the strings in this block will be converted.
--
-- Returns: A DRCDTextBlock object.
--
-- ObjC selector: @- initWithLanguage:encoding:@
initWithLanguage_encoding :: (IsDRCDTextBlock drcdTextBlock, IsNSString lang) => drcdTextBlock -> lang -> CULong -> IO RawId
initWithLanguage_encoding drcdTextBlock  lang enc =
withObjCPtr lang $ \raw_lang ->
    fmap (RawId . castPtr) $ sendMsg drcdTextBlock (mkSelector "initWithLanguage:encoding:") (retPtr retVoid) [argPtr (castPtr raw_lang :: Ptr ()), argCULong (fromIntegral enc)]

-- | properties
--
-- Returns the properties dictionary of the CD-Text block.
--
-- Returns: An NSDictionary containing the properties of the block.
--
-- ObjC selector: @- properties@
properties :: IsDRCDTextBlock drcdTextBlock => drcdTextBlock -> IO (Id NSDictionary)
properties drcdTextBlock  =
  sendMsg drcdTextBlock (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setProperties:
--
-- Sets the properties dictionary of the CD-Text block.
--
-- @properties@ — NSDictionary of the properties to set.
--
-- ObjC selector: @- setProperties:@
setProperties :: (IsDRCDTextBlock drcdTextBlock, IsNSDictionary properties) => drcdTextBlock -> properties -> IO ()
setProperties drcdTextBlock  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg drcdTextBlock (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_properties :: Ptr ())]

-- | trackDictionaries
--
-- Returns a copy of the array of track dictionaries for the block.
--
-- Returns: An autoreleased NSArray of CFDictionaries of CFStrings, containing the CD-Text information.
--
-- Each item in the array is a dictionary, which in turn holds key-value encoded				information about the track/disc.  Array index 0 holds information about the				disc, index 1 holds information about track 1, index 2 holds information about				track 2, etc.
--
-- ObjC selector: @- trackDictionaries@
trackDictionaries :: IsDRCDTextBlock drcdTextBlock => drcdTextBlock -> IO (Id NSArray)
trackDictionaries drcdTextBlock  =
  sendMsg drcdTextBlock (mkSelector "trackDictionaries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setTrackDictionaries:
--
-- Sets the array of track dictionaries for the block.
--
-- @tracks@ — An NSArray of NSDictionaries of NSStrings, containing the CD-Text information.
--
-- Each item in the array is a dictionary, which in turn holds key-value encoded				information about the track/disc.  Array index 0 holds information about the				disc, index 1 holds information about track 1, index 2 holds information about				track 2, etc.
--
-- Any incoming strings are automatically modified to conform to the character				set specified in the language block. Calling -trackDictionaries immediately				after -setTrackDictionaries: will provide the modified values.  These				may not be the same as the ones you passed in, but instead correspond to				what will actually be used.
--
-- ObjC selector: @- setTrackDictionaries:@
setTrackDictionaries :: (IsDRCDTextBlock drcdTextBlock, IsNSArray tracks) => drcdTextBlock -> tracks -> IO ()
setTrackDictionaries drcdTextBlock  tracks =
withObjCPtr tracks $ \raw_tracks ->
    sendMsg drcdTextBlock (mkSelector "setTrackDictionaries:") retVoid [argPtr (castPtr raw_tracks :: Ptr ())]

-- | objectForKey:ofTrack:
--
-- Returns a single value from the block.
--
-- @key@ — Key to get the value of.
--
-- @trackIndex@ — One-based index of the track to query, or 0 to query the disc.
--
-- Returns: Autoreleased NSObject for the key, or nil if not present.
--
-- ObjC selector: @- objectForKey:ofTrack:@
objectForKey_ofTrack :: (IsDRCDTextBlock drcdTextBlock, IsNSString key) => drcdTextBlock -> key -> CULong -> IO RawId
objectForKey_ofTrack drcdTextBlock  key trackIndex =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg drcdTextBlock (mkSelector "objectForKey:ofTrack:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (fromIntegral trackIndex)]

-- | setObject:forKey:ofTrack:
--
-- Changes a single string in the block.
--
-- @value@ — Value - an NSString, NSData, or NSNumber as appropriate.
--
-- @key@ — Key to assign.
--
-- @trackIndex@ — One-based index of the track to modify, or 0 to modify the disc.
--
-- ObjC selector: @- setObject:forKey:ofTrack:@
setObject_forKey_ofTrack :: (IsDRCDTextBlock drcdTextBlock, IsNSString key) => drcdTextBlock -> RawId -> key -> CULong -> IO ()
setObject_forKey_ofTrack drcdTextBlock  value key trackIndex =
withObjCPtr key $ \raw_key ->
    sendMsg drcdTextBlock (mkSelector "setObject:forKey:ofTrack:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argCULong (fromIntegral trackIndex)]

-- | flatten
--
-- Flattens the CD-Text block to determine whether any information will be truncated.
--
-- When burning your CD-Text information to a CD, DiscRecording will automatically				truncate some of the information you've specified if it does not fit.
--
-- The size limit for CD-Text is approximately 3K of strings per block.  This limit				is only approximate because some of this space is taken up as overhead, and				duplicate strings can sometimes be combined.  The only way to tell for sure				how big your CD-Text block is going to be is to ask DiscRecording to try				flattening it.  You can use this function to determine whether truncation				will be needed.
--
-- Some clients will want to accept DiscRecording's truncation since it preserves				the most important information and provides the simplest user experience.  If				you do not wish to use DiscRecording's automatic truncation, it is your				responsibility to make sure that you specify a CD-Text block that will fit.
--
-- Following is a simple algorithm to avoid having your CD-Text data truncated:								Call -[myCDTextBlock flatten].				If the result is 0, no truncation is necessary. Stop.				Otherwise, truncation will occur -- edit or remove some data.				Repeat.
--
-- Returns: The number of bytes that will be truncated from the CD-Text block.  If this				method returns 0, no truncation will occur.
--
-- ObjC selector: @- flatten@
flatten :: IsDRCDTextBlock drcdTextBlock => drcdTextBlock -> IO CULong
flatten drcdTextBlock  =
  sendMsg drcdTextBlock (mkSelector "flatten") retCULong []

-- | language
--
-- Returns the ISO 639 language code describing the language associated with the				CD-Text block.  CD-Text allows the concept of an unknown language, which is				represented here by an empty string.
--
-- Returns: A DRCDTextLanguage.
--
-- ObjC selector: @- language@
language :: IsDRCDTextBlock drcdTextBlock => drcdTextBlock -> IO (Id NSString)
language drcdTextBlock  =
  sendMsg drcdTextBlock (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | encoding
--
-- Returns the string encoding associated with the CD-Text block.
--
-- Returns: A NSStringEncoding.
--
-- ObjC selector: @- encoding@
encoding :: IsDRCDTextBlock drcdTextBlock => drcdTextBlock -> IO CULong
encoding drcdTextBlock  =
  sendMsg drcdTextBlock (mkSelector "encoding") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arrayOfCDTextBlocksFromPacks:@
arrayOfCDTextBlocksFromPacksSelector :: Selector
arrayOfCDTextBlocksFromPacksSelector = mkSelector "arrayOfCDTextBlocksFromPacks:"

-- | @Selector@ for @cdTextBlockWithLanguage:encoding:@
cdTextBlockWithLanguage_encodingSelector :: Selector
cdTextBlockWithLanguage_encodingSelector = mkSelector "cdTextBlockWithLanguage:encoding:"

-- | @Selector@ for @initWithLanguage:encoding:@
initWithLanguage_encodingSelector :: Selector
initWithLanguage_encodingSelector = mkSelector "initWithLanguage:encoding:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @trackDictionaries@
trackDictionariesSelector :: Selector
trackDictionariesSelector = mkSelector "trackDictionaries"

-- | @Selector@ for @setTrackDictionaries:@
setTrackDictionariesSelector :: Selector
setTrackDictionariesSelector = mkSelector "setTrackDictionaries:"

-- | @Selector@ for @objectForKey:ofTrack:@
objectForKey_ofTrackSelector :: Selector
objectForKey_ofTrackSelector = mkSelector "objectForKey:ofTrack:"

-- | @Selector@ for @setObject:forKey:ofTrack:@
setObject_forKey_ofTrackSelector :: Selector
setObject_forKey_ofTrackSelector = mkSelector "setObject:forKey:ofTrack:"

-- | @Selector@ for @flatten@
flattenSelector :: Selector
flattenSelector = mkSelector "flatten"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @encoding@
encodingSelector :: Selector
encodingSelector = mkSelector "encoding"

