{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGlyphInfo@.
module ObjC.AppKit.NSGlyphInfo
  ( NSGlyphInfo
  , IsNSGlyphInfo(..)
  , glyphInfoWithCGGlyph_forFont_baseString
  , glyphInfoWithGlyphName_forFont_baseString
  , glyphInfoWithGlyph_forFont_baseString
  , glyphInfoWithCharacterIdentifier_collection_baseString
  , glyphID
  , baseString
  , glyphName
  , characterIdentifier
  , characterCollection
  , glyphInfoWithCGGlyph_forFont_baseStringSelector
  , glyphInfoWithGlyphName_forFont_baseStringSelector
  , glyphInfoWithGlyph_forFont_baseStringSelector
  , glyphInfoWithCharacterIdentifier_collection_baseStringSelector
  , glyphIDSelector
  , baseStringSelector
  , glyphNameSelector
  , characterIdentifierSelector
  , characterCollectionSelector

  -- * Enum types
  , NSCharacterCollection(NSCharacterCollection)
  , pattern NSIdentityMappingCharacterCollection
  , pattern NSAdobeCNS1CharacterCollection
  , pattern NSAdobeGB1CharacterCollection
  , pattern NSAdobeJapan1CharacterCollection
  , pattern NSAdobeJapan2CharacterCollection
  , pattern NSAdobeKorea1CharacterCollection

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

-- | @+ glyphInfoWithCGGlyph:forFont:baseString:@
glyphInfoWithCGGlyph_forFont_baseString :: (IsNSFont font, IsNSString string) => CUShort -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithCGGlyph_forFont_baseString glyph font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    withObjCPtr font $ \raw_font ->
      withObjCPtr string $ \raw_string ->
        sendClassMsg cls' (mkSelector "glyphInfoWithCGGlyph:forFont:baseString:") (retPtr retVoid) [argCUInt (fromIntegral glyph), argPtr (castPtr raw_font :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ glyphInfoWithGlyphName:forFont:baseString:@
glyphInfoWithGlyphName_forFont_baseString :: (IsNSString glyphName, IsNSFont font, IsNSString string) => glyphName -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithGlyphName_forFont_baseString glyphName font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    withObjCPtr glyphName $ \raw_glyphName ->
      withObjCPtr font $ \raw_font ->
        withObjCPtr string $ \raw_string ->
          sendClassMsg cls' (mkSelector "glyphInfoWithGlyphName:forFont:baseString:") (retPtr retVoid) [argPtr (castPtr raw_glyphName :: Ptr ()), argPtr (castPtr raw_font :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ glyphInfoWithGlyph:forFont:baseString:@
glyphInfoWithGlyph_forFont_baseString :: (IsNSFont font, IsNSString string) => CUInt -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithGlyph_forFont_baseString glyph font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    withObjCPtr font $ \raw_font ->
      withObjCPtr string $ \raw_string ->
        sendClassMsg cls' (mkSelector "glyphInfoWithGlyph:forFont:baseString:") (retPtr retVoid) [argCUInt (fromIntegral glyph), argPtr (castPtr raw_font :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ glyphInfoWithCharacterIdentifier:collection:baseString:@
glyphInfoWithCharacterIdentifier_collection_baseString :: IsNSString string => CULong -> NSCharacterCollection -> string -> IO (Id NSGlyphInfo)
glyphInfoWithCharacterIdentifier_collection_baseString cid characterCollection string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "glyphInfoWithCharacterIdentifier:collection:baseString:") (retPtr retVoid) [argCULong (fromIntegral cid), argCULong (coerce characterCollection), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- glyphID@
glyphID :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO CUShort
glyphID nsGlyphInfo  =
  fmap fromIntegral $ sendMsg nsGlyphInfo (mkSelector "glyphID") retCUInt []

-- | @- baseString@
baseString :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO (Id NSString)
baseString nsGlyphInfo  =
  sendMsg nsGlyphInfo (mkSelector "baseString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- glyphName@
glyphName :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO (Id NSString)
glyphName nsGlyphInfo  =
  sendMsg nsGlyphInfo (mkSelector "glyphName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- characterIdentifier@
characterIdentifier :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO CULong
characterIdentifier nsGlyphInfo  =
  sendMsg nsGlyphInfo (mkSelector "characterIdentifier") retCULong []

-- | @- characterCollection@
characterCollection :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO NSCharacterCollection
characterCollection nsGlyphInfo  =
  fmap (coerce :: CULong -> NSCharacterCollection) $ sendMsg nsGlyphInfo (mkSelector "characterCollection") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @glyphInfoWithCGGlyph:forFont:baseString:@
glyphInfoWithCGGlyph_forFont_baseStringSelector :: Selector
glyphInfoWithCGGlyph_forFont_baseStringSelector = mkSelector "glyphInfoWithCGGlyph:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithGlyphName:forFont:baseString:@
glyphInfoWithGlyphName_forFont_baseStringSelector :: Selector
glyphInfoWithGlyphName_forFont_baseStringSelector = mkSelector "glyphInfoWithGlyphName:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithGlyph:forFont:baseString:@
glyphInfoWithGlyph_forFont_baseStringSelector :: Selector
glyphInfoWithGlyph_forFont_baseStringSelector = mkSelector "glyphInfoWithGlyph:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithCharacterIdentifier:collection:baseString:@
glyphInfoWithCharacterIdentifier_collection_baseStringSelector :: Selector
glyphInfoWithCharacterIdentifier_collection_baseStringSelector = mkSelector "glyphInfoWithCharacterIdentifier:collection:baseString:"

-- | @Selector@ for @glyphID@
glyphIDSelector :: Selector
glyphIDSelector = mkSelector "glyphID"

-- | @Selector@ for @baseString@
baseStringSelector :: Selector
baseStringSelector = mkSelector "baseString"

-- | @Selector@ for @glyphName@
glyphNameSelector :: Selector
glyphNameSelector = mkSelector "glyphName"

-- | @Selector@ for @characterIdentifier@
characterIdentifierSelector :: Selector
characterIdentifierSelector = mkSelector "characterIdentifier"

-- | @Selector@ for @characterCollection@
characterCollectionSelector :: Selector
characterCollectionSelector = mkSelector "characterCollection"

