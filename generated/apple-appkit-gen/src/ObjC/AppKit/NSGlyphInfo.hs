{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , baseStringSelector
  , characterCollectionSelector
  , characterIdentifierSelector
  , glyphIDSelector
  , glyphInfoWithCGGlyph_forFont_baseStringSelector
  , glyphInfoWithCharacterIdentifier_collection_baseStringSelector
  , glyphInfoWithGlyphName_forFont_baseStringSelector
  , glyphInfoWithGlyph_forFont_baseStringSelector
  , glyphNameSelector

  -- * Enum types
  , NSCharacterCollection(NSCharacterCollection)
  , pattern NSIdentityMappingCharacterCollection
  , pattern NSAdobeCNS1CharacterCollection
  , pattern NSAdobeGB1CharacterCollection
  , pattern NSAdobeJapan1CharacterCollection
  , pattern NSAdobeJapan2CharacterCollection
  , pattern NSAdobeKorea1CharacterCollection

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

-- | @+ glyphInfoWithCGGlyph:forFont:baseString:@
glyphInfoWithCGGlyph_forFont_baseString :: (IsNSFont font, IsNSString string) => CUShort -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithCGGlyph_forFont_baseString glyph font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    sendClassMessage cls' glyphInfoWithCGGlyph_forFont_baseStringSelector glyph (toNSFont font) (toNSString string)

-- | @+ glyphInfoWithGlyphName:forFont:baseString:@
glyphInfoWithGlyphName_forFont_baseString :: (IsNSString glyphName, IsNSFont font, IsNSString string) => glyphName -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithGlyphName_forFont_baseString glyphName font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    sendClassMessage cls' glyphInfoWithGlyphName_forFont_baseStringSelector (toNSString glyphName) (toNSFont font) (toNSString string)

-- | @+ glyphInfoWithGlyph:forFont:baseString:@
glyphInfoWithGlyph_forFont_baseString :: (IsNSFont font, IsNSString string) => CUInt -> font -> string -> IO (Id NSGlyphInfo)
glyphInfoWithGlyph_forFont_baseString glyph font string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    sendClassMessage cls' glyphInfoWithGlyph_forFont_baseStringSelector glyph (toNSFont font) (toNSString string)

-- | @+ glyphInfoWithCharacterIdentifier:collection:baseString:@
glyphInfoWithCharacterIdentifier_collection_baseString :: IsNSString string => CULong -> NSCharacterCollection -> string -> IO (Id NSGlyphInfo)
glyphInfoWithCharacterIdentifier_collection_baseString cid characterCollection string =
  do
    cls' <- getRequiredClass "NSGlyphInfo"
    sendClassMessage cls' glyphInfoWithCharacterIdentifier_collection_baseStringSelector cid characterCollection (toNSString string)

-- | @- glyphID@
glyphID :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO CUShort
glyphID nsGlyphInfo =
  sendMessage nsGlyphInfo glyphIDSelector

-- | @- baseString@
baseString :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO (Id NSString)
baseString nsGlyphInfo =
  sendMessage nsGlyphInfo baseStringSelector

-- | @- glyphName@
glyphName :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO (Id NSString)
glyphName nsGlyphInfo =
  sendMessage nsGlyphInfo glyphNameSelector

-- | @- characterIdentifier@
characterIdentifier :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO CULong
characterIdentifier nsGlyphInfo =
  sendMessage nsGlyphInfo characterIdentifierSelector

-- | @- characterCollection@
characterCollection :: IsNSGlyphInfo nsGlyphInfo => nsGlyphInfo -> IO NSCharacterCollection
characterCollection nsGlyphInfo =
  sendMessage nsGlyphInfo characterCollectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @glyphInfoWithCGGlyph:forFont:baseString:@
glyphInfoWithCGGlyph_forFont_baseStringSelector :: Selector '[CUShort, Id NSFont, Id NSString] (Id NSGlyphInfo)
glyphInfoWithCGGlyph_forFont_baseStringSelector = mkSelector "glyphInfoWithCGGlyph:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithGlyphName:forFont:baseString:@
glyphInfoWithGlyphName_forFont_baseStringSelector :: Selector '[Id NSString, Id NSFont, Id NSString] (Id NSGlyphInfo)
glyphInfoWithGlyphName_forFont_baseStringSelector = mkSelector "glyphInfoWithGlyphName:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithGlyph:forFont:baseString:@
glyphInfoWithGlyph_forFont_baseStringSelector :: Selector '[CUInt, Id NSFont, Id NSString] (Id NSGlyphInfo)
glyphInfoWithGlyph_forFont_baseStringSelector = mkSelector "glyphInfoWithGlyph:forFont:baseString:"

-- | @Selector@ for @glyphInfoWithCharacterIdentifier:collection:baseString:@
glyphInfoWithCharacterIdentifier_collection_baseStringSelector :: Selector '[CULong, NSCharacterCollection, Id NSString] (Id NSGlyphInfo)
glyphInfoWithCharacterIdentifier_collection_baseStringSelector = mkSelector "glyphInfoWithCharacterIdentifier:collection:baseString:"

-- | @Selector@ for @glyphID@
glyphIDSelector :: Selector '[] CUShort
glyphIDSelector = mkSelector "glyphID"

-- | @Selector@ for @baseString@
baseStringSelector :: Selector '[] (Id NSString)
baseStringSelector = mkSelector "baseString"

-- | @Selector@ for @glyphName@
glyphNameSelector :: Selector '[] (Id NSString)
glyphNameSelector = mkSelector "glyphName"

-- | @Selector@ for @characterIdentifier@
characterIdentifierSelector :: Selector '[] CULong
characterIdentifierSelector = mkSelector "characterIdentifier"

-- | @Selector@ for @characterCollection@
characterCollectionSelector :: Selector '[] NSCharacterCollection
characterCollectionSelector = mkSelector "characterCollection"

