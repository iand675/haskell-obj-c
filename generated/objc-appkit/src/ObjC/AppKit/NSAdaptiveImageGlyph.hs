{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAdaptiveImageGlyph@.
module ObjC.AppKit.NSAdaptiveImageGlyph
  ( NSAdaptiveImageGlyph
  , IsNSAdaptiveImageGlyph(..)
  , initWithImageContent
  , initWithCoder
  , init_
  , imageContent
  , contentIdentifier
  , contentDescription
  , contentType
  , initWithImageContentSelector
  , initWithCoderSelector
  , initSelector
  , imageContentSelector
  , contentIdentifierSelector
  , contentDescriptionSelector
  , contentTypeSelector


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
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithImageContent:@
initWithImageContent :: (IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph, IsNSData imageContent) => nsAdaptiveImageGlyph -> imageContent -> IO (Id NSAdaptiveImageGlyph)
initWithImageContent nsAdaptiveImageGlyph  imageContent =
withObjCPtr imageContent $ \raw_imageContent ->
    sendMsg nsAdaptiveImageGlyph (mkSelector "initWithImageContent:") (retPtr retVoid) [argPtr (castPtr raw_imageContent :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph, IsNSCoder coder) => nsAdaptiveImageGlyph -> coder -> IO (Id NSAdaptiveImageGlyph)
initWithCoder nsAdaptiveImageGlyph  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsAdaptiveImageGlyph (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSAdaptiveImageGlyph)
init_ nsAdaptiveImageGlyph  =
  sendMsg nsAdaptiveImageGlyph (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- imageContent@
imageContent :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSData)
imageContent nsAdaptiveImageGlyph  =
  sendMsg nsAdaptiveImageGlyph (mkSelector "imageContent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentIdentifier@
contentIdentifier :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSString)
contentIdentifier nsAdaptiveImageGlyph  =
  sendMsg nsAdaptiveImageGlyph (mkSelector "contentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentDescription@
contentDescription :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSString)
contentDescription nsAdaptiveImageGlyph  =
  sendMsg nsAdaptiveImageGlyph (mkSelector "contentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ contentType@
contentType :: IO (Id UTType)
contentType  =
  do
    cls' <- getRequiredClass "NSAdaptiveImageGlyph"
    sendClassMsg cls' (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImageContent:@
initWithImageContentSelector :: Selector
initWithImageContentSelector = mkSelector "initWithImageContent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @imageContent@
imageContentSelector :: Selector
imageContentSelector = mkSelector "imageContent"

-- | @Selector@ for @contentIdentifier@
contentIdentifierSelector :: Selector
contentIdentifierSelector = mkSelector "contentIdentifier"

-- | @Selector@ for @contentDescription@
contentDescriptionSelector :: Selector
contentDescriptionSelector = mkSelector "contentDescription"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

