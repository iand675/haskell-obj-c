{-# LANGUAGE DataKinds #-}
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
  , contentDescriptionSelector
  , contentIdentifierSelector
  , contentTypeSelector
  , imageContentSelector
  , initSelector
  , initWithCoderSelector
  , initWithImageContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithImageContent:@
initWithImageContent :: (IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph, IsNSData imageContent) => nsAdaptiveImageGlyph -> imageContent -> IO (Id NSAdaptiveImageGlyph)
initWithImageContent nsAdaptiveImageGlyph imageContent =
  sendOwnedMessage nsAdaptiveImageGlyph initWithImageContentSelector (toNSData imageContent)

-- | @- initWithCoder:@
initWithCoder :: (IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph, IsNSCoder coder) => nsAdaptiveImageGlyph -> coder -> IO (Id NSAdaptiveImageGlyph)
initWithCoder nsAdaptiveImageGlyph coder =
  sendOwnedMessage nsAdaptiveImageGlyph initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSAdaptiveImageGlyph)
init_ nsAdaptiveImageGlyph =
  sendOwnedMessage nsAdaptiveImageGlyph initSelector

-- | @- imageContent@
imageContent :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSData)
imageContent nsAdaptiveImageGlyph =
  sendMessage nsAdaptiveImageGlyph imageContentSelector

-- | @- contentIdentifier@
contentIdentifier :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSString)
contentIdentifier nsAdaptiveImageGlyph =
  sendMessage nsAdaptiveImageGlyph contentIdentifierSelector

-- | @- contentDescription@
contentDescription :: IsNSAdaptiveImageGlyph nsAdaptiveImageGlyph => nsAdaptiveImageGlyph -> IO (Id NSString)
contentDescription nsAdaptiveImageGlyph =
  sendMessage nsAdaptiveImageGlyph contentDescriptionSelector

-- | @+ contentType@
contentType :: IO (Id UTType)
contentType  =
  do
    cls' <- getRequiredClass "NSAdaptiveImageGlyph"
    sendClassMessage cls' contentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImageContent:@
initWithImageContentSelector :: Selector '[Id NSData] (Id NSAdaptiveImageGlyph)
initWithImageContentSelector = mkSelector "initWithImageContent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSAdaptiveImageGlyph)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSAdaptiveImageGlyph)
initSelector = mkSelector "init"

-- | @Selector@ for @imageContent@
imageContentSelector :: Selector '[] (Id NSData)
imageContentSelector = mkSelector "imageContent"

-- | @Selector@ for @contentIdentifier@
contentIdentifierSelector :: Selector '[] (Id NSString)
contentIdentifierSelector = mkSelector "contentIdentifier"

-- | @Selector@ for @contentDescription@
contentDescriptionSelector :: Selector '[] (Id NSString)
contentDescriptionSelector = mkSelector "contentDescription"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

