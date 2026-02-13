{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddPassMetadataPreview@.
module ObjC.PassKit.PKAddPassMetadataPreview
  ( PKAddPassMetadataPreview
  , IsPKAddPassMetadataPreview(..)
  , initWithPassThumbnail_localizedDescription
  , previewWithPassThumbnail_localizedDescription
  , init_
  , new
  , passThumbnailImage
  , localizedDescription
  , initSelector
  , initWithPassThumbnail_localizedDescriptionSelector
  , localizedDescriptionSelector
  , newSelector
  , passThumbnailImageSelector
  , previewWithPassThumbnail_localizedDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializer preview object to represent the pass being added to Wallet.which requires a CGImage of the pass's card art and a localized description. - Properties:   - passThumbnail: CGImage representing the card artwork of the pass to be presented during provisioning.   - localizedDescription: Localized description of the pass.
--
-- ObjC selector: @- initWithPassThumbnail:localizedDescription:@
initWithPassThumbnail_localizedDescription :: (IsPKAddPassMetadataPreview pkAddPassMetadataPreview, IsNSString description) => pkAddPassMetadataPreview -> Ptr () -> description -> IO (Id PKAddPassMetadataPreview)
initWithPassThumbnail_localizedDescription pkAddPassMetadataPreview passThumbnail description =
  sendOwnedMessage pkAddPassMetadataPreview initWithPassThumbnail_localizedDescriptionSelector passThumbnail (toNSString description)

-- | @+ previewWithPassThumbnail:localizedDescription:@
previewWithPassThumbnail_localizedDescription :: IsNSString description => Ptr () -> description -> IO (Id PKAddPassMetadataPreview)
previewWithPassThumbnail_localizedDescription passThumbnail description =
  do
    cls' <- getRequiredClass "PKAddPassMetadataPreview"
    sendClassMessage cls' previewWithPassThumbnail_localizedDescriptionSelector passThumbnail (toNSString description)

-- | @- init@
init_ :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Id PKAddPassMetadataPreview)
init_ pkAddPassMetadataPreview =
  sendOwnedMessage pkAddPassMetadataPreview initSelector

-- | @+ new@
new :: IO (Id PKAddPassMetadataPreview)
new  =
  do
    cls' <- getRequiredClass "PKAddPassMetadataPreview"
    sendOwnedClassMessage cls' newSelector

-- | CGImage representing the pass in our provisioning UI.
--
-- ObjC selector: @- passThumbnailImage@
passThumbnailImage :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Ptr ())
passThumbnailImage pkAddPassMetadataPreview =
  sendMessage pkAddPassMetadataPreview passThumbnailImageSelector

-- | Localized description of the pass to be referenced during provisioning.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Id NSString)
localizedDescription pkAddPassMetadataPreview =
  sendMessage pkAddPassMetadataPreview localizedDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPassThumbnail:localizedDescription:@
initWithPassThumbnail_localizedDescriptionSelector :: Selector '[Ptr (), Id NSString] (Id PKAddPassMetadataPreview)
initWithPassThumbnail_localizedDescriptionSelector = mkSelector "initWithPassThumbnail:localizedDescription:"

-- | @Selector@ for @previewWithPassThumbnail:localizedDescription:@
previewWithPassThumbnail_localizedDescriptionSelector :: Selector '[Ptr (), Id NSString] (Id PKAddPassMetadataPreview)
previewWithPassThumbnail_localizedDescriptionSelector = mkSelector "previewWithPassThumbnail:localizedDescription:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKAddPassMetadataPreview)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKAddPassMetadataPreview)
newSelector = mkSelector "new"

-- | @Selector@ for @passThumbnailImage@
passThumbnailImageSelector :: Selector '[] (Ptr ())
passThumbnailImageSelector = mkSelector "passThumbnailImage"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

