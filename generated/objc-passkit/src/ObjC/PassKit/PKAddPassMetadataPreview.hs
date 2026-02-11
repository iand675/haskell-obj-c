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
  , initWithPassThumbnail_localizedDescriptionSelector
  , previewWithPassThumbnail_localizedDescriptionSelector
  , initSelector
  , newSelector
  , passThumbnailImageSelector
  , localizedDescriptionSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializer preview object to represent the pass being added to Wallet.which requires a CGImage of the pass's card art and a localized description. - Properties:   - passThumbnail: CGImage representing the card artwork of the pass to be presented during provisioning.   - localizedDescription: Localized description of the pass.
--
-- ObjC selector: @- initWithPassThumbnail:localizedDescription:@
initWithPassThumbnail_localizedDescription :: (IsPKAddPassMetadataPreview pkAddPassMetadataPreview, IsNSString description) => pkAddPassMetadataPreview -> Ptr () -> description -> IO (Id PKAddPassMetadataPreview)
initWithPassThumbnail_localizedDescription pkAddPassMetadataPreview  passThumbnail description =
withObjCPtr description $ \raw_description ->
    sendMsg pkAddPassMetadataPreview (mkSelector "initWithPassThumbnail:localizedDescription:") (retPtr retVoid) [argPtr passThumbnail, argPtr (castPtr raw_description :: Ptr ())] >>= ownedObject . castPtr

-- | @+ previewWithPassThumbnail:localizedDescription:@
previewWithPassThumbnail_localizedDescription :: IsNSString description => Ptr () -> description -> IO (Id PKAddPassMetadataPreview)
previewWithPassThumbnail_localizedDescription passThumbnail description =
  do
    cls' <- getRequiredClass "PKAddPassMetadataPreview"
    withObjCPtr description $ \raw_description ->
      sendClassMsg cls' (mkSelector "previewWithPassThumbnail:localizedDescription:") (retPtr retVoid) [argPtr passThumbnail, argPtr (castPtr raw_description :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Id PKAddPassMetadataPreview)
init_ pkAddPassMetadataPreview  =
  sendMsg pkAddPassMetadataPreview (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKAddPassMetadataPreview)
new  =
  do
    cls' <- getRequiredClass "PKAddPassMetadataPreview"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | CGImage representing the pass in our provisioning UI.
--
-- ObjC selector: @- passThumbnailImage@
passThumbnailImage :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Ptr ())
passThumbnailImage pkAddPassMetadataPreview  =
  fmap castPtr $ sendMsg pkAddPassMetadataPreview (mkSelector "passThumbnailImage") (retPtr retVoid) []

-- | Localized description of the pass to be referenced during provisioning.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsPKAddPassMetadataPreview pkAddPassMetadataPreview => pkAddPassMetadataPreview -> IO (Id NSString)
localizedDescription pkAddPassMetadataPreview  =
  sendMsg pkAddPassMetadataPreview (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPassThumbnail:localizedDescription:@
initWithPassThumbnail_localizedDescriptionSelector :: Selector
initWithPassThumbnail_localizedDescriptionSelector = mkSelector "initWithPassThumbnail:localizedDescription:"

-- | @Selector@ for @previewWithPassThumbnail:localizedDescription:@
previewWithPassThumbnail_localizedDescriptionSelector :: Selector
previewWithPassThumbnail_localizedDescriptionSelector = mkSelector "previewWithPassThumbnail:localizedDescription:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @passThumbnailImage@
passThumbnailImageSelector :: Selector
passThumbnailImageSelector = mkSelector "passThumbnailImage"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

