{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKShareablePassMetadataPreview@.
module ObjC.PassKit.PKShareablePassMetadataPreview
  ( PKShareablePassMetadataPreview
  , IsPKShareablePassMetadataPreview(..)
  , initWithTemplateIdentifier
  , previewWithTemplateIdentifier
  , ownerDisplayName
  , setOwnerDisplayName
  , provisioningTemplateIdentifier
  , initWithTemplateIdentifierSelector
  , ownerDisplayNameSelector
  , previewWithTemplateIdentifierSelector
  , provisioningTemplateIdentifierSelector
  , setOwnerDisplayNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTemplateIdentifier:@
initWithTemplateIdentifier :: (IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview, IsNSString templateIdentifier) => pkShareablePassMetadataPreview -> templateIdentifier -> IO (Id PKShareablePassMetadataPreview)
initWithTemplateIdentifier pkShareablePassMetadataPreview templateIdentifier =
  sendOwnedMessage pkShareablePassMetadataPreview initWithTemplateIdentifierSelector (toNSString templateIdentifier)

-- | @+ previewWithTemplateIdentifier:@
previewWithTemplateIdentifier :: IsNSString templateIdentifier => templateIdentifier -> IO (Id PKShareablePassMetadataPreview)
previewWithTemplateIdentifier templateIdentifier =
  do
    cls' <- getRequiredClass "PKShareablePassMetadataPreview"
    sendClassMessage cls' previewWithTemplateIdentifierSelector (toNSString templateIdentifier)

-- | @- ownerDisplayName@
ownerDisplayName :: IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview => pkShareablePassMetadataPreview -> IO (Id NSString)
ownerDisplayName pkShareablePassMetadataPreview =
  sendMessage pkShareablePassMetadataPreview ownerDisplayNameSelector

-- | @- setOwnerDisplayName:@
setOwnerDisplayName :: (IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview, IsNSString value) => pkShareablePassMetadataPreview -> value -> IO ()
setOwnerDisplayName pkShareablePassMetadataPreview value =
  sendMessage pkShareablePassMetadataPreview setOwnerDisplayNameSelector (toNSString value)

-- | @- provisioningTemplateIdentifier@
provisioningTemplateIdentifier :: IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview => pkShareablePassMetadataPreview -> IO (Id NSString)
provisioningTemplateIdentifier pkShareablePassMetadataPreview =
  sendMessage pkShareablePassMetadataPreview provisioningTemplateIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTemplateIdentifier:@
initWithTemplateIdentifierSelector :: Selector '[Id NSString] (Id PKShareablePassMetadataPreview)
initWithTemplateIdentifierSelector = mkSelector "initWithTemplateIdentifier:"

-- | @Selector@ for @previewWithTemplateIdentifier:@
previewWithTemplateIdentifierSelector :: Selector '[Id NSString] (Id PKShareablePassMetadataPreview)
previewWithTemplateIdentifierSelector = mkSelector "previewWithTemplateIdentifier:"

-- | @Selector@ for @ownerDisplayName@
ownerDisplayNameSelector :: Selector '[] (Id NSString)
ownerDisplayNameSelector = mkSelector "ownerDisplayName"

-- | @Selector@ for @setOwnerDisplayName:@
setOwnerDisplayNameSelector :: Selector '[Id NSString] ()
setOwnerDisplayNameSelector = mkSelector "setOwnerDisplayName:"

-- | @Selector@ for @provisioningTemplateIdentifier@
provisioningTemplateIdentifierSelector :: Selector '[] (Id NSString)
provisioningTemplateIdentifierSelector = mkSelector "provisioningTemplateIdentifier"

