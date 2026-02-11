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
  , previewWithTemplateIdentifierSelector
  , ownerDisplayNameSelector
  , setOwnerDisplayNameSelector
  , provisioningTemplateIdentifierSelector


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

-- | @- initWithTemplateIdentifier:@
initWithTemplateIdentifier :: (IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview, IsNSString templateIdentifier) => pkShareablePassMetadataPreview -> templateIdentifier -> IO (Id PKShareablePassMetadataPreview)
initWithTemplateIdentifier pkShareablePassMetadataPreview  templateIdentifier =
withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
    sendMsg pkShareablePassMetadataPreview (mkSelector "initWithTemplateIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_templateIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @+ previewWithTemplateIdentifier:@
previewWithTemplateIdentifier :: IsNSString templateIdentifier => templateIdentifier -> IO (Id PKShareablePassMetadataPreview)
previewWithTemplateIdentifier templateIdentifier =
  do
    cls' <- getRequiredClass "PKShareablePassMetadataPreview"
    withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
      sendClassMsg cls' (mkSelector "previewWithTemplateIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_templateIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- ownerDisplayName@
ownerDisplayName :: IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview => pkShareablePassMetadataPreview -> IO (Id NSString)
ownerDisplayName pkShareablePassMetadataPreview  =
  sendMsg pkShareablePassMetadataPreview (mkSelector "ownerDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOwnerDisplayName:@
setOwnerDisplayName :: (IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview, IsNSString value) => pkShareablePassMetadataPreview -> value -> IO ()
setOwnerDisplayName pkShareablePassMetadataPreview  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkShareablePassMetadataPreview (mkSelector "setOwnerDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- provisioningTemplateIdentifier@
provisioningTemplateIdentifier :: IsPKShareablePassMetadataPreview pkShareablePassMetadataPreview => pkShareablePassMetadataPreview -> IO (Id NSString)
provisioningTemplateIdentifier pkShareablePassMetadataPreview  =
  sendMsg pkShareablePassMetadataPreview (mkSelector "provisioningTemplateIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTemplateIdentifier:@
initWithTemplateIdentifierSelector :: Selector
initWithTemplateIdentifierSelector = mkSelector "initWithTemplateIdentifier:"

-- | @Selector@ for @previewWithTemplateIdentifier:@
previewWithTemplateIdentifierSelector :: Selector
previewWithTemplateIdentifierSelector = mkSelector "previewWithTemplateIdentifier:"

-- | @Selector@ for @ownerDisplayName@
ownerDisplayNameSelector :: Selector
ownerDisplayNameSelector = mkSelector "ownerDisplayName"

-- | @Selector@ for @setOwnerDisplayName:@
setOwnerDisplayNameSelector :: Selector
setOwnerDisplayNameSelector = mkSelector "setOwnerDisplayName:"

-- | @Selector@ for @provisioningTemplateIdentifier@
provisioningTemplateIdentifierSelector :: Selector
provisioningTemplateIdentifierSelector = mkSelector "provisioningTemplateIdentifier"

