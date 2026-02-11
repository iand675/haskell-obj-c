{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddShareablePassConfiguration@.
module ObjC.PassKit.PKAddShareablePassConfiguration
  ( PKAddShareablePassConfiguration
  , IsPKAddShareablePassConfiguration(..)
  , configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completion
  , configurationForPassMetadata_primaryAction_completion
  , primaryAction
  , credentialsMetadata
  , provisioningPolicyIdentifier
  , configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector
  , configurationForPassMetadata_primaryAction_completionSelector
  , primaryActionSelector
  , credentialsMetadataSelector
  , provisioningPolicyIdentifierSelector

  -- * Enum types
  , PKAddShareablePassConfigurationPrimaryAction(PKAddShareablePassConfigurationPrimaryAction)
  , pattern PKAddShareablePassConfigurationPrimaryActionAdd
  , pattern PKAddShareablePassConfigurationPrimaryActionShare

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
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:@
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completion :: (IsNSArray passMetadata, IsNSString provisioningPolicyIdentifier) => passMetadata -> provisioningPolicyIdentifier -> PKAddShareablePassConfigurationPrimaryAction -> Ptr () -> IO ()
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completion passMetadata provisioningPolicyIdentifier action completion =
  do
    cls' <- getRequiredClass "PKAddShareablePassConfiguration"
    withObjCPtr passMetadata $ \raw_passMetadata ->
      withObjCPtr provisioningPolicyIdentifier $ \raw_provisioningPolicyIdentifier ->
        sendClassMsg cls' (mkSelector "configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:") retVoid [argPtr (castPtr raw_passMetadata :: Ptr ()), argPtr (castPtr raw_provisioningPolicyIdentifier :: Ptr ()), argCULong (coerce action), argPtr (castPtr completion :: Ptr ())]

-- | @+ configurationForPassMetadata:primaryAction:completion:@
configurationForPassMetadata_primaryAction_completion :: IsNSArray passMetadata => passMetadata -> PKAddShareablePassConfigurationPrimaryAction -> Ptr () -> IO ()
configurationForPassMetadata_primaryAction_completion passMetadata action completion =
  do
    cls' <- getRequiredClass "PKAddShareablePassConfiguration"
    withObjCPtr passMetadata $ \raw_passMetadata ->
      sendClassMsg cls' (mkSelector "configurationForPassMetadata:primaryAction:completion:") retVoid [argPtr (castPtr raw_passMetadata :: Ptr ()), argCULong (coerce action), argPtr (castPtr completion :: Ptr ())]

-- | @- primaryAction@
primaryAction :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO PKAddShareablePassConfigurationPrimaryAction
primaryAction pkAddShareablePassConfiguration  =
  fmap (coerce :: CULong -> PKAddShareablePassConfigurationPrimaryAction) $ sendMsg pkAddShareablePassConfiguration (mkSelector "primaryAction") retCULong []

-- | @- credentialsMetadata@
credentialsMetadata :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO (Id NSArray)
credentialsMetadata pkAddShareablePassConfiguration  =
  sendMsg pkAddShareablePassConfiguration (mkSelector "credentialsMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- provisioningPolicyIdentifier@
provisioningPolicyIdentifier :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO (Id NSString)
provisioningPolicyIdentifier pkAddShareablePassConfiguration  =
  sendMsg pkAddShareablePassConfiguration (mkSelector "provisioningPolicyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:@
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector :: Selector
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector = mkSelector "configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:"

-- | @Selector@ for @configurationForPassMetadata:primaryAction:completion:@
configurationForPassMetadata_primaryAction_completionSelector :: Selector
configurationForPassMetadata_primaryAction_completionSelector = mkSelector "configurationForPassMetadata:primaryAction:completion:"

-- | @Selector@ for @primaryAction@
primaryActionSelector :: Selector
primaryActionSelector = mkSelector "primaryAction"

-- | @Selector@ for @credentialsMetadata@
credentialsMetadataSelector :: Selector
credentialsMetadataSelector = mkSelector "credentialsMetadata"

-- | @Selector@ for @provisioningPolicyIdentifier@
provisioningPolicyIdentifierSelector :: Selector
provisioningPolicyIdentifierSelector = mkSelector "provisioningPolicyIdentifier"

