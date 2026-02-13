{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , configurationForPassMetadata_primaryAction_completionSelector
  , configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector
  , credentialsMetadataSelector
  , primaryActionSelector
  , provisioningPolicyIdentifierSelector

  -- * Enum types
  , PKAddShareablePassConfigurationPrimaryAction(PKAddShareablePassConfigurationPrimaryAction)
  , pattern PKAddShareablePassConfigurationPrimaryActionAdd
  , pattern PKAddShareablePassConfigurationPrimaryActionShare

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector (toNSArray passMetadata) (toNSString provisioningPolicyIdentifier) action completion

-- | @+ configurationForPassMetadata:primaryAction:completion:@
configurationForPassMetadata_primaryAction_completion :: IsNSArray passMetadata => passMetadata -> PKAddShareablePassConfigurationPrimaryAction -> Ptr () -> IO ()
configurationForPassMetadata_primaryAction_completion passMetadata action completion =
  do
    cls' <- getRequiredClass "PKAddShareablePassConfiguration"
    sendClassMessage cls' configurationForPassMetadata_primaryAction_completionSelector (toNSArray passMetadata) action completion

-- | @- primaryAction@
primaryAction :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO PKAddShareablePassConfigurationPrimaryAction
primaryAction pkAddShareablePassConfiguration =
  sendMessage pkAddShareablePassConfiguration primaryActionSelector

-- | @- credentialsMetadata@
credentialsMetadata :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO (Id NSArray)
credentialsMetadata pkAddShareablePassConfiguration =
  sendMessage pkAddShareablePassConfiguration credentialsMetadataSelector

-- | @- provisioningPolicyIdentifier@
provisioningPolicyIdentifier :: IsPKAddShareablePassConfiguration pkAddShareablePassConfiguration => pkAddShareablePassConfiguration -> IO (Id NSString)
provisioningPolicyIdentifier pkAddShareablePassConfiguration =
  sendMessage pkAddShareablePassConfiguration provisioningPolicyIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:@
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector :: Selector '[Id NSArray, Id NSString, PKAddShareablePassConfigurationPrimaryAction, Ptr ()] ()
configurationForPassMetadata_provisioningPolicyIdentifier_primaryAction_completionSelector = mkSelector "configurationForPassMetadata:provisioningPolicyIdentifier:primaryAction:completion:"

-- | @Selector@ for @configurationForPassMetadata:primaryAction:completion:@
configurationForPassMetadata_primaryAction_completionSelector :: Selector '[Id NSArray, PKAddShareablePassConfigurationPrimaryAction, Ptr ()] ()
configurationForPassMetadata_primaryAction_completionSelector = mkSelector "configurationForPassMetadata:primaryAction:completion:"

-- | @Selector@ for @primaryAction@
primaryActionSelector :: Selector '[] PKAddShareablePassConfigurationPrimaryAction
primaryActionSelector = mkSelector "primaryAction"

-- | @Selector@ for @credentialsMetadata@
credentialsMetadataSelector :: Selector '[] (Id NSArray)
credentialsMetadataSelector = mkSelector "credentialsMetadata"

-- | @Selector@ for @provisioningPolicyIdentifier@
provisioningPolicyIdentifierSelector :: Selector '[] (Id NSString)
provisioningPolicyIdentifierSelector = mkSelector "provisioningPolicyIdentifier"

