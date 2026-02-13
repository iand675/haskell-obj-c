{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddIdentityDocumentConfiguration@.
module ObjC.PassKit.PKAddIdentityDocumentConfiguration
  ( PKAddIdentityDocumentConfiguration
  , IsPKAddIdentityDocumentConfiguration(..)
  , configurationForMetadata_completion
  , metadata
  , configurationForMetadata_completionSelector
  , metadataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize identity document configuration with corresponding identity document metadata. - Parameters:   - metadata: Set of configured metadata defining the required information to add the corresponding pass to Wallet.   - completion: Returns the identity document configuration if successful, otherwise returns an error.
--
-- ObjC selector: @+ configurationForMetadata:completion:@
configurationForMetadata_completion :: IsPKIdentityDocumentMetadata metadata => metadata -> Ptr () -> IO ()
configurationForMetadata_completion metadata completion =
  do
    cls' <- getRequiredClass "PKAddIdentityDocumentConfiguration"
    sendClassMessage cls' configurationForMetadata_completionSelector (toPKIdentityDocumentMetadata metadata) completion

-- | metadata: Set of configured metadata defining the required information to add the corresponding pass to Wallet.
--
-- ObjC selector: @- metadata@
metadata :: IsPKAddIdentityDocumentConfiguration pkAddIdentityDocumentConfiguration => pkAddIdentityDocumentConfiguration -> IO (Id PKIdentityDocumentMetadata)
metadata pkAddIdentityDocumentConfiguration =
  sendMessage pkAddIdentityDocumentConfiguration metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationForMetadata:completion:@
configurationForMetadata_completionSelector :: Selector '[Id PKIdentityDocumentMetadata, Ptr ()] ()
configurationForMetadata_completionSelector = mkSelector "configurationForMetadata:completion:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id PKIdentityDocumentMetadata)
metadataSelector = mkSelector "metadata"

