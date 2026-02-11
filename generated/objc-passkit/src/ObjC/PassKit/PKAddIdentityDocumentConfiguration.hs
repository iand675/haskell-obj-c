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

-- | Initialize identity document configuration with corresponding identity document metadata. - Parameters:   - metadata: Set of configured metadata defining the required information to add the corresponding pass to Wallet.   - completion: Returns the identity document configuration if successful, otherwise returns an error.
--
-- ObjC selector: @+ configurationForMetadata:completion:@
configurationForMetadata_completion :: IsPKIdentityDocumentMetadata metadata => metadata -> Ptr () -> IO ()
configurationForMetadata_completion metadata completion =
  do
    cls' <- getRequiredClass "PKAddIdentityDocumentConfiguration"
    withObjCPtr metadata $ \raw_metadata ->
      sendClassMsg cls' (mkSelector "configurationForMetadata:completion:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | metadata: Set of configured metadata defining the required information to add the corresponding pass to Wallet.
--
-- ObjC selector: @- metadata@
metadata :: IsPKAddIdentityDocumentConfiguration pkAddIdentityDocumentConfiguration => pkAddIdentityDocumentConfiguration -> IO (Id PKIdentityDocumentMetadata)
metadata pkAddIdentityDocumentConfiguration  =
  sendMsg pkAddIdentityDocumentConfiguration (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationForMetadata:completion:@
configurationForMetadata_completionSelector :: Selector
configurationForMetadata_completionSelector = mkSelector "configurationForMetadata:completion:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

