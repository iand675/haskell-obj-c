{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPersistableContentKeyRequest@.
module ObjC.AVFoundation.AVPersistableContentKeyRequest
  ( AVPersistableContentKeyRequest
  , IsAVPersistableContentKeyRequest(..)
  , persistableContentKeyFromKeyVendorResponse_options_error
  , persistableContentKeyFromKeyVendorResponse_options_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtains a persistable content key from a context.
--
-- The data returned from this method may be used to immediately satisfy an AVPersistableContentKeyRequest, as well as any subsequent requests for the same key url using processContentKeyResponse: method. When you receive an AVContentKeyRequest via -contentKeySession:didProvideContentKeyRequest: and you want to use existing persistent content key from storage, you must invoke -respondByRequestingPersistableContentKeyRequest on that AVContentKeyRequest in order to signal that you want to process an AVPersistableContentKeyRequest instead. If the underlying protocol supports persistable content keys, in response your delegate will receive an AVPersistableContentKeyRequest via -contentKeySession:didProvidePersistableContentKeyRequest:. You can set the persistent key from storage on the AVPersistableContentKeyRequest using processContentKeyResponse:.
--
-- - Parameter keyVendorResponse: The response returned from the key vendor as a result of a request generated from makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:. - Parameter options: Additional information necessary to obtain the persistable content key, or nil if none. - Parameter outError: If obtaining the persistable content key fails, will be set to an instance of NSError describing the failure.
--
-- - Returns: The persistable content key data that may be stored offline to answer future loading requests of the same content key.
--
-- ObjC selector: @- persistableContentKeyFromKeyVendorResponse:options:error:@
persistableContentKeyFromKeyVendorResponse_options_error :: (IsAVPersistableContentKeyRequest avPersistableContentKeyRequest, IsNSData keyVendorResponse, IsNSDictionary options, IsNSError outError) => avPersistableContentKeyRequest -> keyVendorResponse -> options -> outError -> IO (Id NSData)
persistableContentKeyFromKeyVendorResponse_options_error avPersistableContentKeyRequest keyVendorResponse options outError =
  sendMessage avPersistableContentKeyRequest persistableContentKeyFromKeyVendorResponse_options_errorSelector (toNSData keyVendorResponse) (toNSDictionary options) (toNSError outError)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistableContentKeyFromKeyVendorResponse:options:error:@
persistableContentKeyFromKeyVendorResponse_options_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] (Id NSData)
persistableContentKeyFromKeyVendorResponse_options_errorSelector = mkSelector "persistableContentKeyFromKeyVendorResponse:options:error:"

