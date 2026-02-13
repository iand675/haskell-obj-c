{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A value object that encapsulates the response given by an account provider.
--
-- Generated bindings for @VSAccountProviderResponse@.
module ObjC.VideoSubscriberAccount.VSAccountProviderResponse
  ( VSAccountProviderResponse
  , IsVSAccountProviderResponse(..)
  , authenticationScheme
  , status
  , body
  , authenticationSchemeSelector
  , bodySelector
  , statusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Identifies the protocol used in constructing this response.
--
-- ObjC selector: @- authenticationScheme@
authenticationScheme :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
authenticationScheme vsAccountProviderResponse =
  sendMessage vsAccountProviderResponse authenticationSchemeSelector

-- | The status code for this response. May be nil if there is no meaningful value for this type of response.
--
-- ObjC selector: @- status@
status :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
status vsAccountProviderResponse =
  sendMessage vsAccountProviderResponse statusSelector

-- | The raw response from the provider. May be nil if the response contained security-sensitive information.
--
-- ObjC selector: @- body@
body :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
body vsAccountProviderResponse =
  sendMessage vsAccountProviderResponse bodySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authenticationScheme@
authenticationSchemeSelector :: Selector '[] (Id NSString)
authenticationSchemeSelector = mkSelector "authenticationScheme"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSString)
statusSelector = mkSelector "status"

-- | @Selector@ for @body@
bodySelector :: Selector '[] (Id NSString)
bodySelector = mkSelector "body"

