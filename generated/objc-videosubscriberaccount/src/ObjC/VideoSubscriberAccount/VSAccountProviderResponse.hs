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
  , statusSelector
  , bodySelector


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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Identifies the protocol used in constructing this response.
--
-- ObjC selector: @- authenticationScheme@
authenticationScheme :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
authenticationScheme vsAccountProviderResponse  =
  sendMsg vsAccountProviderResponse (mkSelector "authenticationScheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The status code for this response. May be nil if there is no meaningful value for this type of response.
--
-- ObjC selector: @- status@
status :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
status vsAccountProviderResponse  =
  sendMsg vsAccountProviderResponse (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The raw response from the provider. May be nil if the response contained security-sensitive information.
--
-- ObjC selector: @- body@
body :: IsVSAccountProviderResponse vsAccountProviderResponse => vsAccountProviderResponse -> IO (Id NSString)
body vsAccountProviderResponse  =
  sendMsg vsAccountProviderResponse (mkSelector "body") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authenticationScheme@
authenticationSchemeSelector :: Selector
authenticationSchemeSelector = mkSelector "authenticationScheme"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @body@
bodySelector :: Selector
bodySelector = mkSelector "body"

