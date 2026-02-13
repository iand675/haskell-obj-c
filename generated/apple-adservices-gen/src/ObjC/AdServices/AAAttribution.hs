{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The parent class that the framework uses to request a token.
--
-- Generated bindings for @AAAttribution@.
module ObjC.AdServices.AAAttribution
  ( AAAttribution
  , IsAAAttribution(..)
  , attributionTokenWithError
  , attributionTokenWithErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AdServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Generates a token.
--
-- ObjC selector: @+ attributionTokenWithError:@
attributionTokenWithError :: IsNSError error_ => error_ -> IO (Id NSString)
attributionTokenWithError error_ =
  do
    cls' <- getRequiredClass "AAAttribution"
    sendClassMessage cls' attributionTokenWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributionTokenWithError:@
attributionTokenWithErrorSelector :: Selector '[Id NSError] (Id NSString)
attributionTokenWithErrorSelector = mkSelector "attributionTokenWithError:"

