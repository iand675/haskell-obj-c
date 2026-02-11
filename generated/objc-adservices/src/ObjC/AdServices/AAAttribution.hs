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

import ObjC.AdServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Generates a token.
--
-- ObjC selector: @+ attributionTokenWithError:@
attributionTokenWithError :: IsNSError error_ => error_ -> IO (Id NSString)
attributionTokenWithError error_ =
  do
    cls' <- getRequiredClass "AAAttribution"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "attributionTokenWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributionTokenWithError:@
attributionTokenWithErrorSelector :: Selector
attributionTokenWithErrorSelector = mkSelector "attributionTokenWithError:"

