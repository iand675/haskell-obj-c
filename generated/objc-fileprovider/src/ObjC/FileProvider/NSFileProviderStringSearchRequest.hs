{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderStringSearchRequest@.
module ObjC.FileProvider.NSFileProviderStringSearchRequest
  ( NSFileProviderStringSearchRequest
  , IsNSFileProviderStringSearchRequest(..)
  , query
  , desiredNumberOfResults
  , querySelector
  , desiredNumberOfResultsSelector


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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A plaintext string, representing the query the user entered into the system search UI.
--
-- ObjC selector: @- query@
query :: IsNSFileProviderStringSearchRequest nsFileProviderStringSearchRequest => nsFileProviderStringSearchRequest -> IO (Id NSString)
query nsFileProviderStringSearchRequest  =
  sendMsg nsFileProviderStringSearchRequest (mkSelector "query") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | How many results the system is requesting. This is a hint to the extension, to help avoid unnecessary work. The extension may return more results than this.
--
-- ObjC selector: @- desiredNumberOfResults@
desiredNumberOfResults :: IsNSFileProviderStringSearchRequest nsFileProviderStringSearchRequest => nsFileProviderStringSearchRequest -> IO CLong
desiredNumberOfResults nsFileProviderStringSearchRequest  =
  sendMsg nsFileProviderStringSearchRequest (mkSelector "desiredNumberOfResults") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @query@
querySelector :: Selector
querySelector = mkSelector "query"

-- | @Selector@ for @desiredNumberOfResults@
desiredNumberOfResultsSelector :: Selector
desiredNumberOfResultsSelector = mkSelector "desiredNumberOfResults"

