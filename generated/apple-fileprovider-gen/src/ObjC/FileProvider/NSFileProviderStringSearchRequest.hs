{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderStringSearchRequest@.
module ObjC.FileProvider.NSFileProviderStringSearchRequest
  ( NSFileProviderStringSearchRequest
  , IsNSFileProviderStringSearchRequest(..)
  , query
  , desiredNumberOfResults
  , desiredNumberOfResultsSelector
  , querySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A plaintext string, representing the query the user entered into the system search UI.
--
-- ObjC selector: @- query@
query :: IsNSFileProviderStringSearchRequest nsFileProviderStringSearchRequest => nsFileProviderStringSearchRequest -> IO (Id NSString)
query nsFileProviderStringSearchRequest =
  sendMessage nsFileProviderStringSearchRequest querySelector

-- | How many results the system is requesting. This is a hint to the extension, to help avoid unnecessary work. The extension may return more results than this.
--
-- ObjC selector: @- desiredNumberOfResults@
desiredNumberOfResults :: IsNSFileProviderStringSearchRequest nsFileProviderStringSearchRequest => nsFileProviderStringSearchRequest -> IO CLong
desiredNumberOfResults nsFileProviderStringSearchRequest =
  sendMessage nsFileProviderStringSearchRequest desiredNumberOfResultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @query@
querySelector :: Selector '[] (Id NSString)
querySelector = mkSelector "query"

-- | @Selector@ for @desiredNumberOfResults@
desiredNumberOfResultsSelector :: Selector '[] CLong
desiredNumberOfResultsSelector = mkSelector "desiredNumberOfResults"

