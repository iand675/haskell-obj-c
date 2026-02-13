{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTAHeaderParser@.
module ObjC.Matter.MTROTAHeaderParser
  ( MTROTAHeaderParser
  , IsMTROTAHeaderParser(..)
  , headerFromData_error
  , headerFromData_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ headerFromData:error:@
headerFromData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO (Id MTROTAHeader)
headerFromData_error data_ error_ =
  do
    cls' <- getRequiredClass "MTROTAHeaderParser"
    sendClassMessage cls' headerFromData_errorSelector (toNSData data_) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @headerFromData:error:@
headerFromData_errorSelector :: Selector '[Id NSData, Id NSError] (Id MTROTAHeader)
headerFromData_errorSelector = mkSelector "headerFromData:error:"

