{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionDataTask@.
module ObjC.Foundation.NSURLSessionDataTask
  ( NSURLSessionDataTask
  , IsNSURLSessionDataTask(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSURLSessionDataTask nsurlSessionDataTask => nsurlSessionDataTask -> IO (Id NSURLSessionDataTask)
init_ nsurlSessionDataTask =
  sendOwnedMessage nsurlSessionDataTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionDataTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionDataTask"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionDataTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionDataTask)
newSelector = mkSelector "new"

