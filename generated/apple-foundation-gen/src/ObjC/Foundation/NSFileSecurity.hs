{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileSecurity@.
module ObjC.Foundation.NSFileSecurity
  ( NSFileSecurity
  , IsNSFileSecurity(..)
  , initWithCoder
  , initWithCoderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsNSFileSecurity nsFileSecurity, IsNSCoder coder) => nsFileSecurity -> coder -> IO (Id NSFileSecurity)
initWithCoder nsFileSecurity coder =
  sendOwnedMessage nsFileSecurity initWithCoderSelector (toNSCoder coder)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSFileSecurity)
initWithCoderSelector = mkSelector "initWithCoder:"

