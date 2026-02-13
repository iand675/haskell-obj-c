{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebUndefined
--
-- Generated bindings for @WebUndefined@.
module ObjC.WebKit.WebUndefined
  ( WebUndefined
  , IsWebUndefined(..)
  , undefined_
  , undefinedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | undefined
--
-- Returns: The WebUndefined shared instance.
--
-- ObjC selector: @+ undefined@
undefined_ :: IO (Id WebUndefined)
undefined_  =
  do
    cls' <- getRequiredClass "WebUndefined"
    sendClassMessage cls' undefinedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @undefined@
undefinedSelector :: Selector '[] (Id WebUndefined)
undefinedSelector = mkSelector "undefined"

