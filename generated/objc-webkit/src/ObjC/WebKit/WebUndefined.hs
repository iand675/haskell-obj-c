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
    sendClassMsg cls' (mkSelector "undefined") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @undefined@
undefinedSelector :: Selector
undefinedSelector = mkSelector "undefined"

