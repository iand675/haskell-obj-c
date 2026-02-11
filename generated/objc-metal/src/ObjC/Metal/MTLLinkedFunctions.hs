{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLLinkedFunctions
--
-- A class to set functions to be linked.
--
-- All functions set on this object must have unique names.
--
-- Generated bindings for @MTLLinkedFunctions@.
module ObjC.Metal.MTLLinkedFunctions
  ( MTLLinkedFunctions
  , IsMTLLinkedFunctions(..)
  , linkedFunctions
  , linkedFunctionsSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | linkedFunctions
--
-- Create an autoreleased MTLLinkedFunctions object.
--
-- ObjC selector: @+ linkedFunctions@
linkedFunctions :: IO (Id MTLLinkedFunctions)
linkedFunctions  =
  do
    cls' <- getRequiredClass "MTLLinkedFunctions"
    sendClassMsg cls' (mkSelector "linkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @linkedFunctions@
linkedFunctionsSelector :: Selector
linkedFunctionsSelector = mkSelector "linkedFunctions"

