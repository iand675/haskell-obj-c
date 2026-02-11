{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKNode@.
module ObjC.GameplayKit.SKNode
  ( SKNode
  , IsSKNode(..)
  , entity
  , setEntity
  , entitySelector
  , setEntitySelector


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

import ObjC.GameplayKit.Internal.Classes

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- entity@
entity :: IsSKNode skNode => skNode -> IO RawId
entity skNode  =
    fmap (RawId . castPtr) $ sendMsg skNode (mkSelector "entity") (retPtr retVoid) []

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- setEntity:@
setEntity :: IsSKNode skNode => skNode -> RawId -> IO ()
setEntity skNode  value =
    sendMsg skNode (mkSelector "setEntity:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector
setEntitySelector = mkSelector "setEntity:"

