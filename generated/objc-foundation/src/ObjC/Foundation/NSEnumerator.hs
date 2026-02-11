{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEnumerator@.
module ObjC.Foundation.NSEnumerator
  ( NSEnumerator
  , IsNSEnumerator(..)
  , nextObject
  , allObjects
  , nextObjectSelector
  , allObjectsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- nextObject@
nextObject :: IsNSEnumerator nsEnumerator => nsEnumerator -> IO RawId
nextObject nsEnumerator  =
  fmap (RawId . castPtr) $ sendMsg nsEnumerator (mkSelector "nextObject") (retPtr retVoid) []

-- | @- allObjects@
allObjects :: IsNSEnumerator nsEnumerator => nsEnumerator -> IO (Id NSArray)
allObjects nsEnumerator  =
  sendMsg nsEnumerator (mkSelector "allObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextObject@
nextObjectSelector :: Selector
nextObjectSelector = mkSelector "nextObject"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector
allObjectsSelector = mkSelector "allObjects"

