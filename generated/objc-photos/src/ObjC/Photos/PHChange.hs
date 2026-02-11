{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHChange@.
module ObjC.Photos.PHChange
  ( PHChange
  , IsPHChange(..)
  , changeDetailsForObject
  , changeDetailsForFetchResult
  , changeDetailsForObjectSelector
  , changeDetailsForFetchResultSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeDetailsForObject:@
changeDetailsForObject :: (IsPHChange phChange, IsPHObject object) => phChange -> object -> IO (Id PHObjectChangeDetails)
changeDetailsForObject phChange  object =
withObjCPtr object $ \raw_object ->
    sendMsg phChange (mkSelector "changeDetailsForObject:") (retPtr retVoid) [argPtr (castPtr raw_object :: Ptr ())] >>= retainedObject . castPtr

-- | @- changeDetailsForFetchResult:@
changeDetailsForFetchResult :: (IsPHChange phChange, IsPHFetchResult object) => phChange -> object -> IO (Id PHFetchResultChangeDetails)
changeDetailsForFetchResult phChange  object =
withObjCPtr object $ \raw_object ->
    sendMsg phChange (mkSelector "changeDetailsForFetchResult:") (retPtr retVoid) [argPtr (castPtr raw_object :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeDetailsForObject:@
changeDetailsForObjectSelector :: Selector
changeDetailsForObjectSelector = mkSelector "changeDetailsForObject:"

-- | @Selector@ for @changeDetailsForFetchResult:@
changeDetailsForFetchResultSelector :: Selector
changeDetailsForFetchResultSelector = mkSelector "changeDetailsForFetchResult:"

