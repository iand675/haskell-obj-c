{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHChange@.
module ObjC.Photos.PHChange
  ( PHChange
  , IsPHChange(..)
  , changeDetailsForObject
  , changeDetailsForFetchResult
  , changeDetailsForFetchResultSelector
  , changeDetailsForObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeDetailsForObject:@
changeDetailsForObject :: (IsPHChange phChange, IsPHObject object) => phChange -> object -> IO (Id PHObjectChangeDetails)
changeDetailsForObject phChange object =
  sendMessage phChange changeDetailsForObjectSelector (toPHObject object)

-- | @- changeDetailsForFetchResult:@
changeDetailsForFetchResult :: (IsPHChange phChange, IsPHFetchResult object) => phChange -> object -> IO (Id PHFetchResultChangeDetails)
changeDetailsForFetchResult phChange object =
  sendMessage phChange changeDetailsForFetchResultSelector (toPHFetchResult object)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeDetailsForObject:@
changeDetailsForObjectSelector :: Selector '[Id PHObject] (Id PHObjectChangeDetails)
changeDetailsForObjectSelector = mkSelector "changeDetailsForObject:"

-- | @Selector@ for @changeDetailsForFetchResult:@
changeDetailsForFetchResultSelector :: Selector '[Id PHFetchResult] (Id PHFetchResultChangeDetails)
changeDetailsForFetchResultSelector = mkSelector "changeDetailsForFetchResult:"

