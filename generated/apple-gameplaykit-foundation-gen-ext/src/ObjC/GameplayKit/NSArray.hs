{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Array		***************
--
-- Generated bindings for @NSArray@.
module ObjC.GameplayKit.NSArray
  ( NSArray
  , IsNSArray(..)
  , shuffledArrayWithRandomSource
  , shuffledArray
  , shuffledArraySelector
  , shuffledArrayWithRandomSourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- shuffledArrayWithRandomSource:@
shuffledArrayWithRandomSource :: (IsNSArray nsArray, IsGKRandomSource randomSource) => nsArray -> randomSource -> IO (Id NSArray)
shuffledArrayWithRandomSource nsArray randomSource =
  sendMessage nsArray shuffledArrayWithRandomSourceSelector (toGKRandomSource randomSource)

-- | @- shuffledArray@
shuffledArray :: IsNSArray nsArray => nsArray -> IO (Id NSArray)
shuffledArray nsArray =
  sendMessage nsArray shuffledArraySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shuffledArrayWithRandomSource:@
shuffledArrayWithRandomSourceSelector :: Selector '[Id GKRandomSource] (Id NSArray)
shuffledArrayWithRandomSourceSelector = mkSelector "shuffledArrayWithRandomSource:"

-- | @Selector@ for @shuffledArray@
shuffledArraySelector :: Selector '[] (Id NSArray)
shuffledArraySelector = mkSelector "shuffledArray"

