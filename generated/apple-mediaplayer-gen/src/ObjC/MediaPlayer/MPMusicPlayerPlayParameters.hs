{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerPlayParameters@.
module ObjC.MediaPlayer.MPMusicPlayerPlayParameters
  ( MPMusicPlayerPlayParameters
  , IsMPMusicPlayerPlayParameters(..)
  , initWithDictionary
  , dictionary
  , dictionarySelector
  , initWithDictionarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDictionary:@
initWithDictionary :: (IsMPMusicPlayerPlayParameters mpMusicPlayerPlayParameters, IsNSDictionary dictionary) => mpMusicPlayerPlayParameters -> dictionary -> IO (Id MPMusicPlayerPlayParameters)
initWithDictionary mpMusicPlayerPlayParameters dictionary =
  sendOwnedMessage mpMusicPlayerPlayParameters initWithDictionarySelector (toNSDictionary dictionary)

-- | @- dictionary@
dictionary :: IsMPMusicPlayerPlayParameters mpMusicPlayerPlayParameters => mpMusicPlayerPlayParameters -> IO (Id NSDictionary)
dictionary mpMusicPlayerPlayParameters =
  sendMessage mpMusicPlayerPlayParameters dictionarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id MPMusicPlayerPlayParameters)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector '[] (Id NSDictionary)
dictionarySelector = mkSelector "dictionary"

