{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerPlayParameters@.
module ObjC.MediaPlayer.MPMusicPlayerPlayParameters
  ( MPMusicPlayerPlayParameters
  , IsMPMusicPlayerPlayParameters(..)
  , initWithDictionary
  , dictionary
  , initWithDictionarySelector
  , dictionarySelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDictionary:@
initWithDictionary :: (IsMPMusicPlayerPlayParameters mpMusicPlayerPlayParameters, IsNSDictionary dictionary) => mpMusicPlayerPlayParameters -> dictionary -> IO (Id MPMusicPlayerPlayParameters)
initWithDictionary mpMusicPlayerPlayParameters  dictionary =
withObjCPtr dictionary $ \raw_dictionary ->
    sendMsg mpMusicPlayerPlayParameters (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ())] >>= ownedObject . castPtr

-- | @- dictionary@
dictionary :: IsMPMusicPlayerPlayParameters mpMusicPlayerPlayParameters => mpMusicPlayerPlayParameters -> IO (Id NSDictionary)
dictionary mpMusicPlayerPlayParameters  =
  sendMsg mpMusicPlayerPlayParameters (mkSelector "dictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector
dictionarySelector = mkSelector "dictionary"

