{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKFindConfiguration@.
module ObjC.WebKit.WKFindConfiguration
  ( WKFindConfiguration
  , IsWKFindConfiguration(..)
  , backwards
  , setBackwards
  , caseSensitive
  , setCaseSensitive
  , wraps
  , setWraps
  , backwardsSelector
  , caseSensitiveSelector
  , setBackwardsSelector
  , setCaseSensitiveSelector
  , setWrapsSelector
  , wrapsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- backwards@
backwards :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
backwards wkFindConfiguration =
  sendMessage wkFindConfiguration backwardsSelector

-- | @- setBackwards:@
setBackwards :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setBackwards wkFindConfiguration value =
  sendMessage wkFindConfiguration setBackwardsSelector value

-- | @- caseSensitive@
caseSensitive :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
caseSensitive wkFindConfiguration =
  sendMessage wkFindConfiguration caseSensitiveSelector

-- | @- setCaseSensitive:@
setCaseSensitive :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setCaseSensitive wkFindConfiguration value =
  sendMessage wkFindConfiguration setCaseSensitiveSelector value

-- | @- wraps@
wraps :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
wraps wkFindConfiguration =
  sendMessage wkFindConfiguration wrapsSelector

-- | @- setWraps:@
setWraps :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setWraps wkFindConfiguration value =
  sendMessage wkFindConfiguration setWrapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backwards@
backwardsSelector :: Selector '[] Bool
backwardsSelector = mkSelector "backwards"

-- | @Selector@ for @setBackwards:@
setBackwardsSelector :: Selector '[Bool] ()
setBackwardsSelector = mkSelector "setBackwards:"

-- | @Selector@ for @caseSensitive@
caseSensitiveSelector :: Selector '[] Bool
caseSensitiveSelector = mkSelector "caseSensitive"

-- | @Selector@ for @setCaseSensitive:@
setCaseSensitiveSelector :: Selector '[Bool] ()
setCaseSensitiveSelector = mkSelector "setCaseSensitive:"

-- | @Selector@ for @wraps@
wrapsSelector :: Selector '[] Bool
wrapsSelector = mkSelector "wraps"

-- | @Selector@ for @setWraps:@
setWrapsSelector :: Selector '[Bool] ()
setWrapsSelector = mkSelector "setWraps:"

