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
  , setBackwardsSelector
  , caseSensitiveSelector
  , setCaseSensitiveSelector
  , wrapsSelector
  , setWrapsSelector


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

-- | @- backwards@
backwards :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
backwards wkFindConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkFindConfiguration (mkSelector "backwards") retCULong []

-- | @- setBackwards:@
setBackwards :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setBackwards wkFindConfiguration  value =
  sendMsg wkFindConfiguration (mkSelector "setBackwards:") retVoid [argCULong (if value then 1 else 0)]

-- | @- caseSensitive@
caseSensitive :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
caseSensitive wkFindConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkFindConfiguration (mkSelector "caseSensitive") retCULong []

-- | @- setCaseSensitive:@
setCaseSensitive :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setCaseSensitive wkFindConfiguration  value =
  sendMsg wkFindConfiguration (mkSelector "setCaseSensitive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- wraps@
wraps :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> IO Bool
wraps wkFindConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkFindConfiguration (mkSelector "wraps") retCULong []

-- | @- setWraps:@
setWraps :: IsWKFindConfiguration wkFindConfiguration => wkFindConfiguration -> Bool -> IO ()
setWraps wkFindConfiguration  value =
  sendMsg wkFindConfiguration (mkSelector "setWraps:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backwards@
backwardsSelector :: Selector
backwardsSelector = mkSelector "backwards"

-- | @Selector@ for @setBackwards:@
setBackwardsSelector :: Selector
setBackwardsSelector = mkSelector "setBackwards:"

-- | @Selector@ for @caseSensitive@
caseSensitiveSelector :: Selector
caseSensitiveSelector = mkSelector "caseSensitive"

-- | @Selector@ for @setCaseSensitive:@
setCaseSensitiveSelector :: Selector
setCaseSensitiveSelector = mkSelector "setCaseSensitive:"

-- | @Selector@ for @wraps@
wrapsSelector :: Selector
wrapsSelector = mkSelector "wraps"

-- | @Selector@ for @setWraps:@
setWrapsSelector :: Selector
setWrapsSelector = mkSelector "setWraps:"

