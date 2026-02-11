{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMProgressEvent@.
module ObjC.WebKit.DOMProgressEvent
  ( DOMProgressEvent
  , IsDOMProgressEvent(..)
  , lengthComputable
  , loaded
  , total
  , lengthComputableSelector
  , loadedSelector
  , totalSelector


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

-- | @- lengthComputable@
lengthComputable :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO Bool
lengthComputable domProgressEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domProgressEvent (mkSelector "lengthComputable") retCULong []

-- | @- loaded@
loaded :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO CULong
loaded domProgressEvent  =
  sendMsg domProgressEvent (mkSelector "loaded") retCULong []

-- | @- total@
total :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO CULong
total domProgressEvent  =
  sendMsg domProgressEvent (mkSelector "total") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lengthComputable@
lengthComputableSelector :: Selector
lengthComputableSelector = mkSelector "lengthComputable"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @total@
totalSelector :: Selector
totalSelector = mkSelector "total"

