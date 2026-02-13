{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- lengthComputable@
lengthComputable :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO Bool
lengthComputable domProgressEvent =
  sendMessage domProgressEvent lengthComputableSelector

-- | @- loaded@
loaded :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO CULong
loaded domProgressEvent =
  sendMessage domProgressEvent loadedSelector

-- | @- total@
total :: IsDOMProgressEvent domProgressEvent => domProgressEvent -> IO CULong
total domProgressEvent =
  sendMessage domProgressEvent totalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lengthComputable@
lengthComputableSelector :: Selector '[] Bool
lengthComputableSelector = mkSelector "lengthComputable"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] CULong
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @total@
totalSelector :: Selector '[] CULong
totalSelector = mkSelector "total"

