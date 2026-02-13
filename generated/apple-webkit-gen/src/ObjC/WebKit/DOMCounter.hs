{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCounter@.
module ObjC.WebKit.DOMCounter
  ( DOMCounter
  , IsDOMCounter(..)
  , identifier
  , listStyle
  , separator
  , identifierSelector
  , listStyleSelector
  , separatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
identifier domCounter =
  sendMessage domCounter identifierSelector

-- | @- listStyle@
listStyle :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
listStyle domCounter =
  sendMessage domCounter listStyleSelector

-- | @- separator@
separator :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
separator domCounter =
  sendMessage domCounter separatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @listStyle@
listStyleSelector :: Selector '[] (Id NSString)
listStyleSelector = mkSelector "listStyle"

-- | @Selector@ for @separator@
separatorSelector :: Selector '[] (Id NSString)
separatorSelector = mkSelector "separator"

