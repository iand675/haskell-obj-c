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

-- | @- identifier@
identifier :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
identifier domCounter  =
  sendMsg domCounter (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- listStyle@
listStyle :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
listStyle domCounter  =
  sendMsg domCounter (mkSelector "listStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- separator@
separator :: IsDOMCounter domCounter => domCounter -> IO (Id NSString)
separator domCounter  =
  sendMsg domCounter (mkSelector "separator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @listStyle@
listStyleSelector :: Selector
listStyleSelector = mkSelector "listStyle"

-- | @Selector@ for @separator@
separatorSelector :: Selector
separatorSelector = mkSelector "separator"

