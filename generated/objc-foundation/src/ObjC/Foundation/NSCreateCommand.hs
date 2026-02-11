{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCreateCommand@.
module ObjC.Foundation.NSCreateCommand
  ( NSCreateCommand
  , IsNSCreateCommand(..)
  , createClassDescription
  , resolvedKeyDictionary
  , createClassDescriptionSelector
  , resolvedKeyDictionarySelector


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

import ObjC.Foundation.Internal.Classes

-- | @- createClassDescription@
createClassDescription :: IsNSCreateCommand nsCreateCommand => nsCreateCommand -> IO (Id NSScriptClassDescription)
createClassDescription nsCreateCommand  =
  sendMsg nsCreateCommand (mkSelector "createClassDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resolvedKeyDictionary@
resolvedKeyDictionary :: IsNSCreateCommand nsCreateCommand => nsCreateCommand -> IO (Id NSDictionary)
resolvedKeyDictionary nsCreateCommand  =
  sendMsg nsCreateCommand (mkSelector "resolvedKeyDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createClassDescription@
createClassDescriptionSelector :: Selector
createClassDescriptionSelector = mkSelector "createClassDescription"

-- | @Selector@ for @resolvedKeyDictionary@
resolvedKeyDictionarySelector :: Selector
resolvedKeyDictionarySelector = mkSelector "resolvedKeyDictionary"

