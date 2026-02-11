{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPredicateEditor@.
module ObjC.AppKit.NSPredicateEditor
  ( NSPredicateEditor
  , IsNSPredicateEditor(..)
  , rowTemplates
  , setRowTemplates
  , rowTemplatesSelector
  , setRowTemplatesSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rowTemplates@
rowTemplates :: IsNSPredicateEditor nsPredicateEditor => nsPredicateEditor -> IO (Id NSArray)
rowTemplates nsPredicateEditor  =
  sendMsg nsPredicateEditor (mkSelector "rowTemplates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRowTemplates:@
setRowTemplates :: (IsNSPredicateEditor nsPredicateEditor, IsNSArray value) => nsPredicateEditor -> value -> IO ()
setRowTemplates nsPredicateEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPredicateEditor (mkSelector "setRowTemplates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rowTemplates@
rowTemplatesSelector :: Selector
rowTemplatesSelector = mkSelector "rowTemplates"

-- | @Selector@ for @setRowTemplates:@
setRowTemplatesSelector :: Selector
setRowTemplatesSelector = mkSelector "setRowTemplates:"

