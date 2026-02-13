{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rowTemplates@
rowTemplates :: IsNSPredicateEditor nsPredicateEditor => nsPredicateEditor -> IO (Id NSArray)
rowTemplates nsPredicateEditor =
  sendMessage nsPredicateEditor rowTemplatesSelector

-- | @- setRowTemplates:@
setRowTemplates :: (IsNSPredicateEditor nsPredicateEditor, IsNSArray value) => nsPredicateEditor -> value -> IO ()
setRowTemplates nsPredicateEditor value =
  sendMessage nsPredicateEditor setRowTemplatesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rowTemplates@
rowTemplatesSelector :: Selector '[] (Id NSArray)
rowTemplatesSelector = mkSelector "rowTemplates"

-- | @Selector@ for @setRowTemplates:@
setRowTemplatesSelector :: Selector '[Id NSArray] ()
setRowTemplatesSelector = mkSelector "setRowTemplates:"

