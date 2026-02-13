{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSController@.
module ObjC.AppKit.NSController
  ( NSController
  , IsNSController(..)
  , init_
  , initWithCoder
  , objectDidBeginEditing
  , objectDidEndEditing
  , discardEditing
  , commitEditing
  , commitEditingWithDelegate_didCommitSelector_contextInfo
  , editing
  , commitEditingSelector
  , commitEditingWithDelegate_didCommitSelector_contextInfoSelector
  , discardEditingSelector
  , editingSelector
  , initSelector
  , initWithCoderSelector
  , objectDidBeginEditingSelector
  , objectDidEndEditingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSController nsController => nsController -> IO (Id NSController)
init_ nsController =
  sendOwnedMessage nsController initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSController nsController, IsNSCoder coder) => nsController -> coder -> IO (Id NSController)
initWithCoder nsController coder =
  sendOwnedMessage nsController initWithCoderSelector (toNSCoder coder)

-- | @- objectDidBeginEditing:@
objectDidBeginEditing :: IsNSController nsController => nsController -> RawId -> IO ()
objectDidBeginEditing nsController editor =
  sendMessage nsController objectDidBeginEditingSelector editor

-- | @- objectDidEndEditing:@
objectDidEndEditing :: IsNSController nsController => nsController -> RawId -> IO ()
objectDidEndEditing nsController editor =
  sendMessage nsController objectDidEndEditingSelector editor

-- | @- discardEditing@
discardEditing :: IsNSController nsController => nsController -> IO ()
discardEditing nsController =
  sendMessage nsController discardEditingSelector

-- | @- commitEditing@
commitEditing :: IsNSController nsController => nsController -> IO Bool
commitEditing nsController =
  sendMessage nsController commitEditingSelector

-- | @- commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfo :: IsNSController nsController => nsController -> RawId -> Sel -> Ptr () -> IO ()
commitEditingWithDelegate_didCommitSelector_contextInfo nsController delegate didCommitSelector contextInfo =
  sendMessage nsController commitEditingWithDelegate_didCommitSelector_contextInfoSelector delegate didCommitSelector contextInfo

-- | @- editing@
editing :: IsNSController nsController => nsController -> IO Bool
editing nsController =
  sendMessage nsController editingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @objectDidBeginEditing:@
objectDidBeginEditingSelector :: Selector '[RawId] ()
objectDidBeginEditingSelector = mkSelector "objectDidBeginEditing:"

-- | @Selector@ for @objectDidEndEditing:@
objectDidEndEditingSelector :: Selector '[RawId] ()
objectDidEndEditingSelector = mkSelector "objectDidEndEditing:"

-- | @Selector@ for @discardEditing@
discardEditingSelector :: Selector '[] ()
discardEditingSelector = mkSelector "discardEditing"

-- | @Selector@ for @commitEditing@
commitEditingSelector :: Selector '[] Bool
commitEditingSelector = mkSelector "commitEditing"

-- | @Selector@ for @commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
commitEditingWithDelegate_didCommitSelector_contextInfoSelector = mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:"

-- | @Selector@ for @editing@
editingSelector :: Selector '[] Bool
editingSelector = mkSelector "editing"

