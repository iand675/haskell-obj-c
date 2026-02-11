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
  , initSelector
  , initWithCoderSelector
  , objectDidBeginEditingSelector
  , objectDidEndEditingSelector
  , discardEditingSelector
  , commitEditingSelector
  , commitEditingWithDelegate_didCommitSelector_contextInfoSelector
  , editingSelector


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

-- | @- init@
init_ :: IsNSController nsController => nsController -> IO (Id NSController)
init_ nsController  =
  sendMsg nsController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSController nsController, IsNSCoder coder) => nsController -> coder -> IO (Id NSController)
initWithCoder nsController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- objectDidBeginEditing:@
objectDidBeginEditing :: IsNSController nsController => nsController -> RawId -> IO ()
objectDidBeginEditing nsController  editor =
  sendMsg nsController (mkSelector "objectDidBeginEditing:") retVoid [argPtr (castPtr (unRawId editor) :: Ptr ())]

-- | @- objectDidEndEditing:@
objectDidEndEditing :: IsNSController nsController => nsController -> RawId -> IO ()
objectDidEndEditing nsController  editor =
  sendMsg nsController (mkSelector "objectDidEndEditing:") retVoid [argPtr (castPtr (unRawId editor) :: Ptr ())]

-- | @- discardEditing@
discardEditing :: IsNSController nsController => nsController -> IO ()
discardEditing nsController  =
  sendMsg nsController (mkSelector "discardEditing") retVoid []

-- | @- commitEditing@
commitEditing :: IsNSController nsController => nsController -> IO Bool
commitEditing nsController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsController (mkSelector "commitEditing") retCULong []

-- | @- commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfo :: IsNSController nsController => nsController -> RawId -> Selector -> Ptr () -> IO ()
commitEditingWithDelegate_didCommitSelector_contextInfo nsController  delegate didCommitSelector contextInfo =
  sendMsg nsController (mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didCommitSelector), argPtr contextInfo]

-- | @- editing@
editing :: IsNSController nsController => nsController -> IO Bool
editing nsController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsController (mkSelector "editing") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @objectDidBeginEditing:@
objectDidBeginEditingSelector :: Selector
objectDidBeginEditingSelector = mkSelector "objectDidBeginEditing:"

-- | @Selector@ for @objectDidEndEditing:@
objectDidEndEditingSelector :: Selector
objectDidEndEditingSelector = mkSelector "objectDidEndEditing:"

-- | @Selector@ for @discardEditing@
discardEditingSelector :: Selector
discardEditingSelector = mkSelector "discardEditing"

-- | @Selector@ for @commitEditing@
commitEditingSelector :: Selector
commitEditingSelector = mkSelector "commitEditing"

-- | @Selector@ for @commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfoSelector :: Selector
commitEditingWithDelegate_didCommitSelector_contextInfoSelector = mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:"

-- | @Selector@ for @editing@
editingSelector :: Selector
editingSelector = mkSelector "editing"

